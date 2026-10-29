"""Agent loop: improve PyParser.y / PyLexer.x grammar coverage.

Python sibling of `java_agent_loop.py`. Same strict-progress methodology,
same OpenAI structured-output patch schema, same snapshot / apply /
rebuild / regate state machine, same dhscanner.runner compose chain.
Only the language-specific plumbing differs:

                        java_agent_loop.py       py_agent_loop.py (this file)
  front service         frontjava :5005          frontpy :5006
  front endpoint        /to/native/java/ast      /to/native/py/ast
  parsers endpoint      /from/java/to/dhscanner  /from/py/to/dhscanner
  native AST shape      MULTI-LINE JSON          SINGLE-LINE `ast.dump(...,
                                                 include_attributes=True)`
                                                 text, so the failure is a
                                                 char offset on line 1 and
                                                 the (line, col) gate reduces
                                                 to col alone ( same as TS )
  editable files        src/JavaParser.y +       src/PyParser.y +
                        src/JavaParserActions.hs src/PyLexer.x
  lexer edits           forbidden                ALLOWED : every missing
                                                 Python construct is a new
                                                 node name ( NamedExpr,
                                                 YieldFrom, TypeAlias, ... ),
                                                 ie a new lexer keyword
  default corpus        DSpace                   Weblate
  cached baseline       .dev/java-baseline.json  .dev/py-baseline.json
  iterations log        .dev/java-iterations/    .dev/py-iterations/
  snapshot              .dev/java-snapshot/      .dev/py-snapshot/

The baseline also records, per failing file, the native AST node name at
the failure offset ( eg `NamedExpr` ). It is printed as a histogram, and
`--target-node NamedExpr` makes the picker choose only files that fail
on that node.

For one iteration this script:

  1. Builds ( or loads the cached ) baseline of {file -> parse outcome}
     over the production `*.py` files at --corpus ( tests, migrations and
     vendored / build dirs excluded ).
  2. Picks a failing file, reads its native AST and the failure offset.
  3. Asks OpenAI for a minimal patch to PyParser.y and/or PyLexer.x.
  4. Snapshots the editable files, applies the patch, rebuilds ONLY the
     `parsers` service via the runner compose chain + the dev override,
     polls /healthcheck, re-parses the entire corpus.
  5. Strict-progress gate ( per AGENTS.md ): every passing file still
     passes, every failing file passes or fails at a column >= before,
     and at least one file strictly progresses. On pass the new state
     becomes the baseline. Apply / build failures revert the edits;
     gate rejections leave them in place for post-mortem.

Usage (PowerShell):
  cd dhscanner.runner
  docker compose --env-file .env `
    -f ./compose/compose.base.yaml `
    -f ./compose/compose.app.yaml `
    -f ./compose/compose.fronts.yaml `
    -f ./compose/compose.prebuilt.yaml `
    -f ./compose/compose.workers.yaml `
    -f ./compose/compose.parsers-dev.yaml `
    up -d --build                                 # one-time
  python dhscanner.core/dhscanner.service.parsers/py_agent_loop.py --baseline-only
  python dhscanner.core/dhscanner.service.parsers/py_agent_loop.py --target-node NamedExpr
"""

from __future__ import annotations

import argparse
import collections
import hashlib
import json
import os
import random
import re
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import requests


HERE = Path(__file__).resolve().parent
SRC_DIR = HERE / "src"
DEV_DIR = HERE / ".dev"
BASELINE_PATH = DEV_DIR / "py-baseline.json"
ITERATIONS_DIR = DEV_DIR / "py-iterations"
SNAPSHOT_DIR = DEV_DIR / "py-snapshot"

# HERE is `<runner>/dhscanner.core/dhscanner.service.parsers`.
DEFAULT_RUNNER_ROOT = HERE.parents[1]

DEFAULT_CORPUS_DIR = Path(r"C:\Users\tuna_\GitHub\Weblate")
DEFAULT_MODEL = "gpt-5"
DEFAULT_CONTEXT_RADIUS = 600
HEALTHCHECK_TIMEOUT_S = 180

PARSERS_SERVICE = "parsers"

RUNNER_COMPOSE_FILES: tuple[str, ...] = (
    "compose/compose.base.yaml",
    "compose/compose.app.yaml",
    "compose/compose.fronts.yaml",
    "compose/compose.prebuilt.yaml",
    "compose/compose.workers.yaml",
    "compose/compose.parsers-dev.yaml",
)

EDITABLE_RELPATHS: tuple[str, ...] = (
    "src/PyParser.y",
    "src/PyLexer.x",
)

GRAMMAR_HASH_RELPATHS: tuple[str, ...] = EDITABLE_RELPATHS

GRAMMAR_PATCH_SCHEMA: dict[str, Any] = {
    "name": "GrammarPatch",
    "strict": True,
    "schema": {
        "type": "object",
        "additionalProperties": False,
        "properties": {
            "rationale": {"type": "string"},
            "edits": {
                "type": "array",
                "items": {
                    "type": "object",
                    "additionalProperties": False,
                    "properties": {
                        "file": {
                            "type": "string",
                            "enum": list(EDITABLE_RELPATHS),
                        },
                        "old_string": {"type": "string"},
                        "new_string": {"type": "string"},
                    },
                    "required": ["file", "old_string", "new_string"],
                },
            },
        },
        "required": ["rationale", "edits"],
    },
}


# --------------------------------------------------------------------------- #
# Data types                                                                  #
# --------------------------------------------------------------------------- #


@dataclass
class ParseResult:
    status: str          # "ok" | "fail" | "frontpy_fail" | "transport_fail"
    col_start: int | None
    line_start: int | None
    native_ast_len: int | None
    detail: str | None   # short human-readable detail for fails
    node: str | None = None  # native node name at the failure offset

    def to_baseline(self) -> dict[str, Any]:
        return {
            "status": self.status,
            "col_start": self.col_start,
            "line_start": self.line_start,
            "node": self.node,
        }


# --------------------------------------------------------------------------- #
# Corpus discovery                                                            #
# --------------------------------------------------------------------------- #


# Path components we never descend into. Tests are dropped for the same
# reason the other loops drop them ( adversarial constructs skew the picker );
# Django migrations are generated code with no attack surface.
EXCLUDED_PATH_COMPONENTS: frozenset[str] = frozenset({
    "tests",
    "test",
    "migrations",
    "node_modules",
    ".venv",
    "venv",
    ".tox",
    "build",
    "dist",
    "site-packages",
    "__pycache__",
    ".git",
})


def is_test_basename(name: str) -> bool:
    return (
        name in ("tests.py", "conftest.py")
        or name.startswith("test_")
        or name.endswith("_test.py")
        or name.endswith("_tests.py")
    )


def discover_corpus(corpus_dir: Path) -> list[Path]:
    """Return production `*.py` files under corpus_dir."""
    if not corpus_dir.is_dir():
        raise SystemExit(f"corpus directory not found: {corpus_dir}")

    out: list[Path] = []
    for p in corpus_dir.rglob("*.py"):
        rel_parts = set(p.relative_to(corpus_dir).parts)
        if rel_parts & EXCLUDED_PATH_COMPONENTS:
            continue
        if is_test_basename(p.name):
            continue
        out.append(p)
    out.sort()
    return out


# --------------------------------------------------------------------------- #
# HTTP clients                                                                #
# --------------------------------------------------------------------------- #


def frontpy_native_ast(
    session: requests.Session,
    frontpy_url: str,
    file_path: Path,
    timeout: float,
) -> tuple[str | None, str | None]:
    """POST source file to frontpy; return (native_ast, error_detail).

    frontpy ( dhscanner.core/dhscanner.service.fronts/py/main.py ) answers
    200 with the `ast.dump` text, or 500 when `ast.parse` itself raises
    ( eg a syntax error, or a construct newer than its Python 3.12 )."""
    try:
        with file_path.open("rb") as fl:
            files = {"source": (file_path.name, fl, "text/plain")}
            resp = session.post(
                f"{frontpy_url}/to/native/py/ast",
                files=files,
                timeout=timeout,
            )
    except requests.RequestException as exc:
        return None, f"frontpy transport error: {exc}"

    if resp.status_code != 200:
        return None, f"frontpy http {resp.status_code}: {resp.text[:200]}"
    return resp.text, None


def parsers_translate(
    session: requests.Session,
    parsers_url: str,
    filename: str,
    native_ast: str,
    timeout: float,
) -> ParseResult:
    """POST native AST to parsers /from/py/to/dhscanner/ast."""
    body = {
        "filename": filename,
        "content": native_ast,
        "optional_github_url": None,
        "source_containing_dirs": [],
        "all_filenames": [filename],
        "path_mappings": None,
    }
    try:
        resp = session.post(
            f"{parsers_url}/from/py/to/dhscanner/ast",
            json=body,
            timeout=timeout,
            headers={"Content-Type": "application/json"},
        )
    except requests.RequestException as exc:
        return ParseResult("transport_fail", None, None, len(native_ast), str(exc))

    if resp.status_code != 200:
        return ParseResult(
            "transport_fail",
            None,
            None,
            len(native_ast),
            f"http {resp.status_code}: {resp.text[:200]}",
        )

    try:
        data = resp.json()
    except json.JSONDecodeError:
        return ParseResult(
            "transport_fail",
            None,
            None,
            len(native_ast),
            f"non-json response: {resp.text[:200]}",
        )

    if isinstance(data, dict) and data.get("status") == "FAILED":
        loc = data.get("location") or {}
        col = loc.get("colStart")
        return ParseResult(
            status="fail",
            col_start=col,
            line_start=loc.get("lineStart"),
            native_ast_len=len(native_ast),
            detail=None,
            node=node_at(native_ast, col),
        )
    return ParseResult("ok", None, None, len(native_ast), None)


def parse_file(
    session: requests.Session,
    frontpy_url: str,
    parsers_url: str,
    file_path: Path,
    rel_filename: str,
    timeout: float,
) -> tuple[ParseResult, str | None]:
    """Run the full frontpy -> parsers pipeline. Returns (result, native_ast)."""
    native, err = frontpy_native_ast(session, frontpy_url, file_path, timeout)
    if native is None:
        return ParseResult("frontpy_fail", None, None, None, err), None
    res = parsers_translate(session, parsers_url, rel_filename, native, timeout)
    return res, native


def wait_for_healthy(url: str, timeout: float = HEALTHCHECK_TIMEOUT_S) -> None:
    deadline = time.monotonic() + timeout
    last_err: str | None = None
    while time.monotonic() < deadline:
        try:
            resp = requests.get(f"{url}/healthcheck", timeout=5)
            if resp.status_code == 200:
                return
            last_err = f"http {resp.status_code}: {resp.text[:120]}"
        except requests.RequestException as exc:
            last_err = str(exc)
        time.sleep(1)
    raise SystemExit(f"timeout waiting for {url}/healthcheck: {last_err}")


# --------------------------------------------------------------------------- #
# Native AST helpers                                                          #
# --------------------------------------------------------------------------- #


_WORD_BEFORE = re.compile(r"[A-Za-z_][A-Za-z_0-9]*$")
_WORD_AFTER = re.compile(r"^[A-Za-z_0-9]*")


def node_at(native: str, col: int | None) -> str | None:
    """The identifier the 1-indexed offset `col` falls in ( or starts ).

    The parser usually stops INSIDE the unknown node name ( the lexer
    splits `NamedExpr` into a known keyword prefix + the rest ), so glue
    the word chars on both sides of the offset back together."""
    if not native or not isinstance(col, int) or col < 1:
        return None
    i = min(col - 1, len(native))
    before = _WORD_BEFORE.search(native[:i])
    after = _WORD_AFTER.match(native[i:])
    word = (before.group(0) if before else "") + (after.group(0) if after else "")
    return word or None


def node_histogram(files_map: dict[str, dict[str, Any]]) -> list[tuple[str, int]]:
    counts: collections.Counter[str] = collections.Counter(
        info.get("node") or "?"
        for info in files_map.values()
        if info.get("status") == "fail"
    )
    return counts.most_common()


def print_summary(files_map: dict[str, dict[str, Any]], top: int = 15) -> None:
    total = len(files_map)
    ok = sum(1 for v in files_map.values() if v.get("status") == "ok")
    fail = sum(1 for v in files_map.values() if v.get("status") == "fail")
    other = total - ok - fail
    pct = (100.0 * ok / total) if total else 0.0
    print(f"coverage: {ok}/{total} ok ( {pct:.1f}% )  fail={fail}  other={other}")
    hist = node_histogram(files_map)
    if hist:
        print("failing files by native node at the failure offset:")
        for node, n in hist[:top]:
            print(f"  {n:5d}  {node}")


# --------------------------------------------------------------------------- #
# Baseline                                                                    #
# --------------------------------------------------------------------------- #


def grammar_hash() -> str:
    h = hashlib.sha256()
    for rel in GRAMMAR_HASH_RELPATHS:
        p = HERE / rel
        if p.is_file():
            h.update(rel.encode("utf-8"))
            h.update(b"\0")
            h.update(p.read_bytes())
            h.update(b"\0")
    return h.hexdigest()


def build_baseline(
    corpus_dir: Path,
    files: list[Path],
    frontpy_url: str,
    parsers_url: str,
    timeout: float,
) -> dict[str, dict[str, Any]]:
    session = requests.Session()
    out: dict[str, dict[str, Any]] = {}
    n = len(files)
    last_print = 0.0
    for i, p in enumerate(files, start=1):
        rel = str(p.relative_to(corpus_dir)).replace("\\", "/")
        res, _ = parse_file(session, frontpy_url, parsers_url, p, rel, timeout)
        out[rel] = res.to_baseline()
        now = time.monotonic()
        if now - last_print > 1.0 or i == n:
            ok = sum(1 for v in out.values() if v["status"] == "ok")
            fail = sum(1 for v in out.values() if v["status"] == "fail")
            other = i - ok - fail
            sys.stdout.write(
                f"\rbaseline: {i}/{n}  ok={ok}  fail={fail}  other={other}   "
            )
            sys.stdout.flush()
            last_print = now
    sys.stdout.write("\n")
    return out


def load_baseline_or_none() -> dict[str, Any] | None:
    if not BASELINE_PATH.is_file():
        return None
    try:
        return json.loads(BASELINE_PATH.read_text(encoding="utf-8"))
    except (json.JSONDecodeError, OSError):
        return None


def save_baseline(corpus_dir: Path, files_map: dict[str, dict[str, Any]]) -> None:
    DEV_DIR.mkdir(parents=True, exist_ok=True)
    payload = {
        "schema_version": 1,
        "grammar_hash": grammar_hash(),
        "corpus_dir": str(corpus_dir).replace("\\", "/"),
        "files": files_map,
    }
    BASELINE_PATH.write_text(
        json.dumps(payload, indent=2, ensure_ascii=False),
        encoding="utf-8",
    )


def baseline_is_fresh(baseline: dict[str, Any], corpus_dir: Path) -> bool:
    if baseline.get("schema_version") != 1:
        return False
    if baseline.get("grammar_hash") != grammar_hash():
        return False
    if baseline.get("corpus_dir") != str(corpus_dir).replace("\\", "/"):
        return False
    return True


# --------------------------------------------------------------------------- #
# Failure picker / context window                                             #
# --------------------------------------------------------------------------- #


def is_trivial_failure_location(info: dict[str, Any]) -> bool:
    """A col=1 failure means the parser bailed at the first token of the
    native AST ( always `Module(` ); the window can't show the cause."""
    return info.get("line_start") == 1 and info.get("col_start") == 1


def failure_position_key(info: dict[str, Any]) -> tuple[int, int]:
    """Sort/comparison key for a fail location. The native AST is a single
    line, so in practice this is ( 1, col )."""
    line = info.get("line_start") or 0
    col = info.get("col_start") or 0
    return (line, col)


def pick_failure(
    files_map: dict[str, dict[str, Any]],
    strategy: str,
    rng: random.Random,
    target_node: str | None,
) -> str | None:
    failures = [
        rel
        for rel, info in files_map.items()
        if info.get("status") == "fail"
        and isinstance(info.get("col_start"), int)
        and isinstance(info.get("line_start"), int)
        and not is_trivial_failure_location(info)
        and (target_node is None or info.get("node") == target_node)
    ]
    if not failures:
        return None
    if strategy == "random":
        return rng.choice(failures)
    if strategy == "earliest":
        # the smallest native AST offset first : the shortest window to
        # reason about, and usually the plainest instance of the node.
        return min(failures, key=lambda rel: (failure_position_key(files_map[rel]), rel))
    return sorted(failures)[0]


def context_window(native: str, col_start: int | None, radius: int) -> str:
    """Return `radius` chars on each side of the 1-indexed offset `col_start`
    of the single-line native AST, marking it with `<<<HERE col=C>>>`."""
    if not native:
        return "<empty native ast>"
    if not isinstance(col_start, int) or col_start < 1:
        return native[: 2 * radius] + ("..." if len(native) > 2 * radius else "")
    offset = max(0, min(col_start - 1, len(native)))
    lo = max(0, offset - radius)
    hi = min(len(native), offset + radius)
    leading = "..." if lo > 0 else ""
    trailing = "..." if hi < len(native) else ""
    return f"{leading}{native[lo:offset]}<<<HERE col={col_start}>>>{native[offset:hi]}{trailing}"


# --------------------------------------------------------------------------- #
# OpenAI prompt                                                               #
# --------------------------------------------------------------------------- #


SYSTEM_PROMPT = """\
You are an expert Haskell + Happy/Alex grammar engineer working on the
dhscanner parsers service, which translates the native Python AST emitted
by `dhscanner.service.fronts/py` into the dhscanner AST.

The native AST is the SINGLE-LINE text of CPython 3.12

    ast.dump(tree, include_attributes=True)

after one normalization: every `'` and `"` INSIDE string constants is
removed. Its shape is `NodeName(field=value, ..., lineno=L, col_offset=C,
end_lineno=L2, end_col_offset=C2)`:

  * fields come in the node's `_fields` order ( see CPython's `ast` docs /
    Parser/Python.asdl ), then the four location attributes, which the
    grammar consumes with the `loc` nonterminal;
  * `ast.dump` OMITS a field whose value is None ( that is why the grammar
    has `optional(...)` around eg `keyword_arg` ); empty lists are kept
    as `[]`;
  * identifiers appear quoted, eg `Name(id='request', ctx=Load(), ...)`,
    lexed as an `ID` token and unquoted by the `tokenID` nonterminal;
  * expression context is `ctx=Load()` / `Store()` / `Del()`, the `ctx`
    nonterminal.

Your job in this turn: propose ONE minimal, surgical patch that lets the
parser advance past a specific failure offset on a specific input. The
patch may touch ONLY:

  - src/PyParser.y    ( the Happy grammar, all action code is inline )
  - src/PyLexer.x     ( the Alex lexer )

Rules of the road:

  1. The parser reports failure as a 1-indexed column into that single
     line. Your patch is accepted iff EVERY previously-passing corpus
     file still parses AND every previously-failing file now passes or
     fails at a column >= its previous column, AND at least one file
     strictly progresses. You only see the chosen file's window. Aim for
     a conservative, locally-scoped change ( add one production, add one
     alternative to `exp` / `stmt`, make one symbol optional ), not a
     refactor.

  2. A native node that the grammar does not know yet needs a NEW TOKEN.
     Look at how `Await` is wired; a new keyword needs all four of:

       a. PyLexer.x macro      : `@KW_AWAIT           = Await`
       b. PyLexer.x rule       : `@KW_AWAIT           { lex' AlexRawToken_AWAIT           }`
       c. PyLexer.x data ctor  : `     | AlexRawToken_AWAIT           -- ^ Reserved Keyword`
       d. PyParser.y %token    : `'Await'                     { AlexTokenTag AlexRawToken_AWAIT           _ _ }`

     Put each new line next to its sibling ( eg right after the AWAIT
     line ) so every old_string anchor is unique. Field names ( `target`,
     `value`, `targets`, ... ) are tokens too: before adding one, search
     the %token block, most common field names already exist. Alex takes
     the longest match, so a new keyword `NamedExpr` does not disturb the
     existing `Name` / `name` keywords.

  3. The grammar is in the OLD style: rules are `snake_case` with action
     code inline in `{ ... }`. There is NO PyParserActions.hs. Keep the
     style of the neighboring rules. `PyParser.y` imports `Ast`
     unqualified, but every action spells names as `Ast.ExpVar`,
     `Ast.StmtAssign`, ... : keep that.

  4. Reusable helpers already in PyParser.y ( use them, don't add new ):

         optional(a)              -- Nothing | Just a
         commalistof(a)           -- a | a ',' commalistof(a)
         possibly_empty_listof(a) -- '[' ']' | '[' commalistof(a) ']'
         nonempty_listof(a)       -- '[' commalistof(a) ']'
         stmts                    -- possibly empty [stmt] list
         loc, ctx, tokenID, var, exp, stmt

  5. Map to the dhscanner AST the way the neighbors do:
       * if `dhscanner.ast` has a same-shape constructor, build it
         directly. Eg `Ast.ExpAssign $ Ast.ExpAssignContent { Ast.expAssignLhs
         = <Ast.Var>, Ast.expAssignRhs = <Ast.Exp>, Ast.expAssignLocation =
         <Location> }` is an assignment used as an expression ( PhpParser.y
         uses it ). Take the `Ast.Var` out of an `exp` the way
         `stmt_assign` does ( `Ast.ExpVar (Ast.ExpVarContent v)` ).
       * otherwise route it through an existing shape: an expression
         wrapper can return its most informative sub-expression ( see
         `exp_await`, `exp_starred` ), a compound statement can become an
         `Ast.StmtBlock` of its bodies. Keep data flow : don't drop a sub-
         expression that a call or a variable could live in.

  6. If you cannot see a clearly safe minimal edit, return `edits: []` and
     explain in `rationale`. An empty patch beats a guess that regresses.

  7. The project builds with `-Werror=missing-fields` and `-Werror`. Any
     `Ast.<Constructor> { ... }` record build MUST fill every field.

  8. Do NOT introduce anonymous lambdas ( `\\... -> ...` ) anywhere, they
     are the most common cause of `-Werror=incomplete-uni-patterns` build
     failures. Use a `case ... of` with a catch-all `_ ->` branch ( as in
     `stmt_assign` ), or a named function with one clause per constructor.

  9. The dhscanner AST keeps a minimal binary-operator set: only
     `Ast.EQ`, `Ast.PLUS`, `Ast.TIMES` exist. Comparison-ish -> `Ast.EQ`,
     `*` `/` -> `Ast.TIMES`, everything else -> `Ast.PLUS`. Never write
     `Ast.AND`, `Ast.LT`, `Ast.MOD`, ... ( guaranteed build_failed ).

 10. The grammar does not declare `%expect 0`, so a new conflict won't fail
     the build, but a conflict that changes reductions on a passing file
     will be rejected by the gate. Treat conflicts as a real risk.

 11. Each edit is ( file, old_string, new_string ). `old_string` MUST
     appear EXACTLY ONCE in the current file content shown to you,
     including whitespace. Include 1-3 lines of context to make it unique.
     To INSERT, choose an anchor line as old_string and repeat it in
     new_string with the insertion before/after it.
"""


def build_user_prompt(
    failing_relpath: str,
    col: int,
    node: str | None,
    native_window: str,
    full_native_len: int,
    parser_y_text: str,
    lexer_x_text: str,
) -> str:
    return f"""\
## Failing file

`{failing_relpath}` -- the parser fails at column {col} of the single-line \
native AST ( total length {full_native_len} chars ). Native node at the \
failure offset: `{node or '?'}`.

### Native AST window around the failure

```
{native_window}
```

## Current src/PyParser.y

```happy
{parser_y_text}
```

## Current src/PyLexer.x

```alex
{lexer_x_text}
```

## Task

Return a GrammarPatch JSON with at most a few edits. Keep the change as \
narrow as possible. The strict-progress gate will run automatically after \
your patch is applied; do not preemptively explain how to test.
"""


def call_openai(
    model: str,
    system_prompt: str,
    user_prompt: str,
    timeout: float,
) -> dict[str, Any]:
    try:
        from openai import OpenAI
    except ImportError as exc:
        raise SystemExit(
            "openai package not installed. Run: pip install openai python-dotenv"
        ) from exc

    client = OpenAI(timeout=timeout)
    resp = client.chat.completions.create(
        model=model,
        messages=[
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt},
        ],
        response_format={
            "type": "json_schema",
            "json_schema": GRAMMAR_PATCH_SCHEMA,
        },
    )
    msg = resp.choices[0].message
    content = msg.content or "{}"
    return json.loads(content)


# --------------------------------------------------------------------------- #
# Snapshot / apply / revert                                                   #
# --------------------------------------------------------------------------- #


def snapshot_editables() -> dict[str, bytes]:
    snap: dict[str, bytes] = {}
    for rel in EDITABLE_RELPATHS:
        p = HERE / rel
        if p.is_file():
            snap[rel] = p.read_bytes()
    return snap


def restore_snapshot(snap: dict[str, bytes]) -> None:
    for rel, content in snap.items():
        (HERE / rel).write_bytes(content)


def apply_edits(edits: list[dict[str, str]]) -> tuple[bool, str]:
    """Apply each edit in order. Returns (ok, reason)."""
    if not edits:
        return False, "patch contained no edits"

    for i, edit in enumerate(edits):
        rel = edit["file"]
        if rel not in EDITABLE_RELPATHS:
            return False, f"edit {i} targets disallowed file {rel}"
        old = edit["old_string"]
        new = edit["new_string"]
        p = HERE / rel
        if not p.is_file():
            return False, f"edit {i} target file does not exist: {rel}"
        text = p.read_text(encoding="utf-8")
        count = text.count(old)
        if count == 0:
            return False, f"edit {i} old_string not found in {rel}"
        if count > 1:
            return False, f"edit {i} old_string is not unique in {rel} ({count} matches)"
        text = text.replace(old, new, 1)
        p.write_text(text, encoding="utf-8")
    return True, ""


# --------------------------------------------------------------------------- #
# Docker control                                                              #
# --------------------------------------------------------------------------- #


def _compose_base_cmd(runner_root: Path) -> list[str]:
    cmd: list[str] = ["docker", "compose"]
    env_file = runner_root / ".env"
    if env_file.is_file():
        cmd += ["--env-file", str(env_file)]
    for rel in RUNNER_COMPOSE_FILES:
        cmd += ["-f", str(runner_root / rel)]
    return cmd


def run_compose(runner_root: Path, args: list[str]) -> subprocess.CompletedProcess:
    cmd = _compose_base_cmd(runner_root) + args
    return subprocess.run(cmd, cwd=runner_root, capture_output=True, text=True)


def docker_stop_parsers(runner_root: Path) -> None:
    res = run_compose(runner_root, ["stop", PARSERS_SERVICE])
    if res.returncode != 0:
        raise SystemExit(
            f"docker compose stop {PARSERS_SERVICE} failed:\n{res.stderr}"
        )


def docker_rebuild_parsers(runner_root: Path) -> None:
    res = run_compose(runner_root, ["build", PARSERS_SERVICE])
    if res.returncode != 0:
        raise SystemExit(
            f"docker compose build {PARSERS_SERVICE} failed:\n"
            f"{res.stdout[-3000:]}\n{res.stderr[-3000:]}"
        )
    # `--no-deps` : the fronts are stable across grammar iterations.
    res = run_compose(runner_root, ["up", "-d", "--no-deps", PARSERS_SERVICE])
    if res.returncode != 0:
        raise SystemExit(
            f"docker compose up -d --no-deps {PARSERS_SERVICE} failed:\n{res.stderr}"
        )


# --------------------------------------------------------------------------- #
# Strict progress gate                                                        #
# --------------------------------------------------------------------------- #


@dataclass
class ProgressVerdict:
    accepted: bool
    reason: str
    strict_improvements: list[tuple[str, str, str]]  # (file, before, after)
    regressing_file: str | None = None
    regressing_detail: str | None = None


def _fmt_pos(info: dict[str, Any]) -> str:
    return f"col={info.get('col_start')} ({info.get('node') or '?'})"


def strict_progress(
    old: dict[str, dict[str, Any]],
    new: dict[str, dict[str, Any]],
) -> ProgressVerdict:
    improvements: list[tuple[str, str, str]] = []
    for rel in sorted(set(old) & set(new)):
        o = old[rel]
        n = new[rel]
        o_status = o.get("status")
        n_status = n.get("status")

        if o_status == "ok":
            if n_status != "ok":
                detail = (
                    f"parsed before, now status={n_status}"
                    + (f" {_fmt_pos(n)}" if n_status == "fail" else "")
                )
                return ProgressVerdict(False, f"regression: {rel} {detail}", [], rel, detail)
            continue

        if o_status != "fail":
            # frontpy / transport errors in the baseline: don't gate on them
            continue

        if n_status == "ok":
            improvements.append((rel, f"fail@{_fmt_pos(o)}", "ok"))
            continue
        if n_status != "fail":
            detail = f"failed cleanly before, now status={n_status}"
            return ProgressVerdict(False, f"regression: {rel} {detail}", [], rel, detail)
        o_key = failure_position_key(o)
        n_key = failure_position_key(n)
        if n_key < o_key:
            detail = f"fail col went {o_key[1]} -> {n_key[1]}"
            return ProgressVerdict(False, f"regression: {rel} {detail}", [], rel, detail)
        if n_key > o_key:
            improvements.append((rel, f"fail@{_fmt_pos(o)}", f"fail@{_fmt_pos(n)}"))

    if not improvements:
        return ProgressVerdict(False, "no file strictly progressed", [])
    return ProgressVerdict(True, f"{len(improvements)} file(s) strictly progressed", improvements)


# --------------------------------------------------------------------------- #
# Iteration logging                                                           #
# --------------------------------------------------------------------------- #


def log_iteration(payload: dict[str, Any]) -> Path:
    ITERATIONS_DIR.mkdir(parents=True, exist_ok=True)
    ts = time.strftime("%Y%m%d-%H%M%S")
    p = ITERATIONS_DIR / f"{ts}.json"
    p.write_text(json.dumps(payload, indent=2, ensure_ascii=False), encoding="utf-8")
    return p


# --------------------------------------------------------------------------- #
# Main                                                                        #
# --------------------------------------------------------------------------- #


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    ap = argparse.ArgumentParser(
        description=(
            "Iterate one OpenAI-driven Python grammar patch. Apply / build "
            "failures auto-revert; regression-gate failures leave edits in "
            "place for post-mortem inspection."
        )
    )
    ap.add_argument("--corpus", type=Path, default=DEFAULT_CORPUS_DIR,
                    help=f"corpus directory (default: {DEFAULT_CORPUS_DIR})")
    ap.add_argument("--model", default=os.environ.get("OPENAI_MODEL", DEFAULT_MODEL),
                    help=f"OpenAI model (default: {DEFAULT_MODEL} or $OPENAI_MODEL)")
    ap.add_argument("--pick", choices=["first", "random", "earliest"], default="earliest",
                    help="failure-picking strategy (default: earliest failure offset)")
    ap.add_argument("--reshuffle", action="store_true",
                    help="shorthand for --pick random with a non-deterministic seed")
    ap.add_argument("--seed", type=int, default=None, help="random seed")
    ap.add_argument("--target-node", default=None, metavar="NODE",
                    help="only pick files failing on this native node (eg NamedExpr)")
    ap.add_argument("--baseline-only", action="store_true",
                    help="build / load the baseline, print coverage + node histogram, exit")
    ap.add_argument("--rebuild-baseline", action="store_true",
                    help="ignore cached baseline and rebuild from scratch")
    ap.add_argument("--context-radius", type=int, default=DEFAULT_CONTEXT_RADIUS,
                    help="chars before/after the failure offset to show the LLM")
    ap.add_argument("--per-file-timeout", type=float, default=30.0,
                    help="HTTP timeout (seconds) per frontpy/parsers call")
    ap.add_argument("--openai-timeout", type=float, default=300.0,
                    help="OpenAI API call timeout (seconds)")
    ap.add_argument("--frontpy-url", default=os.environ.get("FRONTPY_URL", "http://localhost:5006"))
    ap.add_argument("--parsers-url", default=os.environ.get("PARSERS_URL", "http://localhost:4001"))
    ap.add_argument("--runner-root", type=Path,
                    default=Path(os.environ.get("RUNNER_ROOT", str(DEFAULT_RUNNER_ROOT))),
                    help="dhscanner.runner working copy that owns compose/ (default: ../..)")
    ap.add_argument("--dry-run", action="store_true",
                    help="ask OpenAI but do not apply / rebuild / regate")
    ap.add_argument("--post_mortem_show_native_ast_of", type=Path, default=None, metavar="PATH",
                    help="print the native AST of PATH via frontpy and exit")
    return ap.parse_args(argv)


def maybe_load_dotenv(runner_root: Path) -> None:
    """Load OPENAI_API_KEY from the runner-root `.env` ( else a local one ).
    `override=True` : the file wins over a stale shell export."""
    try:
        from dotenv import load_dotenv  # type: ignore[import-untyped]
    except ImportError:
        return
    for candidate in (runner_root / ".env", HERE / ".env"):
        if candidate.is_file():
            load_dotenv(candidate, override=True)
            return


def ensure_openai_key() -> None:
    if not os.environ.get("OPENAI_API_KEY"):
        raise SystemExit(
            "OPENAI_API_KEY not set. Export it or place it in the runner-root .env"
        )


def ensure_runner_root(runner_root: Path) -> None:
    missing = [rel for rel in RUNNER_COMPOSE_FILES if not (runner_root / rel).is_file()]
    if missing:
        raise SystemExit(
            f"runner root {runner_root} is missing compose files:\n"
            + "\n".join(f"  {rel}" for rel in missing)
            + "\n\npass --runner-root <path-to-dhscanner.runner> or set RUNNER_ROOT."
        )


def read_text_safe(p: Path) -> str:
    return p.read_text(encoding="utf-8") if p.is_file() else ""


def post_mortem_show_native_ast(path: Path, frontpy_url: str, timeout: float) -> int:
    abs_path = path.resolve()
    if not abs_path.is_file():
        print(f"file not found: {abs_path}", file=sys.stderr)
        return 1
    try:
        wait_for_healthy(frontpy_url, timeout=60)
    except SystemExit as exc:
        print(str(exc), file=sys.stderr)
        return 1
    native, err = frontpy_native_ast(requests.Session(), frontpy_url, abs_path, timeout)
    if native is None:
        print(f"frontpy failed: {err}", file=sys.stderr)
        return 1
    print(f"native AST length: {len(native)} chars", file=sys.stderr)
    print(native)
    return 0


def load_or_build_baseline(args: argparse.Namespace, corpus: list[Path]) -> dict[str, dict[str, Any]]:
    cached = load_baseline_or_none() if not args.rebuild_baseline else None
    if cached and baseline_is_fresh(cached, args.corpus):
        corpus_rels = {str(p.relative_to(args.corpus)).replace("\\", "/") for p in corpus}
        cached_files: dict[str, dict[str, Any]] = cached["files"]
        files_map = {rel: info for rel, info in cached_files.items() if rel in corpus_rels}
        missing = corpus_rels - set(files_map)
        if not missing:
            print("baseline: cached")
            return files_map
        print(f"baseline: cache misses {len(missing)} corpus file(s), rebuilding...")
    else:
        print("baseline: rebuilding...")
    files_map = build_baseline(
        args.corpus, corpus, args.frontpy_url, args.parsers_url, args.per_file_timeout
    )
    save_baseline(args.corpus, files_map)
    return files_map


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    maybe_load_dotenv(args.runner_root)

    if args.post_mortem_show_native_ast_of is not None:
        return post_mortem_show_native_ast(
            args.post_mortem_show_native_ast_of, args.frontpy_url, args.per_file_timeout
        )

    if not args.baseline_only:
        ensure_openai_key()
    ensure_runner_root(args.runner_root)

    rng = random.Random(args.seed)

    corpus = discover_corpus(args.corpus)
    if not corpus:
        raise SystemExit(f"no .py files under {args.corpus}")
    print(f"corpus: {len(corpus)} files under {args.corpus}")

    print("checking frontpy and parsers health...")
    wait_for_healthy(args.frontpy_url, timeout=60)
    wait_for_healthy(args.parsers_url, timeout=60)

    files_map = load_or_build_baseline(args, corpus)
    print_summary(files_map)
    if args.baseline_only:
        return 0

    pick_strategy = "random" if args.reshuffle else args.pick
    chosen = pick_failure(files_map, pick_strategy, rng, args.target_node)
    if chosen is None:
        scope = f" on node {args.target_node}" if args.target_node else ""
        print(f"no actionable failures{scope} in baseline -- nothing to do.")
        return 0

    chosen_abs = args.corpus / chosen
    chosen_info = files_map[chosen]
    col = chosen_info.get("col_start")
    node = chosen_info.get("node")

    print(f"chosen failed file: {chosen}")
    print(f"location of failure: col={col} node={node}")

    native, ferr = frontpy_native_ast(
        requests.Session(), args.frontpy_url, chosen_abs, args.per_file_timeout
    )
    if native is None:
        print(f"could not re-fetch native AST for chosen file: {ferr}")
        return 1
    window = context_window(native, col, args.context_radius)
    print("window before/after the failed location (native ast):")
    print(window)

    user_prompt = build_user_prompt(
        chosen,
        col or 1,
        node,
        window,
        len(native),
        read_text_safe(SRC_DIR / "PyParser.y"),
        read_text_safe(SRC_DIR / "PyLexer.x"),
    )

    print(f"asking {args.model} for a grammar patch...")
    try:
        patch = call_openai(args.model, SYSTEM_PROMPT, user_prompt, args.openai_timeout)
    except Exception as exc:  # noqa: BLE001
        print(f"openai call failed: {exc}")
        return 1

    rationale = patch.get("rationale", "")
    edits = patch.get("edits", []) or []
    base_log = {
        "chosen": chosen,
        "location": {"col": col, "node": node},
        "rationale": rationale,
        "edits": edits,
    }

    print(f"found parser adjustment: {rationale}")
    if not edits:
        print("(model returned no edits; aborting)")
        log_iteration({**base_log, "verdict": "no_edits"})
        return 1

    if args.dry_run:
        print("dry-run: would apply", len(edits), "edit(s); skipping rebuild.")
        for e in edits:
            print(f"--- {e['file']}\n-{e['old_string']}\n+{e['new_string']}")
        log_iteration({**base_log, "verdict": "dry_run"})
        return 0

    snap = snapshot_editables()
    SNAPSHOT_DIR.mkdir(parents=True, exist_ok=True)
    for rel, content in snap.items():
        (SNAPSHOT_DIR / rel.replace("/", "__")).write_bytes(content)

    ok, reason = apply_edits(edits)
    if not ok:
        restore_snapshot(snap)
        print(f"could not apply edits: {reason}")
        log_iteration({**base_log, "verdict": f"apply_failed: {reason}"})
        return 1

    print("stopping container ......")
    try:
        docker_stop_parsers(args.runner_root)
    except SystemExit as exc:
        restore_snapshot(snap)
        print(str(exc))
        return 1

    print("rebuilding ......")
    try:
        docker_rebuild_parsers(args.runner_root)
        wait_for_healthy(args.parsers_url, timeout=HEALTHCHECK_TIMEOUT_S)
    except SystemExit as exc:
        restore_snapshot(snap)
        print(str(exc))
        log_iteration({**base_log, "verdict": "build_failed", "build_log": str(exc)[-4000:]})
        # bring the previous grammar back up so the next iteration starts clean
        print("restoring previous grammar container ......")
        docker_rebuild_parsers(args.runner_root)
        return 1

    print("retrying .....")
    new_files_map = build_baseline(
        args.corpus, corpus, args.frontpy_url, args.parsers_url, args.per_file_timeout
    )

    verdict = strict_progress(files_map, new_files_map)
    log_iteration(
        {
            **base_log,
            "verdict": "accepted" if verdict.accepted else f"rejected: {verdict.reason}",
            "strict_improvements": verdict.strict_improvements,
        }
    )

    if not verdict.accepted:
        if verdict.regressing_file is not None:
            regressing_abs = (args.corpus / verdict.regressing_file).resolve()
            print(f"regression failed on: {regressing_abs}")
            if verdict.regressing_detail:
                print(f"reason: {verdict.regressing_detail}")
            print()
            print("inspect its native AST with:")
            print(f'    python "{HERE / "py_agent_loop.py"}" '
                  f'--post_mortem_show_native_ast_of "{regressing_abs}"')
        else:
            print(f"REJECTED: {verdict.reason}")
        print()
        print("(grammar edits left in place; parsers container is running the rejected grammar.")
        print(" to manually revert: restore src/PyParser.y + src/PyLexer.x from")
        print(" .dev/py-snapshot/, then re-run the compose build + up chain for `parsers`.)")
        return 1

    print(f"ACCEPTED: {verdict.reason}")
    for rel, before, after in verdict.strict_improvements[:10]:
        print(f"  {rel}: {before} -> {after}")
    if len(verdict.strict_improvements) > 10:
        print(f"  ... and {len(verdict.strict_improvements) - 10} more")

    save_baseline(args.corpus, new_files_map)
    print_summary(new_files_map)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
