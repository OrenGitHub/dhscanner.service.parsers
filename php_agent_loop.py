"""Agent loop: improve PhpParser.y grammar coverage.

Mirror of `agent_loop.py` (which targets TypeScript) adapted for PHP. The
two loops share the same dev container stack (`compose.yaml` in this
directory) and the same `parsers` service, but each language has its own
front service, its own corpus, its own native-AST shape, and its own
cached baseline / iteration log under `.dev/`.

For one iteration this script:

  1. Builds (or loads cached) baseline of {file -> parse outcome} over the
     PHP corpus at --corpus (default: C:\\Users\\tuna_\\GitHub\\zabbix).
  2. Picks a failing file. Reads its native AST plus the parser's exact
     (line, column) failure location.
  3. Asks OpenAI (gpt-5 by default, structured-output JSON) for a minimal
     patch to PhpParser.y that should make the parser advance past that
     (line, column) location.
  4. Snapshots the editable files, applies the patch, then
     `docker compose stop parsers`, `docker compose build parsers`,
     `docker compose up -d parsers`, polls /healthcheck, re-parses the
     entire corpus.
  5. Strict-progress gate (per AGENTS.md):
       - every previously-passing file still passes;
       - every previously-failing file now passes OR fails at a
         (line, col) >= its previous (line, col) under lexicographic
         ordering. (Native ASTs for PHP are multi-line NodeDumper output,
         so unlike TS we cannot compare on column alone.)
       - at least one file strictly progresses (was failing and now
         passes, OR was failing and now fails at strictly greater
         (line, col)).
     On gate pass the edit stays applied and the new state becomes the
     baseline. On any failure (parse error, build error, container not
     healthy, gate violation) the editable files are restored from the
     snapshot and the iteration aborts non-zero.

Differences vs. agent_loop.py (TypeScript):

  - Editable files are just `src/PhpParser.y`. The PHP grammar still
    keeps action code inline; an actions module split (analogous to
    `TsParserActions.hs`) hasn't started yet, so we don't expose one.
  - Frontend service is `frontphp` (host port 4002 -> container 5000),
    endpoint `/to/php/ast`. Parser endpoint is `/from/php/to/dhscanner/ast`.
  - Native ASTs are multi-line (NodeDumper output of nikic/PhpParser),
    not single-line. The failure location and the strict-progress gate
    therefore compare `(line, col)` lexicographically, not just column.
  - `mk_parsers.py` is NOT invoked: the TS keyword-block regeneration
    doesn't apply to PhpParser.y (no `-- reserved keywords start/end`
    markers there yet).
  - Baseline / iterations / snapshot live under `.dev/php-*` so they
    don't collide with the TS loop's state.

Configuration:
  Environment:
    OPENAI_API_KEY    required; loaded from os.environ or .env in this dir.
    OPENAI_MODEL      optional; defaults to "gpt-5".
    FRONTPHP_URL      optional; defaults to http://localhost:4002.
    PARSERS_URL       optional; defaults to http://localhost:4001.

Usage (PowerShell):
  cd dhscanner.1.parsers
  $env:OPENAI_API_KEY = 'sk-...'
  docker compose up -d --build              # one-time
  python php_agent_loop.py
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import random
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
BASELINE_PATH = DEV_DIR / "php-baseline.json"
ITERATIONS_DIR = DEV_DIR / "php-iterations"
SNAPSHOT_DIR = DEV_DIR / "php-snapshot"

DEFAULT_CORPUS_DIR = Path(r"C:\Users\tuna_\GitHub\zabbix")
DEFAULT_MODEL = "gpt-5"
DEFAULT_CONTEXT_RADIUS = 500
HEALTHCHECK_TIMEOUT_S = 180

PARSERS_SERVICE = "parsers"

EDITABLE_RELPATHS: tuple[str, ...] = (
    "src/PhpParser.y",
)

# Files that participate in the grammar hash (cache invalidation key for the
# baseline). Both the grammar and the lexer are included because either can
# change what the built parser accepts.
GRAMMAR_HASH_RELPATHS: tuple[str, ...] = (
    "src/PhpParser.y",
    "src/PhpLexer.x",
)

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


# Canonical ordering for status buckets in human-readable summaries.
# `ok` and `fail` are the grammar-meaningful outcomes (the loop's job is to
# move files from `fail` -> `ok`); everything after them is a pre-grammar
# pipeline error that the loop cannot improve by editing PhpParser.y --
# the user has to fix the front / network first.
STATUS_ORDER: tuple[str, ...] = ("ok", "fail", "frontphp_fail", "transport_fail")
NON_GRAMMAR_STATUSES: frozenset[str] = frozenset(STATUS_ORDER) - {"ok", "fail"}


@dataclass
class ParseResult:
    status: str          # "ok" | "fail" | "frontphp_fail" | "transport_fail"
    col_start: int | None
    line_start: int | None
    native_ast_len: int | None
    detail: str | None   # short human-readable detail for fails

    def to_baseline(self) -> dict[str, Any]:
        # `detail` is persisted only for the non-grammar statuses
        # (`frontphp_fail`, `transport_fail`); for `ok` it is always None,
        # and for grammar-level `fail` the (line, col) pair is the
        # actionable signal -- the `detail` from `parsers` is empty there
        # by construction (see `parsers_translate`).
        entry: dict[str, Any] = {
            "status": self.status,
            "col_start": self.col_start,
            "line_start": self.line_start,
        }
        if self.status in NON_GRAMMAR_STATUSES and self.detail:
            entry["detail"] = self.detail
        return entry


# --------------------------------------------------------------------------- #
# Corpus discovery                                                            #
# --------------------------------------------------------------------------- #


def discover_corpus(corpus_dir: Path) -> list[Path]:
    """Return *.php files under corpus_dir, excluding vendor directories
    and PHPUnit-style test files.

    Exclusions:
      * any file under a `vendor/` directory (third-party code).
      * any file under a `node_modules/` directory (defensive: some PHP
        projects vendor JS in-tree).
      * files whose basename ends in `Test.php` or contains `.test.`
        (PHPUnit / pest-style tests; analogous to the `.test.` filter
        the TS loop applies).

    NOTE: `*.blade.php` files (Laravel templates) are intentionally
    skipped even though `rglob('*.php')` would include them, because the
    frontphp service no longer ships the blade preprocessing endpoint
    (`/to/php/code`) -- handing a blade template to `/to/php/ast`
    directly would just produce the `"ERROR"` sentinel since blade
    isn't valid PHP. Filtering them here keeps the `frontphp_fail`
    bucket meaningful instead of polluting it with template files."""
    if not corpus_dir.is_dir():
        raise SystemExit(f"corpus directory not found: {corpus_dir}")

    out: list[Path] = []
    for p in corpus_dir.rglob("*.php"):
        parts = set(p.parts)
        if "vendor" in parts:
            continue
        if "node_modules" in parts:
            continue
        name = p.name
        if name.endswith(".blade.php"):
            continue
        if name.endswith("Test.php"):
            continue
        if ".test." in name:
            continue
        out.append(p)
    out.sort()
    return out


# --------------------------------------------------------------------------- #
# HTTP clients                                                                #
# --------------------------------------------------------------------------- #


def frontphp_native_ast(
    session: requests.Session,
    frontphp_url: str,
    file_path: Path,
    timeout: float,
) -> tuple[str | None, str | None]:
    """POST source file to frontphp; return (native_ast, error_detail).

    The PHP front returns the literal string "ERROR" (no JSON envelope)
    when nikic/PhpParser fails to parse the source, and returns the
    NodeDumper output on success. We treat the "ERROR" sentinel as a
    frontphp failure so it shows up as `frontphp_fail` in the baseline
    instead of being mistaken for a successful empty AST.
    """
    try:
        with file_path.open("rb") as fl:
            files = {"source": (file_path.name, fl, "text/plain")}
            resp = session.post(
                f"{frontphp_url}/to/php/ast",
                files=files,
                timeout=timeout,
            )
    except requests.RequestException as exc:
        return None, f"frontphp transport error: {exc}"

    if resp.status_code != 200:
        return None, f"frontphp http {resp.status_code}: {resp.text[:200]}"

    text = resp.text
    if text.strip() == "ERROR":
        return None, "frontphp returned ERROR (nikic/PhpParser failed to parse the source)"
    return text, None


def parsers_translate(
    session: requests.Session,
    parsers_url: str,
    filename: str,
    native_ast: str,
    timeout: float,
) -> ParseResult:
    """POST native AST to parsers /from/php/to/dhscanner/ast."""
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
            f"{parsers_url}/from/php/to/dhscanner/ast",
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
        return ParseResult(
            status="fail",
            col_start=loc.get("colStart"),
            line_start=loc.get("lineStart"),
            native_ast_len=len(native_ast),
            detail=None,
        )
    return ParseResult(
        status="ok",
        col_start=None,
        line_start=None,
        native_ast_len=len(native_ast),
        detail=None,
    )


def parse_file(
    session: requests.Session,
    frontphp_url: str,
    parsers_url: str,
    file_path: Path,
    rel_filename: str,
    timeout: float,
) -> tuple[ParseResult, str | None]:
    """Run the full frontphp -> parsers pipeline. Returns (result, native_ast)."""
    native, err = frontphp_native_ast(session, frontphp_url, file_path, timeout)
    if native is None:
        return ParseResult("frontphp_fail", None, None, None, err), None
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


def wait_for_frontphp_alive(url: str, timeout: float = HEALTHCHECK_TIMEOUT_S) -> None:
    """frontphp now exposes a real `GET /healthcheck` that returns
    `200 {"healthy": true}` (defined in `dhscanner.0.fronts/php/index.php`,
    same shape as the parsers service's healthcheck). Delegate to the
    shared `wait_for_healthy` helper so we get a real liveness signal
    rather than the previous "any <500 response means alive" workaround
    that the Laravel-based front forced on us. The connection-refusal
    phase during container boot is still tolerated -- that's handled
    inside `wait_for_healthy` by retrying on `requests.RequestException`."""
    wait_for_healthy(url, timeout=timeout)


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


def status_counts(files_map: dict[str, dict[str, Any]]) -> dict[str, int]:
    """Count files by status, returning a dict ordered with the canonical
    statuses first (in `STATUS_ORDER`) and any unexpected statuses appended
    in alphabetical order. Keys with count 0 are still included for the
    canonical statuses so the breakdown line is shape-stable across runs."""
    counts: dict[str, int] = {k: 0 for k in STATUS_ORDER}
    for v in files_map.values():
        s = v.get("status") or "unknown"
        counts[s] = counts.get(s, 0) + 1
    canonical = {k: counts[k] for k in STATUS_ORDER}
    extras = {k: counts[k] for k in sorted(counts) if k not in STATUS_ORDER}
    return {**canonical, **extras}


def format_status_counts(counts: dict[str, int]) -> str:
    return "  ".join(f"{k}={v}" for k, v in counts.items())


def build_baseline(
    corpus_dir: Path,
    files: list[Path],
    frontphp_url: str,
    parsers_url: str,
    timeout: float,
) -> dict[str, dict[str, Any]]:
    session = requests.Session()
    out: dict[str, dict[str, Any]] = {}
    n = len(files)
    last_print = 0.0
    for i, p in enumerate(files, start=1):
        rel = str(p.relative_to(corpus_dir)).replace("\\", "/")
        res, _ = parse_file(session, frontphp_url, parsers_url, p, rel, timeout)
        out[rel] = res.to_baseline()
        now = time.monotonic()
        if now - last_print > 1.0 or i == n:
            sys.stdout.write(
                f"\rbaseline: {i}/{n}  {format_status_counts(status_counts(out))}   "
            )
            sys.stdout.flush()
            last_print = now
    sys.stdout.write("\n")
    return out


def diagnose_one_non_grammar(
    files_map: dict[str, dict[str, Any]],
    corpus_dir: Path,
    frontphp_url: str,
    parsers_url: str,
    timeout: float,
) -> str | None:
    """Find the first non-grammar-status file in the baseline, re-probe it
    against frontphp + parsers right now, and return a short human-readable
    diagnostic of the form

        "<rel>: status=<status> -- <detail>"

    Returns None if there are no non-grammar entries (or the file vanished
    from disk since the baseline was built). Used when the loop has no
    actionable failures because the entire corpus is stuck in a pre-grammar
    bucket -- it tells the user *why*, instead of just printing a count."""
    for rel in sorted(files_map):
        info = files_map[rel]
        if info.get("status") not in NON_GRAMMAR_STATUSES:
            continue
        # Prefer a previously-persisted detail (older baselines didn't carry
        # one, but newer entries do via `ParseResult.to_baseline`).
        cached_detail = info.get("detail")
        if cached_detail:
            return f"{rel}: status={info.get('status')} -- {cached_detail}"
        abs_path = corpus_dir / rel
        if not abs_path.is_file():
            continue
        session = requests.Session()
        res, _ = parse_file(session, frontphp_url, parsers_url, abs_path, rel, timeout)
        detail = res.detail or "(no detail)"
        return f"{rel}: status={res.status} -- {detail}"
    return None


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
    """A `(line=1, col=1)` failure means the parser bailed at the very
    first token of the native AST. Empirically these iterations rarely
    produce useful progress: the agent's context window starts at the
    very beginning of the AST and the model can't reason about the
    actual cause -- it tends to propose widenings unrelated to the real
    failure, which then regress other files. Skip them in the picker so
    iteration budget goes to files where the failure location actually
    points at parser-grammar work."""
    return info.get("line_start") == 1 and info.get("col_start") == 1


def failure_position_key(info: dict[str, Any]) -> tuple[int, int]:
    """Lexicographic sort/comparison key for a fail location. Missing
    components are treated as 0 so a None location does not crash the
    comparison; in practice frontphp + parsers always return a complete
    (line, col) for a `fail` status."""
    line = info.get("line_start") or 0
    col = info.get("col_start") or 0
    return (line, col)


def pick_failure(
    files_map: dict[str, dict[str, Any]],
    strategy: str,
    rng: random.Random,
) -> str | None:
    failures = [
        rel
        for rel, info in files_map.items()
        if info.get("status") == "fail"
        and isinstance(info.get("col_start"), int)
        and isinstance(info.get("line_start"), int)
        and not is_trivial_failure_location(info)
    ]
    if not failures:
        return None
    if strategy == "random":
        return rng.choice(failures)
    return sorted(failures)[0]


def context_window(
    native: str,
    line_start: int | None,
    col_start: int | None,
    radius: int,
) -> str:
    """Return a context slice around the failure location.

    The PHP native AST is multi-line (NodeDumper indented tree) so we
    compute a character offset that corresponds to (line_start, col_start)
    -- 1-indexed -- and then take `radius` chars on each side, marking
    the failure point with `<<<HERE line=L col=C>>>`."""
    if not native:
        return "<empty native ast>"
    if not isinstance(line_start, int) or not isinstance(col_start, int) \
            or line_start < 1 or col_start < 1:
        return native[: 2 * radius] + ("..." if len(native) > 2 * radius else "")

    lines = native.splitlines(keepends=True)
    if line_start > len(lines):
        return native[-2 * radius:]

    # Char offset of the first column of the failing line, then add (col-1).
    offset = sum(len(line) for line in lines[: line_start - 1]) + (col_start - 1)
    offset = max(0, min(offset, len(native)))

    lo = max(0, offset - radius)
    hi = min(len(native), offset + radius)
    before = native[lo:offset]
    after = native[offset:hi]
    leading = "..." if lo > 0 else ""
    trailing = "..." if hi < len(native) else ""
    return (
        f"{leading}{before}<<<HERE line={line_start} col={col_start}>>>"
        f"{after}{trailing}"
    )


# --------------------------------------------------------------------------- #
# OpenAI prompt                                                               #
# --------------------------------------------------------------------------- #


SYSTEM_PROMPT = """\
You are an expert Haskell + Happy/Alex grammar engineer working on
dhscanner.1.parsers, which translates the native PHP AST emitted by
`dhscanner.0.fronts/php` (a tiny PHP-CLI HTTP front that pipes
nikic/PhpParser's NodeDumper output -- multi-line, indented) into the
dhscanner AST.

Your job in this turn: propose ONE minimal, surgical patch that lets the
parser advance past a specific (line, column) failure location on a
specific input. The patch is allowed to touch ONLY:

  - src/PhpParser.y               (the Happy grammar; action code is
                                   currently kept inline, no separate
                                   actions module has been split out yet)

Rules of the road:

  1. Native PHP ASTs are MULTI-LINE (unlike TypeScript native ASTs which
     are single-line). The parser reports failure as a 1-indexed
     (lineStart, colStart) pair into that text. Your patch is accepted
     if and only if EVERY previously-passing corpus file still parses
     AND every previously-failing file now passes OR fails at
     (line, col) >= its previous (line, col) under LEXICOGRAPHIC
     ordering (line first, column second), AND at least one file
     strictly progresses. You will not see the rest of the corpus, only
     the chosen failing file's window. Aim for a conservative, locally-
     scoped change (widen one production, add one alternative, make one
     symbol optional, etc.) -- not a refactor.

  2. The PHP grammar has NOT yet been through the Actions-module
     cleanup that the TypeScript grammar is going through. Most rules
     are still `snake_case` with their action code written inline in
     `{ ... }` blocks. This is the current normal -- do NOT refactor
     unrelated rules into camelCase or move actions out into a new
     `PhpParserActions.hs`. Restrict your patch to the smallest set of
     productions actually needed to clear the (line, col) failure.

  3. Your output is structured JSON matching the GrammarPatch schema.
     Each edit is (file, old_string, new_string). `old_string` MUST
     appear EXACTLY ONCE in the current file content shown to you,
     including whitespace. Prefer including 1-3 lines of surrounding
     context to make `old_string` unique. To DELETE a section, set
     `new_string` to "". To INSERT, choose an old_string that includes
     an anchor line and have new_string repeat that anchor with the
     insertion before/after it.

  4. Reusable list/option helpers already defined in `PhpParser.y`
     (use these before introducing new ones):

         optional(a)
         listof(a)
         numbered(a)
         ornull(a)
         arrayof(a)
         possibly_empty_arrayof(a)

     A typical widening pattern is to swap `arrayof(X)` for
     `possibly_empty_arrayof(X)` when the failure location shows the
     parser tripping on an empty `array()`.

  5. If you cannot see a clearly safe minimal edit, return `edits: []`
     and explain in `rationale`. An empty patch is better than a guess
     that risks regressing other files.

  6. The project builds with `-Werror=missing-fields` and `-Werror`
     (see dhscanner.cabal). Any new `Ast.<Constructor> { ... }`
     record build you introduce MUST fill every required field of that
     constructor. If you are unsure of the constructor's fields, prefer
     a widening that reuses an existing action block (e.g. routing a
     new shape through an existing rule) over writing a fresh one.

  7. The project also builds with `-Werror=incomplete-uni-patterns`.
     Your patches MUST NOT introduce anonymous lambdas (`\\... -> ...`)
     at all -- not in `Data.List.map`, not in `fmap`, not in `foldr`,
     not anywhere. Use a NAMED top-level function instead, defined in
     the same file, with an explicit type signature, and (when its
     scrutinee is a sum type) one clause per constructor. Pass that name
     by reference to whatever higher-order combinator needs it.

     In particular, if your scrutinee is one of the dhscanner sum types
     (`Ast.Var`, `Ast.Exp`, `Ast.Stmt`, ...), every clause MUST cover
     every constructor. Partial anonymous lambdas like
     `\\(Ast.VarSimple ...) -> ...` mapped over a list of `Ast.Var` are
     the single most common cause of build_failed iterations -- the
     named-function rule above eliminates them by construction.

  8. `PhpParser.y` imports `Ast` UNQUALIFIED at the top of the file
     (unlike `TsParser.y`, which is mid-migration to a qualified
     `import qualified Ast`). Use the existing local style: write
     `Ast.StmtIf`, `Ast.ExpCall`, etc. for constructors that the file
     already qualifies that way, and stay consistent with the rule you
     are editing. Do not change the import header.

  9. Unlike `TsParser.y`, `PhpParser.y` does NOT declare `%expect 0`,
     so a new shift/reduce or reduce/reduce conflict will NOT fail
     `cabal build`. That is NOT a license to introduce conflicts; the
     strict-progress gate runs after the build and a conflict-induced
     change of reductions on a previously-passing file will reject the
     iteration as a regression. Treat conflicts as a real risk.

 10. NEVER add a new token binding to `PhpParser.y`. The `%token` block
     looks like

         'foo' { AlexTokenTag AlexRawToken_foo _ }

     and every `AlexRawToken_foo` constructor on the right-hand side is
     defined in `src/PhpLexer.x` (which you are NOT allowed to edit).
     PhpLexer.x carries three coupled pieces of plumbing per token --
     the `@foo = foo` macro, the `@foo { lex' AlexRawToken_foo }` rule,
     and the `| AlexRawToken_foo` constructor in the `AlexRawToken`
     ADT. A `PhpParser.y`-only edit that introduces a new `'foo'`
     binding will compile-error inside the Happy-generated
     `PhpParser.hs` because the constructor doesn't exist.

     If the failing window shows a node kind whose name is NOT already
     a token in `PhpParser.y` (search the `%token` block for the
     `AlexRawToken_` prefix), do NOT try to add one. Return `edits: []`
     with a `rationale` that names the missing token(s) explicitly so a
     human can wire them through PhpLexer.x first.
"""


def build_user_prompt(
    failing_relpath: str,
    location: dict[str, int],
    native_window: str,
    full_native_len: int,
    parser_y_text: str,
    agents_md_text: str,
) -> str:
    return f"""\
## Failing file

`{failing_relpath}` -- the parser fails at line {location.get('lineStart')}, \
column {location.get('colStart')} of the multi-line native AST. Total native \
AST length: {full_native_len} chars.

### Native AST window around the failure

```
{native_window}
```

## Current src/PhpParser.y

```happy
{parser_y_text}
```

## Active conventions (from AGENTS.md, abbreviated -- note this doc was
## written primarily for the TypeScript grammar; the PHP grammar is at
## an earlier cleanup stage and the snake_case / inline-actions caveats
## in the system prompt take precedence over anything in this doc.)

```markdown
{agents_md_text}
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


def run_compose(args: list[str]) -> subprocess.CompletedProcess:
    cmd = ["docker", "compose", "-f", str(HERE / "compose.yaml"), *args]
    return subprocess.run(cmd, cwd=HERE, capture_output=True, text=True)


def docker_stop_parsers() -> None:
    res = run_compose(["stop", PARSERS_SERVICE])
    if res.returncode != 0:
        raise SystemExit(
            f"docker compose stop {PARSERS_SERVICE} failed:\n{res.stderr}"
        )


def docker_rebuild_parsers() -> None:
    # NOTE: unlike the TypeScript loop, we do NOT call mk_parsers.py here.
    # That script only regenerates the reserved-keyword block in
    # TsParser.y; PhpParser.y has no such block / markers yet, so the call
    # would be a no-op (and would actually raise SystemExit because the
    # markers it looks for are absent from TsParser.y if someone deleted
    # them -- belt-and-braces: just don't invoke it from this loop).
    res = run_compose(["build", PARSERS_SERVICE])
    if res.returncode != 0:
        raise SystemExit(
            f"docker compose build {PARSERS_SERVICE} failed:\n"
            f"{res.stdout[-2000:]}\n{res.stderr[-2000:]}"
        )

    res = run_compose(["up", "-d", PARSERS_SERVICE])
    if res.returncode != 0:
        raise SystemExit(
            f"docker compose up -d {PARSERS_SERVICE} failed:\n{res.stderr}"
        )


# --------------------------------------------------------------------------- #
# Strict progress gate                                                        #
# --------------------------------------------------------------------------- #


@dataclass
class ProgressVerdict:
    accepted: bool
    reason: str
    strict_improvements: list[tuple[str, str, str]]  # (file, before, after)
    regressing_file: str | None = None      # rel-path of the offending file (if rejection was a regression)
    regressing_detail: str | None = None    # short explanation of the regression for that file


def _fmt_pos(info: dict[str, Any]) -> str:
    line = info.get("line_start")
    col = info.get("col_start")
    return f"line={line} col={col}"


def strict_progress(
    old: dict[str, dict[str, Any]],
    new: dict[str, dict[str, Any]],
) -> ProgressVerdict:
    improvements: list[tuple[str, str, str]] = []
    common = set(old) & set(new)
    for rel in sorted(common):
        o = old[rel]
        n = new[rel]
        o_status = o.get("status")
        n_status = n.get("status")

        # Non-grammar statuses (`frontphp_fail`, `transport_fail`) are
        # treated as non-events by the gate, regardless of which side
        # they appear on. They are typically network / container hiccups
        # rather than grammar correctness signals, and gating on them
        # would block otherwise-good iterations.
        if o_status not in ("ok", "fail") or n_status not in ("ok", "fail"):
            continue

        if o_status == "ok":
            if n_status != "ok":
                detail = f"parsed before, now status=fail {_fmt_pos(n)}"
                return ProgressVerdict(
                    accepted=False,
                    reason=f"regression: {rel} {detail}",
                    strict_improvements=[],
                    regressing_file=rel,
                    regressing_detail=detail,
                )
            continue

        o_key = failure_position_key(o)
        if n_status == "ok":
            improvements.append((rel, f"fail@{_fmt_pos(o)}", "ok"))
            continue
        n_key = failure_position_key(n)
        if n_key < o_key:
            detail = f"fail (line,col) went {o_key} -> {n_key}"
            return ProgressVerdict(
                accepted=False,
                reason=f"regression: {rel} {detail}",
                strict_improvements=[],
                regressing_file=rel,
                regressing_detail=detail,
            )
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
            "Iterate one OpenAI-driven PHP grammar patch. Apply / build / regate "
            "failures auto-revert; regression-gate failures leave edits in "
            "place for post-mortem inspection."
        )
    )
    ap.add_argument(
        "--corpus",
        type=Path,
        default=DEFAULT_CORPUS_DIR,
        help=f"corpus directory (default: {DEFAULT_CORPUS_DIR})",
    )
    ap.add_argument(
        "--model",
        default=os.environ.get("OPENAI_MODEL", DEFAULT_MODEL),
        help=f"OpenAI model (default: {DEFAULT_MODEL} or $OPENAI_MODEL)",
    )
    ap.add_argument(
        "--pick",
        choices=["first", "random"],
        default="first",
        help="failure-picking strategy",
    )
    ap.add_argument(
        "--reshuffle",
        action="store_true",
        help=(
            "shorthand for --pick random with a non-deterministic seed. "
            "Use when the loop appears stuck on the same failing file's "
            "esoteric edge case to start from a different file's failure "
            "this iteration."
        ),
    )
    ap.add_argument("--seed", type=int, default=None, help="random seed")
    ap.add_argument(
        "--rebuild-baseline",
        action="store_true",
        help="ignore cached baseline and rebuild from scratch",
    )
    ap.add_argument(
        "--context-radius",
        type=int,
        default=DEFAULT_CONTEXT_RADIUS,
        help="chars before/after the failure offset to show the LLM",
    )
    ap.add_argument(
        "--per-file-timeout",
        type=float,
        default=30.0,
        help="HTTP timeout (seconds) per per-file frontphp/parsers call",
    )
    ap.add_argument(
        "--openai-timeout",
        type=float,
        default=180.0,
        help="OpenAI API call timeout (seconds)",
    )
    ap.add_argument(
        "--frontphp-url",
        default=os.environ.get("FRONTPHP_URL", "http://localhost:4002"),
    )
    ap.add_argument(
        "--parsers-url",
        default=os.environ.get("PARSERS_URL", "http://localhost:4001"),
    )
    ap.add_argument(
        "--dry-run",
        action="store_true",
        help="ask OpenAI but do not apply / rebuild / regate",
    )
    ap.add_argument(
        "--post_mortem_show_native_ast_of",
        type=Path,
        default=None,
        metavar="PATH",
        help=(
            "post-mortem one-shot: print the native AST of PATH via frontphp and exit. "
            "Use after a 'regression failed on:' message to inspect what the parser saw "
            "(skips OpenAI / parsers / corpus / baseline; only frontphp is contacted)."
        ),
    )
    return ap.parse_args(argv)


def maybe_load_dotenv() -> None:
    try:
        from dotenv import load_dotenv  # type: ignore[import-untyped]
    except ImportError:
        return
    load_dotenv(HERE / ".env")


def ensure_openai_key() -> None:
    if not os.environ.get("OPENAI_API_KEY"):
        raise SystemExit(
            "OPENAI_API_KEY not set. Export it or place it in dhscanner.1.parsers/.env"
        )


def read_text_safe(p: Path) -> str:
    return p.read_text(encoding="utf-8") if p.is_file() else ""


def post_mortem_show_native_ast(
    path: Path,
    frontphp_url: str,
    timeout: float,
) -> int:
    """Print the native AST of `path` (as produced by frontphp) and exit.

    Used after a regression-rejected iteration to inspect what the parser
    actually consumed for the offending file. Only frontphp is contacted;
    the parsers container's state is irrelevant here.
    """
    abs_path = path.resolve()
    if not abs_path.is_file():
        print(f"file not found: {abs_path}", file=sys.stderr)
        return 1
    print(f"file:     {abs_path}", file=sys.stderr)
    print(f"frontphp: {frontphp_url}", file=sys.stderr)
    try:
        wait_for_frontphp_alive(frontphp_url, timeout=60)
    except SystemExit as exc:
        print(str(exc), file=sys.stderr)
        return 1
    session = requests.Session()
    native, err = frontphp_native_ast(session, frontphp_url, abs_path, timeout)
    if native is None:
        print(f"frontphp failed: {err}", file=sys.stderr)
        return 1
    print(f"native AST length: {len(native)} chars", file=sys.stderr)
    print("--- begin native AST ---", file=sys.stderr)
    print(native)
    print("--- end native AST ---", file=sys.stderr)
    return 0


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    maybe_load_dotenv()

    # Post-mortem one-shot: show the native AST of a specific file and exit.
    # Skips everything else (no corpus, no baseline, no parsers, no OpenAI).
    if args.post_mortem_show_native_ast_of is not None:
        return post_mortem_show_native_ast(
            args.post_mortem_show_native_ast_of,
            args.frontphp_url,
            args.per_file_timeout,
        )

    ensure_openai_key()

    rng = random.Random(args.seed)

    corpus = discover_corpus(args.corpus)
    if not corpus:
        raise SystemExit(f"no .php files under {args.corpus}")
    print(f"corpus: {len(corpus)} files under {args.corpus}")

    print("checking frontphp and parsers health...")
    wait_for_frontphp_alive(args.frontphp_url, timeout=60)
    wait_for_healthy(args.parsers_url, timeout=30)

    cached = load_baseline_or_none() if not args.rebuild_baseline else None
    if cached and baseline_is_fresh(cached, args.corpus):
        # Trim cached entries against the current corpus filter -- this lets
        # filter changes take effect without forcing a full
        # --rebuild-baseline.
        corpus_rels = {
            str(p.relative_to(args.corpus)).replace("\\", "/") for p in corpus
        }
        cached_files: dict[str, dict[str, Any]] = cached["files"]
        files_map = {
            rel: info for rel, info in cached_files.items() if rel in corpus_rels
        }
        dropped = len(cached_files) - len(files_map)
        suffix = f" (dropped {dropped} no-longer-in-corpus)" if dropped else ""
        print(
            f"baseline: cached  total={len(files_map)}  "
            f"{format_status_counts(status_counts(files_map))}{suffix}"
        )
    else:
        print("baseline: rebuilding...")
        files_map = build_baseline(
            args.corpus, corpus, args.frontphp_url, args.parsers_url, args.per_file_timeout
        )
        save_baseline(args.corpus, files_map)

    pick_strategy = "random" if args.reshuffle else args.pick
    chosen = pick_failure(files_map, pick_strategy, rng)
    if chosen is None:
        counts = status_counts(files_map)
        total_failures = counts.get("fail", 0)
        trivial_failures = sum(
            1
            for v in files_map.values()
            if v.get("status") == "fail" and is_trivial_failure_location(v)
        )
        actionable = total_failures - trivial_failures
        non_grammar_total = sum(counts.get(s, 0) for s in NON_GRAMMAR_STATUSES)
        ok_total = counts.get("ok", 0)

        # Disambiguate the three "no actionable failure" cases. The
        # dangerous one is the third: the loop has nothing to fix not
        # because the grammar is great, but because every file errored
        # before the grammar saw it. Surface that loudly with a sample
        # diagnostic so the user knows it's a pipeline problem, not a
        # 100% coverage celebration.
        if total_failures == 0 and non_grammar_total == 0 and ok_total > 0:
            print("no failures in baseline -- nothing to do.")
            return 0

        if total_failures > 0:
            print(
                f"no actionable failures in baseline -- "
                f"{trivial_failures} remaining failure(s) are at "
                f"(line=1,col=1) and currently skipped by the picker; "
                f"{actionable} other failure(s) qualified."
            )
            return 0

        # Third case: no grammar failures, no successes worth speaking of,
        # everything ended up in a non-grammar status bucket. This means
        # the frontphp + parsers + transport pipeline isn't working end
        # to end, not that the PHP grammar is parsing everything. Print
        # a clear diagnostic plus a sample so the user can act on it.
        print()
        print(
            "no grammar-level failures in baseline, but only because the "
            "pre-grammar pipeline never produced a usable native AST for any "
            "file. The loop has nothing to fix in PhpParser.y until this is "
            "resolved."
        )
        print(
            f"  ok={ok_total}  fail={total_failures}  "
            f"non-grammar={non_grammar_total}  (status breakdown: "
            f"{format_status_counts(counts)})"
        )
        print()
        print("status meanings:")
        print(
            "  frontphp_fail   -- frontphp (Laravel) refused / timed out, "
            "or nikic/PhpParser returned its 'ERROR' sentinel for that file."
        )
        print(
            "  transport_fail  -- request to parsers (port 4001) failed: "
            "non-200, JSON decode error, or connection refused."
        )
        print("re-probing one such file now to surface the actual error...")
        diagnostic = diagnose_one_non_grammar(
            files_map,
            args.corpus,
            args.frontphp_url,
            args.parsers_url,
            args.per_file_timeout,
        )
        if diagnostic:
            print(f"  sample: {diagnostic}")
        print()
        print("suggested next steps:")
        print("  1. confirm both services are up:")
        print(
            "       docker compose -f dhscanner.1.parsers/compose.yaml ps"
        )
        print("  2. inspect one file end-to-end via the post-mortem path:")
        sample_abs = (args.corpus / sorted(files_map)[0]).resolve() if files_map else None
        if sample_abs is not None:
            print(
                f'       python "{HERE / "php_agent_loop.py"}" '
                f'--post_mortem_show_native_ast_of "{sample_abs}"'
            )
        print(
            "  3. once frontphp + parsers are healthy, re-run with "
            "--rebuild-baseline to discard the all-failed cached baseline."
        )
        return 1

    chosen_abs = args.corpus / chosen
    chosen_info = files_map[chosen]
    line = chosen_info.get("line_start")
    col = chosen_info.get("col_start")

    # 7.a chosen failed file
    print(f"chosen failed file: {chosen}")
    # 7.b location of failure
    print(f"location of failure: line={line} col={col}")

    # 7.c window around failure (the native ast)
    session = requests.Session()
    native, ferr = frontphp_native_ast(
        session, args.frontphp_url, chosen_abs, args.per_file_timeout
    )
    if native is None:
        print(f"could not re-fetch native AST for chosen file: {ferr}")
        return 1
    window = context_window(native, line, col, args.context_radius)
    print("window before/after the failed location (native ast):")
    print(window)

    parser_y_text = read_text_safe(SRC_DIR / "PhpParser.y")
    agents_md_text = read_text_safe(HERE / "AGENTS.md")

    user_prompt = build_user_prompt(
        chosen,
        {"lineStart": line or 1, "colStart": col or 1},
        window,
        len(native),
        parser_y_text,
        agents_md_text,
    )

    print(f"asking {args.model} for a grammar patch...")
    try:
        patch = call_openai(args.model, SYSTEM_PROMPT, user_prompt, args.openai_timeout)
    except Exception as exc:  # noqa: BLE001
        print(f"openai call failed: {exc}")
        return 1

    rationale = patch.get("rationale", "")
    edits = patch.get("edits", []) or []

    # 7.d found parser adjustment
    print(f"found parser adjustment: {rationale}")
    if not edits:
        print("(model returned no edits; aborting)")
        log_iteration(
            {
                "chosen": chosen,
                "location": {"line": line, "col": col},
                "rationale": rationale,
                "edits": [],
                "verdict": "no_edits",
            }
        )
        return 1

    if args.dry_run:
        print("dry-run: would apply", len(edits), "edit(s); skipping rebuild.")
        log_iteration(
            {
                "chosen": chosen,
                "location": {"line": line, "col": col},
                "rationale": rationale,
                "edits": edits,
                "verdict": "dry_run",
            }
        )
        return 0

    snap = snapshot_editables()
    SNAPSHOT_DIR.mkdir(parents=True, exist_ok=True)
    for rel, content in snap.items():
        (SNAPSHOT_DIR / rel.replace("/", "__")).write_bytes(content)

    ok, reason = apply_edits(edits)
    if not ok:
        restore_snapshot(snap)
        print(f"could not apply edits: {reason}")
        log_iteration(
            {
                "chosen": chosen,
                "location": {"line": line, "col": col},
                "rationale": rationale,
                "edits": edits,
                "verdict": f"apply_failed: {reason}",
            }
        )
        return 1

    # 7.e stopping container
    print("stopping container ......")
    try:
        docker_stop_parsers()
    except SystemExit as exc:
        restore_snapshot(snap)
        print(str(exc))
        return 1

    # 7.f rebuilding
    print("rebuilding ......")
    try:
        docker_rebuild_parsers()
        wait_for_healthy(args.parsers_url, timeout=HEALTHCHECK_TIMEOUT_S)
    except SystemExit as exc:
        restore_snapshot(snap)
        # Best-effort: bring the previous container back up.
        run_compose(["up", "-d", PARSERS_SERVICE])
        print(str(exc))
        log_iteration(
            {
                "chosen": chosen,
                "location": {"line": line, "col": col},
                "rationale": rationale,
                "edits": edits,
                "verdict": "build_failed",
            }
        )
        return 1

    # 7.g retrying (re-parse the entire corpus against the new container)
    print("retrying .....")
    new_files_map = build_baseline(
        args.corpus, corpus, args.frontphp_url, args.parsers_url, args.per_file_timeout
    )

    verdict = strict_progress(files_map, new_files_map)
    log_iteration(
        {
            "chosen": chosen,
            "location": {"line": line, "col": col},
            "rationale": rationale,
            "edits": edits,
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
            script_invocation = f'python "{HERE / "php_agent_loop.py"}"'
            print(
                f'    {script_invocation} --post_mortem_show_native_ast_of "{regressing_abs}"'
            )
        else:
            print(f"REJECTED: {verdict.reason}")
        print()
        print("(grammar edits left in place; parsers container is running the rejected grammar.")
        print(" to manually revert: restore src/PhpParser.y from .dev/php-snapshot/,")
        print(" then `docker compose build parsers && docker compose up -d parsers`.)")
        return 1

    print(f"ACCEPTED: {verdict.reason}")
    for rel, before, after in verdict.strict_improvements[:10]:
        print(f"  {rel}: {before} -> {after}")
    if len(verdict.strict_improvements) > 10:
        print(f"  ... and {len(verdict.strict_improvements) - 10} more")

    save_baseline(args.corpus, new_files_map)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
