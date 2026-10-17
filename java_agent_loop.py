"""Agent loop: improve JavaParser.y / JavaParserActions.hs grammar coverage.

Hybrid of `agent_loop.py` (TypeScript) and `php_agent_loop.py` (PHP),
adapted for Java. The three loops share the same strict-progress
methodology, the same OpenAI structured-output patch schema, and the
same snapshot / apply / rebuild / regate state machine. They differ
only in the language-specific plumbing. Java sits closer to PHP on
the AST-shape axis and closer to TS on the editable-files axis:

                        agent_loop.py (TS)       java_agent_loop.py (this file)
  front service         frontts :4000            frontjava :5005
  front endpoint        /to/native/ts/ast        /to/native/java/ast
  parsers endpoint      /from/ts/to/dhscanner    /from/java/to/dhscanner
  native AST shape      single-line JSON         MULTI-LINE pretty-printed
                                                 JSON (same keys: kind /
                                                 loc / children / text /
                                                 startLine / startCol, but
                                                 with newlines + 2-space
                                                 indent, so progress gate
                                                 compares (line, col)
                                                 lexicographically -- same
                                                 as the PHP loop)
  editable files        src/TsParser.y +         src/JavaParser.y +
                        src/TsParserActions.hs   src/JavaParserActions.hs
  grammar-hash files    + src/TyKeywords.txt     + src/JavaLexer.x
  pre-build codegen     mk_parsers.py            none (Java has no
                        (keyword-block regen)    `-- reserved keywords`
                                                 markers in JavaParser.y)
  docker chain          dhscanner.1.parsers      dhscanner.runner compose
                        standalone compose.yaml  chain + compose.parsers-dev
                                                 override (parsers rebuilt
                                                 in place; frontjava stays
                                                 up across iterations)
  default corpus        formbricks               DSpace
  cached baseline       .dev/baseline.json       .dev/java-baseline.json
  iterations log        .dev/iterations/         .dev/java-iterations/
  snapshot              .dev/snapshot/           .dev/java-snapshot/

For one iteration this script:

  1. Builds (or loads cached) baseline of {file -> parse outcome} over
     the Java corpus at --corpus (default: C:\\Users\\tuna_\\GitHub\\DSpace).
     The corpus filter keeps `*.java` under `src/main/java/`-style
     production paths and drops the usual noise: anything under a
     `src/test/`, `target/`, `build/`, or `.gradle/` component, plus
     common test-file basenames (`*Test.java`, `*IT.java`).
  2. Picks a failing file. Reads its native AST plus the parser's exact
     (line, col) failure location. Java native ASTs are multi-line
     pretty-printed JSON (frontjava's `AstPrinter` emits newlines and
     2-space indent), so progress is compared lexicographically on
     (line, col) -- same as the PHP loop, unlike the TS loop which
     compares on column alone.
  3. Asks OpenAI (gpt-5 by default, structured-output JSON) for a
     minimal patch to JavaParser.y and/or JavaParserActions.hs that
     should make the parser advance past that column.
  4. Snapshots the editable files, applies the patch, then rebuilds
     ONLY the `parsers` service via the dhscanner.runner compose chain
     with the dev override:

         docker compose --env-file .env \\
           -f ./compose/compose.base.yaml \\
           -f ./compose/compose.app.yaml \\
           -f ./compose/compose.fronts.yaml \\
           -f ./compose/compose.prebuilt.yaml \\
           -f ./compose/compose.workers.yaml \\
           -f ./compose/compose.parsers-dev.yaml \\
           stop parsers && build parsers && up -d --no-deps parsers

     polls /healthcheck, re-parses the entire corpus.
  5. Strict-progress gate (per AGENTS.md):
       - every previously-passing file still passes;
       - every previously-failing file now passes OR fails at a
         (line, col) >= its previous (line, col) under lexicographic
         ordering (line first, column second). Multi-line AST means
         col alone is not a monotonic progress signal.
       - at least one file strictly progresses (was failing and now
         passes, OR was failing and now fails at strictly greater
         (line, col)).
     On gate pass the edit stays applied and the new state becomes the
     baseline. On any failure (parse error, build error, container not
     healthy, gate violation) the editable files are restored from the
     snapshot and the iteration aborts non-zero (same as the TS loop:
     regression-gate failures leave edits in place for post-mortem).

Configuration:
  Environment:
    OPENAI_API_KEY    required; loaded from os.environ or .env in this dir.
    OPENAI_MODEL      optional; defaults to "gpt-5".
    FRONTJAVA_URL     optional; defaults to http://localhost:5005.
    PARSERS_URL       optional; defaults to http://localhost:4001.
    RUNNER_ROOT       optional; defaults to inferring from this file's
                      location (../.. from the parsers submodule, i.e.
                      the dhscanner.runner working copy that owns the
                      compose/ chain).

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
  $env:OPENAI_API_KEY = 'sk-...'
  python dhscanner.core/dhscanner.service.parsers/java_agent_loop.py
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
BASELINE_PATH = DEV_DIR / "java-baseline.json"
ITERATIONS_DIR = DEV_DIR / "java-iterations"
SNAPSHOT_DIR = DEV_DIR / "java-snapshot"

# Infer the dhscanner.runner working copy root. HERE is
# `<runner>/dhscanner.core/dhscanner.service.parsers`, so two parent hops
# land on the runner root that owns the compose/ chain.
DEFAULT_RUNNER_ROOT = HERE.parents[1]

DEFAULT_CORPUS_DIR = Path(r"C:\Users\tuna_\GitHub\DSpace")
DEFAULT_MODEL = "gpt-5"
DEFAULT_CONTEXT_RADIUS = 500
HEALTHCHECK_TIMEOUT_S = 180

PARSERS_SERVICE = "parsers"

# docker compose file chain, relative to the runner root. Mirrors the
# documented invocation at the top of compose/compose.parsers-dev.yaml.
RUNNER_COMPOSE_FILES: tuple[str, ...] = (
    "compose/compose.base.yaml",
    "compose/compose.app.yaml",
    "compose/compose.fronts.yaml",
    "compose/compose.prebuilt.yaml",
    "compose/compose.workers.yaml",
    "compose/compose.parsers-dev.yaml",
)

EDITABLE_RELPATHS: tuple[str, ...] = (
    "src/JavaParser.y",
    "src/JavaParserActions.hs",
)

# Files that participate in the grammar hash (cache invalidation key for the
# baseline). The lexer is included because adding / renaming a token in the
# lexer changes what the built parser accepts, same as editing the grammar.
GRAMMAR_HASH_RELPATHS: tuple[str, ...] = (
    "src/JavaParser.y",
    "src/JavaParserActions.hs",
    "src/JavaLexer.x",
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


@dataclass
class ParseResult:
    status: str          # "ok" | "fail" | "frontjava_fail" | "transport_fail"
    col_start: int | None
    line_start: int | None
    native_ast_len: int | None
    detail: str | None   # short human-readable detail for fails

    def to_baseline(self) -> dict[str, Any]:
        return {
            "status": self.status,
            "col_start": self.col_start,
            "line_start": self.line_start,
        }


# --------------------------------------------------------------------------- #
# Corpus discovery                                                            #
# --------------------------------------------------------------------------- #


# Path components we never descend into. Maven / Gradle build outputs plus
# the (very common) `src/test/java` convention for unit tests. We drop test
# files for the same reason the TS loop drops `*.test.ts` and the PHP loop
# drops `*Test.php` -- test corpora tend to contain adversarial constructs
# written specifically to probe parser edge cases, which skews the picker
# toward low-value iterations instead of real production coverage.
EXCLUDED_PATH_COMPONENTS: frozenset[str] = frozenset({
    "target",      # Maven build output
    "build",       # Gradle build output
    ".gradle",     # Gradle cache
    "node_modules",  # defensive (some Java repos vendor JS tooling)
})

# Basename suffixes that indicate a test file in the two mainstream Java
# conventions: `FooTest.java` (Maven surefire) and `FooIT.java` (Maven
# failsafe / integration tests). We also skip anything under a path
# component literally named `test` -- that catches both `src/test/java/*`
# and the occasional `*/test/*/Foo.java` layout.
TEST_BASENAME_SUFFIXES: tuple[str, ...] = ("Test.java", "IT.java")


def discover_corpus(corpus_dir: Path) -> list[Path]:
    """Return production `*.java` files under corpus_dir, excluding build
    outputs and test files (per EXCLUDED_PATH_COMPONENTS /
    TEST_BASENAME_SUFFIXES / 'test' path component)."""
    if not corpus_dir.is_dir():
        raise SystemExit(f"corpus directory not found: {corpus_dir}")

    out: list[Path] = []
    for p in corpus_dir.rglob("*.java"):
        parts = set(p.parts)
        if parts & EXCLUDED_PATH_COMPONENTS:
            continue
        if "test" in parts:
            continue
        if any(p.name.endswith(suffix) for suffix in TEST_BASENAME_SUFFIXES):
            continue
        out.append(p)
    out.sort()
    return out


# --------------------------------------------------------------------------- #
# HTTP clients                                                                #
# --------------------------------------------------------------------------- #


def frontjava_native_ast(
    session: requests.Session,
    frontjava_url: str,
    file_path: Path,
    timeout: float,
) -> tuple[str | None, str | None]:
    """POST source file to frontjava; return (native_ast, error_detail).

    frontjava's `/to/native/java/ast` endpoint (defined in
    dhscanner.core/dhscanner.service.fronts/java/.../App.java) returns:
      * 200 with a single-line compact JSON native AST on success;
      * 200 with a `{"status": "FAIL", ...}` envelope on parse failure
        inside javac's `com.sun.source.*` tree API;
      * non-200 on transport / IO error.
    We funnel the FAIL envelope into a `frontjava_fail` status so it is
    recognizable in the baseline instead of masquerading as a short OK."""
    try:
        with file_path.open("rb") as fl:
            files = {"source": (file_path.name, fl, "text/plain")}
            resp = session.post(
                f"{frontjava_url}/to/native/java/ast",
                files=files,
                timeout=timeout,
            )
    except requests.RequestException as exc:
        return None, f"frontjava transport error: {exc}"

    if resp.status_code != 200:
        return None, f"frontjava http {resp.status_code}: {resp.text[:200]}"

    text = resp.text
    if text.startswith("{") and '"status"' in text and '"FAIL"' in text:
        return None, f"frontjava FAIL: {text[:200]}"
    return text, None


def parsers_translate(
    session: requests.Session,
    parsers_url: str,
    filename: str,
    native_ast: str,
    timeout: float,
) -> ParseResult:
    """POST native AST to parsers /from/java/to/dhscanner/ast."""
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
            f"{parsers_url}/from/java/to/dhscanner/ast",
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
    frontjava_url: str,
    parsers_url: str,
    file_path: Path,
    rel_filename: str,
    timeout: float,
) -> tuple[ParseResult, str | None]:
    """Run the full frontjava -> parsers pipeline. Returns (result, native_ast)."""
    native, err = frontjava_native_ast(session, frontjava_url, file_path, timeout)
    if native is None:
        return ParseResult("frontjava_fail", None, None, None, err), None
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
    frontjava_url: str,
    parsers_url: str,
    timeout: float,
) -> dict[str, dict[str, Any]]:
    session = requests.Session()
    out: dict[str, dict[str, Any]] = {}
    n = len(files)
    last_print = 0.0
    for i, p in enumerate(files, start=1):
        rel = str(p.relative_to(corpus_dir)).replace("\\", "/")
        res, _ = parse_file(session, frontjava_url, parsers_url, p, rel, timeout)
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
    """A `(line=1, col=1)` failure means the parser bailed at the very first
    token of the native AST. These iterations rarely produce useful progress
    for the same reason as in the TS loop: the context window around col=1
    shows the first ~250 chars of the AST (always a `COMPILATION_UNIT`
    opener in Java's case), so the model can't reason about the actual
    cause. Skip them in the picker so iteration budget goes to files where
    the failure column actually points at parser-grammar work."""
    return info.get("line_start") == 1 and info.get("col_start") == 1


def failure_position_key(info: dict[str, Any]) -> tuple[int, int]:
    """Lexicographic sort/comparison key for a fail location. Missing
    components are treated as 0 so a None location does not crash the
    comparison; in practice frontjava + parsers always return a complete
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

    Java native ASTs are multi-line pretty-printed JSON (frontjava's
    `AstPrinter` emits newlines + 2-space indent), so we compute a
    character offset that corresponds to (line_start, col_start) --
    1-indexed -- and then take `radius` chars on each side, marking
    the failure point with `<<<HERE line=L col=C>>>`. Same approach
    as the PHP loop."""
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
dhscanner.1.parsers, which translates the native Java AST emitted by
`dhscanner.0.fronts/java` (a tiny jlink-trimmed service built on javac's
`com.sun.source.*` tree API; see `AstPrinter.java`) into the dhscanner AST.

The native AST is MULTI-LINE pretty-printed JSON (newlines + 2-space
indent), keyed by:

    kind        -- SCREAMING_SNAKE_CASE node type (e.g. "COMPILATION_UNIT",
                   "METHOD", "VARIABLE", "MEMBER_SELECT", "IF", "RETURN",
                   "BINARY", "INT_LITERAL", ...). Grammar productions
                   match these as string-literal tokens: 'METHOD', 'IF',
                   etc. See the `%token` block near the top of
                   JavaParser.y for the full list that currently exists.
    loc         -- { startLine, startCol, endLine, endCol }
    children    -- array of child nodes (nested native AST objects)
    text        -- payload text for ID / INT / STR nodes (identifier
                   spelling, numeric literal, string literal, ...)

Your job in this turn: propose ONE minimal, surgical patch that lets the
parser advance past a specific (line, column) failure location on a
specific input. The patch is allowed to touch ONLY:

  - src/JavaParser.y               (the Happy grammar)
  - src/JavaParserActions.hs       (the smart-constructor helpers it imports)

Rules of the road:

  1. Native Java ASTs are MULTI-LINE (frontjava's `AstPrinter` emits
     newlines + 2-space indent, same spirit as nikic/PhpParser's
     NodeDumper; different from the single-line TS front). The parser
     reports failure as a 1-indexed (lineStart, colStart) pair into
     that text. Your patch is accepted if and only if EVERY previously
     -passing corpus file still parses AND every previously-failing
     file now passes OR fails at (line, col) >= its previous
     (line, col) under LEXICOGRAPHIC ordering (line first, column
     second), AND at least one file strictly progresses. You will not
     see the rest of the corpus, only the chosen failing file's window.
     Aim for a conservative, locally-scoped change (widen one
     production, add one alternative, make one symbol optional,
     etc.) -- not a refactor.

  2. The grammar is MID-MIGRATION toward the actions-split convention
     the TS grammar already follows. Rules named in `camelCase`
     (`stmtImport`, `stmtPackage`, ...) are already consolidated and
     their action bodies live as smart constructors in
     `JavaParserActions.hs` (imported as `import qualified
     JavaParserActions` -- NOT abbreviated like `Actions`). Rules in
     `snake_case` (`stmt_class`, `exp_binop`, `stmt_vardec`, ...) are
     TODO: they still carry their action code inline in `{ ... }`.
     Both styles coexist and are both fine to edit; do not refactor
     unrelated snake_case rules into camelCase in the same patch.

  3. Your output is structured JSON matching the GrammarPatch schema.
     Each edit is (file, old_string, new_string). `old_string` MUST
     appear EXACTLY ONCE in the current file content shown to you,
     including whitespace. Prefer including 1-3 lines of surrounding
     context to make `old_string` unique. To DELETE a section, set
     `new_string` to "". To INSERT, choose an old_string that includes
     an anchor line and have new_string repeat that anchor with the
     insertion before/after it.

  4. Reusable list/option helpers already defined in `JavaParser.y`
     (use these before introducing new ones):

         listof(a):      a | a listof(a)                 -- non-empty
         commalistof(a): a | a ',' commalistof(a)        -- non-empty, comma-sep
         optional(a):    nothing | a                     -- Maybe a

     NOTE: Java's grammar does NOT yet have `possibly_empty_commalistof`
     the way the TS grammar does. The current idiom for an optionally-
     empty comma-list is `optional(commalistof(x))`, which returns
     `Maybe [x]`. If you need to accept an empty list where the current
     rule has `commalistof(x)`, prefer wrapping it in `optional(...)`
     and handling the `Nothing` case in the action (`fromMaybe []`) --
     do NOT invent a `possibly_empty_commalistof` helper in this
     iteration; that's a separate consolidation.

  5. If you cannot see a clearly safe minimal edit, return `edits: []`
     and explain in `rationale`. An empty patch is better than a guess
     that risks regressing other files.

  6. The project builds with `-Werror=missing-fields` and `-Werror`.
     Any new `Ast.<Constructor> { ... }` record build you introduce
     MUST fill every required field of that constructor. If you are
     unsure of a constructor's fields, prefer a widening that reuses
     an existing action block (e.g. routing a new shape through an
     existing rule) over writing a fresh one.

  7. The project also builds with `-Werror=incomplete-uni-patterns`
     (and `-Werror=incomplete-patterns`). To eliminate this entire
     class of mistake by construction, your patches MUST NOT introduce
     anonymous lambdas (`\\... -> ...`) at all -- not in
     `Data.List.map`, not in `fmap`, not in `foldr`, not anywhere.
     Use a NAMED top-level function instead, defined in the same file,
     with an explicit type signature, and (when its scrutinee is a sum
     type) one clause per constructor. Pass that name by reference to
     whatever higher-order combinator needs it.

     In particular, if your scrutinee is one of the dhscanner sum
     types (`Ast.Var`, `Ast.Exp`, `Ast.Stmt`, ...), every clause MUST
     cover every constructor. For `Ast.Var` (3 cases) the canonical
     shape is one clause per constructor delegating to a per-
     constructor helper:

         foo :: Ast.Var -> R
         foo (Ast.VarSimple    v) = fooVarSimple    v
         foo (Ast.VarField     v) = fooVarField     v
         foo (Ast.VarSubscript v) = fooVarSubscript v

         fooVarSimple    :: Ast.VarSimpleContent    -> R
         fooVarField     :: Ast.VarFieldContent     -> R
         fooVarSubscript :: Ast.VarSubscriptContent -> R

     Partial anonymous lambdas like `\\(Ast.VarSimple ...) -> ...`
     mapped over a list of `Ast.Var` are the single most common cause
     of build_failed iterations -- the named-function rule above
     eliminates them by construction.

  8. Ast import style DIFFERS between the two editable files, which is
     the current normal mid-migration; do not try to unify them:

       * `src/JavaParser.y` has `import Ast` (unqualified) at the top,
         but every inline action still spells names as `Ast.StmtImport`,
         `Ast.ExpVar`, etc. for grep-ability. Keep that convention in
         your edits -- always write `Ast.<thing>`, do not rely on the
         unqualified import to drop the prefix.
       * `src/JavaParserActions.hs` has `import qualified Ast`, so
         `Ast.<thing>` is the ONLY way to reference AST names there.

     Net result: every new reference to a dhscanner.ast name in your
     patch should look like `Ast.ExpCall`, `Ast.StmtVardec`, etc.,
     regardless of which editable file it lands in. Do not change the
     import headers of either file in your patch.

  9. The grammar does NOT declare `%expect 0`, so a new shift/reduce or
     reduce/reduce conflict will NOT fail `cabal build` on its own.
     That is NOT a license to introduce conflicts: a conflict-induced
     change of reductions on a previously-passing file will reject the
     iteration as a regression under the strict-progress gate. Treat
     conflicts as a real risk, same as in TsParser.y.

 10. NEVER add a new token binding to `JavaParser.y`. The `%token`
     block looks like

         'METHOD' { AlexTokenTag AlexRawToken_METHOD _ }

     and every `AlexRawToken_<FOO>` constructor on the right-hand side
     is defined in `src/JavaLexer.x` (which you are NOT allowed to
     edit). A `JavaParser.y`-only edit that introduces a new
     `'FOO'` token binding will compile-error inside the Happy-
     generated `JavaParser.hs` because the constructor doesn't exist.

     If the failing window shows a `"kind":"<FOO>"` whose name is NOT
     already a token in `JavaParser.y` (search the `%token` block for
     the `AlexRawToken_` prefix), do NOT try to add one. Return
     `edits: []` with a `rationale` that names the missing token(s)
     explicitly so a human can wire them through JavaLexer.x first.

 11. The `dhscanner.ast` intermediate representation deliberately
     keeps a MINIMAL binary-operator set. The only `Ast.<Op>`
     constructors that exist for the `expBinopOperator` / `binop_kind`
     slot are:

         Ast.EQ      -- comparison-ish
         Ast.PLUS    -- additive / "everything else that's not multiplicative"
         Ast.TIMES   -- multiplicative

     Every other native binary operator (logical `&&` / `||`, bitwise
     `&` / `|` / `^`, shifts `<<` / `>>`, modulo `%`, inequalities
     `<` / `<=` / `>` / `>=` / `!=`, string concat, ...) MUST
     COLLAPSE into one of those three in your action body. This is
     by design: dhscanner is a reachability/security AST and does
     not distinguish operators beyond that coarse bucket.

     Writing a reference like `Ast.AND`, `Ast.OR`, `Ast.LT`,
     `Ast.NEQ`, `Ast.XOR`, `Ast.MOD`, `Ast.SHL`, etc. is a
     guaranteed `build_failed` -- those constructors do not exist
     in the dhscanner.ast package. The established convention in
     the other grammars (see `src/RbParser.y`, `src/PhpParser.y`,
     and the discussion comments in `src/TsParser.y`
     / `src/TsParserActions.hs`) is:

         comparison-ish  (`==`, `!=`, `<`, `>=`, ...)  -> `Ast.EQ`
         multiplicative-only (`*`, `/`)                -> `Ast.TIMES`
         everything else (`&&`, `||`, `&`, `|`, `^`,
                          `<<`, `>>`, `%`, `+`, `-`,
                          string concat, ...)          -> `Ast.PLUS`

     Mirror those grammars when widening Java's `binop_kind`. Do
     NOT invent new `Ast.<Op>` constructor names.
"""


def build_user_prompt(
    failing_relpath: str,
    location: dict[str, int],
    native_window: str,
    full_native_len: int,
    parser_y_text: str,
    actions_hs_text: str,
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

## Current src/JavaParser.y

```happy
{parser_y_text}
```

## Current src/JavaParserActions.hs

```haskell
{actions_hs_text}
```

## Active conventions (from AGENTS.md, abbreviated -- note this doc was
## written primarily for the TypeScript grammar; the Java grammar is at
## a similar mid-migration stage, and the Java-specific caveats in the
## system prompt (snake_case rules still carry inline actions, no
## possibly_empty_commalistof helper yet, JavaParserActions imported
## without the `Actions` alias, etc.) take precedence over anything
## here that disagrees.)

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


def _compose_base_cmd(runner_root: Path) -> list[str]:
    """Build the `docker compose [--env-file .env] -f ... -f ...` prefix.
    `--env-file .env` is only added if the file actually exists at the
    runner root; the compose chain itself does not require it."""
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
    # NOTE: unlike the TS loop, we do NOT invoke mk_parsers.py here.
    # mk_parsers.py regenerates the `-- reserved keywords start/end`
    # block in TsParser.y from src/TyKeywords.txt; JavaParser.y has no
    # such markers (its keyword list is hand-maintained inline), so the
    # call would either be a no-op or error out looking for the TS
    # markers in TsParser.y. Just skip it.
    res = run_compose(runner_root, ["build", PARSERS_SERVICE])
    if res.returncode != 0:
        raise SystemExit(
            f"docker compose build {PARSERS_SERVICE} failed:\n"
            f"{res.stdout[-2000:]}\n{res.stderr[-2000:]}"
        )

    # `--no-deps` so compose doesn't try to recreate frontjava / frontts
    # / frontphp / ... every iteration. The fronts are stable across
    # grammar iterations; recreating them would just add latency and
    # risk flakes.
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

        if o_status == "ok":
            if n_status != "ok":
                detail = (
                    f"parsed before, now status={n_status}"
                    + (f" {_fmt_pos(n)}" if n_status == "fail" else "")
                )
                return ProgressVerdict(
                    accepted=False,
                    reason=f"regression: {rel} {detail}",
                    strict_improvements=[],
                    regressing_file=rel,
                    regressing_detail=detail,
                )
            continue

        if o_status != "fail":
            # transport / frontjava errors in baseline: don't gate on them
            # but don't count them either.
            continue

        o_key = failure_position_key(o)
        if n_status == "ok":
            improvements.append((rel, f"fail@{_fmt_pos(o)}", "ok"))
            continue
        if n_status != "fail":
            detail = f"failed cleanly before, now status={n_status}"
            return ProgressVerdict(
                accepted=False,
                reason=f"regression: {rel} {detail}",
                strict_improvements=[],
                regressing_file=rel,
                regressing_detail=detail,
            )
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
            "Iterate one OpenAI-driven Java grammar patch. Apply / build / regate "
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
        help="HTTP timeout (seconds) per per-file frontjava/parsers call",
    )
    ap.add_argument(
        "--openai-timeout",
        type=float,
        default=180.0,
        help="OpenAI API call timeout (seconds)",
    )
    ap.add_argument(
        "--frontjava-url",
        default=os.environ.get("FRONTJAVA_URL", "http://localhost:5005"),
    )
    ap.add_argument(
        "--parsers-url",
        default=os.environ.get("PARSERS_URL", "http://localhost:4001"),
    )
    ap.add_argument(
        "--runner-root",
        type=Path,
        default=Path(os.environ.get("RUNNER_ROOT", str(DEFAULT_RUNNER_ROOT))),
        help=(
            "path to the dhscanner.runner working copy that owns the "
            "compose/ chain (default: inferred as ../.. from this file)."
        ),
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
            "post-mortem one-shot: print the native AST of PATH via frontjava and exit. "
            "Use after a 'regression failed on:' message to inspect what the parser saw "
            "(skips OpenAI / parsers / corpus / baseline; only frontjava is contacted)."
        ),
    )
    return ap.parse_args(argv)


def maybe_load_dotenv(runner_root: Path) -> None:
    """Load the OpenAI API key from a .env file.

    Prefers the dhscanner.runner-root `.env` (where the user's existing
    stack env lives, including OPENAI_API_KEY in quoted form); falls
    back to a parsers-submodule-local `.env` for people who prefer to
    keep their key next to this loop.

    `override=True` is important: in a fresh PowerShell session a stale
    OPENAI_API_KEY may already be set as a session env var (from a
    long-ago `$env:OPENAI_API_KEY = '...'` or a user profile),
    shadowing the real key sitting in `.env`. Without override we'd
    silently keep hitting /v1/.../completions with the bad key and
    see nothing but 401s. With override, the file wins.

    Quoting is also normalized: python-dotenv strips surrounding
    single/double quotes from values by default, so a line like
    `OPENAI_API_KEY="sk-proj-..."` loads cleanly without the quote
    characters leaking into the HTTP header."""
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
            "OPENAI_API_KEY not set. Export it or place it in "
            "dhscanner.core/dhscanner.service.parsers/.env"
        )


def ensure_runner_root(runner_root: Path) -> None:
    """Verify the inferred / supplied runner root actually contains the
    compose chain we expect to invoke. Fail loudly up front rather than
    letting `docker compose -f <missing>` produce a cryptic error partway
    through the iteration."""
    missing = [
        rel for rel in RUNNER_COMPOSE_FILES if not (runner_root / rel).is_file()
    ]
    if missing:
        raise SystemExit(
            f"runner root {runner_root} is missing compose files:\n"
            + "\n".join(f"  {rel}" for rel in missing)
            + "\n\npass --runner-root <path-to-dhscanner.runner>"
            " or set RUNNER_ROOT."
        )


def read_text_safe(p: Path) -> str:
    return p.read_text(encoding="utf-8") if p.is_file() else ""


def post_mortem_show_native_ast(
    path: Path,
    frontjava_url: str,
    timeout: float,
) -> int:
    """Print the native AST of `path` (as produced by frontjava) and exit.

    Used after a regression-rejected iteration to inspect what the parser
    actually consumed for the offending file. Only frontjava is contacted;
    the parsers container's state is irrelevant here.
    """
    abs_path = path.resolve()
    if not abs_path.is_file():
        print(f"file not found: {abs_path}", file=sys.stderr)
        return 1
    print(f"file:      {abs_path}", file=sys.stderr)
    print(f"frontjava: {frontjava_url}", file=sys.stderr)
    try:
        wait_for_healthy(frontjava_url, timeout=60)
    except SystemExit as exc:
        print(str(exc), file=sys.stderr)
        return 1
    session = requests.Session()
    native, err = frontjava_native_ast(session, frontjava_url, abs_path, timeout)
    if native is None:
        print(f"frontjava failed: {err}", file=sys.stderr)
        return 1
    print(f"native AST length: {len(native)} chars", file=sys.stderr)
    print("--- begin native AST ---", file=sys.stderr)
    print(native)
    print("--- end native AST ---", file=sys.stderr)
    return 0


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    maybe_load_dotenv(args.runner_root)

    # Post-mortem one-shot: show the native AST of a specific file and exit.
    # Skips everything else (no corpus, no baseline, no parsers, no OpenAI).
    if args.post_mortem_show_native_ast_of is not None:
        return post_mortem_show_native_ast(
            args.post_mortem_show_native_ast_of,
            args.frontjava_url,
            args.per_file_timeout,
        )

    ensure_openai_key()
    ensure_runner_root(args.runner_root)

    rng = random.Random(args.seed)

    corpus = discover_corpus(args.corpus)
    if not corpus:
        raise SystemExit(f"no .java files under {args.corpus}")
    print(f"corpus: {len(corpus)} files under {args.corpus}")

    print("checking frontjava and parsers health...")
    wait_for_healthy(args.frontjava_url, timeout=60)
    wait_for_healthy(args.parsers_url, timeout=60)

    cached = load_baseline_or_none() if not args.rebuild_baseline else None
    if cached and baseline_is_fresh(cached, args.corpus):
        # Trim cached entries against the current corpus filter -- this lets
        # filter changes (e.g. tightening the test-file exclusions) take
        # effect without forcing a full --rebuild-baseline.
        corpus_rels = {
            str(p.relative_to(args.corpus)).replace("\\", "/") for p in corpus
        }
        cached_files: dict[str, dict[str, Any]] = cached["files"]
        files_map = {
            rel: info for rel, info in cached_files.items() if rel in corpus_rels
        }
        dropped = len(cached_files) - len(files_map)
        ok = sum(1 for v in files_map.values() if v.get("status") == "ok")
        fail = sum(1 for v in files_map.values() if v.get("status") == "fail")
        suffix = f" (dropped {dropped} no-longer-in-corpus)" if dropped else ""
        print(f"baseline: cached  ok={ok}  fail={fail}  total={len(files_map)}{suffix}")
    else:
        print("baseline: rebuilding...")
        files_map = build_baseline(
            args.corpus, corpus, args.frontjava_url, args.parsers_url, args.per_file_timeout
        )
        save_baseline(args.corpus, files_map)

    pick_strategy = "random" if args.reshuffle else args.pick
    chosen = pick_failure(files_map, pick_strategy, rng)
    if chosen is None:
        total_failures = sum(
            1 for v in files_map.values() if v.get("status") == "fail"
        )
        trivial_failures = sum(
            1
            for v in files_map.values()
            if v.get("status") == "fail" and is_trivial_failure_location(v)
        )
        actionable = total_failures - trivial_failures
        if total_failures == 0:
            print("no failures in baseline -- nothing to do.")
        else:
            print(
                f"no actionable failures in baseline -- "
                f"{trivial_failures} remaining failure(s) are at "
                f"(line=1,col=1) and currently skipped by the picker; "
                f"{actionable} other failure(s) qualified."
            )
        return 0

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
    native, ferr = frontjava_native_ast(
        session, args.frontjava_url, chosen_abs, args.per_file_timeout
    )
    if native is None:
        print(f"could not re-fetch native AST for chosen file: {ferr}")
        return 1
    window = context_window(native, line, col, args.context_radius)
    print("window before/after the failed location (native ast):")
    print(window)

    parser_y_text = read_text_safe(SRC_DIR / "JavaParser.y")
    actions_hs_text = read_text_safe(SRC_DIR / "JavaParserActions.hs")
    agents_md_text = read_text_safe(HERE / "AGENTS.md")

    user_prompt = build_user_prompt(
        chosen,
        {"lineStart": line or 1, "colStart": col or 1},
        window,
        len(native),
        parser_y_text,
        actions_hs_text,
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
        docker_stop_parsers(args.runner_root)
    except SystemExit as exc:
        restore_snapshot(snap)
        print(str(exc))
        return 1

    # 7.f rebuilding
    print("rebuilding ......")
    try:
        docker_rebuild_parsers(args.runner_root)
        wait_for_healthy(args.parsers_url, timeout=HEALTHCHECK_TIMEOUT_S)
    except SystemExit as exc:
        restore_snapshot(snap)
        # Best-effort: bring the previous container back up.
        run_compose(args.runner_root, ["up", "-d", "--no-deps", PARSERS_SERVICE])
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
        args.corpus, corpus, args.frontjava_url, args.parsers_url, args.per_file_timeout
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
            script_invocation = f'python "{HERE / "java_agent_loop.py"}"'
            print(
                f'    {script_invocation} --post_mortem_show_native_ast_of "{regressing_abs}"'
            )
        else:
            print(f"REJECTED: {verdict.reason}")
        print()
        print("(grammar edits left in place; parsers container is running the rejected grammar.")
        print(" to manually revert: restore src/JavaParser.y + src/JavaParserActions.hs from")
        print(" .dev/java-snapshot/, then re-run the compose build + up chain for `parsers`.)")
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
