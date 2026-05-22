"""Agent loop: improve TsParser.y / TsParserActions.hs grammar coverage.

For one iteration, this script:

  1. Builds (or loads cached) baseline of {file -> parse outcome} over the
     TypeScript corpus at --corpus (default: C:\\Users\\tuna_\\GitHub\\formbricks).
  2. Picks a failing file. Reads its native AST plus the parser's exact
     failure column.
  3. Asks OpenAI (gpt-5 by default, structured-output JSON) for a minimal
     patch to TsParser.y and/or TsParserActions.hs that should make the
     parser advance past that column.
  4. Snapshots the editable files, applies the patch, then
     `docker compose stop parsers`, `docker compose build parsers`,
     `docker compose up -d parsers`, polls /healthcheck, re-parses the
     entire corpus.
  5. Strict-progress gate (per AGENTS.md):
       - every previously-passing file still passes;
       - every previously-failing file now passes OR fails at colStart >=
         its previous colStart;
       - at least one file strictly progresses (was failing and now
         passes, OR was failing and now fails at strictly greater
         colStart).
     On gate pass the edit stays applied and the new state becomes the
     baseline. On any failure (parse error, build error, container not
     healthy, gate violation) the editable files are restored from the
     snapshot and the iteration aborts non-zero.

Configuration:
  Environment:
    OPENAI_API_KEY    required; loaded from os.environ or .env in this dir.
    OPENAI_MODEL      optional; defaults to "gpt-5".
    FRONTTS_URL       optional; defaults to http://localhost:4000.
    PARSERS_URL       optional; defaults to http://localhost:4001.

Usage (PowerShell):
  cd dhscanner.1.parsers
  $env:OPENAI_API_KEY = 'sk-...'
  docker compose up -d --build              # one-time
  python agent_loop.py
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
BASELINE_PATH = DEV_DIR / "baseline.json"
ITERATIONS_DIR = DEV_DIR / "iterations"
SNAPSHOT_DIR = DEV_DIR / "snapshot"

DEFAULT_CORPUS_DIR = Path(r"C:\Users\tuna_\GitHub\formbricks")
DEFAULT_MODEL = "gpt-5"
DEFAULT_CONTEXT_RADIUS = 250
HEALTHCHECK_TIMEOUT_S = 120

PARSERS_SERVICE = "parsers"

EDITABLE_RELPATHS: tuple[str, ...] = (
    "src/TsParser.y",
    "src/TsParserActions.hs",
)

# Files that participate in the grammar hash (cache invalidation key for the
# baseline). TyKeywords.txt is included because mk_parsers.py expands it into
# TsParser.y at build time, so a change to it changes the built parser.
GRAMMAR_HASH_RELPATHS: tuple[str, ...] = (
    "src/TsParser.y",
    "src/TsParserActions.hs",
    "src/TyKeywords.txt",
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
    status: str          # "ok" | "fail" | "frontts_fail" | "transport_fail"
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


def discover_corpus(corpus_dir: Path, include_tsx: bool) -> list[Path]:
    """Return *.ts (and optionally *.tsx) files under corpus_dir, excluding
    node_modules, TypeScript declaration files (.d.ts), and any file whose
    name contains the substring ``.test.`` (e.g. foo.test.ts, foo.test.tsx)."""
    if not corpus_dir.is_dir():
        raise SystemExit(f"corpus directory not found: {corpus_dir}")

    patterns = ["*.ts"]
    if include_tsx:
        patterns.append("*.tsx")

    out: list[Path] = []
    for pat in patterns:
        for p in corpus_dir.rglob(pat):
            parts = set(p.parts)
            if "node_modules" in parts:
                continue
            if p.name.endswith(".d.ts"):
                continue
            if ".test." in p.name:
                continue
            out.append(p)
    out.sort()
    return out


# --------------------------------------------------------------------------- #
# HTTP clients                                                                #
# --------------------------------------------------------------------------- #


def frontts_native_ast(
    session: requests.Session,
    frontts_url: str,
    file_path: Path,
    timeout: float,
) -> tuple[str | None, str | None]:
    """POST source file to frontts; return (native_ast, error_detail)."""
    try:
        with file_path.open("rb") as fl:
            files = {"source": (file_path.name, fl, "text/plain")}
            resp = session.post(
                f"{frontts_url}/to/native/ts/ast",
                files=files,
                timeout=timeout,
            )
    except requests.RequestException as exc:
        return None, f"frontts transport error: {exc}"

    if resp.status_code != 200:
        return None, f"frontts http {resp.status_code}: {resp.text[:200]}"

    text = resp.text
    if text.startswith("{") and '"status"' in text and '"FAIL"' in text:
        return None, f"frontts FAIL: {text[:200]}"
    return text, None


def parsers_translate(
    session: requests.Session,
    parsers_url: str,
    filename: str,
    native_ast: str,
    timeout: float,
) -> ParseResult:
    """POST native AST to parsers /from/ts/to/dhscanner/ast."""
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
            f"{parsers_url}/from/ts/to/dhscanner/ast",
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
    frontts_url: str,
    parsers_url: str,
    file_path: Path,
    rel_filename: str,
    timeout: float,
) -> tuple[ParseResult, str | None]:
    """Run the full frontts -> parsers pipeline. Returns (result, native_ast)."""
    native, err = frontts_native_ast(session, frontts_url, file_path, timeout)
    if native is None:
        return ParseResult("frontts_fail", None, None, None, err), None
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
    frontts_url: str,
    parsers_url: str,
    timeout: float,
) -> dict[str, dict[str, Any]]:
    session = requests.Session()
    out: dict[str, dict[str, Any]] = {}
    n = len(files)
    last_print = 0.0
    for i, p in enumerate(files, start=1):
        rel = str(p.relative_to(corpus_dir)).replace("\\", "/")
        res, _ = parse_file(session, frontts_url, parsers_url, p, rel, timeout)
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
    token of the native AST. Empirically these iterations rarely produce
    useful progress: the agent's context window around col=1 shows the first
    ~250 chars of the AST (which usually look normal), so the model can't
    reason about the actual cause and tends to propose widenings unrelated
    to the real failure -- which then frequently regress other files.
    Until we understand why these top-of-AST failures happen (frontts error
    envelope? top-level shape the grammar can't even start matching?) we
    skip them in the picker so iteration budget goes to files where the
    failure column actually points at parser-grammar work."""
    return info.get("line_start") == 1 and info.get("col_start") == 1


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
        and not is_trivial_failure_location(info)
    ]
    if not failures:
        return None
    if strategy == "random":
        return rng.choice(failures)
    return sorted(failures)[0]


def context_window(native: str, col_start: int | None, radius: int) -> str:
    if not native:
        return "<empty native ast>"
    if not isinstance(col_start, int) or col_start < 1:
        return native[: 2 * radius] + ("..." if len(native) > 2 * radius else "")
    idx = max(0, col_start - 1)
    lo = max(0, idx - radius)
    hi = min(len(native), idx + radius)
    before = native[lo:idx]
    after = native[idx:hi]
    leading = "..." if lo > 0 else ""
    trailing = "..." if hi < len(native) else ""
    return f"{leading}{before}<<<HERE col={col_start}>>>{after}{trailing}"


# --------------------------------------------------------------------------- #
# OpenAI prompt                                                               #
# --------------------------------------------------------------------------- #


SYSTEM_PROMPT = """\
You are an expert Haskell + Happy/Alex grammar engineer working on
dhscanner.1.parsers, which translates a single-line native TypeScript AST
(emitted by `dhscanner.0.fronts/ts`) into the dhscanner AST.

Your job in this turn: propose ONE minimal, surgical patch that lets the
parser advance past a specific failure column on a specific input. The
patch is allowed to touch ONLY:

  - src/TsParser.y               (the Happy grammar)
  - src/TsParserActions.hs       (the smart-constructor helpers it imports)

Rules of the road:

  1. Native ASTs are single-line. The parser reports failure as a 1-indexed
     column into that single line. Your patch is accepted if and only if
     EVERY previously-passing corpus file still parses AND every previously
     -failing file fails at column >= its previous column (or now passes),
     AND at least one file strictly progresses. You will not see the rest
     of the corpus, only the chosen failing file's window. Aim for a
     conservative, locally-scoped change (widen one production, add one
     alternative, make one symbol optional, etc.) -- not a refactor.

  2. The reserved-keyword token block in TsParser.y, between
     `-- reserved keywords start` and `-- reserved keywords end`, is
     auto-generated from src/TyKeywords.txt by `mk_parsers.py` and
     regenerated before every build. NEVER place edits inside that block;
     they will be overwritten. If you genuinely need a new token, say so
     in `rationale` and stop -- do not return an edit (a human will add
     it to TyKeywords.txt).

  3. Your output is structured JSON matching the GrammarPatch schema. Each
     edit is (file, old_string, new_string). `old_string` MUST appear
     EXACTLY ONCE in the current file content shown to you, including
     whitespace. Prefer including 1-3 lines of surrounding context to make
     `old_string` unique. To DELETE a section, set `new_string` to "". To
     INSERT, choose an old_string that includes an anchor line and have
     new_string repeat that anchor with the insertion before/after it.

  4. The grammar's active conventions (smart constructors live in
     TsParserActions.hs, helpers like `optional`, `commalistof`,
     `possibly_empty_commalistof`, `stmtOrBlock`, `instrumentationCall`,
     etc. already exist -- prefer reusing them over inventing new ones).
     camelCase rule names = already cleaned up; snake_case = TODO. When
     possible, your edit reuses existing helpers.

  5. If you cannot see a clearly safe minimal edit, return `edits: []`
     and explain in `rationale`. An empty patch is better than a guess
     that risks regressing other files.

  6. The project builds with `-Werror=incomplete-uni-patterns` (and
     `-Werror=incomplete-patterns`). To eliminate this entire class of
     mistake by construction, your patches MUST NOT introduce anonymous
     lambdas (`\\... -> ...`) at all -- not in `Data.List.map`, not in
     `fmap`, not in `foldr`, not anywhere. Use a NAMED top-level
     function instead, defined in the same file, with an explicit type
     signature, and (when its scrutinee is a sum type) one clause per
     constructor. Pass that name by reference to whatever higher-order
     combinator needs it.

     In particular, if your scrutinee is one of the dhscanner sum types
     (`Ast.Var`, `Ast.Exp`, `Ast.Stmt`, ...), every clause MUST cover
     every constructor. For `Ast.Var` (3 cases) the canonical shape is
     one clause per constructor delegating to a per-constructor helper:

         foo :: Ast.Var -> R
         foo (Ast.VarSimple    v) = fooVarSimple    v
         foo (Ast.VarField     v) = fooVarField     v
         foo (Ast.VarSubscript v) = fooVarSubscript v

         fooVarSimple    :: Ast.VarSimpleContent    -> R
         fooVarField     :: Ast.VarFieldContent     -> R
         fooVarSubscript :: Ast.VarSubscriptContent -> R

     ...and then `Data.List.map foo someVars` -- never
     `Data.List.map (\\(Ast.VarSimple ...) -> ...) someVars`. Same
     shape for `Ast.Exp`, `Ast.Stmt`, etc. -- one clause per
     constructor, each delegating to a per-constructor helper named
     after that constructor's content type. If a case is genuinely
     impossible, write it explicitly (e.g. with `error "unreachable: ..."`
     or a `_` fallback) rather than leaving it implicit; never rely on
     `-Wno-incomplete-uni-patterns` -- it is not enabled. Partial
     anonymous lambdas like `\\(Ast.VarSimple ...) -> ...` mapped over
     a list of `Ast.Var` are the single most common cause of
     build_failed iterations -- the named-function rule above
     eliminates them by construction.

  7. The project builds with `-Werror` (see dhscanner.cabal), which makes
     `-Wname-shadowing` fatal. Both `TsParser.y` and `TsParserActions.hs`
     now `import qualified Ast` -- so every reference to a dhscanner.ast
     name MUST be spelled `Ast.<thing>` (e.g. `Ast.ExpCall`,
     `Ast.callee`, `Ast.stmtBlockContent`). Never insert `import Ast`
     (unqualified) into a patch; never spell `Ast` names without the
     `Ast.` prefix. Conversely, because Ast names are now only reachable
     as `Ast.<thing>`, you ARE free to use short local names for helper
     parameters / `let`-bindings / `where`-clauses (`callee`, `args`,
     `loc`, `filename`, ...) without shadowing anything. The other
     unqualified imports in `TsParserActions.hs` are deliberately
     narrow (`Location` exports only the `Location` type;
     `Data.List` is imported with an explicit `( map, stripPrefix,
     isPrefixOf )` list); do not widen them in a patch.

  8. The grammar enforces `%expect 0` -- Happy aborts `cabal build`
     (exit code 1) on any shift/reduce or reduce/reduce conflict, and
     `agent_loop.py` auto-reverts that iteration via `build_failed`.
     The single most common conflict trap is adding `optional(X)` next
     to an `optional(Y)` (or before a symbol) where `X` itself derives
     a rule that *already* ends in `optional(X)` somewhere reachable.
     The textbook instance is `optional(generics)` inside a rule whose
     neighbouring symbol's `type` already carries a trailing
     `optional(generics)` (via `firstNode optional(generics)` /
     `internal_type optional(generics)`) -- Happy can't tell whether to
     shift the `<` into the inner `optional(generics)` or reduce out
     and start the outer one. Before introducing a new `optional(X)`
     in your patch, check that `FIRST(X)` is disjoint from `FIRST` of
     the *following* symbol AND from the FOLLOW set if your optional
     sits at the tail of a rule whose parent uses the same `optional(X)`
     suffix.
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
column {location.get('colStart')} of the single-line native AST. Total native \
AST length: {full_native_len} chars.

### Native AST window around the failure

```
{native_window}
```

## Current src/TsParser.y

```happy
{parser_y_text}
```

## Current src/TsParserActions.hs

```haskell
{actions_hs_text}
```

## Active conventions (from AGENTS.md, abbreviated)

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
            "openai package not installed. Run: pip install -r requirements.txt"
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
    # Ensure the keyword block in TsParser.y is populated before the haskell
    # build. mk_parsers.py is idempotent.
    mk = subprocess.run(
        [sys.executable, str(HERE / "mk_parsers.py")],
        cwd=HERE,
        capture_output=True,
        text=True,
    )
    if mk.returncode != 0:
        raise SystemExit(f"mk_parsers.py failed:\n{mk.stderr}")

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
                    + (f" col={n.get('col_start')}" if n_status == "fail" else "")
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
            # transport / frontts errors in baseline: don't gate on them but
            # don't count them either.
            continue

        o_col = o.get("col_start") or 0
        if n_status == "ok":
            improvements.append((rel, f"fail@col={o_col}", "ok"))
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
        n_col = n.get("col_start") or 0
        if n_col < o_col:
            detail = f"fail col went {o_col} -> {n_col}"
            return ProgressVerdict(
                accepted=False,
                reason=f"regression: {rel} {detail}",
                strict_improvements=[],
                regressing_file=rel,
                regressing_detail=detail,
            )
        if n_col > o_col:
            improvements.append((rel, f"fail@col={o_col}", f"fail@col={n_col}"))

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
            "Iterate one OpenAI-driven grammar patch. Apply / build / regate "
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
    ap.add_argument("--include-tsx", action="store_true", help="also include *.tsx")
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
        help="chars before/after the failure column to show the LLM",
    )
    ap.add_argument(
        "--per-file-timeout",
        type=float,
        default=20.0,
        help="HTTP timeout (seconds) per per-file frontts/parsers call",
    )
    ap.add_argument(
        "--openai-timeout",
        type=float,
        default=180.0,
        help="OpenAI API call timeout (seconds)",
    )
    ap.add_argument(
        "--frontts-url",
        default=os.environ.get("FRONTTS_URL", "http://localhost:4000"),
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
            "post-mortem one-shot: print the native AST of PATH via frontts and exit. "
            "Use after a 'regression failed on:' message to inspect what the parser saw "
            "(skips OpenAI / parsers / corpus / baseline; only frontts is contacted)."
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
    frontts_url: str,
    timeout: float,
) -> int:
    """Print the native AST of `path` (as produced by frontts) and exit.

    Used after a regression-rejected iteration to inspect what the parser
    actually consumed for the offending file. Only frontts is contacted;
    the parsers container's state is irrelevant here.
    """
    abs_path = path.resolve()
    if not abs_path.is_file():
        print(f"file not found: {abs_path}", file=sys.stderr)
        return 1
    print(f"file:    {abs_path}", file=sys.stderr)
    print(f"frontts: {frontts_url}", file=sys.stderr)
    try:
        wait_for_healthy(frontts_url, timeout=30)
    except SystemExit as exc:
        print(str(exc), file=sys.stderr)
        return 1
    session = requests.Session()
    native, err = frontts_native_ast(session, frontts_url, abs_path, timeout)
    if native is None:
        print(f"frontts failed: {err}", file=sys.stderr)
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
            args.frontts_url,
            args.per_file_timeout,
        )

    ensure_openai_key()

    rng = random.Random(args.seed)

    corpus = discover_corpus(args.corpus, args.include_tsx)
    if not corpus:
        raise SystemExit(f"no .ts files under {args.corpus}")
    print(f"corpus: {len(corpus)} files under {args.corpus}")

    print("checking frontts and parsers health...")
    wait_for_healthy(args.frontts_url, timeout=30)
    wait_for_healthy(args.parsers_url, timeout=30)

    cached = load_baseline_or_none() if not args.rebuild_baseline else None
    if cached and baseline_is_fresh(cached, args.corpus):
        # Trim cached entries against the current corpus filter -- this lets
        # filter changes (e.g. excluding .test.*) take effect without forcing a
        # full --rebuild-baseline.
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
            args.corpus, corpus, args.frontts_url, args.parsers_url, args.per_file_timeout
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
    native, ferr = frontts_native_ast(
        session, args.frontts_url, chosen_abs, args.per_file_timeout
    )
    if native is None:
        print(f"could not re-fetch native AST for chosen file: {ferr}")
        return 1
    window = context_window(native, col, args.context_radius)
    print("window before/after the failed location (native ast):")
    print(window)

    parser_y_text = read_text_safe(SRC_DIR / "TsParser.y")
    actions_hs_text = read_text_safe(SRC_DIR / "TsParserActions.hs")
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
        args.corpus, corpus, args.frontts_url, args.parsers_url, args.per_file_timeout
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
            script_invocation = f'python "{HERE / "agent_loop.py"}"'
            print(
                f'    {script_invocation} --post_mortem_show_native_ast_of "{regressing_abs}"'
            )
        else:
            print(f"REJECTED: {verdict.reason}")
        print()
        print("(grammar edits left in place; parsers container is running the rejected grammar.")
        print(" to manually revert: restore src/TsParser.y + src/TsParserActions.hs from")
        print(" .dev/snapshot/, then `docker compose build parsers && docker compose up -d parsers`.)")
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
