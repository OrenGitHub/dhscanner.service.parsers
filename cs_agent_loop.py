"""Agent loop: improve CsParser.y / CsParserActions.hs / CsLexer.x grammar coverage.

C# sibling of `rb_agent_loop.py`. Same strict-progress methodology, same
OpenAI structured-output patch schema, same snapshot / apply / rebuild /
regate state machine, same dhscanner.runner compose chain. Only the
language-specific plumbing differs:

                        rb_agent_loop.py         cs_agent_loop.py (this file)
  front service         frontrb :5007            frontcs :5008 ( container :8080 )
  front endpoint        /to/native/cruby/ast     /to/native/cs/ast
  parsers endpoint      /from/rb/to/dhscanner    /from/cs/to/dhscanner
  native AST shape      syntax_tree JSON         Roslyn ( fronts/cs Program.cs ) :
                                                 {kind:K,value:V,location:{..},children: [..]}
                                                 on ONE line, so the gate key is the column
  editable files        RbParser.y +             src/CsParser.y ( syntax only ) +
                        RbParserActions.hs +     src/CsParserActions.hs +
                        RbLexer.x                src/CsLexer.x
  default corpus        Discourse                $CS_CORPUS_DIR ( or --corpus )
  cached baseline       .dev/rb-baseline.json    .dev/cs-baseline.json
  iterations log        .dev/rb-iterations/      .dev/cs-iterations/
  snapshot              .dev/rb-snapshot/        .dev/cs-snapshot/

The baseline also records, per failing file, the Roslyn node path at the
failure offset ( the last two enclosing kinds, eg
`ClassDeclaration/AttributeList` ). It is printed as a histogram, and
`--target-node ClassDeclaration/AttributeList` makes the picker choose only
files that fail there.

For one iteration this script:

  1. Builds ( or loads the cached ) baseline of {file -> parse outcome}
     over the production `*.cs` files at --corpus ( obj / bin / Migrations
     dirs and `*.Tests` projects excluded ).
  2. Picks a failing file, reads its native AST and the failure offset.
  3. Asks OpenAI for a minimal patch to CsParser.y / CsParserActions.hs / CsLexer.x.
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
  python dhscanner.core/dhscanner.service.parsers/cs_agent_loop.py --baseline-only
  python dhscanner.core/dhscanner.service.parsers/cs_agent_loop.py
"""

from __future__ import annotations

import argparse
import collections
import concurrent.futures
import hashlib
import json
import os
import random
import re
import subprocess
import sys
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import requests


HERE = Path(__file__).resolve().parent
SRC_DIR = HERE / "src"
DEV_DIR = HERE / ".dev"
BASELINE_PATH = DEV_DIR / "cs-baseline.json"
ITERATIONS_DIR = DEV_DIR / "cs-iterations"
SNAPSHOT_DIR = DEV_DIR / "cs-snapshot"
ATTEMPTS_PATH = DEV_DIR / "cs-attempts.json"

# a file picked this many times without an accepted iteration is skipped
# until the next accepted iteration ( a new grammar resets every count ) :
# one stuck file can't eat an unattended batch.
MAX_ATTEMPTS_PER_FILE = 2

# HERE is `<runner>/dhscanner.core/dhscanner.service.parsers`.
DEFAULT_RUNNER_ROOT = HERE.parents[1]

# the C# repo to grow the grammar on : $CS_CORPUS_DIR or --corpus
DEFAULT_CORPUS_DIR = Path(os.environ["CS_CORPUS_DIR"]) if os.environ.get("CS_CORPUS_DIR") else None
DEFAULT_MODEL = "gpt-5"
DEFAULT_CONTEXT_RADIUS = 600
HEALTHCHECK_TIMEOUT_S = 180
# frontcs ( kestrel ) and parsers ( warp ) both serve concurrently
BASELINE_WORKERS = 8

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
    "src/CsParser.y",
    "src/CsLexer.x",
    "src/CsParserActions.hs",
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
    status: str          # "ok" | "fail" | "frontcs_fail" | "transport_fail"
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


# Path components we never descend into : build outputs and EF generated
# migrations. Test projects ( `Foo.Tests` ) are dropped for the same reason
# the other loops drop tests ( adversarial constructs skew the picker ).
EXCLUDED_PATH_COMPONENTS: frozenset[str] = frozenset({
    "obj",
    "bin",
    "Migrations",
    "node_modules",
    ".git",
})


def is_test_project(part: str) -> bool:
    return part.endswith(".Tests") or part.endswith(".Test")


def discover_corpus(corpus_dir: Path) -> list[Path]:
    """Return production `*.cs` files under corpus_dir."""
    if not corpus_dir.is_dir():
        raise SystemExit(f"corpus directory not found: {corpus_dir}")

    out: list[Path] = []
    for p in corpus_dir.rglob("*.cs"):
        rel_parts = p.relative_to(corpus_dir).parts
        if set(rel_parts) & EXCLUDED_PATH_COMPONENTS:
            continue
        if any(is_test_project(part) for part in rel_parts):
            continue
        out.append(p)
    out.sort()
    return out


# --------------------------------------------------------------------------- #
# HTTP clients                                                                #
# --------------------------------------------------------------------------- #


def frontcs_native_ast(
    session: requests.Session,
    frontcs_url: str,
    file_path: Path,
    timeout: float,
) -> tuple[str | None, str | None]:
    """POST source file to frontcs; return (native_ast, error_detail).

    frontcs ( dhscanner.core/dhscanner.service.fronts/cs/Program.cs )
    answers 200 with the one-line Roslyn native AST, or 422 with Roslyn's
    own syntax errors."""
    try:
        with file_path.open("rb") as fl:
            files = {"source": (file_path.name, fl, "application/octet-stream")}
            resp = session.post(
                f"{frontcs_url}/to/native/cs/ast",
                files=files,
                timeout=timeout,
            )
    except requests.RequestException as exc:
        return None, f"frontcs transport error: {exc}"

    if resp.status_code != 200:
        return None, f"frontcs http {resp.status_code}: {resp.text[:200]}"
    return resp.text, None


def parsers_translate(
    session: requests.Session,
    parsers_url: str,
    filename: str,
    native_ast: str,
    timeout: float,
) -> ParseResult:
    """POST native AST to parsers /from/cs/to/dhscanner/ast."""
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
            f"{parsers_url}/from/cs/to/dhscanner/ast",
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
            node=node_at(native_ast, loc.get("lineStart"), col),
        )
    return ParseResult("ok", None, None, len(native_ast), None)


def parse_file(
    session: requests.Session,
    frontcs_url: str,
    parsers_url: str,
    file_path: Path,
    rel_filename: str,
    timeout: float,
) -> tuple[ParseResult, str | None]:
    """Run the full frontcs -> parsers pipeline. Returns (result, native_ast)."""
    native, err = frontcs_native_ast(session, frontcs_url, file_path, timeout)
    if native is None:
        return ParseResult("frontcs_fail", None, None, None, err), None
    res = parsers_translate(session, parsers_url, rel_filename, native, timeout)
    return res, native


def wait_for_healthy(
    url: str,
    timeout: float = HEALTHCHECK_TIMEOUT_S,
    headers: dict[str, str] | None = None,
) -> None:
    deadline = time.monotonic() + timeout
    last_err: str | None = None
    while time.monotonic() < deadline:
        try:
            resp = requests.get(f"{url}/healthcheck", timeout=5, headers=headers)
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


def offset_of(native: str, line: int | None, col: int | None) -> int | None:
    """0-indexed offset of the 1-indexed ( line, col ) alex position."""
    if not native or not isinstance(col, int) or col < 1:
        return None
    line = line if isinstance(line, int) and line >= 1 else 1
    off = 0
    for _ in range(line - 1):
        nl = native.find("\n", off)
        if nl < 0:
            break
        off = nl + 1
    return min(off + col - 1, len(native))


_KIND_AT = re.compile(r"\{kind:([A-Za-z_0-9]+)")

# structural wrappers that say nothing about the construct
_TRANSPARENT_KINDS: frozenset[str] = frozenset({
    "Argument", "ArgumentList", "ExpressionStatement", "Block", "EqualsValueClause",
})


def node_at(native: str, line: int | None, col: int | None) -> str | None:
    """The Roslyn construct the failure offset falls in : the last two
    enclosing `{kind:...}` objects ( structural wrappers skipped ), eg
    `ClassDeclaration/AttributeList`. Values are quoted strings without
    escapes ( the front drops quotes / backslashes ), `location:{..}` is
    an object without a kind."""
    off = offset_of(native, line, col)
    if off is None:
        return None
    stack: list[str] = []
    in_str = False
    for i in range(off):
        ch = native[i]
        if ch == '"':
            in_str = not in_str
        elif in_str:
            continue
        elif ch == "{":
            m = _KIND_AT.match(native, i)
            stack.append(m.group(1) if m else "location")
        elif ch == "}" and stack:
            stack.pop()
    path = [k for k in stack if k not in _TRANSPARENT_KINDS and k != "location"]
    return "/".join(path[-2:]) or None


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
        print("failing files by Roslyn node path at the failure offset:")
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
    frontcs_url: str,
    parsers_url: str,
    timeout: float,
) -> dict[str, dict[str, Any]]:
    sessions = threading.local()

    def one(p: Path) -> tuple[str, ParseResult]:
        if not hasattr(sessions, "s"):
            sessions.s = requests.Session()
        rel = str(p.relative_to(corpus_dir)).replace("\\", "/")
        res, _ = parse_file(sessions.s, frontcs_url, parsers_url, p, rel, timeout)
        return rel, res

    out: dict[str, dict[str, Any]] = {}
    n = len(files)
    last_print = 0.0
    pool = concurrent.futures.ThreadPoolExecutor(BASELINE_WORKERS)
    for i, (rel, res) in enumerate(pool.map(one, files), start=1):
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
    pool.shutdown()
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
    """A ( 1, 1 ) failure means the parser bailed at the first token of the
    native AST ( or the error carried no location ); the window can't show
    the cause."""
    return info.get("line_start") == 1 and info.get("col_start") == 1


def failure_position_key(info: dict[str, Any]) -> tuple[int, int]:
    """Sort/comparison key for a fail location. The native AST is almost
    always a single line, so in practice this is ( 1, col )."""
    line = info.get("line_start") or 0
    col = info.get("col_start") or 0
    return (line, col)


def load_attempts() -> dict[str, int]:
    try:
        return json.loads(ATTEMPTS_PATH.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return {}


def save_attempts(attempts: dict[str, int]) -> None:
    DEV_DIR.mkdir(parents=True, exist_ok=True)
    ATTEMPTS_PATH.write_text(json.dumps(attempts, indent=2), encoding="utf-8")


def pick_failure(
    files_map: dict[str, dict[str, Any]],
    strategy: str,
    rng: random.Random,
    target_node: str | None,
    attempts: dict[str, int] | None = None,
) -> str | None:
    failures = [
        rel
        for rel, info in files_map.items()
        if info.get("status") == "fail"
        and isinstance(info.get("col_start"), int)
        and isinstance(info.get("line_start"), int)
        and not is_trivial_failure_location(info)
        and (target_node is None or info.get("node") == target_node)
        and (attempts or {}).get(rel, 0) < MAX_ATTEMPTS_PER_FILE
    ]
    if not failures:
        return None
    if strategy == "top-node":
        # the biggest bucket first, its earliest offset within it
        top = collections.Counter(files_map[rel].get("node") for rel in failures).most_common(1)[0][0]
        failures = [rel for rel in failures if files_map[rel].get("node") == top]
        strategy = "earliest"
    if strategy == "random":
        return rng.choice(failures)
    if strategy == "earliest":
        # the smallest native AST offset first : the shortest window to
        # reason about, and usually the plainest instance of the node.
        return min(failures, key=lambda rel: (failure_position_key(files_map[rel]), rel))
    return sorted(failures)[0]


def context_window(native: str, line: int | None, col_start: int | None, radius: int) -> str:
    """Return `radius` chars on each side of the failure position of the
    native AST, marking it with `<<<HERE line=L col=C>>>`."""
    if not native:
        return "<empty native ast>"
    offset = offset_of(native, line, col_start)
    if offset is None:
        return native[: 2 * radius] + ("..." if len(native) > 2 * radius else "")
    lo = max(0, offset - radius)
    hi = min(len(native), offset + radius)
    leading = "..." if lo > 0 else ""
    trailing = "..." if hi < len(native) else ""
    return f"{leading}{native[lo:offset]}<<<HERE line={line} col={col_start}>>>{native[offset:hi]}{trailing}"


# --------------------------------------------------------------------------- #
# OpenAI prompt                                                               #
# --------------------------------------------------------------------------- #


SYSTEM_PROMPT = """\
You are an expert Haskell + Happy/Alex grammar engineer working on the
dhscanner parsers service, which translates the native C# AST emitted by
`dhscanner.service.fronts/cs` ( Roslyn, C# 15 ) into the dhscanner AST.

The native AST is ONE line of text. Every node is

    {kind:<Kind>,value:<value>,location:{startLine:L1,startColumn:C1,endLine:L2,endColumn:C2},children: [<nodes>]}

  * `kind` is the Roslyn `SyntaxKind` name, a bare word ( `ClassDeclaration`,
    `InvocationExpression`, `SimpleLambdaExpression`, ... ).
  * `children` are the child NODES only, in source order. Tokens are NOT
    nodes : keywords, modifiers ( `public`, `static`, `async` ), operators
    and punctuation never appear. The operator of a binary expression is
    in its kind ( `AddExpression`, `LogicalAndExpression`, ... ).
  * `value` is a quoted string ( `"userId"` ) or the bare word `null`,
    and it is DETERMINISTIC :
      - a node with a name token ( every declaration : ClassDeclaration,
        RecordDeclaration, InterfaceDeclaration, EnumDeclaration,
        EnumMemberDeclaration, MethodDeclaration, ConstructorDeclaration,
        PropertyDeclaration, Parameter, TypeParameter, VariableDeclarator,
        LocalFunctionStatement, ForEachStatement, CatchDeclaration, ... and
        IdentifierName, GenericName, SingleVariableDesignation ) has its
        NAME as value, with or without children;
      - otherwise a LEAF node ( children [] ) has its source text as value
        ( literals, PredefinedType `"string"`, `"this"`, an empty ArgumentList
        `"()"`, an empty Block `"{}"` );
      - otherwise value is null.
    So when a rule must accept BOTH the leaf and the non-leaf form of a
    kind ( an empty vs a non-empty ArgumentList / Block / ParameterList ),
    match the value with `tokenID` ( ID or null ). Values never contain a
    double quote or a backslash.

Your job in this turn: propose ONE minimal, surgical patch that lets the
parser advance past a specific failure column on a specific input. The
patch may touch ONLY:

  - src/CsParser.y           ( the Happy grammar : syntax only )
  - src/CsParserActions.hs   ( the smart constructors behind every action )
  - src/CsLexer.x            ( the Alex lexer )

Rules of the road:

  1. The parser reports failure as a 1-indexed column into the one-line
     native AST. Your patch is accepted iff EVERY previously-passing corpus
     file still parses AND every previously-failing file now passes or
     fails at a column >= its previous one, AND at least one file strictly
     progresses. You only see the chosen file's window. Aim for a
     conservative, locally-scoped change ( add one production, add one
     alternative to `exp` / `stmt`, widen one children list ), not a
     refactor.

  2. Every kind the grammar knows is a bare-word KEYWORD token. A kind the
     grammar does not know yet needs a NEW TOKEN, wired in four places
     ( look at how `InvocationExpression` is wired ) :

       a. CsLexer.x macro      : `@INVOCATION_EXPRESSION = InvocationExpression`
       b. CsLexer.x rule       : `@INVOCATION_EXPRESSION { lex' AlexRawToken_INVOCATION_EXPRESSION }`
       c. CsLexer.x data ctor  : `     | AlexRawToken_INVOCATION_EXPRESSION`
       d. CsParser.y %token    : `'InvocationExpression' { AlexTokenTag AlexRawToken_INVOCATION_EXPRESSION _ }`

     Put each new line next to its sibling so every old_string anchor is
     unique. Before adding a kind, search the %token block : it may exist.
     Alex takes the longest match, so `AttributeList` does not disturb
     `Attribute`. Values are quoted, so a keyword never collides with a name.

  3. `CsParser.y` is a THIN syntax file. An action that builds dhscanner
     AST is ONE call into `CsParserActions.hs`, imported as `Actions`,
     whose arguments are only `$i` references, or token payloads read with
     `tokIDValue $i` / `tokIntValue $i` :

         -- direct dhscanner subtree creation: Ast.ExpCall
         expCall:
         '{'
             'kind' ':' 'InvocationExpression' ','
             'value' ':' 'null' ','
             'location' ':' location ','
             'children' ':' '[' exp ',' args ']'
         '}'
         {
             Actions.expCall $12 $17 $19
         }

     Positions in a node rule : $4 kind, $8 value, $12 location, $16 a
     children list nonterminal, or for `'[' a ',' b ',' c ']'` : $17 a,
     $19 b, $21 c. Never build `Ast.<Constructor> ..` records, tuples,
     lists, `Just` / `fromMaybe` / lambdas inside `CsParser.y`. Reuse an
     existing `Actions.*` constructor when one fits, else add ONE new,
     typed, documented smart constructor to `CsParserActions.hs`, next to
     its siblings. Pass-through actions ( `{ $1 }`, `{ $17 }`, `{ Nothing }` )
     stay inline. A NEW rule that calls `Actions.*` is named camelCase and
     gets a one-line mapping comment right above it, either
     `-- direct dhscanner subtree creation: Ast.<Constructor>` or
     `-- instrumented as dhscanner Ast.<Constructor>`.

  4. Reusable grammar helpers already in CsParser.y ( use them, don't add new ):

         optional(a)              -- Nothing | Just a
         commalistof(a)           -- a | a ',' commalistof(a)
         possibly_empty_listof(a) -- '[' ']' | '[' commalistof(a) ']'
         nonempty_listof(a)       -- '[' commalistof(a) ']'
         prefixlistof(a)          -- a ',' | prefixlistof(a) a ','   ( left recursive )
         location, tokenID, identifier, var, nominalType, annotatedType,
         attributeList, exp, stmt, args, params, classChild

     and in CsParserActions.hs : `named`, `simpleVar`, `varSimple`,
     `fieldVar`, `expVar`, `expvarify`, `expStr`, `expInt`, `expBinop`,
     `expCall`, `call`, `expNew`, `instrumentationCall`, `stmtExp`,
     `stmtBlock`, `unblock`, `stmtAssign`, `stmtIf`, `stmtWhile`,
     `stmtReturn`, `stmtThrow`, `stmtVardec`, `declarator`, `parameter`,
     `params`, `stmtMethod`, `stmtCtor`, `stmtClass` ( + `ClassPart` ),
     `attribute`, `kwArg`, `annotated` / `unannotated`.

     A class / record / interface's children are ONE list of `classChild`
     in any order ( attribute lists, the primary ctor ParameterList, the
     BaseList, members ). A new kind of class child ( TypeParameterList,
     TypeParameterConstraintClause, .. ) is one more `classChild`
     alternative returning a `ClassPart`, never a new positional slot.

  5. Map to the dhscanner AST the way the neighbors do ( and the Java /
     Ruby grammars ) :
       * if `dhscanner.ast` has a same-shape constructor, build it directly
         ( lambdas -> `Ast.ExpLambda`, try -> `Ast.StmtTry`, interface /
         record / struct -> `Ast.StmtClass` like `stmtClass`, `=> e` method
         bodies -> `[ Ast.StmtReturn e ]` ).
       * transparent wrappers return their inner expression ( `await e`,
         `e!`, `(T) e`, `( e )` ).
       * otherwise use `Actions.instrumentationCall "<tag>" loc args`, eg
         interpolated strings -> tag `fstring` with args interleaved
         [ ExpStr, exp, ExpStr, ... ], object / anonymous initializers ->
         `dictify`, tuples -> `tuple`, `throw e` expressions -> `throw`,
         patterns -> `pattern`. `a?.b` is a plain field access on `a`.
       * Keep data flow : never drop a sub-expression a call or a variable
         could live in, and keep declaration NAMES ( method parameters are
         the user inputs ).
       * Attributes ( `[HttpPost]`, `[Route("..")]`, `[FromQuery]` ) are
         not noise : lower an `Attribute` to an `Ast.Exp` ( a call of the
         attribute name on its arguments ) so a rule can later put it in
         `stmtMethodAnnotations` / `paramAnnotations`.

  6. If you cannot see a clearly safe minimal edit, return `edits: []` and
     explain in `rationale`. An empty patch beats a guess that regresses.

  7. `CsParserActions.hs` builds with `-Wall -Werror -Werror=missing-fields`:
     every record build fills every field, every new function has a type
     signature, no unused bindings / imports, no name shadowing ( never
     name a local `exp`, `filename`, `location`, `lineStart`, `args`,
     `stmts`, `params` ... ), total pattern matches. Every `$i` you pass
     must exist in the production.

  8. Do NOT introduce anonymous lambdas ( `\\... -> ...` ) anywhere, they
     are the most common cause of `-Werror=incomplete-uni-patterns` build
     failures. Use a named function with one clause per constructor.

  9. The dhscanner AST binary operators are exactly `Ast.EQ`, `Ast.NEQ`,
     `Ast.PLUS`, `Ast.MINUS`, `Ast.TIMES`, `Ast.DIVIDE`, `Ast.PERCENT`.
     `==` / `<` / `>` / `<=` / `>=` / `is` -> `Ast.EQ`, `!=` -> `Ast.NEQ`,
     `-` -> MINUS, `*` -> TIMES, `/` -> DIVIDE, `%` -> PERCENT, everything
     else ( `+`, `&&`, `||`, `??`, `&`, `|`, `^`, shifts ) -> `Ast.PLUS`.
     Expose a new operator as a constant in CsParserActions.hs ( like
     `opEq` ) and use it from the `operator` rule. Never write `Ast.AND`,
     `Ast.LT`, ... ( guaranteed build_failed ).

 10. The grammar declares `%expect 0` : ANY shift/reduce or reduce/reduce
     conflict fails the build ( = rejected ). The two classic traps :
       * `optional(a) b` where `a` and `b` both start with '{' : the parser
         must decide on the empty `optional` before it sees the kind.
         Write two alternatives instead ( see `stmtCtor` ). Likewise
         `commalistof(a) ',' b` : after an `a` it can't tell the list's ','
         from the one before `b`. Use the left-recursive `prefixlistof(a) b`
         ( see `annotatedType` : attribute lists in front of a type ).
       * two rules that match the same kind with the same children shape,
         or a new `stmt` alternative that is also reachable as an `exp`
         ( `stmtExp` already wraps every `exp` ) : widen the existing rule.
     New rules go BELOW `program`.

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
    actions_hs_text: str,
) -> str:
    return f"""\
## Failing file

`{failing_relpath}` -- the parser fails at column {col} of the native \
AST ( total length {full_native_len} chars ). Roslyn node path at the \
failure offset: `{node or '?'}`.

### Native AST window around the failure

```
{native_window}
```

## Current src/CsParser.y

```happy
{parser_y_text}
```

## Current src/CsLexer.x

```alex
{lexer_x_text}
```

## Current src/CsParserActions.hs

```haskell
{actions_hs_text}
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


def loose_unique_match(text: str, old: str) -> str | None:
    """The exact text span `old` matches when runs of spaces may differ in
    length, iff there is exactly one such span."""
    parts = re.split(r" +", old)
    pattern = " +".join(re.escape(part) for part in parts)
    hits = list(re.finditer(pattern, text))
    if len(hits) != 1:
        return None
    return hits[0].group(0)


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
            # the model often squeezes the column padding of aligned lines
            loose = loose_unique_match(text, old)
            if loose is not None:
                old, count = loose, 1
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
            # frontcs / transport errors in the baseline: don't gate on them
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
            "Iterate one OpenAI-driven C# grammar patch. Apply / build "
            "failures auto-revert; regression-gate failures leave edits in "
            "place for post-mortem inspection."
        )
    )
    ap.add_argument("--corpus", type=Path, default=DEFAULT_CORPUS_DIR,
                    required=DEFAULT_CORPUS_DIR is None,
                    help="corpus directory (default: $CS_CORPUS_DIR)")
    ap.add_argument("--model", default=os.environ.get("OPENAI_MODEL", DEFAULT_MODEL),
                    help=f"OpenAI model (default: {DEFAULT_MODEL} or $OPENAI_MODEL)")
    ap.add_argument("--pick", choices=["first", "random", "earliest", "top-node"], default="earliest",
                    help="failure-picking strategy (default: earliest failure offset)")
    ap.add_argument("--reshuffle", action="store_true",
                    help="shorthand for --pick random with a non-deterministic seed")
    ap.add_argument("--seed", type=int, default=None, help="random seed")
    ap.add_argument("--target-node", default=None, metavar="NODE",
                    help="only pick files failing on this Roslyn node path (eg ClassDeclaration/AttributeList)")
    ap.add_argument("--baseline-only", action="store_true",
                    help="build / load the baseline, print coverage + node histogram, exit")
    ap.add_argument("--rebuild-baseline", action="store_true",
                    help="ignore cached baseline and rebuild from scratch")
    ap.add_argument("--context-radius", type=int, default=DEFAULT_CONTEXT_RADIUS,
                    help="chars before/after the failure offset to show the LLM")
    ap.add_argument("--per-file-timeout", type=float, default=30.0,
                    help="HTTP timeout (seconds) per frontcs/parsers call")
    ap.add_argument("--openai-timeout", type=float, default=300.0,
                    help="OpenAI API call timeout (seconds)")
    ap.add_argument("--frontcs-url", default=os.environ.get("FRONTCS_URL", "http://localhost:5008"))
    ap.add_argument("--parsers-url", default=os.environ.get("PARSERS_URL", "http://localhost:4001"))
    ap.add_argument("--runner-root", type=Path,
                    default=Path(os.environ.get("RUNNER_ROOT", str(DEFAULT_RUNNER_ROOT))),
                    help="dhscanner.runner working copy that owns compose/ (default: ../..)")
    ap.add_argument("--revert-on-reject", action="store_true",
                    help="on a gate rejection restore the snapshot and rebuild parsers "
                         "( for unattended runs ; default leaves the edits for post-mortem )")
    ap.add_argument("--dry-run", action="store_true",
                    help="ask OpenAI but do not apply / rebuild / regate")
    ap.add_argument("--post_mortem_show_native_ast_of", type=Path, default=None, metavar="PATH",
                    help="print the native AST of PATH via frontcs and exit")
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


def post_mortem_show_native_ast(path: Path, frontcs_url: str, timeout: float) -> int:
    abs_path = path.resolve()
    if not abs_path.is_file():
        print(f"file not found: {abs_path}", file=sys.stderr)
        return 1
    try:
        wait_for_healthy(frontcs_url, timeout=60)
    except SystemExit as exc:
        print(str(exc), file=sys.stderr)
        return 1
    native, err = frontcs_native_ast(requests.Session(), frontcs_url, abs_path, timeout)
    if native is None:
        print(f"frontcs failed: {err}", file=sys.stderr)
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
        args.corpus, corpus, args.frontcs_url, args.parsers_url, args.per_file_timeout
    )
    save_baseline(args.corpus, files_map)
    return files_map


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    maybe_load_dotenv(args.runner_root)

    if args.post_mortem_show_native_ast_of is not None:
        return post_mortem_show_native_ast(
            args.post_mortem_show_native_ast_of, args.frontcs_url, args.per_file_timeout
        )

    if not args.baseline_only:
        ensure_openai_key()
    ensure_runner_root(args.runner_root)

    rng = random.Random(args.seed)

    corpus = discover_corpus(args.corpus)
    if not corpus:
        raise SystemExit(f"no .cs files under {args.corpus}")
    print(f"corpus: {len(corpus)} files under {args.corpus}")

    print("checking frontcs and parsers health...")
    wait_for_healthy(args.frontcs_url, timeout=60)
    wait_for_healthy(args.parsers_url, timeout=60)

    files_map = load_or_build_baseline(args, corpus)
    print_summary(files_map)
    if args.baseline_only:
        return 0

    pick_strategy = "random" if args.reshuffle else args.pick
    attempts = load_attempts()
    chosen = pick_failure(files_map, pick_strategy, rng, args.target_node, attempts)
    if chosen is None:
        scope = f" on node {args.target_node}" if args.target_node else ""
        print(f"no actionable failures{scope} in baseline -- nothing to do.")
        return 0

    chosen_abs = args.corpus / chosen
    chosen_info = files_map[chosen]
    col = chosen_info.get("col_start")
    node = chosen_info.get("node")

    print(f"chosen failed file: {chosen}")
    if not args.dry_run:
        attempts[chosen] = attempts.get(chosen, 0) + 1
        save_attempts(attempts)
    print(f"location of failure: col={col} node={node}")

    native, ferr = frontcs_native_ast(
        requests.Session(), args.frontcs_url, chosen_abs, args.per_file_timeout
    )
    if native is None:
        print(f"could not re-fetch native AST for chosen file: {ferr}")
        return 1
    window = context_window(native, chosen_info.get("line_start"), col, args.context_radius)
    print("window before/after the failed location (native ast):")
    print(window)

    user_prompt = build_user_prompt(
        chosen,
        col or 1,
        node,
        window,
        len(native),
        read_text_safe(SRC_DIR / "CsParser.y"),
        read_text_safe(SRC_DIR / "CsLexer.x"),
        read_text_safe(SRC_DIR / "CsParserActions.hs"),
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
        args.corpus, corpus, args.frontcs_url, args.parsers_url, args.per_file_timeout
    )

    verdict = strict_progress(files_map, new_files_map)
    log_iteration(
        {
            **base_log,
            "verdict": "accepted" if verdict.accepted else f"rejected: {verdict.reason}",
            "strict_improvements": verdict.strict_improvements,
        }
    )

    if not verdict.accepted and args.revert_on_reject:
        print(f"REJECTED: {verdict.reason}")
        print("reverting the grammar and rebuilding parsers ......")
        restore_snapshot(snap)
        docker_stop_parsers(args.runner_root)
        docker_rebuild_parsers(args.runner_root)
        wait_for_healthy(args.parsers_url, timeout=HEALTHCHECK_TIMEOUT_S)
        return 1

    if not verdict.accepted:
        if verdict.regressing_file is not None:
            regressing_abs = (args.corpus / verdict.regressing_file).resolve()
            print(f"regression failed on: {regressing_abs}")
            if verdict.regressing_detail:
                print(f"reason: {verdict.regressing_detail}")
            print()
            print("inspect its native AST with:")
            print(f'    python "{HERE / "cs_agent_loop.py"}" '
                  f'--post_mortem_show_native_ast_of "{regressing_abs}"')
        else:
            print(f"REJECTED: {verdict.reason}")
        print()
        print("(grammar edits left in place; parsers container is running the rejected grammar.")
        print(" to manually revert: restore src/CsParser.y + src/CsLexer.x + src/CsParserActions.hs from")
        print(" .dev/cs-snapshot/, then re-run the compose build + up chain for `parsers`.)")
        return 1

    print(f"ACCEPTED: {verdict.reason}")
    for rel, before, after in verdict.strict_improvements[:10]:
        print(f"  {rel}: {before} -> {after}")
    if len(verdict.strict_improvements) > 10:
        print(f"  ... and {len(verdict.strict_improvements) - 10} more")

    save_baseline(args.corpus, new_files_map)
    save_attempts({})
    print_summary(new_files_map)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
