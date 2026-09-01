# AGENTS

## Improving Parsers Coverage

**Goal**: parse every source code file. A single parse error drops the entire file from the framework's analysis, so any coverage improvement directly increases what we can scan.

1. **Regression repo**: a fixed set of source files acts as the coverage benchmark. Each file is either parsed successfully or fails at some location; record this baseline.
2. **One rule adjustment at a time**: each iteration adds or adjusts at most one production in the relevant `.y` file (currently `TsParser.y`).
3. **Acceptance per iteration**:
   1. Every file that previously parsed must still parse.
   2. Every file that previously failed at column `c` must now either parse, or fail strictly past `c`. (Native ASTs are single-line, so column is the only progress metric.)
4. **Guardrails — must pass before the regression check**:
   1. `cabal build` succeeds.
   2. Happy reports **no new shift/reduce or reduce/reduce conflicts** vs the previous baseline. This guarantees that every input the previous grammar accepted is still accepted with the same reductions, i.e. existing rules keep their meaning.

### Start symbol and file layout

The grammar's start symbol is declared explicitly via `%name parse <rule>` at the top of each `.y` file — for `TsParser.y` that rule is `program`. Even though the explicit `%name` form makes Happy independent of in-file ordering, **new rules MUST be added below `program`, never above it**, so each iteration's diff sits visually below the entry rule and stays trivially reviewable.

### Example adjustments

Each example below makes the grammar strictly more permissive. If Happy reports no new conflicts, the change is a strict superset — every previous input still parses with the same reductions, and at least one new shape now also parses.

1. **Widen a list to allow empty**: replace `commalistof(a)` with `possibly_empty_commalistof(a)` in some production. Non-empty lists still parse; empty lists now parse too.
2. **Make a symbol optional**: replace `a` with `optional(a)` in some production. Inputs containing `a` still parse; inputs missing `a` now parse too.

### Active cleanup conventions

The grammar is being prepared so each per-iteration prompt stays small. Until that's done, edits should follow these conventions:

1. **Action code lives in `TsParserActions.hs`**. Each production's `{ ... }` body shrinks to a single call `{ Actions.helperName $i $j ... }`; the helper is a smart constructor in `TsParserActions.hs`, imported as `import qualified TsParserActions as Actions`.
2. **Consolidate when possible**. When two near-identical productions differ only in an optional / repeated / alternative position, collapse them using a shared helper:
   - one with a list, one without → `possibly_empty_commalistof(a)`
   - one with an extra symbol, one without → `optional(a)`
   - one accepting a single form, one accepting an aggregate → a sum nonterminal (e.g. `stmtOrBlock: stmt { [$1] } | block { $1 }`)
3. **camelCase = handled, snake_case = TODO**. A production starts in snake_case (`stmt_if`, `else_part`). Once it's consolidated and its action moved into `Actions.*`, rename it to camelCase (`stmtIf`, `elsePart`). The mix is the at-a-glance progress indicator inside `TsParser.y`.
4. **One-line mapping comment** above each rule we touch from now on. Pick one of the two forms based on the dhscanner-AST mapping (see the next section):
   - direct: `-- direct dhscanner subtree creation: Ast.<Constructor>` (e.g. `Ast.StmtIf`, `Ast.ExpBinop`).
   - instrumented: `-- instrumented as dhscanner Ast.ExpCall` (or `Ast.StmtBlock`, etc., whichever shape the smart constructor returns).

   Do **not** retro-add this comment to rules already cleaned up before this convention was introduced; only the rules edited from now on get it. The comment replaces the old multi-line block-banner above the rule.
5. **Handled so far**: `root` (was `program`), `stmtIf`, `elsePart`, `expCall`, `stmtImport`, `stmtFunc` (was `stmtFunction`), `stmtDecvar`, `stmtReturn`, `stmtAssign`, `stmtExp`, `stmtBreak`, `stmtWhile`, `stmtEnum`, `expBinop`, `expBool` (incl. `expTrue`/`expFalse`), `expArrowFunction`, `expNull`, `varField`, `varSubscript`, `expDelete`, `expNew`, `stmtTry`, `stmtThrow`, `stmtClass` (consolidated from the old `stmt_class` + `stmt_interface` pair), `property` (consolidated `PropertyAssignment` plain + `ComputedPropertyName` shapes via shared `property_1`/`property_2` sub-rules; lowers via `instrumentationCall "kv"` to an `Ast.ExpCall` whose callee is `<dhscanner-instrumentation>[kv]`, the standard instrumented-call shape), `expTypeof` (was `exp_typeof`; tag `[typeof]`, operand currently dropped — preserved from original semantics, can be promoted to an arg later), `expTernary` (was `exp_ternary`; tag `[ternary]` wrapping three per-branch sub-calls — `<dhscanner-instrumentation>[ternary_cond]`, `<dhscanner-instrumentation>[ternary_then]`, `<dhscanner-instrumentation>[ternary_else]` — each carrying its operand as its single arg, so consumers can pattern-match branches by callee name rather than by positional index), `fstring` (routed through `instrumentationCall "fstring"` so the callee is `<dhscanner-instrumentation>[fstring]`; args are a regularly-interleaved `[ExpStr, exp, ExpStr, exp, ..., ExpStr]` sequence — even indices are the template's constant string fragments captured via the frontts's new `TemplateHead` / `TemplateMiddle` / `TemplateTail` / `NoSubstitutionTemplateLiteral` STR-payload emitter, odd indices are the interpolated expressions; enables an intrinsic const/dynamic split on Ast.Exp constructor with no side-channel tag) — plus support helpers `block` (was `body`), `stmtOrBlock`, `decvarLhs`, `stmtExpBody`, `trueKeyword`, `falseKeyword`, `catchPart` (was `catch_clause`), `possibly_empty_commalistof` (was `possibly_empty_listof`), `instrumentationCall` (shared lowering for instrumented rules — every `<dhscanner-instrumentation>[<tag>]` `Ast.ExpCall` should go through it; itself implemented on top of `expvarify`), and `expvarify` (`Token.Named -> Ast.Exp`, the `Ast.Exp`-level analogue of `varify`; reach for it whenever a rule needs a bare-name expression instead of inlining the `Ast.ExpVar . Ast.ExpVarContent . Ast.VarSimple . Ast.VarSimpleContent . Token.VarName` chain).
6. **Reusable list/option helpers** (use these before introducing new ones): `optional(a)`, `listof(a)`, `commalistof(a)`, `possibly_empty_commalistof(a)`, `commalistof_with_optional_trailing_comma(a)`, `possibly_empty_commalistof_with_optional_trailing_comma(a)`, `barlistof(a)` (bar-separated, for union types), `ampersandlistof(a)` (ampersand-separated, for intersection types), `stmtOrBlock`.

### Mapping to the dhscanner AST

The framework's normalized AST lives in the **`dhscanner.ast`** git submodule and is imported as `import Ast` from inside `TsParserActions.hs`. Every smart constructor ultimately returns a value in that AST. There are two flavors of mapping; knowing which flavor a rule belongs to lets you size the cleanup work upfront, and **direct rules should be picked first** because they are mechanical.

1. **Direct (1:1, natural fit)** — the native TypeScript node has a same-shape constructor in `dhscanner.ast`, so the smart constructor is a thin record builder: pattern-match the parts, fill the record, done. No semantic decisions.
   - `program` → `Ast.Root`
   - `stmtIf` → `Ast.StmtIf`
   - `stmtFunction` → `Ast.StmtFunc`
   - `stmtReturn` → `Ast.StmtReturn`
   - `stmtAssign` → `Ast.StmtAssign`
   - `stmtExp` → `Ast.StmtExp`
   - `stmtBreak` → `Ast.StmtBreak`
   - `stmtWhile` → `Ast.StmtWhile`
   - `stmtTry` → `Ast.StmtTry` (catch's exception-binding identifier is currently dropped — the catch body's statements are kept verbatim as `stmtCatchPart`; can be enriched later by prepending a synthetic vardec for the caught variable)
   - `stmtClass` → `Ast.StmtClass` (currently consumes only `InterfaceDeclaration`; will get a second alternative for `ClassDeclaration`. Today it only fills `stmtClassName` — `stmtClassSupers`, `stmtClassDataMembers`, `stmtClassMethods` are all empty; can be enriched later when the class body's stmts get reified into members)
   - `stmtDecvar` (single-name case) → `Ast.StmtVardec`
   - `expBinop` → `Ast.ExpBinop`
   - `expBool` → `Ast.ExpBool`
   - `expNull` → `Ast.ExpNull`
   - `expCall` → `Ast.ExpCall`
   - `expArrowFunction` → `Ast.ExpLambda`
   - `varField` → `Ast.VarField`
   - `varSubscript` → `Ast.VarSubscript`
   - `var_simple` → `Ast.VarSimple` (via `Actions.varify`)
   - Still TODO and direct: `exp_str` → `Ast.ExpStr`, `exp_int` → `Ast.ExpInt`, `exp_var` → `Ast.ExpVar`, `exp_paren` (transparent — returns inner exp), `exp_unop` (currently transparent), `exp_as` (transparent), `exp_await` (transparent), `exp_non_null` (transparent).

2. **Instrumented (adapter needed)** — there is no matching constructor in `dhscanner.ast`, so the rule normalizes the native node into something the framework already understands. The two recurring tricks are: wrap as `Ast.ExpCall` with a magic callee name, or wrap as `Ast.StmtBlock`. These edits cost more: pick a normalization, document the magic name, and stay consistent with the existing instrumented rules.

   The standard callee-name format for `Ast.ExpCall`-flavored instrumentation is `<dhscanner-instrumentation>[<tag>]`, built by the shared `Actions.instrumentationCall :: String -> Location -> [Ast.Exp] -> Ast.Exp`. New instrumented rules of this shape MUST go through that helper rather than spell the format inline, so the tag namespace stays grep-able and consistent.
   - `stmtImport` → `Ast.StmtBlock` of one `Ast.StmtImport` per imported name
   - `stmtEnum` → `Ast.StmtClass` (no `Ast.StmtEnum` exists; modeled as a class whose `stmtClassDataMembers` are the enum cases — name only, init values currently dropped — and whose `stmtClassMethods` is empty. Does NOT use the `<dhscanner-instrumentation>[…]` tag — the class shape itself is the instrumentation.)
   - `stmtDecvar` (multi-name case) → `Ast.StmtBlock` of per-name `Ast.StmtAssign`s
   - `expDelete` → `Ast.ExpCall` tagged `<dhscanner-instrumentation>[delete]` (operand currently dropped — preserved from original semantics; can be promoted to an arg later)
   - `expNew` → `Ast.ExpCall` whose **callee is the constructed type's name** (so `new Logger(x)` lowers to `Logger(x)`, which is what downstream analyses want for typing/data-flow); falls back to the synthetic name `"nondet"` when the type isn't reducible to an identifier. Does NOT use the `<dhscanner-instrumentation>[…]` tag — the type-name callee is the instrumentation here.
   - `stmtThrow` → `Ast.StmtExp` wrapping an `Ast.ExpCall` tagged `<dhscanner-instrumentation>[throw]` whose single arg is the thrown expression. (Old behavior dropped the throw-ness entirely; new behavior keeps both the throw site and the thrown value reachable to downstream analyses.)
   - Still TODO and instrumented: `exp_meta` → `ExpVar "meta"`, `exp_dict` → tag `[dictify]`, `exp_array` → tag `[arrayify]`, `exp_jsx` (currently returns its child expression — drops JSX structure), `exp_regex` → `ExpInt 888` placeholder, `stmt_property` → `Ast.StmtVardec` (used both as standalone stmt and as class field), `stmt_method` → `Ast.StmtMethodContent` (lifted to `Ast.ExpLambda` by `lambdame'` when it appears as a property value).

When picking the next rule to clean up, prefer the **direct** list above: the smart constructor is a one-liner-style record build, the dhscanner shape already exists, and there is no normalization decision to make. Reach for the **instrumented** list when the easy ones are exhausted, when a particular instrumented rule is blocking parser coverage, or when the user explicitly asks for it.
