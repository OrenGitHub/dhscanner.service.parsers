# Java progress ( TEMPORARY )

> This file is **scratch state** for the in-flight work on the Java pipeline
> ( `frontjava` native-AST printer + `JavaParser.y` / `JavaLexer.x` /
> `JavaParserActions.hs` grammar ). Once the Java grammar reaches a stable
> coverage baseline comparable to TS — a real regression repo, the strict
> per-iteration gate from `AGENTS.md` applies cleanly, smart constructors
> stop being stubs — the useful content here gets folded into `AGENTS.md`
> and this file is deleted.
>
> Do not treat anything here as a contract. It is a working log.

## Pipeline shape

Two stages, both still being changed:

1. **native AST** — `frontjava` ( `com.sun.source.*` on a jlink-trimmed JDK ) emits
   JSON via `dhscanner.service.fronts/java/src/main/java/dhscanner/front/java/AstPrinter.java`.
   Endpoint: `POST frontjava:5000/to/native/java/ast` ( `source` multipart field ).
   Unlike TS, the Java native AST is **multi-line** ( we pretty-print it ourselves
   with `json.dumps(indent=2)` for human-readable column tracking ).
2. **dhscanner AST** — `parsers` service runs the Happy grammar on the native AST.
   Endpoint: `POST parsers:3000/from/java/to/dhscanner/ast` ( JSON body:
   `filename`, `content`, `optional_github_url`, `source_containing_dirs`,
   `all_filenames`, `path_mappings` ).

Because both stages are moving at the same time, a change in the printer
usually shows up as a change in the grammar's required rules. The two always
land together: `dhscanner.service.fronts` + `dhscanner.service.parsers` get
pushed in the same session, then `dhscanner.core`'s submodule bump picks up
both, then the runner PR bumps `compose/compose.prebuilt.yaml`'s
`orenishdocker/dhscanner-parsers:<ver>-x64`.

## Regression targets

Two files from DSpace are currently in play. Growth strategy: add a
new file whenever the previously-onboarded file parses end-to-end AND
the new file forces a strict grammar extension ( construct not
exercised by any previously-onboarded file ). Every iteration must
keep **all** onboarded files passing their recorded baseline ( either
still parsing cleanly, or still failing at the same-or-strictly-later
column ) -- this is the "every previously-passing file still passes"
clause of the acceptance gate, scaled to multiple files.

### 1. `MetadataValue.java` ( shipped, end-to-end )

- **`MetadataValue.java`** at **DSpace tag `dspace-10.1`** ( commit
  `a1f5969b3a6a1ac324328bbe2f1d0df8567df9f3` ), path
  `dspace-api/src/main/java/org/dspace/content/MetadataValue.java`.
- A canonical JPA entity: package, 10+ imports ( `jakarta.*`, project-internal,
  3rd-party ), class-level + field-level annotations ( `@Entity`, `@Table`,
  `@Id`, `@Column`, `@GeneratedValue`, `@SequenceGenerator`, `@ManyToOne`,
  `@JoinColumn`, `@Nullable`, `@Transient`, `@Override` ),
  `implements ReloadableEntity<Integer>`, protected ctor, getters/setters,
  overridden `equals` / `hashCode` / `toString` with explicit cast + string concat.
- Lives in the runner workspace at `dspace_java_sample/MetadataValue.java`
  ( untracked on purpose — this is still iteration scratch ).
- When the file parses cleanly end-to-end, the pipeline-wide CI sensor already
  prepared on the runner's `.github/workflows/tests.yaml` ( local-mod,
  not yet committed ) flips green: it clones DSpace at `dspace-10.1`,
  scans only this one file, and asserts both `NATIVE_PARSING_SUCCEEDED` and
  `DHSCANNER_PARSING_SUCCEEDED` rows land in the logger DB for the file.

### 2. `UUIDUtils.java` ( shipped, end-to-end )

- **`UUIDUtils.java`** at the same DSpace tag, path
  `dspace-api/src/main/java/org/dspace/util/UUIDUtils.java`.
- Tiny ( 976 B source, 39 lines ) utility class picked out of the
  previous DSpace scan's `DHSCANNER_PARSING_FAILED` set
  ( ~2541 failing Java files vs ~607 passing = ~19% Dhscanner-side
  coverage, bottlenecked by every file hitting an unknown construct ).
  Deliberately chosen because its two methods exercise exactly the
  constructs `MetadataValue.java` didn't: `try` / `catch`, a ternary
  `?:` ( `return identifier == null ? null : identifier.toString();` ),
  and `static UUID.fromString(...)` / `StringUtils.isNotBlank(...)`
  qualified calls on identifier receivers.
- Lives next to `MetadataValue.java` in the runner workspace at
  `dspace_java_sample/UUIDUtils.java` ( still untracked -- same
  convention until the regression repo is formalised ).
- Shipped in two iterations ( see the dedicated progression table
  below ) : `stmt_try` + `stmt_catch` ( moved wall from pretty-JSON
  line 369 to line 673 ) then `expTernary` ( full file ).

## Enriched native-AST shape ( as of latest push )

The TS printer flattens most javac nodes via `TreeScanner.scan`'s default
visit methods into positional `children:[]`. For Java we've been overriding
the per-kind visit to produce **named**, semantically explicit keys instead.
Current state:

| kind | enriched? | shape |
|------|-----------|-------|
| `COMPILATION_UNIT` | no  | `{ kind, loc, text, children:[ stmt* ] }` |
| `PACKAGE`          | no  | flat `children:[]` |
| `IMPORT`           | no  | flat `children:[]` |
| **`CLASS`**        | yes | `{ kind, loc, text, annotations:[ exp* ], supers:[ type* ], children:[ member* ] }` -- `MODIFIERS` dropped entirely ; `extends` + `implements` coalesced into `supers` |
| **`VARIABLE`**     | yes | `{ kind, loc, text, annotations:[ exp* ], type: type, init: exp \| <absent> }` -- `MODIFIERS` dropped ; `children:[]` dropped ; `type` + `init` are **named keys**, not positional children |
| **`METHOD`**       | partial | `{ kind, loc, text, annotations:[ exp* ], parameters:[ PARAMETER* ], children:[ returnType, BLOCK ] }` -- regular methods only ( `MethodTree` with non-null `getReturnType()` ) ; `returnType` is always a **mandatory** first child ( even `void` lowers to `PRIMITIVE_TYPE("VOID")` ). `parameters` is now a **named key** ( symmetric with `VARIABLE`'s `type` / `init` ) ; `MODIFIERS` dropped, `annotations` hoisted, `parameters` hoisted. `children` carries only `returnType + BLOCK` ( plus the rare `typeParameters` / `receiverParameter` / `throws` / `defaultValue`, which stay in `children` until a file surfaces them ). Promoting `returnType` and `body` to named keys as well is the remaining enrichment work. |
| **`CONSTRUCTOR`**  | partial | `{ kind, loc, text="<init>", annotations:[ exp* ], parameters:[ PARAMETER* ], children:[ BLOCK ] }` -- ctors ( `MethodTree` with `getReturnType() == null` ) are split out of `"METHOD"` at the printer level so the grammar can discriminate with a clean kind-token terminal. No return-type child ever ( Java spec forbids it ). `MODIFIERS` dropped ; `annotations` + `parameters` hoisted ; `children` carries just the `BLOCK` body. |
| **`PARAMETER`**    | n/a | `{ kind, loc, text, type: type }` -- slim envelope minted by the printer out of a `VariableTree` sitting under `MethodTree.getParameters()` ( symmetric with the `CONSTRUCTOR` kind-split: the javac tree-kind would be `VARIABLE`, but a dedicated kind lets the grammar discriminate with a kind-token terminal instead of overloading the general `variable` rule, which carries `annotations` / `init` slots that params in practice never populate ). Grows an `annotations` slot the first time a file surfaces `@NotNull String s`-style parameter annotations. |
| **`IF`**           | yes | `{ kind, loc, cond: exp, body: stmt, children: [ elseStmt? ] }` -- `cond` holds the full `getCondition()` tree ( always a `PARENTHESIZED` wrapper per Java spec, unwrapped grammar-side via `exp_parenthesized` ) ; `body` holds the then-branch ; `children:[]` carries at most one element -- the optional else-arm ( absent → `[]`, present → `[elseStmt]` ). Shape mirrors `VARIABLE`'s `type` / `init` presence-vs-absence pattern on the condition/then-branch slots and `METHOD`'s `parameters` promotion pattern on the hoisted keys. |
| **binops** ( `PLUS` / `MULTIPLY` / `EQUAL_TO` / …, any `BinaryTree` ) | yes | `{ kind: <OP_NAME>, loc, left: exp, right: exp, children: [] }` -- the javac `Tree.Kind` already encodes the operator via the kind name, so there's no separate operator payload to hoist ; the two operands are promoted from positional `children:[ lhs, rhs ]` into named `left` + `right` keys ( symmetric with `IF`'s `cond` / `body` ). `children:[]` is kept but empty, matching every other enriched kind's uniform-envelope convention. Grammar side consumes the enriched shape via `exp_binop` + a sibling `binop_kind` discriminator sub-rule. |
| `PARAMETERIZED_TYPE` | n/a | `{ kind, loc, children:[ type+ ] }` ( first child is the base, rest are the type args ). `UNBOUNDED_WILDCARD` (`?` in `Foo<?>`) is dropped at the printer -- same early-return as `MODIFIERS` -- so `Class<?>` lands as a single-child `PARAMETERIZED_TYPE` wrapping `IDENTIFIER("Class")`. |
| `PRIMITIVE_TYPE`     | n/a | `{ kind, loc, text, children:[] }` |
| `STRING_LITERAL` / `INT_LITERAL` / `NULL_LITERAL` | n/a | `{ kind, loc, text, children:[] }` |

Design principle so far: **drop `MODIFIERS` entirely** ( `public` / `private` /
`static` / `final` noise that downstream analyses don't need ), **drop
`UNBOUNDED_WILDCARD`** ( `?` in `Foo<?>` -- the raw type is enough ),
**transparently unwrap `TYPE_CAST`** ( `(T) expr` -> `expr` -- the target
type carries no analysis signal ), but **always preserve `annotations`**
( semantically meaningful, especially JPA / Spring ). Implemented universally
by early-returns in `AstPrinter.JsonTreePrinter.scan()` for `Tree.Kind.MODIFIERS`
and `Tree.Kind.UNBOUNDED_WILDCARD` ( both silent drops ) and by an
`instanceof TypeCastTree` branch that recursively scans only the
`getExpression()` child ( child-promoting unwrap ). `CLASS` / `VARIABLE` /
`METHOD` each hoist its annotations via `emitAnnotations(x.getModifiers())`
BEFORE delegating to `super.scan`.

## Grammar surface ( as of latest push )

Shipped productions:

- `program` -- consumes `COMPILATION_UNIT`.
- `stmt_package`, `stmt_import`.
- `stmt_class` -- consumes the enriched CLASS envelope.
- `attribute` = `stmt_vardec | stmt_ctor | stmt_method`.
- `stmt_vardec` = `stmt_vardec_init | stmt_vardec_no_init` -- both consume the
  enriched VARIABLE shape with named `type` + `init` keys.
- `stmt_ctor` -- consumes the `"CONSTRUCTOR"` kind envelope ( printer splits
  it out of `"METHOD"` on `getReturnType() == null` ). Enriched shape :
  `annotations` + `parameters` are hoisted keys ; `children` is the body
  stmt list only ( no leading return type, since constructors don't
  declare one ). **Stub** smart constructor
  ( `stmtCtor :: Location -> String -> [Ast.Param] -> Ast.Stmt` ) accepts
  the real params list ( parser builds it from the hoisted `parameters`
  JSON key ) but still returns an `Ast.StmtMethod` with name = `"<init>"`,
  `stmtMethodReturnType = Nothing`, and params / body dropped until the
  next METHOD-enrichment pass.
- `stmt_method` -- consumes the `"METHOD"` kind envelope ( regular methods,
  with a mandatory return-type first child ). Enriched shape mirrors
  `stmt_ctor` : `annotations` + `parameters` are hoisted keys. Current
  `children` shape is still `'[' type ',' stmt_body ']'` -- exactly 2
  positional children: `type` ( return type ) + 1 body stmt. Enough for
  every method whose body is a single `BLOCK` ( parameterless or not --
  parameters are now consumed via the sibling `parameters` key, not
  interleaved in `children` ). **Stub** smart constructor
  ( `stmtMethod :: Location -> String -> [Ast.Param] -> Ast.Var -> Ast.Stmt` )
  accepts both the real params list and the return type but still drops
  all three ( params / return type / body ) until it grows past the stub
  form.
- `parameter` -- consumes the slim `"PARAMETER"` envelope ( `{ kind, loc,
  text, type: type }` ). Real smart constructor
  ( `parameter :: Location -> String -> Ast.Var -> Ast.Param` ) returns
  an actual `Ast.Param` with `paramName` + `paramNominalType = Just typ`
  + `paramSerialIdx = 0`. The serial index will be re-enumerated with
  `zipWith` when `stmtMethod` / `stmtCtor` actually thread the list into
  `stmtMethodParams`.
- `type` = `simple_var | parameterized_type | primitive_type`.
- `exp` = `exp_var | exp_str | exp_int | exp_null | exp_bool | exp_assign | exp_parenthesized | exp_call | exp_binop | unop`.
- `unop` = `not exp ']' '}'` -- generic unary-operator envelope ; each
  operator keyword gets its own sibling sub-rule ( currently just `not`
  → `LOGICAL_COMPLEMENT` ) that consumes the native envelope up through
  its opening `[`, lets the inner `exp` parse, and leaves the trailing
  `']' '}'` to `unop`. Lowered via `instrumentationCall "<tag>" loc
  [operand]` -- same `<dhscanner-instrumentation>[<tag>]` convention as
  every other parser-synthetic construct ( see TsParserActions.hs for
  the TypeScript side : `throw`, `kv`, `fstring`, `ternary_*`, `jsx`,
  `typeof`, `delete`, `fallthrough_return_null`, … ).
- `annotation` -- consumed inside `annotations:[...]` as an `exp` ( assign expression ).
- `stmt_body` = `stmt_block | stmt_exp | stmt_return | stmt_if | stmt_vardec`
  -- shared sum nonterminal for anything that can appear nested inside a
  BLOCK body OR directly in a METHOD body. Grows as more Java statement
  kinds ( `stmt_while` / `stmt_try` / `stmt_for` / … ) are added.
  `stmt_vardec` is the same production already used as a class-member
  `attribute` -- local vars and fields share the VARIABLE envelope.
- `stmt_block` -- consumes `"BLOCK"` → `Ast.StmtBlock` ( record with
  `stmtBlockContent :: [Ast.Stmt]` + `stmtBlockLocation :: Location` ).
- `stmt_exp` -- consumes `"EXPRESSION_STATEMENT"` → `Ast.StmtExp`, which
  in the dhscanner AST is a direct `Ast.Exp -> Ast.Stmt` wrapper ( no
  record, no separate location — the location lives on the inner
  expression; same shape every other front uses, e.g.
  `TsParserActions.stmtPackage` ).
- `stmt_return` -- consumes `"RETURN"` → `Ast.StmtReturn` ( record with
  `stmtReturnValue :: Maybe Ast.Exp` + `stmtReturnLocation :: Location` ).
  Currently only the with-value form `{ kind: RETURN, loc, children:[ exp ] }`
  is accepted; bare `return;` ( `children:[]` ) gets a second grammar
  alternative when a file that uses it shows up.
- `stmt_if` -- consumes `"IF"` → `Ast.StmtIf` ( record with
  `stmtIfCond :: Ast.Exp`, `stmtIfBody :: [Ast.Stmt]`,
  `stmtElseBody :: [Ast.Stmt]`, `stmtIfLocation :: Location` ).
  **Enriched shape** : `cond` + `body` are named keys ( promoted at
  the printer level, symmetric with `VARIABLE`'s `type` / `init` and
  `METHOD`'s `parameters` ) ; `children:[]` carries just the optional
  `else`-arm ( zero or one entry via `possibly_empty_commalistof(stmt_body)` ).
  The condition's `PARENTHESIZED` wrapper the javac tree always emits
  ( Java spec requires `if ( ... )` parens ) is kept in the `cond`
  value and unwrapped by the sibling `exp_parenthesized` production,
  so the smart constructor sees a clean `Ast.Exp`. Real smart
  constructor ( not a stub ) : `stmtIfBody` is a singleton `[thenStmt]`,
  `stmtElseBody` is the parser-built list ( `[]` for no-else,
  `[elseStmt]` for else ).
- `exp_parenthesized` -- consumes `"PARENTHESIZED"` and **unwraps** --
  no smart constructor, action is just `{ $13 }` ( the inner `exp` ).
  Reusable in any `exp` position ; parentheses carry no dhscanner
  semantics beyond grouping, which is already encoded by the AST shape.
  Reachable via `exp` → `stmt_if`'s condition, `stmt_return`'s value,
  `exp_assign`'s rhs, etc.
- `exp_call` -- consumes `"METHOD_INVOCATION"` → `Ast.ExpCall` ( record
  with `callee :: Ast.Exp`, `args :: [Ast.Exp]`, `expCallLocation ::
  Location` ). Children shape `[ callee, arg* ]` -- expressed as
  `commalistof(exp)` which guarantees at least one element ; smart
  constructor splits head ( callee ) from tail ( args ). Callee can be
  an `IDENTIFIER` ( bare call like `foo(x)` ), a nested `MEMBER_SELECT`
  chain ( qualified call like `a.b.c(x)` ), **or** an arbitrary `exp`
  for chained calls ( `foo().bar()`, enabled by `field_var`'s receiver
  broadening -- see below ). Real smart constructor ( not a stub ) :
  empty-children fallback is a defensive `Ast.ExpNull` ( the grammar
  makes empty impossible, but it silences `-Wall` ).
- `field_var` -- consumes `"MEMBER_SELECT"` → `Ast.VarField` ( record
  with `varFieldLhs :: Ast.Exp` ). Receiver slot is a full `exp` ( was
  previously restricted to `var`, i.e. `simple_var | field_var` only --
  the dhscanner AST side had always carried `Exp` for `varFieldLhs`,
  the mismatch was purely on the grammar side ). Enables chained method
  calls like `getMetadataField().getID()` to parse without any further
  rules. The outer `fieldVar` smart constructor is now a thin shim over
  `fieldVar''` ( which has always taken `Ast.Exp` ) -- the ad-hoc
  `Ast.ExpVar . Ast.ExpVarContent` wrap at the call site is gone.
- `exp_bool` -- consumes `"BOOLEAN_LITERAL"` → `Ast.ExpBool` ( record
  with `expBoolValue :: Token.ConstBool`, which in turn carries a plain
  Haskell `Bool` + `Location` ). Shape is identical to `exp_null` /
  `exp_int` / `exp_str` : `{ kind, loc, text: "true"|"false",
  children: [] }`. The text payload is lowered to `Bool` via `(value ==
  "true")` -- defensive-total since javac only ever emits one of the
  two keyword forms for a `BooleanLiteral` tree node. Real smart
  constructor ( not a stub ).
- `exp_binop` -- consumes any binary-operator envelope ( enriched shape
  with named `left` / `right` keys ) → `Ast.ExpBinop` ( record with
  `expBinopLeft :: Exp`, `expBinopRight :: Exp`, `expBinopOperator ::
  Ast.Operator`, `expBinopLocation :: Location` ). The per-kind
  operator mapping is a sibling `binop_kind` sub-rule that discriminates
  on the kind-token and returns the right `Ast.Operator` value :
  `EQUAL_TO` → `Ast.EQ`, `PLUS` → `Ast.PLUS`, `MULTIPLY` → `Ast.TIMES`
  for now. Growing `binop_kind` with more alternatives ( `NOT_EQUAL_TO`
  → `Ast.NEQ`, `MINUS` → `Ast.MINUS`, `DIVIDE` → `Ast.DIVIDE`,
  `REMAINDER` → `Ast.PERCENT` ) is a one-liner-per-op the first time a
  file surfaces them -- no new infrastructure needed. Real smart
  constructor ( not a stub ). **Limitation** : `Ast.Operator` currently
  only exposes 7 variants ; comparison ops ( `LESS_THAN` /
  `GREATER_THAN` / … ) and short-circuit ops ( `CONDITIONAL_AND` /
  `CONDITIONAL_OR` ) would need the enum extended on the dhscanner-ast
  side first.

Helpers in use:
- `commalistof(a)`, `listof(a)`.
- `possibly_empty_commalistof(a): '[' ']' { [] } | '[' commalistof(a) ']' { $2 }`
  -- bundles the brackets so `annotations:[...]`, `supers:[...]`, `children:[...]`
  stay uniform whether empty or not. Used in `stmt_class` and `stmt_vardec_*`.
  Prefer this over `optional(commalistof(a))` for JSON array positions.

Smart constructors in `JavaParserActions.hs` are **all still stubs** -- they
build a minimal `Ast` shape ( name only, empty supers, empty members, etc. )
so the grammar type-checks and the pipeline round-trips. Fleshing them out
is a separate pass that happens after the grammar accepts the whole file.

Stubs in play right now:

- `stmtClass loc name _annotations _supers _members` -- returns
  `Ast.StmtClass` with just `stmtClassName` set.
- `stmtVardec loc name typ init` -- returns `Ast.StmtVardec` with
  `stmtVardecName`, `stmtVardecNominalType = Just typ`, `stmtVardecInitValue = init`,
  `stmtVardecLocation`.
- `stmtCtor loc name _params` -- placeholder; emits a trivial
  `Ast.StmtMethod` with name = `"<init>"`. Params list arrives real
  ( built from the hoisted `parameters` JSON key ) but is dropped.
- `stmtMethod loc _name _params _typ` -- placeholder; emits a trivial
  `Ast.StmtMethod`. Params + return type arrive real but are dropped.
- `parameter loc name typ` -- **real** smart constructor ( not a stub ):
  returns `Ast.Param { paramName, paramNominalType = Just typ,
  paramSerialIdx = 0 }`. Serial-idx re-enumeration happens the day
  `stmtMethod` / `stmtCtor` thread the list into `stmtMethodParams`.
- `stmtIf loc cond thenStmt elseStmts` -- **real** smart constructor
  ( not a stub ): returns `Ast.StmtIf { stmtIfCond = cond, stmtIfBody =
  [thenStmt], stmtElseBody = elseStmts, stmtIfLocation = loc }`.
- `expCall loc children` -- **real** smart constructor ( not a stub ):
  splits `children = callee:args`, returns `Ast.ExpCall { callee,
  args, expCallLocation = loc }`. Empty-children fallback returns
  `Ast.ExpNull` ( defensive, since the grammar's `commalistof(exp)`
  guarantees ≥1 element ).
- `expBinop loc lhs rhs op` -- **real** smart constructor ( not a stub ):
  returns `Ast.ExpBinop { expBinopLeft = lhs, expBinopRight = rhs,
  expBinopOperator = op, expBinopLocation = loc }`. Operator discrimination
  happens at the grammar level in the `binop_kind` sub-rule.
- `expBool loc value` -- **real** smart constructor ( not a stub ):
  returns `Ast.ExpBool (ExpBoolContent (Token.ConstBool (value == "true")
  loc))`. The surface `value` string ( always `"true"` or `"false"` per
  javac ) is lowered to a Haskell `Bool` via equality on `"true"` ; any
  non-"true" string, including the impossible malformed case, defaults
  to `False`.
- `parameterizedType loc children` -- first child wins ( type args dropped ).
- `primitiveType loc name` -- bare `Ast.VarSimple` of the primitive name.
- `expStr loc value`, `expInt loc value` -- straight `ExpStr` / `ExpInt`.

## Column-progression log ( pretty JSON of `MetadataValue.java` native AST )

Progress is measured against the **pretty-printed native AST** ( `json.dumps(indent=2)` ),
not the Java source, because the parse-failure column the Happy grammar reports
is a column inside the token stream the parsers service actually sees.

| iteration                                           | fail line | fail cols | token                 |
|-----------------------------------------------------|-----------|-----------|-----------------------|
| start ( `stmt_import` only )                        |      899  |     15    | `"CLASS"` kind        |
| + `stmt_class` / modifiers / annotation / exp_assign |     959  |     18    | `,` after single-child annotation |
| + `commalistof(exp)` in annotation                   |     981  |  31–47    | `"STRING_LITERAL"` kind |
| + `exp_str`                                          |     996  |  10–11    | `,` after modifiers inside unenriched CLASS |
| + CLASS printer-enrichment + grammar for it          |    1316  |  31–44    | `"INT_LITERAL"`       |
| + `exp_int`                                          |    1488  |  21–35    | `"NULL_LITERAL"` kind |
| + `optional(commalistof(a))`                         |    1488  |  21–35    | `"NULL_LITERAL"` ( pure refactor, no behavior change ) |
| + `possibly_empty_commalistof(a)` ( shipped: PR #82 )|    1488  |  21–35    | `"NULL_LITERAL"` ( pure refactor, no behavior change ) |
| + `exp_null` ( new `exp` alternative + `expNull` smart ctor + `'NULL_LITERAL'` token ) |    2288  |  13–14    | `[` opening the non-empty `children` of the first `METHOD` node — hits the `stmt_method` placeholder's `'children' : '[' ']'` wall |
| + printer: `MODIFIERS` universally dropped + `METHOD` hoists `annotations:[]` (parallel to `CLASS` / `VARIABLE`); grammar: `stmt_method` gains an `'annotations' ':' possibly_empty_commalistof(annotation)` slot ( paired printer + grammar iteration — printer alone would have regressed the fail position to `(2287, 11–24)` on the new `"annotations"` key ) |    2289  |  13–14    | `{` opening the first non-empty entry of the `<init>` ctor's `children` — the `BLOCK` of the body. Still the `stmt_method` placeholder's `'children' : '[' ']'` wall; just one line further in. |
| + `stmtBlock` ( `"BLOCK"` → `Ast.StmtBlock` ) + `stmtExp` ( `"EXPRESSION_STATEMENT"` → `Ast.StmtExp`, which in the dhscanner AST is a direct wrapper around an `Ast.Exp` — the statement form of an expression ) + a shared `stmtBody` sum nonterminal ( `stmtBlock \| stmtExp`, extensible with the full statement zoo later ). All three deliberately NOT reachable from `program` yet: this iteration stages the building blocks ; the next iteration wires them into `stmt_method`'s `children`. Happy "unused rules" for JavaParser goes `1` → `8` ( the 3 new productions + helper instantiations of `possibly_empty_commalistof` / `commalistof` / `listof` on `stmtBody` ). No new shift/reduce or reduce/reduce conflicts. |    2289  |  13–14    | unchanged ( staged infrastructure, no reachable-grammar change ) |
| + renamed staged rules to snake_case ( `stmtBody` → `stmt_body`, `stmtBlock` → `stmt_block`, `stmtExp` → `stmt_exp` ) to match the rest of the Java grammar's in-flight style ( snake_case = TODO per the TS convention; smart-constructor Haskell functions stay camelCase ) + wired `stmt_body` into `stmt_method`: `'children' ':' '[' ']'` → `'children' ':' possibly_empty_commalistof(stmt_body)`. `stmtMethod` smart constructor still drops the body list ( `$20` unused ) — grows past its stub form when the next printer-level METHOD enrichment lands. Happy "unused rules" collapses `8` → not-reported ( zero ) ; "unused terminals" `127` → `125` ( `BLOCK` + `EXPRESSION_STATEMENT` now reachable ). |    2381  |  23–35    | `"IDENTIFIER"` kind token sitting before the `BLOCK` in `getID()`'s METHOD-children `[ IDENTIFIER("Integer"), BLOCK ]`. Parameterless `<init>()` ctor ( children `[ BLOCK ]` ) now parses end-to-end ; any METHOD that carries a return-type envelope ( or a `PARAMETER`, further on ) fails at the first non-BLOCK/non-EXPRESSION_STATEMENT child. |
| + `stmt_return` ( `"RETURN"` → `Ast.StmtReturn`, record with `stmtReturnValue :: Maybe Ast.Exp` always wrapped `Just` for now; bare `return;` form will add a second alternative when a file that uses it shows up ) added as a third `stmt_body` alternative + `'RETURN'` parser-token binding ( lexer-token `AlexRawToken_RETURN` already existed ). Reachable from day one via `stmt_body` → `stmt_method` → ... → `program`, so no "unused rules" bump. No new shift/reduce or reduce/reduce conflicts. |    2381  |  23–35    | unchanged — same category as the previous `optional(commalistof(a))` / `possibly_empty_commalistof(a)` prep rows: the new rule is wired in and well-formed, but the current wall is on the return-TYPE `IDENTIFIER` sitting as a bare child of METHOD, not on anything inside a body yet. `stmt_return` activates on the next iteration that unblocks return-type parsing and lets the parser descend into `getID()`'s `BLOCK`. |
| + split `"METHOD"` into `"CONSTRUCTOR"` + `"METHOD"` at the **printer level** ( `AstPrinter.scan`: when `MethodTree.getReturnType() == null`, emit `kind: "CONSTRUCTOR"` instead of `kind: "METHOD"` — spec-defined distinction, more robust than string-matching the `<init>` name ) + corresponding grammar split: new `stmt_ctor` rule ( ex-`stmt_method` body, discriminates on `'CONSTRUCTOR'` kind token, children `possibly_empty_commalistof(stmt_body)` ) + tightened `stmt_method` rule to require a mandatory return-type first child ( `'[' type ',' stmt_body ']'` — exactly 2 positional children for now: `type` + 1 body stmt ). `attribute` gains the new `stmt_ctor` alternative. Smart ctors: `stmtMethod` is split into `stmtCtor` ( identical stub ) + extended `stmtMethod :: Location -> String -> Ast.Var -> Ast.Stmt` ( accepts the type but still drops it alongside params/body until the next enrichment ). New `'CONSTRUCTOR'` parser token + `AlexRawToken_CONSTRUCTOR` lexer token + `@KW_CONSTRUCTOR = \"CONSTRUCTOR\"` lexer macro. No new Happy conflicts ; "unused rules" / "unused terminals" baselines unchanged ( `0` / `125` ). |    2506  |  23–33    | `"VARIABLE"` kind token at the **second** positional child of `setDSpaceObject(DSpaceObject dso)` ( Java line 131; native children `[ PRIMITIVE_TYPE("VOID"), VARIABLE("dso"), BLOCK ]` ). The parser got through the `<init>` ctor + every parameterless getter ( `getID` / `getDSpaceObject` / … ) and now hits the first setter whose METHOD-children contains a `PARAMETER`-shaped `VARIABLE` envelope before the `BLOCK`. Current `stmt_method` accepts exactly `[ type, stmt_body ]` — 2 positional children — so any method with parameters fails on the first non-body child after `type`. |
| + **printer-level `parameters` key promotion** ( `AstPrinter.MethodTree` branch: hoist `getParameters()` into a dedicated `parameters:[]` sibling array, each entry as a slim `{ kind: "PARAMETER", loc, text, type }` envelope — the printer stops calling `super.scan` for `MethodTree` and walks `typeParameters` / `returnType` / `receiverParameter` / `throws` / `body` / `defaultValue` explicitly so parameter `VariableTree`s aren't double-emitted ). Dedicated `"PARAMETER"` kind mirrors the earlier `CONSTRUCTOR` split pattern — lets the grammar discriminate with a clean kind-token terminal instead of overloading the full `VARIABLE` envelope. Grammar: new `parameter` rule, both `stmt_ctor` + `stmt_method` grow a `'parameters' ':' possibly_empty_commalistof(parameter) ','` slot ( between `annotations` and `children` ). Smart ctors: new real `parameter :: Location -> String -> Ast.Var -> Ast.Param`; `stmtCtor` / `stmtMethod` signatures gain a `[Ast.Param]` argument. New `'PARAMETER'` + `'parameters'` parser tokens + `AlexRawToken_PARAMETER` + `AlexRawToken_parameters` lexer tokens + `@KW_PARAMETER` + `@KW_PARAMETERS_KEY` lexer macros. No new Happy conflicts ; "unused rules: 0" / "unused terminals: 125" baselines unchanged. |    2713  |  27–31    | `"IF"` kind token inside the `BLOCK` body of `setLanguage(String language)` ( Java line 149 — body contains `if (Strings.CS.equals(language, Item.ANY)) { language = null; }` ). The parser now descends into method bodies and parses setter envelopes end-to-end — the first six class members ( `<init>` ctor, `getID`, `getDSpaceObject`, `setDSpaceObject`, `getLanguage`, and most of `setLanguage` ) all round-trip through `stmt_ctor` / `stmt_method` → `stmt_block` → `stmt_body`. New wall is the first non-trivial statement form : `stmt_body` only accepts `stmt_block \| stmt_exp \| stmt_return` today, so `"IF"` needs a new `stmt_if` alternative. |
| + `stmt_if` ( `"IF"` → `Ast.StmtIf`, record with `stmtIfCond :: Ast.Exp` / `stmtIfBody :: [Ast.Stmt]` / `stmtElseBody :: [Ast.Stmt]` / `stmtIfLocation :: Location` ) added as a 4th `stmt_body` alternative. Children shape `[ exp, stmt_body, stmt_body? ]` ; else-arm is an inline `optional_else` helper production ( empty → `[]`, `, stmt_body` → `[stmt]` ). Paired with `exp_parenthesized` ( `"PARENTHESIZED"` → unwraps directly to the inner `exp` ) added as a 6th `exp` alternative, so `if (cond)` condition's always-emitted javac `PARENTHESIZED` wrapper is handled transparently and the smart ctor sees a clean `Ast.Exp`. Real smart constructor ( not a stub ) : `stmtIf loc cond thenStmt elseStmts = Ast.StmtIf (Ast.StmtIfContent cond [thenStmt] elseStmts loc)`. New `'IF'` + `'PARENTHESIZED'` parser-token bindings ( lexer-tokens `AlexRawToken_IF` + `AlexRawToken_PARENTHESIZED` + `@KW_IF` + `@KW_PARENTHESIZED` lexer macros already existed from earlier work ). No new Happy conflicts ; "unused rules: 0" / "unused terminals: 125" baselines unchanged. |    2731  |  35–54    | `"METHOD_INVOCATION"` kind token inside the `PARENTHESIZED` condition of the `if (Strings.CS.equals(language, Item.ANY))` in `setLanguage` ( Java line 150 ). The parser now consumes the `IF` envelope + unwraps the `PARENTHESIZED` wrapper and lands on the condition's inner expression ; current `exp` sum ( `var \| str \| int \| null \| assign \| parenthesized` ) has no method-call alternative, so `"METHOD_INVOCATION"` is the next natural wall. Clean strict-progress handoff ( 18 lines deeper, strictly inside what the previous iteration couldn't enter ). |
| + **printer-level `IF` cond/body key promotion** ( `AstPrinter` : new `IfTree` branch hoisting `getCondition()` to a named `cond` key and `getThenStatement()` to a named `body` key ; else-arm stays in `children:[]` as its only remaining positional slot ). Grammar : `stmt_if` rewritten to consume the new shape `'cond' ':' exp ',' 'body' ':' stmt_body ',' 'children' ':' possibly_empty_commalistof(stmt_body)` ; inline `optional_else` helper deleted ( no longer needed -- else is a 0/1-element `children:[]` ). Lexer : wired the missing `@KW_COND_KEY = \"cond\"` + `@KW_BODY_KEY = \"body\"` macros + rules routing to the pre-existing `AlexRawToken_COND` / `AlexRawToken_BODY` enum variants ( parser-side `'cond'` + `'body'` bindings were legacy TS-parser leftovers, defined-but-dangling until this iteration ). Smart constructor `stmtIf` unchanged -- signature was already a perfect fit. **Pure shape refactor, not a strict-progress iteration** : same semantic wall ( `METHOD_INVOCATION` inside the condition ) at the same functional position ; the small 2731 → 2730 line shift is pretty-JSON indentation only, because `cond:` now sits at the IF's envelope level instead of nested under `children:[` + `{`. No new Happy conflicts ; "unused rules" baseline unchanged at `0` ; "unused terminals" drops `125` → `123` ( the two long-dangling TS-era `'cond'` + `'body'` tokens are now reachable ). |    2730  |  33–52    | `"METHOD_INVOCATION"` kind token -- same wall as the previous iteration, just at a slightly different pretty-JSON line/col due to the shape change. Previously-passing prefix still passes end-to-end ( parser consumes the new named-key IF shape, unwraps PARENTHESIZED, then fails on the same `exp` form it couldn't handle before ). |
| + `exp_call` ( `"METHOD_INVOCATION"` → `Ast.ExpCall`, record with `callee :: Ast.Exp` / `args :: [Ast.Exp]` / `expCallLocation :: Location` ) added as a 7th `exp` alternative. Children shape `[ callee, arg* ]` expressed as `commalistof(exp)` ( ≥1 element guaranteed ) ; smart ctor splits head → callee, tail → args. Callee as `MEMBER_SELECT` chain or plain `IDENTIFIER` falls out of the existing `exp_var` / `var` / `field_var` recursion -- no new rules needed for the common `a.b.c(...)` shape. New `'METHOD_INVOCATION'` parser-token binding ( lexer-token `AlexRawToken_METHOD_INVOCATION` + `@KW_METHOD_INVOCATION` lexer macro already existed from earlier work ). Real smart constructor ( not a stub ) : empty-list fallback is a defensive `Ast.ExpNull` ( grammar makes empty impossible, silences `-Wall` ). No new Happy conflicts ; "unused rules: 0" / "unused terminals: 123" baselines unchanged ( new terminal is reached immediately on introduction ). |    3356  |  39–58    | `"METHOD_INVOCATION"` kind token sitting as the **receiver** of a `MEMBER_SELECT` ( Java line 187: `return getMetadataField().getID();` -- the outer `METHOD_INVOCATION("getID")`'s callee is `MEMBER_SELECT("getID") { children: [ METHOD_INVOCATION("getMetadataField") ] }` ). The parser got through ~37 Java lines ( the rest of `setLanguage`'s if-then + the trailing assignment, `getPlace`, `setPlace`, `getPlace`-adjacent getters, and most of `getMetadataFieldId` ) -- 626 lines of native-AST further ! -- and now hits the first **chained** method call, where `field_var`'s receiver slot is still `var` ( = `simple_var \| field_var` ) and can't accept the `exp_call` receiver. Next grammar step is to broaden `field_var`'s receiver to `exp` ( or at least to admit `exp_call` ) -- the AST side already has `varFieldLhs :: Exp`, so there's no shape mismatch to work around. |
| + **broaden `field_var`'s receiver slot from `var` to `exp`** ( one-char grammar change : `'children' ':' '[' var ']'` → `'children' ':' '[' exp ']'` ) + re-type `fieldVar` smart constructor from `Location -> String -> Ast.Var -> Ast.Var` to `Location -> String -> Ast.Exp -> Ast.Var` ( the inner `fieldVar''` helper had always taken `Ast.Exp` ; the ad-hoc `Ast.ExpVar . Ast.ExpVarContent` wrap at the outer call site is gone -- the mismatch had been purely cosmetic since `varFieldLhs :: Exp` on the dhscanner AST side ). Enables chained method calls like `getMetadataField().getID()` to parse through, since the `MEMBER_SELECT`'s receiver slot can now accept an `exp_call` ( or any other `exp` form -- `exp_str` for `"foo".length()` etc. ). No new Happy conflicts ( LR(1) disambiguates cleanly on the receiver's `'kind' : <X>` sentinel, as the grammar does everywhere else ) ; "unused rules: 0" / "unused terminals: 123" baselines unchanged. |    3998  |  33–43    | `"EQUAL_TO"` kind token inside the `obj == null` condition of `if (obj == null)` in `equals(Object obj)` ( Java line 255 ). The parser now rips through all the chained-call methods ( `getMetadataFieldId` + `getValue` / `setValue` / `getLength` / etc. ) and enters the `equals` override, parses its METHOD envelope + BLOCK body + first IF envelope + PARENTHESIZED cond, and now hits the first binary-operator expression form : `EQUAL_TO` ( the `==` operator ). 642 lines of native-AST further ! Next grammar step is `exp_binop` ( `EQUAL_TO`, `NOT_EQUAL_TO`, `PLUS`, `LESS_THAN`, ... → `Ast.ExpBinop` ). |
| + **printer-level `BinaryTree` left/right key promotion** ( `AstPrinter` : new `BinaryTree` branch hoisting `getLeftOperand()` → `left` key and `getRightOperand()` → `right` key, with `children:[]` kept empty -- the kind name itself encodes the operator ) + grammar `exp_binop` ( 8th `exp` alternative ) consuming the enriched shape, with a sibling `binop_kind` discriminator sub-rule mapping kind-token → `Ast.Operator` ( `EQUAL_TO`→`EQ`, `PLUS`→`PLUS`, `MULTIPLY`→`TIMES` -- the only three kinds that appear in this file ; `NEQ` / `MINUS` / `DIVIDE` / `PERCENT` are one-liner `binop_kind` alternatives away ). New `'EQUAL_TO'` + `'PLUS'` + `'MULTIPLY'` parser-token bindings ( all three `AlexRawToken_*` + `@KW_*` lexer-side pre-existed ) ; lexer gains `@KW_LEFT_KEY = \"left\"` + `@KW_RIGHT_KEY = \"right\"` macros + rules routing to pre-existing `AlexRawToken_LEFT` / `_RIGHT` enum variants ( parser-side `'left'` + `'right'` bindings were legacy TS-era leftovers, defined-but-dangling until this iteration -- same cleanup pattern as the `cond`/`body` reclamation two iterations back ). Real smart constructor ( not a stub ) : `expBinop loc lhs rhs op = Ast.ExpBinop (Ast.ExpBinopContent lhs rhs op loc)`. No new Happy conflicts ; "unused rules: 0" baseline unchanged ; "unused terminals" drops `123` → `121` ( the two `'left'` + `'right'` TS-era tokens finally reachable ). |    4050  |  37–54    | `"BOOLEAN_LITERAL"` kind token at the `return false;` inside the first then-branch of `equals()` ( Java line 256 ). The `obj == null` condition parsed cleanly ( `IDENTIFIER("obj")` as `left`, `NULL_LITERAL` as `right`, `EQUAL_TO` → `Ast.EQ` ) ; parser then entered the then-branch `BLOCK`, started the `RETURN` envelope, and dies on the `false` boolean literal -- current `exp` sum has no `exp_bool` alternative. Natural trivial next iteration : `exp_bool` ( like `exp_str` / `exp_int` / `exp_null`, same shape ). |
| + **add `exp_bool` as 5th `exp` literal-family alternative** ( `JavaParser.y` : new `exp_bool` production consuming the uniform literal envelope `{ kind: BOOLEAN_LITERAL, loc, text: ID, children: [] }`, identical shape to `exp_null` / `exp_int` / `exp_str` ) + new `'BOOLEAN_LITERAL'` parser-token binding ( `@KW_BOOLEAN_LITERAL` lexer-side rule + `AlexRawToken_BOOLEAN_LITERAL` enum variant were pre-provisioned -- same legacy-reclaim pattern as `METHOD_INVOCATION` and the op kinds ) + real smart constructor `expBool :: Location -> String -> Ast.Exp` on `JavaParserActions.hs` returning `Ast.ExpBool (Ast.ExpBoolContent (Token.ConstBool (value == "true") loc))` -- defensive-total string compare since javac is guaranteed to emit exactly one of `"true"` / `"false"` for a `BooleanLiteral` tree. No printer-side changes needed ( the literal shape was already uniform with the other literals ). No new Happy conflicts ; "unused rules: 0" / "unused terminals: 121" baselines unchanged ( `BOOLEAN_LITERAL` was not previously counted among "unused" because it had never been declared parser-side ; net declared-and-used change is +1 / +1 ). |    4067  |  27–37    | `"VARIABLE"` kind token at the local variable declaration `Class<?> objClass = HibernateProxyHelper.getClassWithoutInitializingProxy(obj);` ( Java line 258, the first stmt after the `if (obj == null) return false;` guard in `equals()` ). The full `return false;` parses cleanly ( `BOOLEAN_LITERAL` lowered to `Ast.ExpBool (Token.ConstBool False <loc>)` ) ; parser consumes the IF envelope, then expects another `stmt_body` and sees a `VARIABLE` kind -- but the current `stmt_body` sum is `stmt_block \| stmt_exp \| stmt_return \| stmt_if`, no `stmt_vardec`. Trivial next iteration : wire the already-existing `stmt_vardec` production ( currently only reachable as a class-member rule ) in as a 5th `stmt_body` alternative. ( Secondary wall waiting right after : the VARIABLE's `type` slot carries a `PARAMETERIZED_TYPE` -- `Class<?>` -- that the grammar's `type` sum doesn't yet accept. ) |
| + **wire `stmt_vardec` as a 5th `stmt_body` alternative** ( one-liner : `stmt_body: ... \| stmt_vardec` ). Same production already used as a class-member `attribute` -- local vars and fields share the VARIABLE envelope ( `stmt_vardec_init` / `stmt_vardec_no_init` + `stmtVardec` smart ctor all pre-exist ). No printer / lexer / actions change. No new Happy conflicts ; "unused rules: 0" / "unused terminals: 121" baselines unchanged ( the production was already reachable via `attribute` ). |    4097  |  33–53    | `"UNBOUNDED_WILDCARD"` kind token -- the `?` type-argument of `Class<?> objClass = ...` ( Java line 258 ). Parser entered the local VARIABLE, consumed its `PARAMETERIZED_TYPE` envelope ( already a `type` alternative ), parsed the `IDENTIFIER("Class")` base, and dies on the second child. Current `type` sum is `simple_var \| parameterized_type \| primitive_type` -- no wildcard. Next : `type_unbounded_wildcard` ( lexer `@KW_UNBOUNDED_WILDCARD` + `AlexRawToken_UNBOUNDED_WILDCARD` already exist ; parser-token binding + a `{ kind, loc, children: [] }` production are the missing wires ). |
| + **printer-level drop of `UNBOUNDED_WILDCARD`** ( `AstPrinter.scan` : early-return on `Tree.Kind.UNBOUNDED_WILDCARD`, same elision pattern as `MODIFIERS` ). `Class<?>` now emits as `PARAMETERIZED_TYPE` with a single `IDENTIFIER("Class")` child -- no grammar / lexer / actions change needed (`parameterized_type` already accepts `commalistof(type)`, and `parameterizedType` already keeps only the base). No new Happy conflicts ; unused-terminal baseline unchanged ( `AlexRawToken_UNBOUNDED_WILDCARD` stays lexer-side unused, which is the point ). |    4162  |  33–53    | `"LOGICAL_COMPLEMENT"` kind token -- the `!` in `if (!objClass.equals(this.getClass()))` ( Java line 259 ). The local `Class<?> objClass = HibernateProxyHelper.getClassWithoutInitializingProxy(obj);` now parses end-to-end. Current `exp` sum has no unary-op alternative. Lexer `@KW_LOGICAL_COMPLEMENT` + `AlexRawToken_LOGICAL_COMPLEMENT` already exist ; dhscanner-ast has no `ExpUnop` -- next iteration is either unwrap-to-inner-exp ( if we don't care about the `!` ) or a new `exp_not` wrapping whatever unary form the AST already has. |
| + **unary ops lowered via `<dhscanner-instrumentation>[<tag>]`** ( port of TS convention ) -- new grammar sum `unop: not exp ']' '}'` as 10th `exp` alternative, with a dedicated `not` sub-rule consuming `'{' 'kind' ':' 'LOGICAL_COMPLEMENT' ',' 'loc' ':' location ',' 'children' ':' '['` and returning the operator envelope's `loc`. Each future unary op ( `NEGATE`, `BITWISE_COMPLEMENT`, `UNARY_PLUS`, `UNARY_MINUS`, `PREFIX_INCREMENT`, … ) adds a sibling sub-rule + one `unop` alternative that passes its own tag string. New `'LOGICAL_COMPLEMENT'` parser-token binding ( `@KW_LOGICAL_COMPLEMENT` + `AlexRawToken_LOGICAL_COMPLEMENT` already existed lexer-side ). Actions-side ports two helpers from `TsParserActions.hs` : `expvarify :: Token.Named -> Ast.Exp` ( four-layer `Ast.ExpVar . ExpVarContent . VarSimple . VarSimpleContent . Token.VarName` wrap ) + `instrumentationCall :: String -> Location -> [Ast.Exp] -> Ast.Exp` ( builds an `Ast.ExpCall` whose callee is `<dhscanner-instrumentation>[<tag>]` ) -- same wire shape every downstream consumer ( codegen / kbgen / queryengine's `utils.pl` / SARIF post-processing ) already recognizes. Thin `expUnop tag loc operand = instrumentationCall tag loc [operand]` wraps the operand. No new Happy conflicts ; "unused rules: 0" / "unused terminals: 121" baselines unchanged ( new terminal reached immediately on introduction ). |    4287  |  29–40    | `"TYPE_CAST"` kind token -- the `(MetadataValue) obj` cast in `MetadataValue mv = (MetadataValue) obj;` ( Java line 262, inside the else-branch of `if (!objClass.equals(this.getClass()))` ). Parser consumed the `unop` wrapper of `!...`, parsed the entire guarded `return false;` body, exited the IF, and now lands on the next local vardec whose `init` slot is a `TYPE_CAST`. Current `exp` sum has no cast alternative. Next : add `exp_cast` ( printer-level drop of the cast wrapper is the simplest option -- just unwrap to the inner `exp`, since dhscanner analyses don't care about Java-specific type coercions ). |
| + **printer-level unwrap of `TYPE_CAST`** ( `AstPrinter.scan` : new `instanceof TypeCastTree` branch BEFORE the comma / `{` opening, recursively `scan`s only `castTree.getExpression()` and returns -- the target type child is dropped entirely ). `(T) expr` lands as just `expr` in the native AST ; dhscanner analyses have no use for Java's static-type coercions, same philosophy as the `UNBOUNDED_WILDCARD` / `MODIFIERS` drops. No grammar / lexer / actions change needed. New import : `com.sun.source.tree.TypeCastTree`. |    --    |  --       | **FULL FILE PARSES END-TO-END.** `MetadataValue.java` → native AST ( 197767 pretty bytes, 0 `TYPE_CAST` nodes ) → dhscanner AST ( 5920 bytes, 19 top-level stmts : 1 `StmtExp` package marker + 17 `StmtImport` + 1 `StmtClass` for the `MetadataValue` JPA entity ). Parser returns the AST object, no `FAILED` payload. This is the single-file regression target the `.github/workflows/tests.yaml` sensor has been waiting on. |

**Next**: `MetadataValue.java` is done -- the pipeline-wide CI sensor on
runner's `.github/workflows/tests.yaml` should flip green once the
submodule-pointer bumps propagate ( fronts + parsers push → `dhscanner.core`
submodule bump → runner PR bumps `compose/compose.prebuilt.yaml`'s
`orenishdocker/dhscanner-parsers:<ver>-x64` ). After that, scale the
regression repo : pick a second DSpace file ( ideally one that uses
constructs this file didn't -- `instanceof`, lambdas, `try` / `catch`,
`switch`, `for` / `while` / `do`, enum constants, inner classes,
arrays, `new T(...)` -- so each new file forces a strict grammar
extension ) and run the same column-tracking loop against it.

## Column-progression log ( pretty JSON of `UUIDUtils.java` native AST )

Second regression file -- same progress-tracking methodology as the
`MetadataValue.java` log above. Fail position is a `(line, cols)` pair
inside the pretty-printed native AST that the parsers service sees ;
strict progress = position moves forward past the previous row, with
every previously-passing file ( `MetadataValue.java` + any earlier
`UUIDUtils.java` rows ) still passing its own gate.

| iteration | fail line | fail cols | token |
|-----------|-----------|-----------|-------|
| baseline ( file freshly staged ; grammar at the end-of-MetadataValue state ) | 369 | 33-38 | `"TRY"` kind token at the entry of `fromString()`'s `try { output = UUID.fromString(identifier.trim()); } catch (IllegalArgumentException e) { output = null; }`. `stmt_body` only accepts `stmt_block \| stmt_exp \| stmt_return \| stmt_if \| stmt_vardec` at this point -- no TRY alternative. |
| + `stmt_try` + `stmt_catch` productions ( first iteration against this file ) -- new `stmt_try` consuming the native TRY envelope `{ kind: TRY, loc, children: [ BLOCK, CATCH ] }` ; sibling `stmt_catch` consuming `{ kind: CATCH, loc, children: [ VARIABLE, BLOCK ] }` ( caught-exception VARIABLE reuses the existing `stmt_vardec_no_init` shape, then dropped on the floor -- same `catch`-binding-dropped TODO documented on the TS side ) + wired `stmt_try` as a 6th `stmt_body` alternative. Smart constructor `stmtTry :: Location -> Ast.Stmt -> Ast.Stmt -> Ast.Stmt` ( byte-for-byte match of TsParserActions' `stmtTry` wire shape : `stmtTryPart` = try-body stmts list, `stmtCatchPart` = catch-body stmts list, caught-exception identifier dropped ) + private `unblock :: Ast.Stmt -> [Ast.Stmt]` helper that pattern-matches an `Ast.StmtBlock` and returns its inner `[Ast.Stmt]` ( symmetric to TS's `block` production returning `[Ast.Stmt]` directly ; the Java-side `stmt_block` already wraps into `Ast.StmtBlock` because it's reachable as a stand-alone `stmt_body` alternative, so the unwrap has to live in the action ). Scope restricted to `[ BLOCK, CATCH ]` for now ; multi-catch, finally-block, and try-with-resources resource-list widen in follow-up iterations once a regression file surfaces each ( `Check.java`'s `try { ... } finally { ... }` is the natural next gate ). New `'TRY'` + `'CATCH'` parser-token bindings + `@KW_TRY` / `@KW_CATCH` lexer macros + rules + `AlexRawToken_TRY` / `AlexRawToken_CATCH` enum variants. No new Happy conflicts ; "unused rules: 0" baseline unchanged ; "unused terminals: 121" baseline unchanged ( new terminals reached immediately on introduction via `stmt_try` ). | 673 | 31-55 | `"CONDITIONAL_EXPRESSION"` kind token inside `toString()`'s `return identifier == null ? null : identifier.toString();` ( Java line 37 -- the ternary ). The entire `try { ... } catch { ... }` block in `fromString()` parses end-to-end ; parser then consumes the second method's METHOD envelope + `String` return type + BLOCK body + RETURN envelope, and dies on the ternary. Current `exp` sum has no ternary alternative. Next iteration ports the TS `exp_ternary` pattern ( `instrumentationCall "ternary"` with three per-branch sub-calls `[ternary_cond]` / `[ternary_then]` / `[ternary_else]` -- see TsParserActions.hs's `expTernary` ). |
| + `expTernary` ( `"CONDITIONAL_EXPRESSION"` -> instrumented `Ast.ExpCall` ) added as an 11th `exp` alternative. Native javac shape is a flat 3-positional-child envelope `{ kind: CONDITIONAL_EXPRESSION, loc, children: [ <cond>, <then>, <else> ] }` -- simpler than the TS side's `ConditionalExpression ( exp '?' exp ':' exp )` which has explicit delimiters, but the dhscanner lowering is identical. Smart constructor is a verbatim port of `TsParserActions.expTernary` : nested `instrumentationCall` where the outer call is tagged `[ternary]` and each of its three args is itself a tagged sub-call ( `[ternary_cond]` / `[ternary_then]` / `[ternary_else]` ), one per branch, so a nested ternary ( e.g. then-branch is itself a ternary ) stays discriminable by callee name rather than by the ambiguous three-args-at-each-level positional index. No new infrastructure needed -- `instrumentationCall` + `expvarify` were already shipped with the `unop` iteration. New `'CONDITIONAL_EXPRESSION'` parser-token binding + `@KW_CONDITIONAL_EXPRESSION` lexer macro + rule + `AlexRawToken_CONDITIONAL_EXPRESSION` enum variant. No new Happy conflicts ; "unused rules: 0" / "unused terminals: 121" baselines unchanged ( new terminal reached immediately on introduction via `expTernary` ). | -- | -- | **FULL FILE PARSES END-TO-END.** `UUIDUtils.java` -> native AST ( 26872 pretty bytes, 1 `TRY` + 1 `CATCH` + 1 `CONDITIONAL_EXPRESSION` ) -> dhscanner AST, no `FAILED` payload. Second regression file is done ; both `MetadataValue.java` and `UUIDUtils.java` round-trip cleanly. Scale next by picking a third DSpace file from the previous scan's `DHSCANNER_PARSING_FAILED` set that exercises a construct neither has : `Check.java` ( try / finally ), `Dispatcher.java` ( `new T<...>()` + `super()` + `throws` ), `Mutative.java` ( `@interface` annotation decl ), or any JPA entity ( `@OneToMany`, `List<T>`, streams / lambdas ). |

### 3. `BundleResource.java` ( shipped, end-to-end )

- **`BundleResource.java`** at the same DSpace tag, path
  `dspace-server-webapp/src/main/java/org/dspace/app/rest/model/hateoas/BundleResource.java`.
- Picked as the archetype of the single biggest fail cluster in the
  post-`UUIDUtils` scan : 78 HATEOAS Resource wrapper files all failing
  at the exact same pretty-JSON span `[201:23-201:36]`. All share the
  same import pattern :
  `import org.dspace.app.rest.model.hateoas.annotations.RelNameDSpaceResource;`
  -- a 7-segment import whose 6th segment is the lowercase word
  `annotations`, which collides with the Alex reserved keyword token
  used as the structural JSON field name for the `annotations:[...]`
  array in every `stmt_class` / `stmt_interface` / `stmt_vardec` /
  `stmt_ctor` / `stmt_method` envelope.
- Lives in the runner workspace at `dspace_java_sample/BundleResource.java`.

| iteration | fail line | fail cols | token |
|-----------|-----------|-----------|-------|
| baseline ( file freshly staged ; grammar at the end-of-`UUIDUtils` state ) | 201 | 23-36 | `'annotations'` keyword token where the parser expected an `ID` -- Alex tokenizes the string `"annotations"` as `AlexRawToken_annotations` ( needed for the structural `annotations:[...]` JSON field in class envelopes ) ; `field_var`'s `'text' ':' ID ','` pattern can't accept it. |
| + **`tokenID` helper nonterminal** ( convention established in Cs / Go / Rb / Py / Php / Js parsers : a scalar helper that unifies `ID` with any reserved keyword tokens that could legitimately appear as an identifier string value ) : `tokenID: ID { tokIDValue $1 } \| 'annotations' { "annotations" }`. Mechanical bulk swap of every `'text' ':' ID ','` ( 15 sites across `stmt_class` / `stmt_interface` / `stmt_vardec_init` / `stmt_vardec_no_init` / `primitive_type` / `parameter` / `stmt_ctor` / `stmt_method` / `exp_str` / `exp_int` / `exp_null` / `exp_bool` / `simple_var` / `field_var` -- any text-value position ) to `'text' ':' tokenID ','`, with the paired action-side cleanup of `(tokIDValue $12)` → `$12` ( 14 sites : `exp_null` was the one site that discards the text ). No lexer / `JavaParserActions.hs` change -- the `'annotations'` token has existed as `AlexRawToken_annotations` from day one, and `tokIDValue` already lives in the actions preamble. No new Happy conflicts ; "unused rules: 0" / "unused terminals: 2" baselines unchanged. | -- | -- | **FULL FILE PARSES END-TO-END.** `BundleResource.java` -> native AST ( 16921 pretty bytes, 7-segment import tree with `annotations` as the 6th segment ) -> dhscanner AST, no `FAILED` payload. All 78 HATEOAS Resource files that shared this exact failure signature now parse end-to-end ; plus 6 additional files outside the cluster that were also blocked by Java identifiers literally spelled `annotations`. Future keyword-vs-identifier collisions ( e.g. a Java method / variable / class literally named `kind` / `text` / `loc` / `children` / `parameters` / `supers` / `start` / `end` ) get resolved by appending the colliding token to `tokenID` -- same incremental-growth pattern every other parser follows. |

## DSpace scan-wide coverage trajectory

Each row is a full-repo DSpace scan ( tag `dspace-10.1`, 3148 Java files
after `NATIVE_PARSING_SUCCEEDED` -- Java coverage denominator ). Rows list
the iterations landed since the previous row.

| after iteration(s) | success | fail | % | Δ files |
|---|---|---|---|---|
| ( baseline, pre-session ) | 607 | 2541 | 19.28% | -- |
| + `stmt_try` + `stmt_catch` + `expTernary` | 620 | 2528 | 19.70% | +13 |
| + `tokenID` ( `annotations` identifier/keyword collision ) | 704 | 2444 | 22.36% | +84 |

Methodology for picking the next iteration target : after each full-repo
scan, bucket `DHSCANNER_PARSING_FAILED` rows by `more_details`
( the `(line, col)` span of the failing lexeme ) ; the largest bucket is
the single biggest unlock available at that point ( N files all failing
on the same construct ). Pick one representative file from the bucket,
inspect its native-AST pretty JSON at the recorded span to identify the
blocking kind / token, and land one grammar / printer adjustment -- same
`AGENTS.md` four-gate contract as the per-file column-progression work.

## Open items ( roughly ordered )

1. ~~**`exp_null`** — add as `exp` alternative. Smart constructor returns `Ast.ExpNull`.~~
   Shipped. `exp_null` production + `expNull :: Location -> Ast.Exp` smart
   constructor + `'NULL_LITERAL'` %token in `JavaParser.y`. Fail column moved
   `1488` → `2288`, no new Happy conflicts, previously-passing prefix still passes.
2. ~~Likely next failures~~ **No remaining walls on `MetadataValue.java`,
   `UUIDUtils.java`, or `BundleResource.java`** -- all three regression files
   parse end-to-end.
   `UUIDUtils.java` consumed `try` / `catch` ( shipped as `stmt_try`
   + `stmt_catch` ) and the ternary ( shipped as `expTernary` --
   verbatim port of the TS `[ternary]` / `[ternary_cond]` /
   `[ternary_then]` / `[ternary_else]` instrumentation nest ). Still
   to come across the full DSpace scan : `instanceof`, `new T(...)`,
   lambdas, enhanced-for, `try` / `finally` ( `Check.java` ),
   multi-catch, try-with-resources, `switch`, enum constants,
   inner-class decls, array indexing / literal, generic-method-call
   type arguments, etc.
3. **Enrich METHOD in the printer** — same pattern as CLASS/VARIABLE:
   top-level `annotations` ✅ ( shipped ), named `parameters:[]` ✅
   ( shipped ), named `returnType`, named `body` ( or `null` for
   abstract ). Then promote `stmt_method` from placeholder to real
   production.
4. **Flesh out smart constructors from stubs** —
   `stmtClass` should fill `stmtClassSupers` / `stmtClassDataMembers` /
   `stmtClassMethods`; `stmtMethod` should return a real `StmtMethodContent`;
   `parameterizedType` should preserve type args somehow.
5. Fold useful content back into `AGENTS.md`, **delete this file**.

## Dev loop ( from the runner workspace )

### One-time bring-up

```powershell
docker compose --env-file .env `
    -f ./compose/compose.base.yaml `
    -f ./compose/compose.app.yaml `
    -f ./compose/compose.fronts.yaml `
    -f ./compose/compose.prebuilt.yaml `
    -f ./compose/compose.workers.yaml `
    -f ./compose/compose.parsers-dev.yaml `
    up -d
```

`compose/compose.parsers-dev.yaml` ( runner-level override, untracked for now )
swaps the Dockerhub-pinned parsers image for a local `build:` so edits to
`JavaParser.y` / `JavaLexer.x` / `JavaParserActions.hs` actually land.

### Per iteration

1. Edit grammar / smart constructor.
2. Rebuild parsers:
   ```powershell
   docker compose --env-file .env -f ./compose/compose.base.yaml -f ./compose/compose.app.yaml -f ./compose/compose.fronts.yaml -f ./compose/compose.prebuilt.yaml -f ./compose/compose.workers.yaml -f ./compose/compose.parsers-dev.yaml build parsers
   docker compose --env-file .env -f ./compose/compose.base.yaml -f ./compose/compose.app.yaml -f ./compose/compose.fronts.yaml -f ./compose/compose.prebuilt.yaml -f ./compose/compose.workers.yaml -f ./compose/compose.parsers-dev.yaml up -d parsers
   ```
3. ( Only when the printer changed ) rebuild frontjava:
   ```powershell
   docker compose ... build --no-cache frontjava
   docker compose ... up -d frontjava
   ```
   `--no-cache` is deliberate — buildx has been aggressive about caching
   the `COPY src` layer here.
4. ( Only when the printer changed ) refresh the pretty JSON :
   ```powershell
   docker cp dspace_java_sample/MetadataValue.java compose-app-1:/tmp/MetadataValue.java
   docker cp tmp_post_java.py compose-app-1:/tmp/tmp_post_java.py
   docker exec compose-app-1 python3 /tmp/tmp_post_java.py
   docker cp compose-app-1:/tmp/MetadataValue.java.native.ast.pretty.json dspace_java_sample/MetadataValue.java.native.ast.pretty.json
   ```
5. POST the pretty JSON to parsers to get the parse-failure location:
   ```powershell
   docker cp tmp_post_parsers.py compose-app-1:/tmp/tmp_post_parsers.py
   docker cp dspace_java_sample/MetadataValue.java.native.ast.pretty.json compose-app-1:/tmp/MetadataValue.java.native.ast.pretty.json
   docker exec compose-app-1 python3 /tmp/tmp_post_parsers.py
   ```
   On failure, the response is `{ "status": "FAILED", "location": { "lineStart", "colStart", "lineEnd", "colEnd", "filename" } }`.
6. Record the new ( line, col ) in the progression table above.

### Shipping an iteration

1. Commit + push `dhscanner.service.parsers` to `main` -- CI auto-publishes
   `orenishdocker/dhscanner-parsers:<new-ver>-x64` and tacks on a
   `:arrow_up: Bump version:` commit.
2. ( If the printer changed ) commit + push `dhscanner.service.fronts` to
   `main` ( no CI, source-built by `compose/compose.fronts.yaml` ).
3. Pull both submodules locally, commit + push `dhscanner.core`'s pointer
   bumps to `main` ( no PR per `AGENTS.md` ).
4. On a feature branch in `dhscanner.runner`: bump `dhscanner.core`
   submodule pointer + bump `compose/compose.prebuilt.yaml` parsers pin.
   Open a PR ( runner is the only repo with the PR workflow ).
