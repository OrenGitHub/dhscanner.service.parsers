module JavaParserActions where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import qualified Ast
import qualified Token
import Location

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import qualified Data.Map

-- ***************
-- *             *
-- * stmtPackage *
-- *             *
-- ***************
stmtPackage :: Location -> Ast.Var -> Ast.Stmt
stmtPackage _loc v = Ast.StmtExp (Ast.ExpVar (Ast.ExpVarContent v))

-- **************
-- *            *
-- * stmtImport *
-- *            *
-- **************
stmtImport :: Ast.Var -> Location -> Ast.Stmt
stmtImport (Ast.VarField v) = stmtImportVarField v
stmtImport (Ast.VarSimple v) = stmtImportVarSimple v
stmtImport (Ast.VarSubscript v) = stmtImportVarSubscript v

stmtImportVarField :: Ast.VarFieldContent -> Location -> Ast.Stmt
stmtImportVarField (Ast.VarFieldContent v f _) = stmtImportVarField' v f

stmtImportVarField' :: Ast.Exp -> Token.FieldName -> Location -> Ast.Stmt
stmtImportVarField' v (Token.FieldName (Token.Named f _)) = stmtImport' (var2name v) (Ast.ImportSpecific f)

stmtImportVarSimple :: Ast.VarSimpleContent -> Location -> Ast.Stmt
stmtImportVarSimple (Ast.VarSimpleContent varName) = stmtImportVarSimple' varName

stmtImportVarSimple' :: Token.VarName -> Location -> Ast.Stmt
stmtImportVarSimple' (Token.VarName (Token.Named v _)) = stmtImport' v (Ast.ImportSpecific "")

stmtImportVarSubscript :: Ast.VarSubscriptContent -> Location -> Ast.Stmt
stmtImportVarSubscript (Ast.VarSubscriptContent v _ _) = stmtImportVarSubscript' v

stmtImportVarSubscript' :: Ast.Exp -> Location -> Ast.Stmt
stmtImportVarSubscript' (Ast.ExpVar (Ast.ExpVarContent v)) = stmtImport v
stmtImportVarSubscript' _ = stmtImport' "" (Ast.ImportSpecific "")

stmtImport' :: String -> Ast.ImportSpecific -> Location -> Ast.Stmt
stmtImport' source specific loc = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = Ast.ImportThirdParty (Ast.ImportThirdPartyContent source),
    Ast.stmtImportSpecific = Just specific,
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = loc
}

-- ************
-- *          *
-- * var2name *
-- *          *
-- ************
var2name :: Ast.Exp -> String
var2name (Ast.ExpVar (Ast.ExpVarContent v)) = var2name' v
var2name _ = ""

var2name' :: Ast.Var -> String
var2name' (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named n _)))) = n
var2name' (Ast.VarField (Ast.VarFieldContent lhs (Token.FieldName (Token.Named n _)) _)) = var2name lhs ++ "." ++ n
var2name' _ = ""

-- *************
-- *           *
-- * simpleVar *
-- *           *
-- *************
simpleVar :: Location -> String -> Ast.Var
simpleVar loc name = simpleVar' (Token.Named name loc)

-- *************
-- *           *
-- * simpleVar *
-- *           *
-- *************
simpleVar' :: Token.Named -> Ast.Var
simpleVar' = Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

-- ************
-- *          *
-- * fieldVar *
-- *          *
-- ************
-- Receiver side of a MEMBER_SELECT is now a full `Ast.Exp` ( matches
-- `varFieldLhs :: Exp` on the dhscanner AST side -- the two had been
-- out-of-sync only because the grammar's `field_var` rule was still
-- restricting its receiver slot to `var` ). Dotted paths like
-- `a.b.c` keep working ( `b`'s receiver is still an `Ast.ExpVar`
-- wrapper around the inner `simple_var` -- just now built via the
-- regular `exp_var` → `var` → `simple_var` chain instead of a
-- here-site wrap ) ; the extra expressive power is **chained** method
-- calls like `foo().getID()`, where the receiver is now an
-- `Ast.ExpCall` instead of an `Ast.ExpVar`.
fieldVar :: Location -> String -> Ast.Exp -> Ast.Var
fieldVar loc field e = fieldVar'' e (Token.FieldName (Token.Named field loc)) loc

-- ************
-- *          *
-- * fieldVar *
-- *          *
-- ************
fieldVar'' :: Ast.Exp -> Token.FieldName -> Location -> Ast.Var
fieldVar'' lhs name loc = Ast.VarField $ Ast.VarFieldContent {
    Ast.varFieldLhs = lhs,
    Ast.varFieldName = name,
    Ast.varFieldLocation = loc
}

-- *************
-- *           *
-- * stmtClass *
-- *           *
-- *************
stmtClass :: Location -> String -> [ Ast.Exp ] -> [ Ast.Var ] -> [ Ast.Stmt ] -> Ast.Stmt
stmtClass loc name _annotations _supers _members = Ast.StmtClass $ Ast.StmtClassContent {
    Ast.stmtClassName = Token.ClassName (Token.Named name loc),
    Ast.stmtClassSupers = [],
    Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
    Ast.stmtClassMethods = Ast.Methods Data.Map.empty
}

-- **************
-- *            *
-- * stmtVardec *
-- *            *
-- **************
stmtVardec :: Location -> String -> Ast.Var -> Maybe Ast.Exp -> Ast.Stmt
stmtVardec loc name typ initValue = Ast.StmtVardec $ Ast.StmtVardecContent {
    Ast.stmtVardecName = Token.VarName (Token.Named name loc),
    Ast.stmtVardecNominalType = Just typ,
    Ast.stmtVardecInitValue = initValue,
    Ast.stmtVardecLocation = loc
}

-- *************
-- *           *
-- * parameter *
-- *           *
-- *************
-- Lower a native PARAMETER envelope ( `{ kind: PARAMETER, loc, text, type }`,
-- minted by the printer out of a `VariableTree` sitting under a
-- `MethodTree.getParameters()` ) into an `Ast.Param`. `paramSerialIdx`
-- is left at 0 for every parameter -- when `stmtMethod` / `stmtCtor`
-- actually thread the param list into `stmtMethodParams` ( next
-- enrichment pass ) they'll re-enumerate via `zipWith (\\i p -> p { ...
-- paramSerialIdx = i })` so this placeholder is harmless.
parameter :: Location -> String -> Ast.Var -> Ast.Param
parameter loc name typ = Ast.Param {
    Ast.paramName = Token.ParamName (Token.Named name loc),
    Ast.paramNominalType = Just typ,
    Ast.paramSerialIdx = 0
}

-- ************
-- *          *
-- * stmtCtor *
-- *          *
-- ************
-- Constructors ( native kind `"CONSTRUCTOR"` -- the printer splits them
-- out of `"METHOD"` on `getReturnType() == null` ). Currently still a
-- stub : returns an `Ast.StmtMethod` with name = `<init>` and
-- `stmtMethodReturnType = Nothing`, matching the Java semantics that
-- ctors have no return type. Params list arrives real ( parser builds
-- it from the hoisted `parameters` JSON key ) but is dropped on the
-- floor along with the body until the next METHOD-enrichment pass.
stmtCtor :: Location -> String -> [ Ast.Param ] -> Ast.Stmt
stmtCtor loc name _params = Ast.StmtMethod $ Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Nothing,
    Ast.stmtMethodName = Token.MethodName (Token.Named name loc),
    Ast.stmtMethodParams = [],
    Ast.stmtMethodBody = [],
    Ast.stmtMethodLocation = loc,
    Ast.hostingClassName = Token.ClassName (Token.Named "" loc),
    Ast.hostingClassSupers = []
}

-- **************
-- *            *
-- * stmtMethod *
-- *            *
-- **************
-- Regular methods ( native kind `"METHOD"` ) -- distinguished from
-- constructors by the printer, so they always arrive with a mandatory
-- return-type `Ast.Var` AND ( as of the parameters-hoisting iteration )
-- a parser-built `[Ast.Param]` list from the enriched `parameters`
-- JSON key. Both are still dropped on the floor along with the body
-- stmt until this smart ctor grows past its stub form -- when it does,
-- `stmtMethodReturnType` becomes `Just typ`, `stmtMethodParams` becomes
-- the enumerated-serial-idx list, and `stmtMethodBody` becomes the
-- lowered body stmts.
stmtMethod :: Location -> String -> [ Ast.Param ] -> Ast.Var -> Ast.Stmt
stmtMethod loc name _params _typ = Ast.StmtMethod $ Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Nothing,
    Ast.stmtMethodName = Token.MethodName (Token.Named name loc),
    Ast.stmtMethodParams = [],
    Ast.stmtMethodBody = [],
    Ast.stmtMethodLocation = loc,
    Ast.hostingClassName = Token.ClassName (Token.Named "" loc),
    Ast.hostingClassSupers = []
}

-- *********************
-- *                   *
-- * parameterizedType *
-- *                   *
-- *********************
parameterizedType :: Location -> [ Ast.Var ] -> Ast.Var
parameterizedType _loc (base:_) = base
parameterizedType loc [] = Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named "unknown" loc)))

-- *****************
-- *               *
-- * primitiveType *
-- *               *
-- *****************
primitiveType :: Location -> String -> Ast.Var
primitiveType loc name = Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named name loc)))

-- *************
-- *           *
-- * modifiers *
-- *           *
-- *************
modifiers :: Location -> [ Ast.Exp ] -> [ Ast.Exp ]
modifiers _loc annotations = annotations

-- **************
-- *            *
-- * annotation *
-- *            *
-- **************
annotation :: Location -> [ Ast.Exp ] -> Ast.Exp
annotation loc [] = Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named "" loc)))))
annotation loc (callee:args) = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = callee,
    Ast.args = args,
    Ast.expCallLocation = loc
}

-- *************
-- *           *
-- * expAssign *
-- *           *
-- *************
expAssign :: Location -> Ast.Var -> Ast.Exp -> Ast.Exp
expAssign loc lhs rhs = Ast.ExpAssign $ Ast.ExpAssignContent {
    Ast.expAssignLhs = lhs,
    Ast.expAssignRhs = rhs,
    Ast.expAssignLocation = loc
}

-- ***********
-- *         *
-- * expStr *
-- *         *
-- ***********
expStr :: Location -> String -> Ast.Exp
expStr loc value = Ast.ExpStr $ Ast.ExpStrContent (Token.ConstStr value loc)

-- ***********
-- *         *
-- * expInt *
-- *         *
-- ***********
expInt :: Location -> String -> Ast.Exp
expInt loc value = Ast.ExpInt $ Ast.ExpIntContent (Token.ConstInt (readInt value) loc)

readInt :: String -> Int
readInt s = case reads s of { [(n, "")] -> n; _ -> 0 }

-- ***********
-- *         *
-- * expNull *
-- *         *
-- ***********
expNull :: Location -> Ast.Exp
expNull loc = Ast.ExpNull $ Ast.ExpNullContent (Token.ConstNull loc)

-- *************
-- *           *
-- * expvarify *
-- *           *
-- *************
-- Lift a Token.Named into an Ast.Exp -- the four-layer wrapper used
-- whenever a rule needs a bare-name expression. Direct port of
-- TsParserActions' `expvarify`. Primary consumer here is
-- `instrumentationCall` below, which uses it to build the sentinel
-- callee `<dhscanner-instrumentation>[<tag>]` without repeating the
-- `Ast.ExpVar . Ast.ExpVarContent . Ast.VarSimple . ...` chain inline.
expvarify :: Token.Named -> Ast.Exp
expvarify = Ast.ExpVar . Ast.ExpVarContent . Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

-- ***********************
-- *                     *
-- * instrumentationCall *
-- *                     *
-- ***********************
-- Standard dhscanner instrumentation marker : lower a parser-synthetic
-- construct ( Java-side: `!x` -> `not` ; future: `throw`, cast,
-- `instanceof`, string-concat lowering, ... ) to an Ast.ExpCall whose
-- callee is the sentinel string `<dhscanner-instrumentation>[<tag>]`.
--
-- Same convention every other front has adopted ( see TsParserActions.hs's
-- `instrumentationCall` / `fallthroughReturnNullMarker` / `stmtThrow` /
-- `property` / `fstring` / `expTernary` / `jsxChoose` / `expTypeof` /
-- `expDelete` ). Codegen, kbgen, queryengine's `utils.pl`, and SARIF
-- post-processing all discover these markers by the `<tag>` naming
-- scheme, so matching the exact wire shape is important.
instrumentationCall :: String -> Location -> [Ast.Exp] -> Ast.Exp
instrumentationCall tag loc callArgs = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = expvarify (Token.Named ("<dhscanner-instrumentation>[" ++ tag ++ "]") loc),
    Ast.args = callArgs,
    Ast.expCallLocation = loc
}

-- ***********
-- *         *
-- * expUnop *
-- *         *
-- ***********
-- Lower a Java unary-operator envelope ( currently LOGICAL_COMPLEMENT
-- only -- grammar `not` sub-rule ; NEGATE / BITWISE_COMPLEMENT /
-- UNARY_PLUS / UNARY_MINUS can add sibling sub-rules to `unop` and
-- reuse this action by passing their own tag ) as an instrumentation
-- call tagged by the operator name. The operand is the single arg ;
-- the operator-envelope's own `loc` becomes the call location, which
-- matches how TsParserActions treats e.g. `stmtThrow` and `expTypeof`.
expUnop :: String -> Location -> Ast.Exp -> Ast.Exp
expUnop tag loc operand = instrumentationCall tag loc [operand]

-- ***********
-- *         *
-- * expBool *
-- *         *
-- ***********
-- javac emits { kind: BOOLEAN_LITERAL, loc, text: "true" | "false",
-- children: [] } -- same uniform literal envelope as INT_LITERAL /
-- STRING_LITERAL / NULL_LITERAL. The `text` field carries the surface
-- form of the literal ; we lower it to a Haskell Bool via equality
-- against "true" ( any other value, including the malformed case,
-- defaults to False -- this is defensive, since javac is guaranteed
-- to emit exactly one of the two keyword forms for a BooleanLiteral
-- tree node, so the string compare is total in practice ).
expBool :: Location -> String -> Ast.Exp
expBool loc value = Ast.ExpBool $ Ast.ExpBoolContent (Token.ConstBool (value == "true") loc)

-- ************
-- *          *
-- * expBinop *
-- *          *
-- ************
-- Lower a native binary-op envelope ( `{ kind: <OP_NAME>, loc,
-- left: exp, right: exp, children: [] }` -- the printer promotes the
-- two operands from positional `children` to named `left` / `right`
-- keys, symmetric with `IF`'s cond / body ) into Ast.ExpBinop. The
-- `Ast.Operator` enum is passed through verbatim from the grammar's
-- `binop_kind` sub-rule, which handles the per-kind-token mapping
-- ( `EQUAL_TO` → `Ast.EQ`, `PLUS` → `Ast.PLUS`, `MULTIPLY` →
-- `Ast.TIMES` for now ; the remaining `Ast.Operator` variants
-- `NEQ` / `MINUS` / `DIVIDE` / `PERCENT` can be wired up the first
-- time a file surfaces them -- same `binop_kind` sub-rule, one new
-- alternative per op ).
expBinop :: Location -> Ast.Exp -> Ast.Exp -> Ast.Operator -> Ast.Exp
expBinop loc lhs rhs op = Ast.ExpBinop $ Ast.ExpBinopContent {
    Ast.expBinopLeft = lhs,
    Ast.expBinopRight = rhs,
    Ast.expBinopOperator = op,
    Ast.expBinopLocation = loc
}

-- ***********
-- *         *
-- * expCall *
-- *         *
-- ***********
-- Lower a native METHOD_INVOCATION envelope ( `{ kind: METHOD_INVOCATION,
-- loc, children: [ callee, arg* ] }` ) directly to Ast.ExpCall. Grammar
-- uses `commalistof(exp)` for the children, which guarantees at least
-- one element -- the callee -- so the `callee:args` pattern is total
-- in practice ; we still handle the impossible empty-list case with a
-- defensive ExpNull fallback ( same defensive-stub pattern used by
-- `annotation` for the no-callee degenerate ) to silence -Wall. The
-- callee can be an IDENTIFIER ( bare call like `foo(x)` ) or a nested
-- MEMBER_SELECT chain ( qualified call like `a.b.c(x)` ) -- both land
-- as valid `Ast.Exp` values via the existing `exp_var` / `var` /
-- `field_var` rules, so no callee-type coercion is needed here.
expCall :: Location -> [ Ast.Exp ] -> Ast.Exp
expCall loc [] = Ast.ExpNull $ Ast.ExpNullContent (Token.ConstNull loc)
expCall loc (callee:args) = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = callee,
    Ast.args = args,
    Ast.expCallLocation = loc
}

-- *************
-- *           *
-- * stmtBlock *
-- *           *
-- *************
-- Lower a native BLOCK envelope ( `{ kind: BLOCK, loc, children: [ stmt* ] }` )
-- directly to Ast.StmtBlock. Same record shape used by TsParserActions'
-- stmtDecvar / stmtExport / stmtTypeAlias etc.
stmtBlock :: Location -> [ Ast.Stmt ] -> Ast.Stmt
stmtBlock loc stmts = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = stmts,
    Ast.stmtBlockLocation = loc
}

-- ***********
-- *         *
-- * stmtExp *
-- *         *
-- ***********
-- Lower a native EXPRESSION_STATEMENT envelope to Ast.StmtExp. The
-- dhscanner AST uses a direct wrapper ( no separate location field )
-- because the location already lives on the inner expression -- same
-- form every other parser uses ( see TsParserActions' stmtPackage,
-- fallthroughReturnNullMarker, stmtThrow and this file's own
-- stmtPackage below ).
stmtExp :: Ast.Exp -> Ast.Stmt
stmtExp = Ast.StmtExp

-- **************
-- *            *
-- * stmtReturn *
-- *            *
-- **************
-- Lower a native RETURN envelope ( `{ kind: RETURN, loc, children: [ exp ] }` )
-- directly to Ast.StmtReturn. Returned value is always `Just` for now --
-- the bare `return;` form would wrap `Nothing` once its grammar
-- alternative is added. Record shape is identical to what GoParser uses
-- ( `stmtReturnValue :: Maybe Ast.Exp`, `stmtReturnLocation :: Location` ).
stmtReturn :: Location -> Ast.Exp -> Ast.Stmt
stmtReturn loc value = Ast.StmtReturn $ Ast.StmtReturnContent {
    Ast.stmtReturnValue = Just value,
    Ast.stmtReturnLocation = loc
}

-- **********
-- *        *
-- * stmtIf *
-- *        *
-- **********
-- Lower a native IF envelope ( `{ kind: IF, loc, children: [ exp,
-- stmt_body, stmt_body? ] }` where children[0] is the condition
-- -- usually wrapped in a `PARENTHESIZED` envelope on the javac side,
-- unwrapped by `exp_parenthesized` -- children[1] is the `then`
-- statement, and the optional children[2] is the `else` statement )
-- into Ast.StmtIf. `stmtIfBody` / `stmtElseBody` are lists of stmts in
-- the dhscanner AST ( to accommodate parsers that unwrap a BLOCK into
-- its contents ); we wrap the `then` in a singleton list and pass the
-- `else` list straight through -- `[]` when absent, `[elseStmt]` when
-- present. Real smart constructor ( not a stub ) : the resulting
-- `Ast.StmtIfContent` is already dhscanner-canonical.
stmtIf :: Location -> Ast.Exp -> Ast.Stmt -> [ Ast.Stmt ] -> Ast.Stmt
stmtIf loc cond thenStmt elseStmts = Ast.StmtIf $ Ast.StmtIfContent {
    Ast.stmtIfCond = cond,
    Ast.stmtIfBody = [ thenStmt ],
    Ast.stmtElseBody = elseStmts,
    Ast.stmtIfLocation = loc
}
