module RbParserActions where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
-- `Ast` is imported qualified on purpose, like in `PyParserActions.hs` :
-- with `-Wall -Werror`, an unqualified import leaks every record-field
-- accessor of dhscanner.ast into scope, and a local binding that shares
-- one of those names fails the build on `-Wname-shadowing`.
import qualified Ast
import qualified Token
import Location

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import qualified Data.Map
import Data.Maybe ( catMaybes, fromMaybe )
import Text.Read ( readMaybe )

-- ******************************************************************
-- * The smart constructors behind `RbParser.y` : every production  *
-- * whose action builds dhscanner AST calls exactly one of these.  *
-- * Statement rules return `Maybe Ast.Stmt` ( `Nothing` = dropped  *
-- * by the lowering, eg comments, void statements ).               *
-- ******************************************************************

-- *********************
-- *                   *
-- * legacy lowerings  *
-- *                   *
-- *********************
-- The Ruby grammar predates the `<dhscanner-instrumentation>[<tag>]`
-- convention ( `TsParserActions.instrumentationCall` ). These keep the
-- exact output the grammar had when the actions moved here, so the move
-- itself is behavior-preserving. TODO : migrate both to the convention.

-- | a call to a bare magic name : `listify` ( arrays, `%w[]` ), `fstring`
-- ( string literals ), `expify` ( bare `args` nodes ).
legacyNamedCall :: String -> Location -> [Ast.Exp] -> Ast.Exp
legacyNamedCall name loc callArgs = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = expvarify (Token.Named name loc),
    Ast.args = callArgs,
    Ast.expCallLocation = loc
}

-- | an integer constant standing in for a construct the lowering does not
-- model yet. The values are grep-able markers, not semantics :
--
--   * 222  regexp literal
--   * 333  backref / global var / dynamic symbol
--   * 555  yield / paren
--   * 888  an `if` whose predicate is an assignment, an embedded string
--          expression that is not a single expression statement
--   * 999  module ( the `if` wrapping its body )
--   * 9999 hash / bare hash
--   * 1    begin ( the `if` wrapping its body )
placeholderInt :: Int -> Location -> Ast.Exp
placeholderInt value loc = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt {
    Token.constIntValue = value,
    Token.constIntLocation = loc
}

-- **********************
-- *                    *
-- * names and tokens   *
-- *                    *
-- **********************
mkLocation :: FilePath -> Int -> Int -> Int -> Int -> Location
mkLocation fp ls cs le ce = Location {
    Location.filename = fp,
    lineStart = fromIntegral ls,
    colStart = fromIntegral cs,
    lineEnd = fromIntegral le,
    colEnd = fromIntegral ce
}

named :: String -> Location -> Token.Named
named = Token.Named

unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

varme :: Token.Named -> Ast.Var
varme = Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

expvarify :: Token.Named -> Ast.Exp
expvarify = Ast.ExpVar . Ast.ExpVarContent . varme

-- | `superclass` / `module` names : a `Foo::Bar` constant path keeps its
-- parent when the parent is a bare name ( the old behavior ).
identifierWrapper :: Maybe Ast.Exp -> Token.Named -> Token.Named
identifierWrapper (Just (Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent v))))) _ = Token.getVarNameToken v
identifierWrapper _ name = name

-- ********
-- *      *
-- * root *
-- *      *
-- ********
-- direct dhscanner subtree creation: Ast.Root
root :: FilePath -> [Maybe Ast.Stmt] -> Ast.Root
root fp body = Ast.Root {
    Ast.filename = fp,
    Ast.stmts = catMaybes body
}

-- *************
-- *           *
-- * variables *
-- *           *
-- *************
-- direct dhscanner subtree creation: Ast.VarSimple
varSimple :: Token.Named -> Ast.Var
varSimple = varme

-- direct dhscanner subtree creation: Ast.VarField
varField :: Location -> Ast.Exp -> Token.Named -> Ast.Var
varField loc lhs name = Ast.VarField $ Ast.VarFieldContent {
    Ast.varFieldLhs = lhs,
    Ast.varFieldName = Token.FieldName name,
    Ast.varFieldLocation = loc
}

-- | `parent.name` where the parent is itself a bare name ( `var_field` ).
varFieldOfName :: Location -> Token.Named -> Token.Named -> Ast.Var
varFieldOfName loc parentName = varField loc (expvarify parentName)

-- | `Foo::Bar` / `::Bar` : without a parent, the constant is its own lhs.
varParented :: Location -> Maybe Ast.Exp -> Token.Named -> Ast.Var
varParented loc (Just parentExp) name = varField loc parentExp name
varParented loc Nothing name = varField loc (expvarify name) name

-- direct dhscanner subtree creation: Ast.VarSubscript
varSubscript :: Location -> Ast.Exp -> Ast.Exp -> Ast.Var
varSubscript loc lhs idx = Ast.VarSubscript $ Ast.VarSubscriptContent {
    Ast.varSubscriptLhs = lhs,
    Ast.varSubscriptIdx = idx,
    Ast.varSubscriptLocation = loc
}

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************
-- direct dhscanner subtree creation: Ast.ExpVar
expVar :: Ast.Var -> Ast.Exp
expVar = Ast.ExpVar . Ast.ExpVarContent

-- direct dhscanner subtree creation: Ast.ExpBinop
expBinop :: Location -> Ast.Exp -> Ast.Operator -> Ast.Exp -> Ast.Exp
expBinop loc lhs op rhs = Ast.ExpBinop $ Ast.ExpBinopContent {
    Ast.expBinopLeft = lhs,
    Ast.expBinopRight = rhs,
    Ast.expBinopOperator = op,
    Ast.expBinopLocation = loc
}

-- instrumented as dhscanner Ast.ExpBinop ( the operand on both sides )
expUnop :: Location -> Ast.Operator -> Ast.Exp -> Ast.Exp
expUnop loc op operand = expBinop loc operand op operand

-- instrumented as dhscanner Ast.ExpBinop ( `+` )
strConcat :: Location -> Ast.Exp -> Ast.Exp -> Ast.Exp
strConcat loc lhs rhs = expBinop loc lhs Ast.PLUS rhs

-- direct dhscanner subtree creation: Ast.ExpStr
expStr :: Location -> String -> Ast.Exp
expStr loc value = Ast.ExpStr $ Ast.ExpStrContent $ Token.ConstStr {
    Token.constStrValue = value,
    Token.constStrLocation = loc
}

-- | `:sym` : the symbol's name as a string.
expSymbol :: Location -> Token.Named -> Ast.Exp
expSymbol loc name = expStr loc (Token.content name)

-- | `"#{...}"` : the embedded expression when it is a single expression
-- statement, a placeholder otherwise.
stringEmbexpr :: Location -> [Maybe Ast.Stmt] -> Ast.Exp
stringEmbexpr _ (Just (Ast.StmtExp e) : _) = e
stringEmbexpr loc _ = placeholderInt 888 loc

-- direct dhscanner subtree creation: Ast.ExpInt
-- Ruby digit separators ( `100_000` ) are dropped ; a literal Haskell can't
-- read ( `0b101`, `0o17`, `1r` ) becomes 0 instead of crashing the request
-- ( `read` threw, and the parsers service answered http 500 ).
expInt :: Location -> String -> Ast.Exp
expInt loc quoted = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt {
    Token.constIntValue = fromMaybe 0 (readMaybe (filter (/= '_') (unquote quoted))),
    Token.constIntLocation = loc
}

-- direct dhscanner subtree creation: Ast.ExpNull
expNull :: Location -> Ast.Exp
expNull loc = Ast.ExpNull $ Ast.ExpNullContent $ Token.ConstNull loc

-- | `recv.name` without arguments : a field access.
fieldAccess :: Location -> Ast.Exp -> Token.Named -> Ast.Exp
fieldAccess loc rcvr name = expVar (varField loc rcvr name)

-- direct dhscanner subtree creation: Ast.ExpCall
-- `recv.name(args)` : the callee field spans from the receiver to the name.
methodCall :: Location -> Ast.Exp -> Token.Named -> [Ast.Exp] -> Ast.Exp
methodCall loc rcvr name callArgs = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = expVar (varField calleeLoc rcvr name),
    Ast.args = callArgs,
    Ast.expCallLocation = loc
} where
    calleeLoc = Location {
        Location.filename = Location.filename loc,
        lineStart = Location.lineStart loc,
        colStart = Location.colStart loc,
        lineEnd = Location.lineEnd (Token.location name),
        colEnd = Location.colEnd (Token.location name)
    }

-- direct dhscanner subtree creation: Ast.ExpCall
-- `name(args)` without a receiver.
call :: Location -> Token.Named -> [Ast.Exp] -> Ast.Exp
call loc name callArgs = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = expvarify name,
    Ast.args = callArgs,
    Ast.expCallLocation = loc
}

-- direct dhscanner subtree creation: Ast.ExpCall
-- Ruby `super(args)` lowered to a call named "super".
superCall :: Location -> [Ast.Exp] -> Ast.Exp
superCall loc callArgs = call loc (Token.Named "super" loc) callArgs

-- **************
-- *            *
-- * statements *
-- *            *
-- **************
-- direct dhscanner subtree creation: Ast.StmtExp
stmtExp :: Ast.Exp -> Maybe Ast.Stmt
stmtExp = Just . Ast.StmtExp

-- direct dhscanner subtree creation: Ast.StmtAssign
stmtAssign :: Ast.Var -> Ast.Exp -> Maybe Ast.Stmt
stmtAssign lhs rhs = Just $ Ast.StmtAssign $ Ast.StmtAssignContent {
    Ast.stmtAssignLhs = lhs,
    Ast.stmtAssignRhs = rhs
}

-- | `x op= e` -> `x = x + e` ( every operator lowers to `+` ).
stmtOpAssign :: Location -> Ast.Var -> Ast.Exp -> Maybe Ast.Stmt
stmtOpAssign loc lhs rhs = stmtAssign lhs (expBinop loc (expVar lhs) Ast.PLUS rhs)

-- instrumented as dhscanner Ast.ExpBinop ( expression form of opassign )
expOpAssign :: Location -> Ast.Var -> Ast.Exp -> Ast.Exp
expOpAssign loc lhs rhs = expBinop loc (expVar lhs) Ast.PLUS rhs

-- direct dhscanner subtree creation: Ast.StmtIf
-- the else part is not kept yet.
stmtIf :: Location -> Ast.Exp -> [Maybe Ast.Stmt] -> Maybe Ast.Stmt
stmtIf loc cond body = Just $ Ast.StmtIf $ Ast.StmtIfContent {
    Ast.stmtIfCond = cond,
    Ast.stmtIfBody = catMaybes body,
    Ast.stmtElseBody = [],
    Ast.stmtIfLocation = loc
}

-- | `if x = e` : the assignment predicate is not kept yet.
stmtIfAssign :: Location -> [Maybe Ast.Stmt] -> Maybe Ast.Stmt
stmtIfAssign loc = stmtIf loc (placeholderInt 888 loc)

-- instrumented as dhscanner Ast.StmtIf ( always-true marker condition )
stmtBegin :: Location -> [Maybe Ast.Stmt] -> Maybe Ast.Stmt
stmtBegin loc = stmtIf loc (placeholderInt 1 loc)

-- instrumented as dhscanner Ast.StmtIf : the module body, with its classes /
-- functions renamed under the module name.
stmtModule :: Location -> Token.Named -> [Maybe Ast.Stmt] -> Maybe Ast.Stmt
stmtModule loc moduleName body = Just $ Ast.StmtIf $ Ast.StmtIfContent {
    Ast.stmtIfCond = placeholderInt 999 loc,
    Ast.stmtIfBody = modulize moduleName (catMaybes body),
    Ast.stmtElseBody = [],
    Ast.stmtIfLocation = loc
}

-- direct dhscanner subtree creation: Ast.ExpLambda
-- params and body are not kept yet.
-- direct dhscanner subtree creation: Ast.ExpLambda
expLambda :: Location -> Ast.Exp
expLambda loc = Ast.ExpLambda $ Ast.ExpLambdaContent {
    Ast.expLambdaParams = [],
    Ast.expLambdaBody = [],
    Ast.expLambdaLocation = loc
}

stmtLambda :: Location -> Maybe Ast.Stmt
stmtLambda loc = Just $ Ast.StmtExp $ Ast.ExpLambda $ Ast.ExpLambdaContent {
    Ast.expLambdaParams = [],
    Ast.expLambdaBody = [],
    Ast.expLambdaLocation = loc
}

-- direct dhscanner subtree creation: Ast.StmtFunc
stmtFunc :: Location -> Token.Named -> [Ast.Param] -> [Maybe Ast.Stmt] -> Maybe Ast.Stmt
stmtFunc loc name funcParams body = Just $ Ast.StmtFunc $ Ast.StmtFuncContent {
    Ast.stmtFuncReturnType = Just (varme (Token.Named "any" loc)),
    Ast.stmtFuncName = Token.FuncName name,
    Ast.stmtFuncParams = funcParams,
    Ast.stmtFuncBody = catMaybes body,
    Ast.stmtFuncLocation = loc,
    Ast.stmtFuncAnnotations = []
}

-- direct dhscanner subtree creation: Ast.StmtClass
-- the `def`s of the body become its methods.
stmtClass :: Ast.Var -> Maybe Token.Named -> [Maybe Ast.Stmt] -> Maybe Ast.Stmt
stmtClass name super body = Just $ Ast.StmtClass $ Ast.StmtClassContent {
    Ast.stmtClassName = className,
    Ast.stmtClassSupers = maybe [] ((: []) . Token.SuperName) super,
    Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
    Ast.stmtClassMethods = Ast.Methods methods
} where
    className = Token.ClassName (nameify name)
    funcs = visibilityOf Nothing (catMaybes body)
    methods = Data.Map.fromList (methodify className super funcs)

-- | the `def`s of a class body, in order. A bare `private` / `protected`
-- ( a `vcall` ) makes every later `def` carry the annotation
-- `<dhscanner-instrumentation>[private]` ( `[protected]` ) at its location,
-- until a bare `public` ( runner #138 : a private helper of a Rails
-- controller is not an action ).
visibilityOf :: Maybe Ast.Exp -> [Ast.Stmt] -> [Ast.StmtFuncContent]
visibilityOf _ [] = []
visibilityOf current (Ast.StmtFunc f : rest) = annotate current f : visibilityOf current rest
visibilityOf current (stmt : rest) = case visibilityMarker stmt of
    Just (Token.Named "public" _) -> visibilityOf Nothing rest
    Just (Token.Named v loc) -> visibilityOf (Just (instrumentationCall v loc [])) rest
    Nothing -> visibilityOf current rest

visibilityMarker :: Ast.Stmt -> Maybe Token.Named
visibilityMarker (Ast.StmtExp (Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v))))))
    | Token.content v `elem` [ "private", "protected", "public" ] = Just v
visibilityMarker _ = Nothing

annotate :: Maybe Ast.Exp -> Ast.StmtFuncContent -> Ast.StmtFuncContent
annotate Nothing f = f
annotate (Just a) f = f { Ast.stmtFuncAnnotations = a : Ast.stmtFuncAnnotations f }

-- | the standard instrumented call : callee `<dhscanner-instrumentation>[<tag>]`
-- ( the same shape as `TsParserActions.instrumentationCall` ).
instrumentationCall :: String -> Location -> [Ast.Exp] -> Ast.Exp
instrumentationCall tag loc callArgs = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = expvarify (Token.Named ("<dhscanner-instrumentation>[" ++ tag ++ "]") loc),
    Ast.args = callArgs,
    Ast.expCallLocation = loc
}

-- **********
-- *        *
-- * params *
-- *        *
-- **********
-- | requireds ++ optionals ( keyword / rest / block params are not kept yet ).
params :: Maybe [Token.Named] -> Maybe [Token.Named] -> [Ast.Param]
params requireds optionals = map paramify (concat (catMaybes [ requireds, optionals ]))

paramify :: Token.Named -> Ast.Param
paramify token = let
    paramName = Token.ParamName token
    nominalType = Just (varme (Token.Named "any" (Token.location token)))
    in Ast.Param paramName nominalType 156 []

-- | pick the first identifier from an mlhs_paren destructuring; fallback to a
-- placeholder when empty
mlhsParenFirst :: Location -> [Token.Named] -> Token.Named
mlhsParenFirst loc names = case names of
  (n:_) -> n
  _     -> Token.Named "any" loc

-- ***********
-- *         *
-- * helpers *
-- *         *
-- ***********
nameify :: Ast.Var -> Token.Named
nameify (Ast.VarSimple    v) = nameifyVarSimple    v
nameify (Ast.VarField     v) = nameifyVarField     v
nameify (Ast.VarSubscript v) = nameifyVarSubscript v

nameifyVarSimple :: Ast.VarSimpleContent -> Token.Named
nameifyVarSimple = Token.getVarNameToken . Ast.varName

nameifyVarField :: Ast.VarFieldContent -> Token.Named
nameifyVarField = Token.getFieldNameToken . Ast.varFieldName

nameifyVarSubscript :: Ast.VarSubscriptContent -> Token.Named
nameifyVarSubscript v = Token.Named "popo" (Ast.varSubscriptLocation v)

modulize :: Token.Named -> [ Ast.Stmt ] -> [ Ast.Stmt ]
modulize moduleName = map (modulize' moduleName)

modulize' :: Token.Named -> Ast.Stmt -> Ast.Stmt
modulize' moduleName (Ast.StmtClass c) = modulizeStmtClass c moduleName
modulize' moduleName (Ast.StmtFunc  f) = modulizeStmtFunc  f moduleName
modulize' moduleName (Ast.StmtIf    s) = modulizeStmtIf    s moduleName
modulize' _ stmt = stmt

modulizeStmtIf :: Ast.StmtIfContent -> Token.Named -> Ast.Stmt
modulizeStmtIf s moduleName = Ast.StmtIf (s { Ast.stmtIfBody = modulize moduleName (Ast.stmtIfBody s) })

fixHostingClass :: String -> Ast.StmtMethodContent -> Ast.StmtMethodContent
fixHostingClass moduleName method = let
    c = Token.getClassNameToken (Ast.hostingClassName method)
    hostingClassName' = c { Token.content = moduleName ++ "." ++ (Token.content c) }
    in method { Ast.hostingClassName = Token.ClassName hostingClassName' }

modulizeStmtClass :: Ast.StmtClassContent -> Token.Named -> Ast.Stmt
modulizeStmtClass c (Token.Named moduleName _) = let
    methods = Data.Map.map (fixHostingClass moduleName) (Ast.actualMethods (Ast.stmtClassMethods c))
    in Ast.StmtClass $ c { Ast.stmtClassMethods = Ast.Methods methods }

modulizeStmtFunc :: Ast.StmtFuncContent -> Token.Named -> Ast.Stmt
modulizeStmtFunc f (Token.Named moduleName _) = let
    token = Token.getFuncNameToken (Ast.stmtFuncName f)
    location' = Token.location token
    name' = Token.content token
    name'' = moduleName ++ "." ++ name'
    f' = Token.Named name'' location'
    in Ast.StmtFunc $ f { Ast.stmtFuncName = Token.FuncName f' }

methodify :: Token.ClassName -> Maybe Token.Named -> [ Ast.StmtFuncContent ] -> [(Token.MethodName, Ast.StmtMethodContent)]
methodify c super = map (methodify' c (Token.SuperName <$> super))

methodify' :: Token.ClassName -> Maybe Token.SuperName -> Ast.StmtFuncContent -> (Token.MethodName, Ast.StmtMethodContent)
methodify' c s f = let m = Token.MethodName $ Token.getFuncNameToken (Ast.stmtFuncName f) in (m, Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = (Ast.stmtFuncReturnType f),
    Ast.stmtMethodName = m,
    Ast.stmtMethodParams = (Ast.stmtFuncParams f),
    Ast.stmtMethodBody = (Ast.stmtFuncBody f),
    Ast.stmtMethodAnnotations = (Ast.stmtFuncAnnotations f),
    Ast.stmtMethodLocation = (Ast.stmtFuncLocation f),
    Ast.hostingClassName = c,
    Ast.hostingClassSupers = maybe [] (: []) s,
    Ast.hostingClassAnnotations = []
})
