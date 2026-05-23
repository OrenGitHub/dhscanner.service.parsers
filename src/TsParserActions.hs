module TsParserActions where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
-- NOTE: `Ast` is imported qualified on purpose. With `-Wall -Werror` (see
-- dhscanner.cabal), `-Wname-shadowing` is fatal, so an unqualified
-- `import Ast` would leak every record-field accessor from dhscanner.ast
-- (`callee`, `args`, `filename`, `location`, `stmtBlockContent`, ...) into
-- scope as a bare identifier. A helper parameter or `let`/`where` binding
-- that happens to share one of those names would then fail the build. By
-- importing qualified, the only way to name an Ast field is `Ast.<field>`,
-- which means local bindings can use any short names freely.
import qualified Ast
import Location
import qualified Token
import qualified Common

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.Maybe ( fromMaybe, catMaybes, mapMaybe )
import Data.List ( map, stripPrefix, isPrefixOf )
import qualified Data.Map

-- ********
-- *      *
-- * root *
-- *      *
-- ********
root :: [Ast.Stmt] -> Ast.Root
root rootStmts = Ast.Root
    {
        Ast.filename = "",
        Ast.stmts = rootStmts
    }

-- ***********
-- *         *
-- * stmt if *
-- *         *
-- ***********
stmtIf :: Location -> Ast.Exp -> [Ast.Stmt] -> Maybe [Ast.Stmt] -> Ast.Stmt
stmtIf loc cond body elsePart = Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = cond,
        Ast.stmtIfBody = body,
        Ast.stmtElseBody = fromMaybe [] elsePart,
        Ast.stmtIfLocation = loc
    }

-- ************
-- *          *
-- * stmt try *
-- *          *
-- ************
stmtTry :: Location -> [Ast.Stmt] -> [Ast.Stmt] -> Ast.Stmt
stmtTry loc tryBody catchBody = Ast.StmtTry $ Ast.StmtTryContent
    {
        Ast.stmtTryPart = tryBody,
        Ast.stmtCatchPart = catchBody,
        Ast.stmtTryLocation = loc
    }

-- ************
-- *          *
-- * exp call *
-- *          *
-- ************
expCall :: Location -> Ast.Exp -> [Ast.Exp] -> Ast.Exp
expCall loc funcExp callArgs = Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = funcExp,
        Ast.args = callArgs,
        Ast.expCallLocation = loc
    }

-- *****************
-- *               *
-- * stmt function *
-- *               *
-- *****************
stmtFunc :: Location -> Token.Named -> [Ast.Param] -> Maybe [Ast.Stmt] -> Ast.Stmt
stmtFunc loc fname params body = Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Just (varify (Token.Named "any" loc)),
        Ast.stmtFuncName = Token.FuncName fname,
        Ast.stmtFuncParams = params,
        Ast.stmtFuncBody = fromMaybe [] body,
        Ast.stmtFuncAnnotations = [],
        Ast.stmtFuncLocation = loc
    }

-- ********************
-- *                  *
-- * parameter chunk *
-- *                  *
-- ********************
-- Lowering for the trivial Parameter(Identifier ..., type_hint?, default?)
-- shape used by `parameterChunk_1` in TsParser.y. Returns a singleton
-- [Ast.Param] so the surrounding `parameters` rule can `concat` per-slot
-- lists from a comma-separated `parameterChunk` sequence.
--
-- The doubly-wrapped `Maybe (Maybe Token.Named)` reflects the grammar
-- exactly: outer `Maybe` is `optional(type_hint)`, inner `Maybe` is
-- `type`'s own `Maybe Token.Named` (most type alternatives carry no name
-- and return `Nothing`). Only `Just (Just t)` has a usable nominal type.
parameterChunk1 :: Token.Named -> Maybe (Maybe Token.Named) -> [Ast.Param]
parameterChunk1 name maybeTypeHint = [Ast.Param
    {
        Ast.paramName = Token.ParamName name,
        Ast.paramNominalType = case maybeTypeHint of { Just (Just t) -> Just (varify t); _ -> Nothing },
        Ast.paramSerialIdx = 156
    }]

-- ***************
-- *             *
-- * stmt decvar *
-- *             *
-- ***************
varify :: Token.Named -> Ast.Var
varify = Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

-- Lifts varify all the way to Ast.Exp. Use this whenever a rule needs a
-- bare-name expression -- e.g. an instrumented callee like
-- `<dhscanner-instrumentation>[kv]` (see `instrumentationCall`) -- instead
-- of repeating the four-layer wrapper inline.
expvarify :: Token.Named -> Ast.Exp
expvarify = Ast.ExpVar . Ast.ExpVarContent . varify

assignify :: [Ast.Var] -> Ast.Exp -> [Ast.Stmt]
assignify vars e = Data.List.map (\v -> Ast.StmtAssign (Ast.StmtAssignContent v e)) vars

normalizeVardec :: Token.VarName -> Ast.Exp -> Ast.Stmt
normalizeVardec v (Ast.ExpLambda lambda) = Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Just (varify (Token.Named "any" (Token.getVarNameLocation v))),
        Ast.stmtFuncName = Token.FuncName (Token.getVarNameToken v),
        Ast.stmtFuncParams = Ast.expLambdaParams lambda,
        Ast.stmtFuncBody = Ast.expLambdaBody lambda,
        Ast.stmtFuncAnnotations = [],
        Ast.stmtFuncLocation = Ast.expLambdaLocation lambda
    }
normalizeVardec v initValue = Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = v,
        Ast.stmtVardecNominalType = Just (varify (Token.Named "any" (Token.getVarNameLocation v))),
        Ast.stmtVardecInitValue = Just initValue,
        Ast.stmtVardecLocation = Token.getVarNameLocation v
    }

-- Consolidated `stmtDecvar` — the init expression is now optional
-- (`optional(decvarInit)` in the grammar), so the grammar's two old
-- alternatives are a single production. Dispatch internally:
--   single-name LHS + init  -> normalizeVardec (Ast.StmtFunc if init is a
--                              lambda, otherwise Ast.StmtVardec)
--   single-name LHS, no init -> Ast.StmtVardec with init=Nothing
--   multi-name LHS  + init   -> Ast.StmtBlock of per-name Ast.StmtAssigns
--   multi-name LHS, no init  -> empty Ast.StmtBlock (the names are declared
--                               at the destructure level but we have no
--                               initializer to thread into them)
stmtDecvar :: Location -> [Ast.Var] -> Maybe Ast.Exp -> Ast.Stmt
stmtDecvar _   [Ast.VarSimple (Ast.VarSimpleContent v)] (Just initExp) = normalizeVardec v initExp
stmtDecvar loc vars                                     (Just initExp) = Ast.StmtBlock $ Ast.StmtBlockContent (assignify vars initExp) loc
stmtDecvar _   [Ast.VarSimple (Ast.VarSimpleContent v)] Nothing        = Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = v,
        Ast.stmtVardecNominalType = Just (varify (Token.Named "any" (Token.getVarNameLocation v))),
        Ast.stmtVardecInitValue = Nothing,
        Ast.stmtVardecLocation = Token.getVarNameLocation v
    }
stmtDecvar loc _                                        Nothing        = Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = [],
        Ast.stmtBlockLocation = loc
    }

-- Aggregate lowering for comma-separated VariableDeclaration items inside a
-- single VariableDeclarationList. Preserves prior single-declaration shape
-- and produces an Ast.StmtBlock for multiple declarations.
stmtDecvarList :: Location -> [([Ast.Var], Maybe Ast.Exp)] -> Ast.Stmt
stmtDecvarList loc us = case us of
    [u] -> buildStmtFromUnit loc u
    _   -> Ast.StmtBlock $ Ast.StmtBlockContent
        {
            Ast.stmtBlockContent = flattenStmts (mapUnits loc us),
            Ast.stmtBlockLocation = loc
        }

buildStmtFromUnit :: Location -> ([Ast.Var], Maybe Ast.Exp) -> Ast.Stmt
buildStmtFromUnit loc unit = case unit of
    (vars, maybeInit) -> stmtDecvar loc vars maybeInit

mapUnits :: Location -> [([Ast.Var], Maybe Ast.Exp)] -> [Ast.Stmt]
mapUnits loc us = Data.List.map (buildStmtFromUnit loc) us

toListFromStmt :: Ast.Stmt -> [Ast.Stmt]
toListFromStmt s = case s of
    Ast.StmtBlock b -> Ast.stmtBlockContent b
    _               -> [s]

flattenStmts :: [Ast.Stmt] -> [Ast.Stmt]
flattenStmts stmtsTag = concat (Data.List.map toListFromStmt stmtsTag)

-- *************
-- *           *
-- * exp binop *
-- *           *
-- *************
expBinop :: Location -> Ast.Exp -> Ast.Exp -> Ast.Exp
expBinop loc left right = Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = left,
        Ast.expBinopRight = right,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = loc
    }

-- ************
-- *          *
-- * exp bool *
-- *          *
-- ************
expBool :: Bool -> Location -> Ast.Exp
expBool value loc = Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool
    {
        Token.constBoolValue = value,
        Token.constBoolLocation = loc
    }

-- ***************
-- *             *
-- * stmt return *
-- *             *
-- ***************
stmtReturn :: Location -> Maybe Ast.Exp -> Ast.Stmt
stmtReturn loc value = Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = value,
        Ast.stmtReturnLocation = loc
    }

-- **************
-- *            *
-- * stmt throw *
-- *            *
-- **************
stmtThrow :: Location -> Ast.Exp -> Ast.Stmt
stmtThrow loc thrownExp = Ast.StmtExp $ instrumentationCall "throw" loc [thrownExp]

-- **************
-- *            *
-- * stmt break *
-- *            *
-- **************
stmtBreak :: Location -> Ast.Stmt
stmtBreak loc = Ast.StmtBreak $ Ast.StmtBreakContent { Ast.stmtBreakLocation = loc }

-- ***************
-- *             *
-- * stmt continue *
-- *             *
-- ***************
stmtContinue :: Location -> Ast.Stmt
stmtContinue loc = Ast.StmtContinue $ Ast.StmtContinueContent { Ast.stmtContinueLocation = loc }

-- **************
-- *            *
-- * stmt while *
-- *            *
-- **************
stmtWhile :: Location -> Ast.Exp -> [Ast.Stmt] -> Ast.Stmt
stmtWhile loc cond body = Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = cond,
        Ast.stmtWhileBody = body,
        Ast.stmtWhileLocation = loc
    }

-- **************
-- *            *
-- * stmt class *
-- *            *
-- **************
stmtClass :: Token.Named -> Maybe [Maybe Token.Named] -> [Maybe Ast.Stmt] -> Ast.Stmt
stmtClass name maybeSupers bodyStmts = Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName name,
        Ast.stmtClassSupers = collectSupers maybeSupers,
        Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
        Ast.stmtClassMethods = stmtsToMethods bodyStmts
    }

stmtsToMethods :: [Maybe Ast.Stmt] -> Ast.Methods
stmtsToMethods maybeStmts = Ast.Methods $ Data.Map.fromList
    (Data.List.map methodKV (mapMaybe extractMethod (catMaybes maybeStmts)))

extractMethod :: Ast.Stmt -> Maybe Ast.StmtMethodContent
extractMethod stmt = case stmt of
    Ast.StmtMethod m -> Just m
    _                -> Nothing

methodKV :: Ast.StmtMethodContent -> (Token.MethodName, Ast.StmtMethodContent)
methodKV m = (Ast.stmtMethodName m, m)

collectSupers :: Maybe [Maybe Token.Named] -> [Token.SuperName]
collectSupers maybeTs = case maybeTs of
    Nothing -> []
    Just ts -> Data.List.map Token.SuperName (catMaybes ts)

-- ***************
-- *             *
-- * stmt method *
-- *             *
-- ***************
-- `hostingClassName` is a placeholder here -- methods are parsed bottom-up
-- so the enclosing class's name isn't known at this point. The natural
-- follow-up is for `Actions.stmtClass` to walk its body list and inject the
-- real class name into each `Ast.StmtMethod`.
stmtMethod :: Location -> Token.Named -> [Ast.Param] -> Maybe (Maybe Token.Named) -> Maybe [Ast.Stmt] -> Maybe Ast.Stmt
stmtMethod loc name params maybeReturnType maybeBody = Just $ Ast.StmtMethod $ Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = methodReturnType loc maybeReturnType,
        Ast.stmtMethodName = Token.MethodName name,
        Ast.stmtMethodParams = params,
        Ast.stmtMethodBody = fromMaybe [] maybeBody,
        Ast.stmtMethodLocation = loc,
        Ast.hostingClassName = Token.ClassName (Token.Named "" loc),
        Ast.hostingClassSupers = []
    }

methodReturnType :: Location -> Maybe (Maybe Token.Named) -> Maybe Ast.Var
methodReturnType loc maybeReturnType = case maybeReturnType of
    Nothing       -> Just (varify (Token.Named "any" loc))
    Just Nothing  -> Just (varify (Token.Named "any" loc))
    Just (Just t) -> Just (varify t)

stmtConstructor :: Location -> [Ast.Param] -> Maybe [Ast.Stmt] -> Maybe Ast.Stmt
stmtConstructor loc params maybeBody = stmtMethod loc (Token.Named "constructor" loc) params Nothing maybeBody

-- *************
-- *           *
-- * stmt enum *
-- *           *
-- *************
-- A TS `enum Foo { A, B, C }` has no direct dhscanner counterpart, so we
-- model it as an `Ast.StmtClass` whose data members are the enum cases and
-- whose methods set is empty. Init values are dropped for now; can be wired
-- through `enumInitializer` later.
enumDataMember :: Token.Named -> Ast.DataMember
enumDataMember name = Ast.DataMember
    {
        Ast.dataMemberName = Token.MemberName name,
        Ast.dataMemberNominalType = Nothing,
        Ast.dataMemberInitValue = Nothing
    }

stmtEnum :: Token.Named -> [Ast.DataMember] -> Ast.Stmt
stmtEnum name members = Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName name,
        Ast.stmtClassSupers = [],
        Ast.stmtClassDataMembers = Ast.DataMembers $ Data.Map.fromList
            [ (Ast.dataMemberName m, m) | m <- members ],
        Ast.stmtClassMethods = Ast.Methods Data.Map.empty
    }

-- ******************
-- *                *
-- * exp arrow func *
-- *                *
-- ******************
expArrowFunction :: Location -> [Ast.Param] -> [Ast.Stmt] -> Ast.Exp
expArrowFunction loc params body = Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        Ast.expLambdaParams = params,
        Ast.expLambdaBody = body,
        Ast.expLambdaLocation = loc
    }

-- ******************
-- *                *
-- * exp func expr  *
-- *                *
-- ******************
expFunctionExpression :: Location -> [Ast.Param] -> [Ast.Stmt] -> Ast.Exp
expFunctionExpression loc params body = Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        Ast.expLambdaParams = params,
        Ast.expLambdaBody = body,
        Ast.expLambdaLocation = loc
    }

-- ***************
-- *             *
-- * stmt assign *
-- *             *
-- ***************
stmtAssign :: Ast.Var -> Ast.Exp -> Ast.Stmt
stmtAssign lhs rhs = Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = lhs,
        Ast.stmtAssignRhs = rhs
    }

-- ************
-- *          *
-- * exp null *
-- *          *
-- ************
expNull :: Location -> Ast.Exp
expNull loc = Ast.ExpNull $ Ast.ExpNullContent
    {
        Ast.expNullValue = Token.ConstNull
            {
                Token.constNullLocation = loc
            }
    }

-- *************
-- *           *
-- * var field *
-- *           *
-- *************
varField :: Location -> Ast.Exp -> Token.Named -> Ast.Var
varField loc lhs name = Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = lhs,
        Ast.varFieldName = Token.FieldName name,
        Ast.varFieldLocation = loc
    }

-- *****************
-- *               *
-- * var subscript *
-- *               *
-- *****************
varSubscript :: Location -> Ast.Exp -> Ast.Exp -> Ast.Var
varSubscript loc lhs idx = Ast.VarSubscript $ Ast.VarSubscriptContent
    {
        Ast.varSubscriptLhs = lhs,
        Ast.varSubscriptIdx = idx,
        Ast.varSubscriptLocation = loc
    }

-- **********************
-- *                    *
-- * instrumentation    *
-- * (shared lowering   *
-- *  for native nodes  *
-- *  that have no 1:1  *
-- *  dhscanner shape)  *
-- *                    *
-- **********************
instrumentationCall :: String -> Location -> [Ast.Exp] -> Ast.Exp
instrumentationCall tag loc callArgs = Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = expvarify (Token.Named ("<dhscanner-instrumentation>[" ++ tag ++ "]") loc),
        Ast.args = callArgs,
        Ast.expCallLocation = loc
    }

-- **************
-- *            *
-- * exp delete *
-- *            *
-- **************
expDelete :: Location -> Ast.Exp -> Ast.Exp
expDelete loc _operand = instrumentationCall "delete" loc []

-- ***********
-- *         *
-- * exp new *
-- *         *
-- ***********
expNew :: Location -> Maybe Token.Named -> Maybe [Ast.Exp] -> Ast.Exp
expNew loc maybeType maybeArgs = Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
            {
                Token.content = case maybeType of { Just t -> Token.content t; _ -> "nondet" },
                Token.location = loc
            },
        Ast.args = fromMaybe [] maybeArgs,
        Ast.expCallLocation = loc
    }

-- NewExpression with an expression/var callee (e.g., new google.auth.OAuth2(...))
expNewCalleeVar :: Location -> Ast.Var -> Maybe [Ast.Exp] -> Ast.Exp
expNewCalleeVar loc v maybeArgs = Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent v,
        Ast.args = fromMaybe [] maybeArgs,
        Ast.expCallLocation = loc
    }

-- **************
-- *            *
-- * exp typeof *
-- *            *
-- **************
expTypeof :: Location -> Ast.Exp -> Ast.Exp
expTypeof loc _operand = instrumentationCall "typeof" loc []

-- ***************
-- *             *
-- * exp ternary *
-- *             *
-- ***************
expTernary :: Location -> Ast.Exp -> Ast.Exp -> Ast.Exp -> Ast.Exp
expTernary loc _cond _thenExp _elseExp = instrumentationCall "ternary" loc []

-- ************
-- *          *
-- * property *
-- *          *
-- ************
-- Object-literal `key: value` (or `[expr]: value`) assignments. Lowered as
-- a standard Ast.ExpCall instrumentation tagged `[kv]`, so the callee is
-- `<dhscanner-instrumentation>[kv]` -- same convention as every other
-- instrumented Ast.ExpCall in this module (see `instrumentationCall`).
property :: Location -> Ast.Exp -> Ast.Exp -> Ast.Exp
property loc keyExp valueExp = instrumentationCall "kv" loc [keyExp, valueExp]

-- *****************
-- *               *
-- * jsx selection *
-- *               *
-- *****************
chooseFirstJust :: [Maybe Ast.Exp] -> Maybe Ast.Exp
chooseFirstJust xs = case xs of
    [] -> Nothing
    (y:ys) -> case y of
        Just e  -> Just e
        Nothing -> chooseFirstJust ys

jsxChoose :: Location -> [Maybe Ast.Exp] -> Ast.Exp
jsxChoose loc maybes = case chooseFirstJust maybes of
    Just e  -> e
    Nothing -> instrumentationCall "jsx" loc []

-- ***************
-- *             *
-- * stmt import *
-- *             *
-- ***************
normalizeImportPath :: String -> String
normalizeImportPath i = case Data.List.stripPrefix "./" i of { Just p -> p; _ -> i }

resolvePathAlias :: [ Common.PathMapping ] -> String -> String
resolvePathAlias [] imported = imported
resolvePathAlias (m:rest) imported =
    case Data.List.stripPrefix (Common.path_mapping_from m) imported of
        Just suffix -> (Common.path_mapping_to m) ++ suffix
        Nothing -> resolvePathAlias rest imported

isKnownFilename :: [ String ] -> String -> Bool
isKnownFilename filenames imported = (imported ++ ".ts") `elem` filenames

resolveImportSource :: Common.AdditionalRepoInfo -> String -> Ast.ImportSource
resolveImportSource repoInfo imported = let
    aliases = Common.path_mappings repoInfo
    knownFilenames = Common.filenames repoInfo
    importedNorm = normalizeImportPath imported
    resolved = resolvePathAlias aliases importedNorm
    in resolveImportSource' knownFilenames importedNorm resolved

resolveImportSource' :: [ String ] -> String -> String -> Ast.ImportSource
resolveImportSource' knownFilenames importedNorm resolved = case Data.List.isPrefixOf "@" importedNorm of
    False -> if isKnownFilename knownFilenames importedNorm
        then Ast.ImportLocal (Ast.ImportLocalFile (importedNorm ++ ".ts"))
        else Ast.ImportThirdParty (Ast.ImportThirdPartyContent importedNorm)
    True -> if (resolved /= importedNorm) && (isKnownFilename knownFilenames resolved)
        then Ast.ImportLocal (Ast.ImportLocalFile (resolved ++ ".ts"))
        else Ast.ImportThirdParty (Ast.ImportThirdPartyContent importedNorm)

importify' :: Common.AdditionalRepoInfo -> Token.ConstStr -> Token.Named -> Ast.Stmt
importify' repoInfo importSource importFromSource = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = resolveImportSource repoInfo (Token.constStrValue importSource),
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific (Token.content importFromSource)),
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = Token.location importFromSource
}

importify :: Common.AdditionalRepoInfo -> Token.ConstStr -> [ Token.Named ] -> [ Ast.Stmt ]
importify repoInfo = Data.List.map . (importify' repoInfo)

stmtImport :: Common.AdditionalRepoInfo -> Location -> Maybe [Token.Named] -> Token.ConstStr -> Ast.Stmt
stmtImport repoInfo loc maybeImports importSource = Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = importify repoInfo importSource (fromMaybe [] maybeImports),
        Ast.stmtBlockLocation = loc
    }

-- **************
-- *            *
-- * stmt export*
-- *            *
-- **************
stmtExport :: Location -> Ast.Stmt
stmtExport loc = Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = [],
        Ast.stmtBlockLocation = loc
    }

-- **************
-- *            *
-- * stmt type alias *
-- *            *
-- **************
stmtTypeAlias :: Location -> Ast.Stmt
stmtTypeAlias loc = Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = [],
        Ast.stmtBlockLocation = loc
    }
