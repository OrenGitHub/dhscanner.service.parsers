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
import Data.Maybe ( catMaybes, fromMaybe, maybeToList )

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

-- ***************
-- *             *
-- * subscriptVar *
-- *             *
-- ***************
subscriptVar :: Location -> Ast.Exp -> Ast.Exp -> Ast.Var
subscriptVar loc lhs index = Ast.VarSubscript (Ast.VarSubscriptContent lhs index loc)

-- *************
-- *           *
-- * stmtClass *
-- *           *
-- *************
stmtClass :: Location -> String -> Maybe [ Ast.Exp ] -> Maybe [ Ast.Var ] -> Maybe [ Ast.Stmt ] -> Ast.Stmt
stmtClass loc name _annotations supers members = stmtClass' (Token.ClassName (Token.Named name loc)) (superify (fromMaybe [] supers)) [ m | Ast.StmtMethod m <- fromMaybe [] members ]

stmtClass' :: Token.ClassName -> [ Token.SuperName ] -> [ Ast.StmtMethodContent ] -> Ast.Stmt
stmtClass' className supers methods = Ast.StmtClass $ Ast.StmtClassContent {
    Ast.stmtClassName = className,
    Ast.stmtClassSupers = supers,
    Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
    Ast.stmtClassMethods = Ast.Methods $ Data.Map.fromList [ (Ast.stmtMethodName m, m { Ast.hostingClassName = className, Ast.hostingClassSupers = supers }) | m <- methods ]
}

-- ************
-- *          *
-- * superify *
-- *          *
-- ************
superify' :: Ast.Var -> Token.SuperName
superify' (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v))) = Token.SuperName v
superify' v@(Ast.VarField (Ast.VarFieldContent _ (Token.FieldName (Token.Named _ l)) _)) = Token.SuperName (Token.Named (var2name' v) l)
superify' (Ast.VarSubscript (Ast.VarSubscriptContent _ _ l)) = Token.SuperName (Token.Named "subscript" l)

superify :: [ Ast.Var ] -> [ Token.SuperName ]
superify = map superify'

-- ********************
-- *                  *
-- * withSuperImports *
-- *                  *
-- ********************
-- a qualified super ( class A extends java.io.Reader ) gets a synthetic
-- import aliased to its dotted name ( from java.io import Reader as "java.io.Reader" ),
-- so codegen resolves it like any imported name - no Java identifier contains a '.'
withSuperImports :: [ Ast.Stmt ] -> [ Ast.Stmt ]
withSuperImports stmts = [ superImport s | Ast.StmtClass c <- stmts, Token.SuperName s <- Ast.stmtClassSupers c, '.' `elem` Token.content s ] ++ stmts

superImport :: Token.Named -> Ast.Stmt
superImport (Token.Named qualified loc) = superImport' qualified (break (== '.') (reverse qualified)) loc

superImport' :: String -> (String, String) -> Location -> Ast.Stmt
superImport' qualified (specific, source) loc = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = Ast.ImportThirdParty (Ast.ImportThirdPartyContent (reverse (drop 1 source))),
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific (reverse specific)),
    Ast.stmtImportAlias = Just (Ast.ImportAlias qualified),
    Ast.stmtImportLocation = loc
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
stmtCtor :: Location -> String -> Maybe [ Ast.Exp ] -> Maybe [ Ast.Param ] -> Maybe Ast.Stmt -> Ast.Stmt
stmtCtor loc name annotations _params body = Ast.StmtMethod $ Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Nothing,
    Ast.stmtMethodName = Token.MethodName (Token.Named name loc),
    Ast.stmtMethodParams = [],
    Ast.stmtMethodBody = maybeToList body,
    Ast.stmtMethodAnnotations = fromMaybe [] annotations,
    Ast.stmtMethodLocation = loc,
    Ast.hostingClassName = Token.ClassName (Token.Named "" loc),
    Ast.hostingClassSupers = []
}

-- **************
-- *            *
-- * stmtMethod *
-- *            *
-- **************
stmtMethod :: Location -> String -> Maybe [ Ast.Exp ] -> Maybe [ Ast.Param ] -> Ast.Var -> Maybe Ast.Stmt -> Ast.Stmt
stmtMethod loc name annotations _params _returnType body = Ast.StmtMethod $ Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Nothing,
    Ast.stmtMethodName = Token.MethodName (Token.Named name loc),
    Ast.stmtMethodParams = [],
    Ast.stmtMethodBody = maybeToList body,
    Ast.stmtMethodAnnotations = fromMaybe [] annotations,
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

-- *********
-- *       *
-- * arrayType *
-- *       *
-- *********
arrayType :: Location -> Ast.Var -> Ast.Var
arrayType _loc elementType = elementType

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
expvarify :: Token.Named -> Ast.Exp
expvarify = Ast.ExpVar . Ast.ExpVarContent . Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

-- ***********************
-- *                     *
-- * instrumentationCall *
-- *                     *
-- ***********************
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
expUnop :: String -> Location -> Ast.Exp -> Ast.Exp
expUnop tag loc operand = instrumentationCall tag loc [operand]

-- **************
-- *            *
-- * expTernary *
-- *            *
-- **************
expTernary :: Location -> Ast.Exp -> Ast.Exp -> Ast.Exp -> Ast.Exp
expTernary loc cond thenExp elseExp = instrumentationCall "ternary" loc [
        instrumentationCall "ternary_cond" loc [cond],
        instrumentationCall "ternary_then" loc [thenExp],
        instrumentationCall "ternary_else" loc [elseExp]
    ]

-- ***********
-- *         *
-- * expBool *
-- *         *
-- ***********
expBool :: Location -> String -> Ast.Exp
expBool loc value = Ast.ExpBool $ Ast.ExpBoolContent (Token.ConstBool (value == "true") loc)

-- ************
-- *          *
-- * expBinop *
-- *          *
-- ************
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
expCall :: Location -> [ Ast.Exp ] -> Ast.Exp
expCall loc [] = Ast.ExpNull $ Ast.ExpNullContent (Token.ConstNull loc)
expCall loc (callee:args) = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = callee,
    Ast.args = args,
    Ast.expCallLocation = loc
}

-- *************
-- *           *
-- * expLambda *
-- *           *
-- *************
expLambda :: Location -> Maybe [ Maybe Ast.Exp ] -> Ast.Exp
expLambda loc children = instrumentationCall "lambda" loc (lambdaArgs children)

lambdaArgs :: Maybe [ Maybe Ast.Exp ] -> [ Ast.Exp ]
lambdaArgs Nothing = []
lambdaArgs (Just children) = catMaybes children

-- **********************
-- *                    *
-- * expMemberReference *
-- *                    *
-- **********************
expMemberReference :: Location -> [ Ast.Exp ] -> Ast.Exp
expMemberReference loc children = instrumentationCall "member_reference" loc children

-- *************
-- *           *
-- * stmtBlock *
-- *           *
-- *************
stmtBlock :: Location -> Maybe [ Ast.Stmt ] -> Ast.Stmt
stmtBlock loc stmts = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = fromMaybe [] stmts,
    Ast.stmtBlockLocation = loc
}

-- ***********
-- *         *
-- * stmtExp *
-- *         *
-- ***********
stmtExp :: Ast.Exp -> Ast.Stmt
stmtExp = Ast.StmtExp

-- **************
-- *            *
-- * stmtReturn *
-- *            *
-- **************
stmtReturn :: Location -> Maybe Ast.Exp -> Ast.Stmt
stmtReturn loc value = Ast.StmtReturn $ Ast.StmtReturnContent {
    Ast.stmtReturnValue = value,
    Ast.stmtReturnLocation = loc
}

-- *************
-- *           *
-- * stmtThrow *
-- *           *
-- *************
stmtThrow :: Location -> Ast.Exp -> Ast.Stmt
stmtThrow loc value = Ast.StmtExp (instrumentationCall "throw" loc [value])

-- ****************
-- *              *
-- * stmtContinue *
-- *              *
-- ****************
stmtContinue :: Location -> Ast.Stmt
stmtContinue loc = Ast.StmtExp (instrumentationCall "continue" loc [])

-- *************
-- *           *
-- * stmtBreak *
-- *           *
-- *************
stmtBreak :: Location -> Ast.Stmt
stmtBreak loc = Ast.StmtExp (instrumentationCall "break" loc [])

-- **********
-- *        *
-- * stmtIf *
-- *        *
-- **********
stmtIf :: Location -> Ast.Exp -> Ast.Stmt -> Maybe [ Ast.Stmt ] -> Ast.Stmt
stmtIf loc cond thenStmt elseStmts = Ast.StmtIf $ Ast.StmtIfContent {
    Ast.stmtIfCond = cond,
    Ast.stmtIfBody = [ thenStmt ],
    Ast.stmtElseBody = fromMaybe [] elseStmts,
    Ast.stmtIfLocation = loc
}

-- *************
-- *           *
-- * stmtWhile *
-- *           *
-- *************
stmtWhile :: Location -> Ast.Exp -> Ast.Stmt -> Ast.Stmt
stmtWhile loc cond body = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = [ Ast.StmtExp cond ] ++ unblock body,
    Ast.stmtBlockLocation = loc
}

-- *******************
-- *                 *
-- * stmtEnhancedFor *
-- *                 *
-- *******************
stmtEnhancedFor :: Location -> Ast.Stmt -> Ast.Exp -> Ast.Stmt -> Ast.Stmt
stmtEnhancedFor loc varStmt iterable body = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = [ varStmt, Ast.StmtExp iterable ] ++ unblock body,
    Ast.stmtBlockLocation = loc
}

-- ***********
-- *         *
-- * stmtFor *
-- *         *
-- ***********
stmtFor :: Location -> Ast.Stmt -> Ast.Exp -> Ast.Stmt -> Ast.Stmt -> Ast.Stmt
stmtFor loc initStmt cond updateStmt body = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = [ initStmt, Ast.StmtExp cond, updateStmt ] ++ unblock body,
    Ast.stmtBlockLocation = loc
}

stmtForNoUpdate :: Location -> Ast.Stmt -> Ast.Exp -> Ast.Stmt -> Ast.Stmt
stmtForNoUpdate loc initStmt cond body = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = [ initStmt, Ast.StmtExp cond ] ++ unblock body,
    Ast.stmtBlockLocation = loc
}

stmtForNoInit :: Location -> Ast.Exp -> Ast.Stmt -> Ast.Stmt -> Ast.Stmt
stmtForNoInit loc cond updateStmt body = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = [ Ast.StmtExp cond, updateStmt ] ++ unblock body,
    Ast.stmtBlockLocation = loc
}

-- ***********
-- *         *
-- * unblock *
-- *         *
-- ***********
unblock :: Ast.Stmt -> [ Ast.Stmt ]
unblock (Ast.StmtBlock (Ast.StmtBlockContent stmts _)) = stmts
unblock s = [ s ]

-- ***********************
-- *                     *
-- * stmtTryWithResource *
-- *                     *
-- ***********************
stmtTryWithResource :: Location -> Ast.Stmt -> Ast.Stmt -> Maybe [ Ast.Stmt ] -> Ast.Stmt
stmtTryWithResource loc resource tryBlock catchBlocks = stmtTry loc (prependTryResource loc resource tryBlock) (fromMaybe [] catchBlocks)

stmtTryWithResources :: Location -> [ Ast.Stmt ] -> Ast.Stmt -> Maybe [ Ast.Stmt ] -> Ast.Stmt
stmtTryWithResources loc resources tryBlock catchBlocks = stmtTry loc (prependTryResources loc resources tryBlock) (fromMaybe [] catchBlocks)

prependTryResources :: Location -> [ Ast.Stmt ] -> Ast.Stmt -> Ast.Stmt
prependTryResources loc resources tryBlock = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = resources ++ unblock tryBlock,
    Ast.stmtBlockLocation = loc
}

prependTryResource :: Location -> Ast.Stmt -> Ast.Stmt -> Ast.Stmt
prependTryResource loc resource tryBlock = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = [ resource ] ++ unblock tryBlock,
    Ast.stmtBlockLocation = loc
}

-- ***********
-- *         *
-- * stmtTry *
-- *         *
-- ***********
stmtTry :: Location -> Ast.Stmt -> [ Ast.Stmt ] -> Ast.Stmt
stmtTry loc tryBlock catchBlocks = Ast.StmtTry $ Ast.StmtTryContent {
    Ast.stmtTryPart = unblock tryBlock,
    Ast.stmtCatchPart = concatMap unblock catchBlocks,
    Ast.stmtTryLocation = loc
}
