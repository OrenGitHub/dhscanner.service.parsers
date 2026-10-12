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
fieldVar :: Location -> String -> Ast.Var -> Ast.Var
fieldVar loc field v = fieldVar'' (Ast.ExpVar (Ast.ExpVarContent v)) (Token.FieldName (Token.Named field loc)) loc

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

-- **************
-- *            *
-- * stmtMethod *
-- *            *
-- **************
stmtMethod :: Location -> String -> Ast.Stmt
stmtMethod loc name = Ast.StmtMethod $ Ast.StmtMethodContent {
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
