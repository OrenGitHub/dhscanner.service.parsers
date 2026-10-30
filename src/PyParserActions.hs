module PyParserActions where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
-- `Ast` is imported qualified on purpose, like in `TsParserActions.hs` :
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
import Data.Maybe ( mapMaybe )

-- **********************
-- *                    *
-- * instrumentation    *
-- * (shared lowering   *
-- *  for native nodes  *
-- *  that have no 1:1  *
-- *  dhscanner shape)  *
-- *                    *
-- **********************
instrumentationName :: String -> String
instrumentationName tag = "<dhscanner-instrumentation>[" ++ tag ++ "]"

instrumentationCall :: String -> Location -> [Ast.Exp] -> Ast.Exp
instrumentationCall tag loc callArgs = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named (instrumentationName tag) loc,
    Ast.args = callArgs,
    Ast.expCallLocation = loc
}

-- ***************
-- *             *
-- * stmt import *
-- *             *
-- ***************
-- `import a.b.c [as m], ...` -> `Ast.StmtBlock` of one `Ast.StmtImport` per name.
--
-- Python binds only the root name : `import weblate.trans.views.about`
-- binds `weblate`, and the code reaches the module through the dotted
-- access `weblate.trans.views.about.X`, which codegen appends field by
-- field ( -> 'weblate.trans.views.about.X' ). Binding the whole dotted
-- string left the root unresolved ( 'Unknown.trans.views.about.X' ).
-- With an alias, `import a.b.c as m` binds `m` to the full `a.b.c`.
-- 1st vs 3rd party is decided downstream, by name ( runner #126, G1 ).
stmtImport :: Location -> [(String, Maybe String)] -> Ast.Stmt
stmtImport loc names = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = map (stmtImportName loc) names,
    Ast.stmtBlockLocation = loc
}

stmtImportName :: Location -> (String, Maybe String) -> Ast.Stmt
stmtImportName loc (src, alias) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = Ast.ImportThirdParty (Ast.ImportThirdPartyContent (maybe (rootName src) (const src) alias)),
    Ast.stmtImportSpecific = Nothing,
    Ast.stmtImportAlias = fmap Ast.ImportAlias alias,
    Ast.stmtImportLocation = loc
}

-- `weblate.trans.views.about` -> `weblate`
rootName :: String -> String
rootName = takeWhile (/= '.')

-- **************
-- *            *
-- * stmt class *
-- *            *
-- **************
-- `ClassDef( name, bases, body, ... )` -> `Ast.StmtClass`.
--
-- Every class gets one more method, `<dhscanner-instrumentation>[default-method]`,
-- with no params and an empty body, located at the class itself. A class
-- with no methods ( eg `class TermsView(LegalView): page = "terms"` ) would
-- otherwise leave no trace in the kb : codegen / kbgen emit the class def
-- and its supers through its methods ( runner #126, G2 ).
stmtClass :: Location -> String -> [ Ast.Var ] -> [ Ast.Stmt ] -> Ast.Stmt
stmtClass loc name bases body = Ast.StmtClass $ Ast.StmtClassContent {
    Ast.stmtClassName = className,
    Ast.stmtClassSupers = superify bases,
    Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
    Ast.stmtClassMethods = Ast.Methods $ Data.Map.fromList $ defaultMethod className bases loc : methodify className bases body
} where className = Token.ClassName (Token.Named name loc)

defaultMethod :: Token.ClassName -> [ Ast.Var ] -> Location -> (Token.MethodName, Ast.StmtMethodContent)
defaultMethod c bases loc = let m = Token.MethodName (Token.Named (instrumentationName "default-method") loc) in (m, Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Nothing,
    Ast.stmtMethodName = m,
    Ast.stmtMethodParams = [],
    Ast.stmtMethodBody = [],
    Ast.stmtMethodAnnotations = [],
    Ast.stmtMethodLocation = loc,
    Ast.hostingClassName = c,
    Ast.hostingClassSupers = superify bases,
    Ast.hostingClassAnnotations = []
})

methodify :: Token.ClassName -> [ Ast.Var ] -> [ Ast.Stmt ] -> [(Token.MethodName, Ast.StmtMethodContent)]
methodify c bases = mapMaybe (methodify' c bases)

methodify' :: Token.ClassName -> [ Ast.Var ] -> Ast.Stmt -> Maybe (Token.MethodName, Ast.StmtMethodContent)
methodify' c bases (Ast.StmtFunc f) = Just (methodify'' c bases f)
methodify' _ _ _ = Nothing

methodify'' :: Token.ClassName -> [ Ast.Var ] -> Ast.StmtFuncContent -> (Token.MethodName, Ast.StmtMethodContent)
methodify'' c bases f = let m = Token.MethodName $ Token.getFuncNameToken (Ast.stmtFuncName f) in (m, Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Ast.stmtFuncReturnType f,
    Ast.stmtMethodName = m,
    Ast.stmtMethodParams = Ast.stmtFuncParams f,
    Ast.stmtMethodBody = Ast.stmtFuncBody f,
    Ast.stmtMethodAnnotations = Ast.stmtFuncAnnotations f,
    Ast.stmtMethodLocation = Ast.stmtFuncLocation f,
    Ast.hostingClassName = c,
    Ast.hostingClassSupers = superify bases,
    Ast.hostingClassAnnotations = []
})

superify' :: Ast.Var -> Token.SuperName
superify' (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v))) = Token.SuperName v
superify' (Ast.VarField (Ast.VarFieldContent _ (Token.FieldName v) _)) = Token.SuperName v
superify' (Ast.VarSubscript (Ast.VarSubscriptContent _ _ l)) = Token.SuperName (Token.Named "subscript" l)

superify :: [ Ast.Var ] -> [ Token.SuperName ]
superify = map superify'
