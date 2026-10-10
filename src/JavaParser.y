{
{-# OPTIONS -Werror=missing-fields #-}

module JavaParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import JavaLexer
import Location
import qualified Token
import qualified Common

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.Maybe
import Data.Either
import Data.List ( map, isPrefixOf )
import Data.Map ( fromList )
import System.FilePath

}

-- ***********************
-- *                     *
-- * API function: parse *
-- *                     *
-- ***********************
%name parse program

-- *************
-- *           *
-- * tokentype *
-- *           *
-- *************
%tokentype { AlexTokenTag }

-- *********
-- *       *
-- * monad *
-- *       *
-- *********
%monad { Alex }

-- *********
-- *       *
-- * lexer *
-- *       *
-- *********
%lexer { lexwrap } { AlexTokenTag TokenEOF _ }

-- ***************************************************
-- * Call this function when an error is encountered *
-- ***************************************************
%error { parseError }

%token 

-- ***************
-- *             *
-- * parentheses *
-- *             *
-- ***************

'('    { AlexTokenTag AlexRawToken_LPAREN _ }
')'    { AlexTokenTag AlexRawToken_RPAREN _ }
'['    { AlexTokenTag AlexRawToken_LBRACK _ }
']'    { AlexTokenTag AlexRawToken_RBRACK _ }
'{'    { AlexTokenTag AlexRawToken_LBRACE _ }
'}'    { AlexTokenTag AlexRawToken_RBRACE _ }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

':'    { AlexTokenTag AlexRawToken_COLON  _ }
','    { AlexTokenTag AlexRawToken_COMMA  _ }
'-'    { AlexTokenTag AlexRawToken_HYPHEN _ }

-- *********************
-- *                   *
-- * reserved keywords *
-- *                   *
-- *********************

'id'                    { AlexTokenTag AlexRawToken_KWID            _ }
'end'                   { AlexTokenTag AlexRawToken_END             _ }
'raw'                   { AlexTokenTag AlexRawToken_RAW             _ }
'loc'                   { AlexTokenTag AlexRawToken_LOC             _ }
'Arg'                   { AlexTokenTag AlexRawToken_ARG             _ }
'var'                   { AlexTokenTag AlexRawToken_VAR             _ }
'tail'                  { AlexTokenTag AlexRawToken_TAIL            _ }
'kind'                  { AlexTokenTag AlexRawToken_KIND            _ }
'null'                  { AlexTokenTag AlexRawToken_NULL            _ }
'test'                  { AlexTokenTag AlexRawToken_TEST            _ }
'line'                  { AlexTokenTag AlexRawToken_LINE            _ }
'true'                  { AlexTokenTag AlexRawToken_TRUE            _ }
'args'                  { AlexTokenTag AlexRawToken_ARGS            _ }
'name'                  { AlexTokenTag AlexRawToken_NAME            _ }
'expr'                  { AlexTokenTag AlexRawToken_EXPR            _ }
'Name'                  { AlexTokenTag AlexRawToken_MAME            _ }
'type'                  { AlexTokenTag AlexRawToken_TYPE            _ }
'left'                  { AlexTokenTag AlexRawToken_LEFT            _ }
'loop'                  { AlexTokenTag AlexRawToken_LOOP            _ }
'init'                  { AlexTokenTag AlexRawToken_INIT            _ }
'cond'                  { AlexTokenTag AlexRawToken_COND            _ }
'body'                  { AlexTokenTag AlexRawToken_BODY            _ }
'quasis'                { AlexTokenTag AlexRawToken_QUASIS          _ }
'cooked'                { AlexTokenTag AlexRawToken_COOKED          _ }
'update'                { AlexTokenTag AlexRawToken_UPDATE          _ }
'false'                 { AlexTokenTag AlexRawToken_FALSE           _ }
'start'                 { AlexTokenTag AlexRawToken_START           _ }
'exprs'                 { AlexTokenTag AlexRawToken_EXPRS           _ }
'value'                 { AlexTokenTag AlexRawToken_VALUE           _ }
'right'                 { AlexTokenTag AlexRawToken_RIGHT           _ }
'stmts'                 { AlexTokenTag AlexRawToken_STMTS           _ }
'array'                 { AlexTokenTag AlexRawToken_ARRAY           _ }
'Param'                 { AlexTokenTag AlexRawToken_PARAM           _ }
'object'                { AlexTokenTag AlexRawToken_OBJECT          _ }
'prefix'                { AlexTokenTag AlexRawToken_PREFIX          _ }
'params'                { AlexTokenTag AlexRawToken_PARAMS          _ }
'column'                { AlexTokenTag AlexRawToken_COLUMN          _ }
'Literal'               { AlexTokenTag AlexRawToken_LITERAL         _ }
'Program'               { AlexTokenTag AlexRawToken_PROGRAM         _ }
'property'              { AlexTokenTag AlexRawToken_PROPERTY        _ }
'computed'              { AlexTokenTag AlexRawToken_COMPUTED        _ }
'operator'              { AlexTokenTag AlexRawToken_OPERATOR        _ }
'alternate'             { AlexTokenTag AlexRawToken_ALTERNATE       _ }
'consequent'            { AlexTokenTag AlexRawToken_CONSEQUENT      _ }
'argument'              { AlexTokenTag AlexRawToken_ARGUMENT        _ }
'arguments'             { AlexTokenTag AlexRawToken_ARGUMENTS       _ }
'generator'             { AlexTokenTag AlexRawToken_GENERATOR       _ }
'expression'            { AlexTokenTag AlexRawToken_EXPRESSION      _ }
'expressions'           { AlexTokenTag AlexRawToken_EXPRESSIONS     _ }
'declarations'          { AlexTokenTag AlexRawToken_DECLARATIONS    _ }
'async'                 { AlexTokenTag AlexRawToken_ASYNC           _ }
'callee'                { AlexTokenTag AlexRawToken_CALLEE          _ }
'sourceType'            { AlexTokenTag AlexRawToken_SRC_TYPE        _ }
'Scalar_Int'            { AlexTokenTag AlexRawToken_SCALAR_INT      _ }
'Identifier'            { AlexTokenTag AlexRawToken_IDENTIFIER      _ }
'returnType'            { AlexTokenTag AlexRawToken_RETURN_TYPE     _ }
'FunctionDeclaration'   { AlexTokenTag AlexRawToken_FUNCTION_DEC    _ }
'VariableDeclaration'   { AlexTokenTag AlexRawToken_VAR_DECLARATION _ }
'VariableDeclarator'    { AlexTokenTag AlexRawToken_VAR_DECLARATOR  _ }
'local' { AlexTokenTag AlexRawToken_local _ }
'source' { AlexTokenTag AlexRawToken_source _ }
'specifiers' { AlexTokenTag AlexRawToken_specifiers _ }
'imported' { AlexTokenTag AlexRawToken_imported _ }
'ImportDeclaration' { AlexTokenTag AlexRawToken_ImportDeclaration _ }
'ImportSpecifier' { AlexTokenTag AlexRawToken_ImportSpecifier _ }
'ImportDefaultSpecifier' { AlexTokenTag AlexRawToken_ImportDefaultSpecifier _ }
'key' { AlexTokenTag AlexRawToken_key _ }
'properties' { AlexTokenTag AlexRawToken_properties _ }
'ObjectExpression' { AlexTokenTag AlexRawToken_ObjectExpression _ }
'shorthand' { AlexTokenTag AlexRawToken_shorthand _ }
'method' { AlexTokenTag AlexRawToken_method _ }
'Property' { AlexTokenTag AlexRawToken_Property _ }
'AssignmentPattern' { AlexTokenTag AlexRawToken_AssignmentPattern _ }
'LogicalExpression' { AlexTokenTag AlexRawToken_LogicalExpression _ }
'ArrayPattern' { AlexTokenTag AlexRawToken_ArrayPattern _ }
'elements' { AlexTokenTag AlexRawToken_elements _ }
'ObjectPattern' { AlexTokenTag AlexRawToken_ObjectPattern _ }
'TryStatement' { AlexTokenTag AlexRawToken_TryStatement _ }
'block' { AlexTokenTag AlexRawToken_block _ }
'handler' { AlexTokenTag AlexRawToken_handler _ }
'finalizer' { AlexTokenTag AlexRawToken_finalizer _ }
'AwaitExpression' { AlexTokenTag AlexRawToken_AwaitExpression _ }
'CatchClause' { AlexTokenTag AlexRawToken_CatchClause _ }
'param' { AlexTokenTag AlexRawToken_param _ }
'ArrayExpression' { AlexTokenTag AlexRawToken_ArrayExpression _ }
'UnaryExpression' { AlexTokenTag AlexRawToken_UnaryExpression _ }
'declaration' { AlexTokenTag AlexRawToken_declaration _ }
'ExportDefaultDeclaration' { AlexTokenTag AlexRawToken_ExportDefaultDeclaration _ }
-- last keywords first part

-- *********
-- *       *
-- * other *
-- *       *
-- *********

QUOTED_INT  { AlexTokenTag AlexRawToken_QUOTED_INT  _ }
QUOTED_STR  { AlexTokenTag AlexRawToken_QUOTED_STR  _ }
QUOTED_BOOL { AlexTokenTag AlexRawToken_QUOTED_BOOL _ }
QUOTED_NULL { AlexTokenTag AlexRawToken_QUOTED_NULL _ }

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

'NewExpression'    { AlexTokenTag AlexRawToken_EXPR_NEW    _ }
'CallExpression'   { AlexTokenTag AlexRawToken_EXPR_CALL   _ }
'MemberExpression' { AlexTokenTag AlexRawToken_EXPR_MEMBER _ }
'BinaryExpression' { AlexTokenTag AlexRawToken_EXPR_BINOP  _ }
'UpdateExpression' { AlexTokenTag AlexRawToken_EXPR_UPDATE _ }
'AssignExpression' { AlexTokenTag AlexRawToken_EXPR_ASSIGN _ }
'LambdaExpression' { AlexTokenTag AlexRawToken_EXPR_LAMBDA _ }
'FunctionExpression' { AlexTokenTag AlexRawToken_EXPR_FUNCTION _ }

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

'Expr_Variable'         { AlexTokenTag AlexRawToken_EXPR_VAR        _ }
'Expr_ConstFetch'       { AlexTokenTag AlexRawToken_EXPR_CONST_GET  _ }
'Expr_BinaryOp_Plus'    { AlexTokenTag AlexRawToken_EXPR_BINOP_PLUS _ }
'Expr_BinaryOp_Smaller' { AlexTokenTag AlexRawToken_EXPR_BINOP_LT   _ }

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

'IfStatement'         { AlexTokenTag AlexRawToken_STMT_IF     _ }
'ForStatement'        { AlexTokenTag AlexRawToken_STMT_FOR    _ }
'BlockStatement'      { AlexTokenTag AlexRawToken_STMT_BLOCK  _ }
'ReturnStatement'     { AlexTokenTag AlexRawToken_STMT_RETURN _ }
'TemplateLiteral'     { AlexTokenTag AlexRawToken_TEMPLATE_LI _ }
'TemplateElement'     { AlexTokenTag AlexRawToken_TEMPLATE_EL _ }
'ExpressionStatement' { AlexTokenTag AlexRawToken_STMT_EXP    _ }

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

'Stmt_Echo'     { AlexTokenTag AlexRawToken_STMT_ECHO       _ }
'Stmt_Expr'     { AlexTokenTag AlexRawToken_STMT_EXPR       _ }
'Stmt_Function' { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ }

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

'<'  { AlexTokenTag AlexRawToken_OP_LT       _ }
'+'  { AlexTokenTag AlexRawToken_OP_PLUS     _ }
'>'  { AlexTokenTag AlexRawToken_OP_GT       _ }
'==' { AlexTokenTag AlexRawToken_OP_EQ       _ }
'='  { AlexTokenTag AlexRawToken_OP_ASSIGN   _ }
'*'  { AlexTokenTag AlexRawToken_OP_TIMES    _ }
'++' { AlexTokenTag AlexRawToken_OP_PLUSPLUS _ }
'||' { AlexTokenTag AlexRawToken_OP_OR _ }
'&&' { AlexTokenTag AlexRawToken_OP_AND _ }
'!'  { AlexTokenTag AlexRawToken_bang _ }
'!==' { AlexTokenTag AlexRawToken_OP_NEQ _ }
'in' { AlexTokenTag AlexRawToken_OP_IN _ }

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************

INT    { AlexTokenTag (AlexRawToken_INT  i) _ }
ID     { AlexTokenTag (AlexRawToken_ID  id) _ }

-- *************************
-- *                       *
-- * grammar specification *
-- *                       *
-- *************************
%%

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
listof(a):      a { [$1] } | a          listof(a) { $1:$2 }
commalistof(a): a { [$1] } | a ',' commalistof(a) { $1:$3 }

-- ***********************
-- *                     *
-- * parametrized maybes *
-- *                     *
-- ***********************
ornull(a): 'null' { Nothing } | a { Just $1 }

-- ****************
-- *              *
-- * stmt (stub)  *
-- *              *
-- ****************
-- The `program` rule below is a verbatim copy from JsParser.y and
-- references `stmt` + `location` grammar rules we didn't carry over.
-- The two tiny stubs below only exist so Happy / GHC are happy; the
-- parser is intentionally not expected to actually succeed on any
-- frontjava input yet (different token shape). Replace these with
-- real productions during the coverage iterations.
stmt :: { Ast.Stmt }
stmt : ID { undefined }

location :: { () }
location : '{' '}' { () }

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program:
'{'
    'type' ':' 'Program' ','
    'body' ':' '[' commalistof(stmt) ']' ','
    'sourceType' ':' ID ','
    'loc' ':' location
'}'
{
    Ast.Root
    {
        Ast.filename = getFilename $1,
        Ast.stmts = $9
    }
}
{

unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

takeParentDir :: FilePath -> FilePath
takeParentDir = takeDirectory

joinPaths :: FilePath -> FilePath -> FilePath
joinPaths = (</>)

addJsExt :: FilePath -> FilePath
addJsExt = addExtension "js"

resolveImport :: FilePath -> FilePath -> FilePath
resolveImport importedFrom content = addJsExt (joinPaths (takeParentDir importedFrom) content)

require7 :: Token.Named -> String -> Location -> Ast.Stmt
require7 v imported loc = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent (resolveImport (Location.filename loc) imported)),
    Ast.stmtImportSpecific = Nothing,
    Ast.stmtImportAlias = Just (Ast.ImportAlias (Token.content v)),
    Ast.stmtImportLocation = loc
}

require6 :: Token.Named -> String -> Location -> Ast.Stmt
require6 v imported loc = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent imported),
    Ast.stmtImportSpecific = Nothing,
    Ast.stmtImportAlias = Just (Ast.ImportAlias (Token.content v)),
    Ast.stmtImportLocation = loc
}

require5 :: Token.ConstStr -> Token.Named -> Maybe Ast.Stmt
require5 (Token.ConstStr value loc) v = case "./" `isPrefixOf` value of {
    False -> Just (require6 v value loc);
    True -> Just (require7 v value loc)
}

require'''' :: [ Ast.Exp ] -> Token.Named -> Maybe Ast.Stmt
require'''' [ (Ast.ExpStr (Ast.ExpStrContent s)) ] v = require5 s v
require'''' _ _ = Nothing

require''' :: Token.Named -> Ast.ExpCallContent -> Token.Named -> Maybe Ast.Stmt
require''' callee call v = case (Token.content callee) == "require" of {
    True -> require'''' (Ast.args call) v;
    False -> Nothing
} 

require'' :: Ast.ExpVarContent -> Ast.ExpCallContent -> Token.Named -> Maybe Ast.Stmt
require'' (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName t)))) c v = require''' t c v
require'' _ _ _ = Nothing

require' :: Ast.ExpCallContent -> Token.Named -> Maybe Ast.Stmt
require' call v = case (Ast.callee call) of {
    (Ast.ExpVar callee) -> require'' callee call v;
    _ -> Nothing
}

require :: Maybe Ast.Exp -> Token.Named -> Maybe Ast.Stmt
require (Just (Ast.ExpCall call)) v = require' call v
require _ _ = Nothing

non_require :: Token.Named -> Maybe Ast.Exp -> Location -> Ast.Stmt
non_require v init loc = Ast.StmtVardec $ Ast.StmtVardecContent {
    Ast.stmtVardecName = Token.VarName v,
    Ast.stmtVardecNominalType = Just (varme (Token.Named "any" loc)),
    Ast.stmtVardecInitValue = init,
    Ast.stmtVardecLocation = loc
}

assignify' :: Location -> Ast.Exp -> Token.VarName -> Ast.Stmt
assignify' loc init v = Ast.StmtVardec $ Ast.StmtVardecContent {
    Ast.stmtVardecName = v,
    Ast.stmtVardecNominalType = Just (varme (Token.Named "any" loc)),
    Ast.stmtVardecInitValue = Just init,
    Ast.stmtVardecLocation = loc
}

assignify :: Location -> Ast.Exp -> [ Token.VarName ] -> [ Ast.Stmt ]
assignify loc init = Data.List.map (assignify' loc init)

-- ***********
-- *         *
-- * lexwrap *
-- *         *
-- ***********
lexwrap :: (AlexTokenTag -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

varme :: Token.Named -> Ast.Var
varme = Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

importify :: Location -> String -> [ Ast.StmtImportContent ] -> [ Ast.Stmt ]
importify l src = Data.List.map (importifySingle l src)

importifySingle :: Location -> String -> Ast.StmtImportContent -> Ast.Stmt
importifySingle l src stmt = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent src),
    Ast.stmtImportSpecific = Ast.stmtImportSpecific stmt,
    Ast.stmtImportAlias = Ast.stmtImportAlias stmt,
    Ast.stmtImportLocation = l
}

-- **************
-- *            *
-- * parseError *
-- *            *
-- **************
parseError :: AlexTokenTag -> Alex a
parseError t = alexError' (tokenLoc t)

-- ****************
-- *              *
-- * parseProgram *
-- *              *
-- ****************
parseProgram :: Common.SourceCodeFilePath -> Common.SourceCodeContent -> Common.AdditionalRepoInfo -> Either String Ast.Root
parseProgram (Common.SourceCodeFilePath fp) (Common.SourceCodeContent content) additionalInfo = runAlex' parse fp additionalInfo content
}
