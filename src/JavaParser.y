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
import qualified JavaParserActions

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
'IDENTIFIER'            { AlexTokenTag AlexRawToken_IDENTIFIER      _ }
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

-- ****************************
-- *                          *
-- * native ast location keys *
-- *                          *
-- ****************************

'startLine' { AlexTokenTag AlexRawToken_START_LINE _ }
'startCol'  { AlexTokenTag AlexRawToken_START_COL  _ }
'endLine'   { AlexTokenTag AlexRawToken_END_LINE   _ }
'endCol'    { AlexTokenTag AlexRawToken_END_COL    _ }
'children'  { AlexTokenTag AlexRawToken_CHILDREN   _ }
'text'      { AlexTokenTag AlexRawToken_TEXT       _ }
'COMPILATION_UNIT' { AlexTokenTag AlexRawToken_COMPILATION_UNIT _ }
'PACKAGE'          { AlexTokenTag AlexRawToken_PACKAGE          _ }
'IMPORT'           { AlexTokenTag AlexRawToken_IMPORT           _ }
'MEMBER_SELECT'    { AlexTokenTag AlexRawToken_MEMBER_SELECT    _ }
'CLASS'            { AlexTokenTag AlexRawToken_CLASS            _ }
'INTERFACE'        { AlexTokenTag AlexRawToken_INTERFACE        _ }
'ENUM'             { AlexTokenTag AlexRawToken_ENUM             _ }
'NEW_CLASS'        { AlexTokenTag AlexRawToken_NEW_CLASS        _ }
'MODIFIERS'        { AlexTokenTag AlexRawToken_MODIFIERS        _ }
'ANNOTATION'       { AlexTokenTag AlexRawToken_ANNOTATION       _ }
'ASSIGNMENT'       { AlexTokenTag AlexRawToken_ASSIGNMENT       _ }
'STRING_LITERAL'   { AlexTokenTag AlexRawToken_STRING_LITERAL   _ }
'VARIABLE'         { AlexTokenTag AlexRawToken_VARIABLE         _ }
'METHOD'           { AlexTokenTag AlexRawToken_METHOD           _ }
'PARAMETER'        { AlexTokenTag AlexRawToken_PARAMETER        _ }
'PARAMETERIZED_TYPE' { AlexTokenTag AlexRawToken_PARAMETERIZED_TYPE _ }
'PRIMITIVE_TYPE'   { AlexTokenTag AlexRawToken_PRIMITIVE_TYPE   _ }
'INT_LITERAL'      { AlexTokenTag AlexRawToken_INT_LITERAL      _ }
'NULL_LITERAL'     { AlexTokenTag AlexRawToken_NULL_LITERAL     _ }
'BOOLEAN_LITERAL'  { AlexTokenTag AlexRawToken_BOOLEAN_LITERAL  _ }
'BLOCK'            { AlexTokenTag AlexRawToken_BLOCK            _ }
'RETURN'           { AlexTokenTag AlexRawToken_RETURN           _ }
'CONSTRUCTOR'      { AlexTokenTag AlexRawToken_CONSTRUCTOR      _ }
'IF'               { AlexTokenTag AlexRawToken_IF               _ }
'METHOD_INVOCATION' { AlexTokenTag AlexRawToken_METHOD_INVOCATION _ }
'EQUAL_TO'         { AlexTokenTag AlexRawToken_EQUAL_TO         _ }
'PLUS'             { AlexTokenTag AlexRawToken_PLUS             _ }
'MULTIPLY'         { AlexTokenTag AlexRawToken_MULTIPLY         _ }
'LOGICAL_COMPLEMENT' { AlexTokenTag AlexRawToken_LOGICAL_COMPLEMENT _ }
'EXPRESSION_STATEMENT' { AlexTokenTag AlexRawToken_EXPRESSION_STATEMENT _ }
'TRY'              { AlexTokenTag AlexRawToken_TRY              _ }
'CATCH'            { AlexTokenTag AlexRawToken_CATCH            _ }
'CONDITIONAL_EXPRESSION' { AlexTokenTag AlexRawToken_CONDITIONAL_EXPRESSION _ }
'annotations'      { AlexTokenTag AlexRawToken_annotations      _ }
'parameters'       { AlexTokenTag AlexRawToken_parameters       _ }
'supers'           { AlexTokenTag AlexRawToken_supers           _ }

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
possibly_empty_commalistof(a): '[' ']' { [] } | '[' commalistof(a) ']' { $2 }

-- ***********************
-- *                     *
-- * parametrized maybes *
-- *                     *
-- ***********************
ornull(a): 'null' { Nothing } | a { Just $1 }

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program:
'{'
    'kind' ':' 'COMPILATION_UNIT' ','
    'loc' ':' location ','
    'children' ':' '[' commalistof(stmt) ']'
'}'
{
    Ast.Root
    {
        Ast.filename = getFilename $1,
        Ast.stmts = $13
    }
}

stmt:
stmt_package   { $1 } |
stmt_import    { $1 } |
stmt_class     { $1 } |
stmt_interface { $1 } |
stmt_enum      { $1 }

-- **************
-- *            *
-- * stmt_class *
-- *            *
-- **************
stmt_class:
'{'
    'kind' ':' 'CLASS' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'annotations' ':' possibly_empty_commalistof(annotation) ','
    'supers' ':' possibly_empty_commalistof(type) ','
    'children' ':' possibly_empty_commalistof(attribute)
'}'
{
    JavaParserActions.stmtClass $8 $12 $16 $20 $24
}

-- ******************
-- *                *
-- * stmt_interface *
-- *                *
-- ******************
stmt_interface:
'{'
    'kind' ':' 'INTERFACE' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'annotations' ':' possibly_empty_commalistof(annotation) ','
    'supers' ':' possibly_empty_commalistof(type) ','
    'children' ':' possibly_empty_commalistof(attribute)
'}'
{
    JavaParserActions.stmtClass $8 $12 $16 $20 $24
}

-- *************
-- *           *
-- * stmt_enum *
-- *           *
-- *************
stmt_enum:
'{'
    'kind' ':' 'ENUM' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'annotations' ':' possibly_empty_commalistof(annotation) ','
    'supers' ':' possibly_empty_commalistof(type) ','
    'children' ':' possibly_empty_commalistof(attribute)
'}'
{
    JavaParserActions.stmtClass $8 $12 $16 $20 $24
}

-- *************
-- *           *
-- * attribute *
-- *           *
-- *************
attribute:
stmt_vardec { $1 } |
stmt_ctor   { $1 } |
stmt_method { $1 }

stmt_vardec:
stmt_vardec_init    { $1 } |
stmt_vardec_no_init { $1 }

-- *******************
-- *                 *
-- * stmt_vardec_init *
-- *                 *
-- *******************
stmt_vardec_init:
'{'
    'kind' ':' 'VARIABLE' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'annotations' ':' possibly_empty_commalistof(annotation) ','
    'type' ':' type ','
    'init' ':' exp
'}'
{
    JavaParserActions.stmtVardec $8 $12 $20 (Just $24)
}

-- **********************
-- *                    *
-- * stmt_vardec_no_init *
-- *                    *
-- **********************
stmt_vardec_no_init:
'{'
    'kind' ':' 'VARIABLE' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'annotations' ':' possibly_empty_commalistof(annotation) ','
    'type' ':' type
'}'
{
    JavaParserActions.stmtVardec $8 $12 $20 Nothing
}

-- ********
-- *      *
-- * type *
-- *      *
-- ********
type:
simple_var         { $1 } |
parameterized_type { $1 } |
primitive_type     { $1 }

-- **********************
-- *                    *
-- * parameterized_type *
-- *                    *
-- **********************
parameterized_type:
'{'
    'kind' ':' 'PARAMETERIZED_TYPE' ','
    'loc' ':' location ','
    'children' ':' '[' commalistof(type) ']'
'}'
{
    JavaParserActions.parameterizedType $8 $13
}

-- ******************
-- *                *
-- * primitive_type *
-- *                *
-- ******************
primitive_type:
'{'
    'kind' ':' 'PRIMITIVE_TYPE' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'children' ':' '[' ']'
'}'
{
    JavaParserActions.primitiveType $8 $12
}

-- *************
-- *           *
-- * parameter *
-- *           *
-- *************
parameter:
'{'
    'kind' ':' 'PARAMETER' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'type' ':' type
'}'
{
    JavaParserActions.parameter $8 $12 $16
}

-- *************
-- *           *
-- * stmt_ctor *
-- *           *
-- *************
stmt_ctor:
'{'
    'kind' ':' 'CONSTRUCTOR' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'annotations' ':' possibly_empty_commalistof(annotation) ','
    'parameters' ':' possibly_empty_commalistof(parameter) ','
    'children' ':' possibly_empty_commalistof(stmt_body)
'}'
{
    JavaParserActions.stmtCtor $8 $12 $20
}

-- ***************
-- *             *
-- * stmt_method *
-- *             *
-- ***************
stmt_method:
'{'
    'kind' ':' 'METHOD' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'annotations' ':' possibly_empty_commalistof(annotation) ','
    'parameters' ':' possibly_empty_commalistof(parameter) ','
    'children' ':' '[' type ',' stmt_body ']'
'}'
{
    JavaParserActions.stmtMethod $8 $12 $20 $25
}

-- **************
-- *            *
-- * annotation *
-- *            *
-- **************
annotation:
'{'
    'kind' ':' 'ANNOTATION' ','
    'loc' ':' location ','
    'children' ':' '[' commalistof(exp) ']'
'}'
{
    JavaParserActions.annotation $8 $13
}

exp:
exp_var           { $1 } |
exp_str           { $1 } |
exp_int           { $1 } |
exp_null          { $1 } |
exp_bool          { $1 } |
exp_assign        { $1 } |
exp_call          { $1 } |
exp_new           { $1 } |
exp_binop         { $1 } |
exp_ternary        { $1 } |
exp_unop          { $1 }

-- ***********
-- *         *
-- * exp_var *
-- *         *
-- ***********
exp_var: var { Ast.ExpVar (Ast.ExpVarContent $1) }

-- ***********
-- *         *
-- * exp_str *
-- *         *
-- ***********
exp_str:
'{'
    'kind' ':' 'STRING_LITERAL' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'children' ':' '[' ']'
'}'
{
    JavaParserActions.expStr $8 $12
}

-- ***********
-- *         *
-- * exp_int *
-- *         *
-- ***********
exp_int:
'{'
    'kind' ':' 'INT_LITERAL' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'children' ':' '[' ']'
'}'
{
    JavaParserActions.expInt $8 $12
}

-- ************
-- *          *
-- * exp_null *
-- *          *
-- ************
exp_null:
'{'
    'kind' ':' 'NULL_LITERAL' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'children' ':' '[' ']'
'}'
{
    JavaParserActions.expNull $8
}

-- ************
-- *          *
-- * exp_bool *
-- *          *
-- ************
exp_bool:
'{'
    'kind' ':' 'BOOLEAN_LITERAL' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'children' ':' '[' ']'
'}'
{
    JavaParserActions.expBool $8 $12
}

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call:
'{'
    'kind' ':' 'METHOD_INVOCATION' ','
    'loc' ':' location ','
    'children' ':' '[' commalistof(exp) ']'
'}'
{
    JavaParserActions.expCall $8 $13
}

-- ***********
-- *         *
-- * exp_new *
-- *         *
-- ***********
exp_new:
'{'
    'kind' ':' 'NEW_CLASS' ','
    'loc' ':' location ','
    'children' ':' '[' commalistof(exp) ']'
'}'
{
    JavaParserActions.expCall $8 $13
}

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop:
'{'
    'kind' ':' binop_kind ','
    'loc' ':' location ','
    'left' ':' exp ','
    'right' ':' exp ','
    'children' ':' '[' ']'
'}'
{
    JavaParserActions.expBinop $8 $12 $16 $4
}

binop_kind:
'EQUAL_TO' { Ast.EQ    } |
'PLUS'     { Ast.PLUS  } |
'MULTIPLY' { Ast.TIMES }

-- ***************
-- *             *
-- * exp_ternary *
-- *             *
-- ***************
exp_ternary:
'{'
    'kind' ':' 'CONDITIONAL_EXPRESSION' ','
    'loc' ':' location ','
    'children' ':' '[' exp ',' exp ',' exp ']'
'}'
{
    JavaParserActions.exp_ternary $8 $13 $15 $17
}

-- ************
-- *          *
-- * exp_unop *
-- *          *
-- ************
exp_unop:
exp_not { $1 }

exp_not:
'{'
    'kind' ':' 'LOGICAL_COMPLEMENT' ','
    'loc' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    JavaParserActions.expUnop "not" $8 $13
}

-- *************
-- *           *
-- * stmt_body *
-- *           *
-- *************
stmt_body:
stmt_block  { $1 } |
stmt_exp    { $1 } |
stmt_return { $1 } |
stmt_if     { $1 } |
stmt_vardec { $1 } |
stmt_try    { $1 }

-- **************
-- *            *
-- * stmt_block *
-- *            *
-- **************
stmt_block:
'{'
    'kind' ':' 'BLOCK' ','
    'loc' ':' location ','
    'children' ':' possibly_empty_commalistof(stmt_body)
'}'
{
    JavaParserActions.stmtBlock $8 $12
}

-- ************
-- *          *
-- * stmt_exp *
-- *          *
-- ************
stmt_exp:
'{'
    'kind' ':' 'EXPRESSION_STATEMENT' ','
    'loc' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    JavaParserActions.stmtExp $13
}

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return:
'{'
    'kind' ':' 'RETURN' ','
    'loc' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    JavaParserActions.stmtReturn $8 $13
}

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if:
'{'
    'kind' ':' 'IF' ','
    'loc' ':' location ','
    'cond' ':' exp ','
    'body' ':' stmt_body ','
    'children' ':' possibly_empty_commalistof(stmt_body)
'}'
{
    JavaParserActions.stmtIf $8 $12 $16 $20
}

-- ************
-- *          *
-- * stmt_try *
-- *          *
-- ************
stmt_try:
'{'
    'kind' ':' 'TRY' ','
    'loc' ':' location ','
    'children' ':' '[' stmt_block ',' catch_part ']'
'}'
{
    JavaParserActions.stmtTry $8 $13 $15
}

-- **************
-- *            *
-- * catch_part *
-- *            *
-- **************
catch_part:
'{'
    'kind' ':' 'CATCH' ','
    'loc' ':' location ','
    'children' ':' '[' stmt_vardec ',' stmt_block ']'
'}'
{
    $15
}

-- **************
-- *            *
-- * exp_assign *
-- *            *
-- **************
exp_assign:
'{'
    'kind' ':' 'ASSIGNMENT' ','
    'loc' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    JavaParserActions.expAssign $8 $13 $15
}

-- ****************
-- *              *
-- * stmt_package *
-- *              *
-- ****************
stmt_package:
'{'
    'kind' ':' 'PACKAGE' ','
    'loc' ':' location ','
    'children' ':' '[' var ']'
'}'
{
    JavaParserActions.stmtPackage $8 $13
}

-- ***************
-- *             *
-- * stmt_import *
-- *             *
-- ***************
stmt_import:
'{'
    'kind' ':' 'IMPORT' ','
    'loc' ':' location ','
    'children' ':' '[' var ']'
'}'
{
    JavaParserActions.stmtImport $13 $8
}

var:
simple_var { $1} |
field_var { $1}

-- **************
-- *            *
-- * simple_var *
-- *            *
-- **************
simple_var:
'{'
    'kind' ':' 'IDENTIFIER' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'children' ':' '[' ']'
'}'
{
    JavaParserActions.simpleVar $8 $12
}

-- *************
-- *           *
-- * field_var *
-- *           *
-- *************
field_var:
'{'
    'kind' ':' 'MEMBER_SELECT' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'children' ':' '[' exp ']'
'}'
{
    JavaParserActions.fieldVar $8 $12 $17
}

-- ***********
-- *         *
-- * tokenID *
-- *         *
-- ***********
tokenID:
ID            { tokIDValue $1 } |
'annotations' { "annotations"  }

-- ************
-- *          *
-- * location *
-- *          *
-- ************
location:
'{'
    'startLine' ':' INT ','
    'startCol' ':' INT ','
    'endLine' ':' INT ','
    'endCol' ':' INT
'}'
{
    Location
    {
        Location.filename = getFilename $1,
        lineStart = fromIntegral (tokIntValue $4),
        colStart  = fromIntegral (tokIntValue $8),
        lineEnd   = fromIntegral (tokIntValue $12),
        colEnd    = fromIntegral (tokIntValue $16)
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
