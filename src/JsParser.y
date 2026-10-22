{
{-# OPTIONS -Werror=missing-fields #-}

module JsParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import JsLexer
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
%name parse

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

-- ***************
-- *             *
-- * dec or stmt *
-- *             *
-- ***************
dec_or_stmt:
stmt { Right $1 }

-- ***************
-- *             *
-- * dec_var_tag *
-- *             *
-- ***************
dec_var_tag_1:
'{'
    'type' ':' 'VariableDeclarator' ','
    'id' ':' identifier ','
    'init' ':' ornull(exp) ','
    'loc' ':' location
'}'
{
    case (require $12 $8) of {
        Nothing -> non_require $8 $12 $16;
        Just import_stmt -> import_stmt
    } 
}

array_pattern_vars:
'{'
    'type' ':' 'ArrayPattern' ','
    'elements' ':' '[' commalistof(identifier) ']' ','
    'loc' ':' location
'}'
{
    [ Token.VarName v | v <- $9 ]
}

dec_var_tag_2:
'{'
    'type' ':' 'VariableDeclarator' ','
    'id' ':' array_pattern_vars ','
    'init' ':' exp ','
    'loc' ':' location
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = assignify $16 $12 $8,
        Ast.stmtBlockLocation = $16
    }
}

property_for_vardec:
'{'
    'type' ':' 'Property' ','
    'key' ':' identifier ','
    'computed' ':' 'false' ','
    'value' ':' exp ','
    'kind' ':' 'init' ','
    'method' ':' 'false' ','
    'shorthand' ':' bool ','
    'loc' ':' location
'}'
{
    Token.VarName $8
}

object_pattern:
'{'
    'type' ':' 'ObjectPattern' ','
    'properties' ':' '[' commalistof(property_for_vardec) ']' ','
    'loc' ':' location
'}'
{
    $9
}

dec_var_tag_3:
'{'
    'type' ':' 'VariableDeclarator' ','
    'id' ':' object_pattern ','
    'init' ':' exp ','
    'loc' ':' location
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = assignify $16 $12 $8,
        Ast.stmtBlockLocation = $16
    }
}

dec_var_tag:
dec_var_tag_1 { $1 } |
dec_var_tag_2 { $1 } |
dec_var_tag_3 { $1 }

-- ***********
-- *         *
-- * dec_var *
-- *         *
-- ***********
dec_var:
'{'
    'type' ':' 'VariableDeclaration' ','
    'declarations' ':' '[' commalistof(dec_var_tag) ']' ','
    'kind' ':' tokenID ','
    'loc' ':' location
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent {
        Ast.stmtBlockContent = $9,
        Ast.stmtBlockLocation = $18
    }
}

-- ************
-- *          *
-- * location *
-- *          *
-- ************
location:
'{'
    'start' ':' '{' 'line' ':' INT ',' 'column' ':' INT '}' ','
    'end'   ':' '{' 'line' ':' INT ',' 'column' ':' INT '}'
'}'
{
    Location
    {
        Location.filename = Location.filename (location $7),
        lineStart = fromIntegral (tokIntValue $7),
        colStart = fromIntegral (1 + (tokIntValue $11)),
        lineEnd = fromIntegral (tokIntValue $19),
        colEnd = fromIntegral (1 + (tokIntValue $23))
    }
}

-- ************
-- *          *
-- * token ID *
-- *          *
-- ************
tokenID:
ID       { tokIDValue $1 } |
'end'    { "end"         } |
'name'   { "name"        } |
'type'   { "type"        } |
'start'  { "start"       } |
'params' { "<params>"    } |
'object' { "object"      }

-- **************
-- *            *
-- * identifier *
-- *            *
-- **************
identifier:
'{'
    'type' ':' 'Identifier' ','
    'name' ':' tokenID ','
    'loc' ':' location
'}'
{
    Token.Named
    {
        Token.content = unquote $8,
        Token.location = $12
    }
}

-- *********
-- *       *
-- * param *
-- *       *
-- *********
param_1:
'{'
    'type' ':' 'Identifier' ','
    'name' ':' tokenID ','
    'loc' ':' location
'}'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $ Token.Named (unquote $8) $12,
        Ast.paramNominalType = Nothing,
        Ast.paramSerialIdx = 156,
        Ast.paramAnnotations = []
    }
}

-- *********
-- *       *
-- * param *
-- *       *
-- *********
param_2:
'{'
    'type' ':' 'AssignmentPattern' ','
    'left' ':' identifier ','
    'right' ':' exp ','
    'loc' ':' location
'}'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $8,
        Ast.paramNominalType = Just (varme (Token.Named "any" $16)),
        Ast.paramSerialIdx = 0,
        Ast.paramAnnotations = []
    }
}

-- *********
-- *       *
-- * param *
-- *       *
-- *********
param_3:
'{'
    'type' ':' 'ObjectPattern' ','
    'properties' ':' '[' commalistof(property) ']' ','
    'loc' ':' location
'}'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName ( Token.Named "Moshe" $14),
        Ast.paramNominalType = Just (varme (Token.Named "any" $14)),
        Ast.paramSerialIdx = 0,
        Ast.paramAnnotations = []
    }
}

param:
param_1 { $1 } |
param_2 { $1 } |
param_3 { $1 }

-- ***********
-- *         *
-- * exp_var *
-- *         *
-- ***********
exp_var:
var
{
    ExpVar $ ExpVarContent
    {
        actualExpVar = $1
    }
}

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop:
'{'
    'type' ':' 'BinaryExpression' ','
    'operator' ':' operator ','
    'left' ':' exp ','
    'right' ':' exp ','
    'loc' ':' location
'}'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $12,
        Ast.expBinopRight = $16,
        Ast.expBinopOperator = $8,
        Ast.expBinopLocation = $20
    }
}

-- ******************
-- *                *
-- * field variable *
-- *                *
-- ******************
var_field:
'{'
    'type' ':' 'MemberExpression' ','
    'computed' ':' bool ','
    'object' ':' exp ','
    'property' ':' identifier ','
    'loc' ':' location
'}'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $12,
        Ast.varFieldName = Token.FieldName $16,
        Ast.varFieldLocation = $20
    }
}

-- *******************
-- *                 *
-- * simple variable *
-- *                 *
-- *******************
var_simple:
identifier
{
    Ast.VarSimple $ Ast.VarSimpleContent
    {
        Ast.varName = Token.VarName $1
    }
}

-- ************
-- *          *
-- * variable *
-- *          *
-- ************
var:
var_simple { $1 } |
var_field  { $1 }

-- **************
-- *            *
-- * exp_assign *
-- *            *
-- **************
exp_assign:
'{'
    'type' ':' 'AssignExpression' ','
    'operator' ':' operator ','
    'left' ':' var ','
    'right' ':' exp ','
    'loc' ':' location
'}'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        stmtAssignLhs = $12,
        stmtAssignRhs = $16
    }
}

-- **************
-- *            *
-- * exp_assign *
-- *            *
-- **************
exp_assign_tag:
'{'
    'type' ':' 'ExpressionStatement' ','
    'expression' ':' exp_assign ','
    'loc' ':' location
'}'
{
    $8
}

-- ***********
-- *         *
-- * exp_str *
-- *         *
-- ***********
str:
'{'
    'type' ':' 'Literal' ','
    'value' ':' ID ','
    'raw' ':' QUOTED_STR ','
    'loc' ':' location
'}'
{
    Token.ConstStr (unquote (tokIDValue $8)) $16
}

exp_str: str { Ast.ExpStr $ Ast.ExpStrContent $1 }

-- ***********
-- *         *
-- * exp_int *
-- *         *
-- ***********
exp_int:
'{'
    'type' ':' 'Literal' ','
    'value' ':' INT ','
    'raw' ':' QUOTED_INT ','
    'loc' ':' location
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent
    {
        expIntValue = Token.ConstInt
        {
            Token.constIntValue = tokIntValue $8,
            Token.constIntLocation = $16
        }
    }
}

-- ************
-- *          *
-- * exp_bool *
-- *          *
-- ************
exp_bool:
'{'
    'type' ':' 'Literal' ','
    'value' ':' bool ','
    'raw' ':' QUOTED_BOOL ','
    'loc' ':' location
'}'
{
    Ast.ExpBool $ Ast.ExpBoolContent
    {
        expBoolValue = Token.ConstBool
        {
            Token.constBoolValue = $8,
            Token.constBoolLocation = $16
        }
    }
}

exp_null:
'{'
    'type' ':' 'Literal' ','
    'value' ':' 'null' ','
    'raw' ':' QUOTED_NULL ','
    'loc' ':' location
'}'
{
    Ast.ExpNull (Ast.ExpNullContent (Token.ConstNull $16))
}

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************
expressions: { [] } | commalistof(exp) { $1 }

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call:
'{'
    'type' ':' 'CallExpression' ','
    'callee' ':' exp ','
    'arguments' ':' '[' expressions ']' ','
    'loc' ':' location
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = $8,
        Ast.args = $13,
        Ast.expCallLocation = $18
    }
}

-- **************
-- *            *
-- * exp_lambda *
-- *            *
-- **************
exp_lambda:
'{'
    'type' ':' 'LambdaExpression' ','
    'id' ':' 'null' ','
    'params' ':' params ','
    'body' ':' stmt ','
    'generator' ':' bool ','
    'expression' ':' bool ','
    'async' ':' bool ','
    'loc' ':' location
'}'
{
    Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        expLambdaParams = $12,
        expLambdaBody = [$16],
        expLambdaLocation = $32
    }
}

-- *******************
-- *                 *
-- * fstring_element *
-- *                 *
-- *******************
fstring_element:
'{'
    'type' ':' 'TemplateElement' ','
    'value' ':' '{'
        'raw' ':' ID ','
        'cooked' ':' ID
    '}' ','
    'tail' ':' bool ','
    'loc' ':' location
'}'
{
    Nothing    
}

-- ***************
-- *             *
-- * exp_fstring *
-- *             *
-- ***************
exp_fstring:
'{'
    'type' ':' 'TemplateLiteral' ','
    'quasis' ':' '[' commalistof(fstring_element) ']' ','
    'expressions' ':' '[' expressions ']' ','
    'loc' ':' location
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent
        {
            Ast.varName = Token.VarName $ Token.Named "fstring" $20
        },
        Ast.args = $15,
        Ast.expCallLocation = $20
    }
}

-- ***********
-- *         *
-- * exp_new *
-- *         *
-- ***********
exp_new:
'{'
    'type' ':' 'NewExpression' ','
    'callee' ':' exp ','
    'arguments' ':' '[' expressions ']' ','
    'loc' ':' location
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = $8,
        Ast.args = $13,
        Ast.expCallLocation = $18
    }
}

property:
'{'
    'type' ':' 'Property' ','
    'key' ':' identifier ','
    'computed' ':' 'false' ','
    'value' ':' exp ','
    'kind' ':' 'init' ','
    'method' ':' 'false' ','
    'shorthand' ':' bool ','
    'loc' ':' location
'}'
{
    $16
}

exp_object:
'{'
    'type' ':' 'ObjectExpression' ','
    'properties' ':' '[' commalistof(property) ']' ','
    'loc' ':' location
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "dictify",
            Token.location = $14
        },
        Ast.args = $9,
        Ast.expCallLocation = $14
    }
}

exp_relop:
'{'
    'type' ':' 'LogicalExpression' ','
    'operator' ':' operator ','
    'left' ':' exp ','
    'right' ':' exp ','
    'loc' ':' location
'}'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $12,
        Ast.expBinopRight = $16,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $20
    }
}

exp_await:
'{'
    'type' ':' 'AwaitExpression' ','
    'argument' ':' exp ','
    'loc' ':' location
'}'
{
    $8
}

exp_array:
'{'
    'type' ':' 'ArrayExpression' ','
    'elements' ':' '[' commalistof(exp) ']' ','
    'loc' ':' location
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "arrayify" $14,
        Ast.args = $9,
        Ast.expCallLocation = $14
    }
}

exp_unary:
'{'
    'type' ':' 'UnaryExpression' ','
    'operator' ':' operator ','
    'argument' ':' exp ','
    'prefix' ':' bool ','
    'loc' ':' location
'}'
{
    $12
}

exp_function:
'{'
    'type' ':' 'FunctionExpression' ','
    'id' ':' 'null' ','
    'params' ':' params ','
    'body' ':' stmt ','
    'generator' ':' bool ','
    'expression' ':' bool ','
    'async' ':' bool ','
    'loc' ':' location
'}'
{
    Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        expLambdaParams = $12,
        expLambdaBody = [$16],
        expLambdaLocation = $32
    }
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_int     { $1 } |
exp_str     { $1 } |
exp_var     { $1 } |
exp_new     { $1 } |
exp_bool    { $1 } |
exp_null    { $1 } |
exp_call    { $1 } |
exp_binop   { $1 } |
exp_array   { $1 } |
exp_relop   { $1 } |
exp_await   { $1 } |
exp_lambda  { $1 } |
exp_unary   { $1 } |
exp_object  { $1 } |
exp_function { $1 } |
exp_fstring { $1 }

-- ************
-- *          *
-- * stmt_for *
-- *          *
-- ************
stmt_for:
'{'
    'type' ':' 'ForStatement' ','
    'init' ':' exp ','
    'test' ':' exp ','
    'update' ':' stmt ','
    'body' ':' stmt ','
    'loc' ':' location
'}'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $12,
        Ast.stmtWhileBody = [$20],
        Ast.stmtWhileLocation = $24
    }
}

-- ************
-- *          *
-- * operator *
-- *          *
-- ************
operator:
'++' { Ast.TIMES } |
'==' { Ast.TIMES } |
'!==' { Ast.TIMES } |
'&&' { Ast.TIMES } |
'in' { Ast.TIMES } |
'||' { Ast.TIMES } |
'*'  { Ast.TIMES } |
'!'  { Ast.TIMES } |
'<'  { Ast.TIMES } |
'+'  { Ast.TIMES } |
'>'  { Ast.TIMES } |
'='  { Ast.TIMES }

-- ********
-- *      *
-- * bool *
-- *      *
-- ********
bool:
'true'  { True  } |
'false' { False }

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return:
'{'
    'type' ':' 'ReturnStatement' ','
    'argument' ':' exp ','
    'loc' ':' location
'}'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = Just $8,
        Ast.stmtReturnLocation = $12
    }
}

-- ***************
-- *             *
-- * stmt_update *
-- *             *
-- ***************
stmt_update:
'{'
    'type' ':' 'UpdateExpression' ','
    'operator' ':' operator ','
    'argument' ':' identifier ','
    'prefix' ':' bool ','
    'loc' ':' location
'}'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $12,
        Ast.stmtAssignRhs = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 0 $20
    }
}

-- ***************
-- *             *
-- * stmt_assign *
-- *             *
-- ***************
stmt_assign:
stmt_update    { $1 } |
exp_assign     { $1 } |
exp_assign_tag { $1 }

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if:
'{'
    'type' ':' 'IfStatement' ','
    'test' ':' exp ','
    'consequent' ':' stmt ','
    'alternate' ':' ornull(stmt) ','
    'loc' ':' location
'}'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $8,
        Ast.stmtIfBody = [$12],
        Ast.stmtElseBody = case $16 of { Nothing -> []; Just s -> [s] },
        Ast.stmtIfLocation = $20
    }
}

-- *************
-- *           *
-- * stmt_call *
-- *           *
-- *************
stmt_exp:
'{'
    'type' ':' 'ExpressionStatement' ','
    'expression' ':' exp ','
    'loc' ':' location
'}'
{
    Ast.StmtExp $8
}

-- ***************
-- *             *
-- * stmt_decvar *
-- *             *
-- ***************
stmt_decvar:
dec_var
{
    $1
}

specifier_default:
'{'
    'type' ':' 'ImportDefaultSpecifier' ','
    'local' ':' identifier ','
    'loc' ':' location
'}'
{
    Ast.StmtImportContent
    {
        Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent ""),
        Ast.stmtImportSpecific = Just (Ast.ImportSpecific "default"),
        Ast.stmtImportAlias = Just (Ast.ImportAlias (Token.content $8)),
        Ast.stmtImportLocation = $12
    } 
}

specifier_named:
'{'
    'type' ':' 'ImportSpecifier' ','
    'local' ':' identifier ','
    'imported' ':' identifier ','
    'loc' ':' location
'}'
{
    Ast.StmtImportContent
    {
        Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent ""),
        Ast.stmtImportSpecific = Just (Ast.ImportSpecific (Token.content $8)),
        Ast.stmtImportAlias = Just (Ast.ImportAlias (Token.content $8)),
        Ast.stmtImportLocation = $16
    }
}

specifier:
specifier_default { $1 } |
specifier_named   { $1 }

stmt_import:
'{'
    'type' ':' 'ImportDeclaration' ','
    'specifiers' ':' '[' commalistof(specifier) ']' ','
    'source' ':' str ','
    'loc' ':' location
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent (importify $18 (Token.constStrValue $14) $9) $18
}

stmt_try:
'{'
    'type' ':' 'TryStatement' ','
    'block' ':' stmt ','
    'handler' ':' stmt ','
    'finalizer' ':' 'null' ','
    'loc' ':' location
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = [$8],
        Ast.stmtBlockLocation = $20
    }
}

stmt_block:
'{'
    'type' ':' 'BlockStatement' ','
    'body' ':' '[' commalistof(stmt) ']' ','
    'loc' ':' location
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = $9,
        Ast.stmtBlockLocation = $14
    }
}

stmt_catch:
'{'
    'type' ':' 'CatchClause' ','
    'param' ':' param ','
    'body' ':' stmt ','
    'loc' ':' location
'}'
{
    $12
}

stmt_export:
'{'
    'type' ':' 'ExportDefaultDeclaration' ','
    'declaration' ':' exp ','
    'loc' ':' location
'}'
{
    Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = Token.VarName (Token.Named "default" $12),
        Ast.stmtVardecNominalType = Just (varme (Token.Named "any" $12)),
        Ast.stmtVardecInitValue = Just $8,
        Ast.stmtVardecLocation = $12
    }
}

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if     { $1 } |
stmt_for    { $1 } |
stmt_exp    { $1 } |
stmt_try    { $1 } |
stmt_block  { $1 } |
stmt_catch  { $1 } |
stmt_assign { $1 } |
stmt_import { $1 } |
stmt_export { $1 } |
stmt_decvar { $1 } |
stmt_return { $1 } |
dec_function { $1 }

-- **********
-- *        *
-- * params *
-- *        *
-- **********
params: '[' ']' { [] } | '[' commalistof(param) ']' { $2 }

-- ****************
-- *              *
-- * dec_function *
-- *              *
-- ****************
dec_function:
'{'
    'type' ':' 'FunctionDeclaration' ','
    'id' ':' identifier ','
    'params' ':' params ','
    'body' ':' stmt ','
    'generator' ':' bool ','
    'expression' ':' bool ','
    'async' ':' bool ','
    'loc' ':' location
'}'
{
    Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Just (varme (Token.Named "any" $32)),
        Ast.stmtFuncName = Token.FuncName $8,
        Ast.stmtFuncParams = $12,
        Ast.stmtFuncBody = [$16],
        Ast.stmtFuncAnnotations = [],
        Ast.stmtFuncLocation = $32
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

