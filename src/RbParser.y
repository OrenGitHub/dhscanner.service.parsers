{
{-# OPTIONS -Werror=missing-fields #-}

module RbParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import RbLexer
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
import Data.List ( map, singleton, foldl', filter )
import Data.Map ( fromList, empty, map )

}

-- ***********************
-- *                     *
-- * API function: parse *
-- *                     *
-- ***********************
%name parse

-- *************
-- * tokentype *
-- *************
%tokentype { AlexTokenTag }

-- *********
-- * monad *
-- *********
%monad { Alex }

-- *********
-- * lexer *
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

-- *********************
-- *                   *
-- * reserved keywords *
-- *                   *
-- *********************

'kw'                    { AlexTokenTag AlexRawToken_KW              _ }
'id'                    { AlexTokenTag AlexRawToken_KWID            _ }
'int'                   { AlexTokenTag AlexRawToken_KWINT           _ }
'op'                    { AlexTokenTag AlexRawToken_OP              _ }
'end'                   { AlexTokenTag AlexRawToken_END             _ }
'raw'                   { AlexTokenTag AlexRawToken_RAW             _ }
'self'                  { AlexTokenTag AlexRawToken_SELF            _ }
'gvar'                  { AlexTokenTag AlexRawToken_GVAR            _ }
'begin'                 { AlexTokenTag AlexRawToken_BEGIN           _ }
'break'                 { AlexTokenTag AlexRawToken_BREAK           _ }
'call'                  { AlexTokenTag AlexRawToken_CALL            _ }
'vcall'                 { AlexTokenTag AlexRawToken_VCALL           _ }
'superclass'            { AlexTokenTag AlexRawToken_SUPER           _ }
'hash'                  { AlexTokenTag AlexRawToken_DICT2           _ }
'bare_assoc_hash'       { AlexTokenTag AlexRawToken_DICT            _ }
'assoc'                 { AlexTokenTag AlexRawToken_ASSOC           _ }
'assoc_splat'           { AlexTokenTag AlexRawToken_ASSOC2          _ }
'void_stmt'             { AlexTokenTag AlexRawToken_STMT_VOID       _ }
'label'                 { AlexTokenTag AlexRawToken_LABEL           _ }
'keywords'              { AlexTokenTag AlexRawToken_KEYWORDS        _ }
'block'                 { AlexTokenTag AlexRawToken_BLOCK           _ }
'block_var'             { AlexTokenTag AlexRawToken_BLOCK_VAR       _ }
'blockarg'              { AlexTokenTag AlexRawToken_BLOCK_ARG       _ }
'module'                { AlexTokenTag AlexRawToken_MODULE          _ }
'method_add_block'      { AlexTokenTag AlexRawToken_BLOCK2          _ }
'period'                { AlexTokenTag AlexRawToken_PERIOD          _ }
'parent'                { AlexTokenTag AlexRawToken_PARENT          _ }
'receiver'              { AlexTokenTag AlexRawToken_RECEIVER        _ }
'assocs'                { AlexTokenTag AlexRawToken_ASSOCS          _ }
'string_literal'        { AlexTokenTag AlexRawToken_STRING1         _ }
'symbol_literal'        { AlexTokenTag AlexRawToken_STRING3         _ }
'string_embexpr'        { AlexTokenTag AlexRawToken_STRING4         _ }
'tstring_content'       { AlexTokenTag AlexRawToken_STRING2         _ }
'class'                 { AlexTokenTag AlexRawToken_CLASS           _ }
'sclass'                { AlexTokenTag AlexRawToken_SCLASS          _ }
'assign'                { AlexTokenTag AlexRawToken_ASSIGN          _ }
'massign'               { AlexTokenTag AlexRawToken_ASSIGN3         _ }
'opassign'              { AlexTokenTag AlexRawToken_ASSIGN2         _ }
'location'              { AlexTokenTag AlexRawToken_LOC             _ }
'command'               { AlexTokenTag AlexRawToken_COMMAND         _ }
'command_call'          { AlexTokenTag AlexRawToken_COMMAND2        _ }
'message'               { AlexTokenTag AlexRawToken_MESSAGE         _ }
'comment'               { AlexTokenTag AlexRawToken_COMMENT         _ }
'constant'              { AlexTokenTag AlexRawToken_CONSTANT        _ }
'const_ref'             { AlexTokenTag AlexRawToken_CONSTANT2       _ }
'aref_field'            { AlexTokenTag AlexRawToken_AREF_FIELD      _ }
'const_path_ref'        { AlexTokenTag AlexRawToken_CONSTANT4       _ }
'top_const_ref'         { AlexTokenTag AlexRawToken_CONSTANT5       _ }
'const'                 { AlexTokenTag AlexRawToken_CONSTANT3       _ }
'key'                   { AlexTokenTag AlexRawToken_KEY             _ }
'var_field'             { AlexTokenTag AlexRawToken_VAR             _ }
'field'                 { AlexTokenTag AlexRawToken_FIELD           _ }
'null'                  { AlexTokenTag AlexRawToken_NULL            _ }
'test'                  { AlexTokenTag AlexRawToken_TEST            _ }
'line'                  { AlexTokenTag AlexRawToken_LINE            _ }
'true'                  { AlexTokenTag AlexRawToken_TRUE            _ }
'args'                  { AlexTokenTag AlexRawToken_ARGS            _ }
'arg_star'              { AlexTokenTag AlexRawToken_ARG_STAR        _ }
'regexp_literal'        { AlexTokenTag AlexRawToken_REGEX           _ }
'name'                  { AlexTokenTag AlexRawToken_NAME            _ }
'expr'                  { AlexTokenTag AlexRawToken_EXPR            _ }
'Name'                  { AlexTokenTag AlexRawToken_MAME            _ }
'type'                  { AlexTokenTag AlexRawToken_TYPE            _ }
'left'                  { AlexTokenTag AlexRawToken_LEFT            _ }
'mlhs'                  { AlexTokenTag AlexRawToken_LEFT2           _ }
'arg_block'             { AlexTokenTag AlexRawToken_ARG_BLOCK       _ }
'keyword_rest'          { AlexTokenTag AlexRawToken_REST            _ }
'kwrest_param'          { AlexTokenTag AlexRawToken_REST2           _ }
'dyna_symbol'           { AlexTokenTag AlexRawToken_DYNA            _ }
'exception'             { AlexTokenTag AlexRawToken_EXCEPTION       _ }
'exceptions'            { AlexTokenTag AlexRawToken_EXCEPTIONS      _ }
'rescue'                { AlexTokenTag AlexRawToken_RESCUE          _ }
'rescue_clause'         { AlexTokenTag AlexRawToken_RESCUE2         _ }
'else_clause'           { AlexTokenTag AlexRawToken_RESCUE4         _ }
'rescue_ex'             { AlexTokenTag AlexRawToken_RESCUE3         _ }
'variable'              { AlexTokenTag AlexRawToken_VARIABLE        _ }
'backref'               { AlexTokenTag AlexRawToken_BACKREF         _ }
'next'                  { AlexTokenTag AlexRawToken_NEXT            _ }
'loop'                  { AlexTokenTag AlexRawToken_LOOP            _ }
'init'                  { AlexTokenTag AlexRawToken_INIT            _ }
'cond'                  { AlexTokenTag AlexRawToken_COND            _ }
'body'                  { AlexTokenTag AlexRawToken_BODY            _ }
'update'                { AlexTokenTag AlexRawToken_UPDATE          _ }
'parts'                 { AlexTokenTag AlexRawToken_PARTS           _ }
'options'               { AlexTokenTag AlexRawToken_OPTIONS         _ }
'range'                 { AlexTokenTag AlexRawToken_RANGE           _ }
'index'                 { AlexTokenTag AlexRawToken_INDEX           _ }
'paren'                 { AlexTokenTag AlexRawToken_PAREN           _ }
'false'                 { AlexTokenTag AlexRawToken_FALSE           _ }
'falsy'                 { AlexTokenTag AlexRawToken_FALSY           _ }
'truthy'                { AlexTokenTag AlexRawToken_TRUTHY          _ }
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
'target'                { AlexTokenTag AlexRawToken_TARGET          _ }
'Literal'               { AlexTokenTag AlexRawToken_LITERAL         _ }
'program'               { AlexTokenTag AlexRawToken_PROGRAM         _ }
'property'              { AlexTokenTag AlexRawToken_PROPERTY        _ }
'computed'              { AlexTokenTag AlexRawToken_COMPUTED        _ }
'contents'              { AlexTokenTag AlexRawToken_CONTENTS        _ }
'operator'              { AlexTokenTag AlexRawToken_OPERATOR        _ }
'comments'              { AlexTokenTag AlexRawToken_COMMENTS        _ }
'predicate'             { AlexTokenTag AlexRawToken_PREDICATE       _ }
'requireds'             { AlexTokenTag AlexRawToken_REQUIREDS       _ }
'optionals'             { AlexTokenTag AlexRawToken_OPTIONALS       _ }
'alternate'             { AlexTokenTag AlexRawToken_ALTERNATE       _ }
'consequent'            { AlexTokenTag AlexRawToken_CONSEQUENT      _ }
'argument'              { AlexTokenTag AlexRawToken_ARGUMENT        _ }
'bodystmt'              { AlexTokenTag AlexRawToken_BODYSTMT        _ }
'statement'             { AlexTokenTag AlexRawToken_STATEMENT       _ }
'arguments'             { AlexTokenTag AlexRawToken_ARGUMENTS       _ }
'arg_paren'             { AlexTokenTag AlexRawToken_ARGUMENTS2      _ }
'collection'            { AlexTokenTag AlexRawToken_COLLECTION      _ }
'generator'             { AlexTokenTag AlexRawToken_GENERATOR       _ }
'expression'            { AlexTokenTag AlexRawToken_EXPRESSION      _ }
'async'                 { AlexTokenTag AlexRawToken_ASYNC           _ }
'callee'                { AlexTokenTag AlexRawToken_CALLEE          _ }
'sourceType'            { AlexTokenTag AlexRawToken_SRC_TYPE        _ }
'var_ref'               { AlexTokenTag AlexRawToken_EXPR_VAR        _ }
'aref'                  { AlexTokenTag AlexRawToken_EXPR_SUBSCRIPT  _ }
'Stmt_Expr'             { AlexTokenTag AlexRawToken_STMT_EXPR       _ }
'Scalar_Int'            { AlexTokenTag AlexRawToken_SCALAR_INT      _ }
'ident'                 { AlexTokenTag AlexRawToken_IDENTIFIER      _ }
'yield'                 { AlexTokenTag AlexRawToken_YIELD           _ }
'ivar'                  { AlexTokenTag AlexRawToken_IVAR            _ }
'returnType'            { AlexTokenTag AlexRawToken_RETURN_TYPE     _ }
'Stmt_Function'         { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ }
'def'                   { AlexTokenTag AlexRawToken_FUNCTION_DEC    _ }

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

'CallExpression'   { AlexTokenTag AlexRawToken_EXPR_CALL   _ }
'MemberExpression' { AlexTokenTag AlexRawToken_EXPR_MEMBER _ }
'binary'           { AlexTokenTag AlexRawToken_EXPR_BINOP  _ }
'unary'            { AlexTokenTag AlexRawToken_EXPR_UNOP   _ }
'UpdateExpression' { AlexTokenTag AlexRawToken_EXPR_UPDATE _ }
'AssignExpression' { AlexTokenTag AlexRawToken_EXPR_ASSIGN _ }

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

'if'                  { AlexTokenTag AlexRawToken_STMT_IF     _ }
'else'                { AlexTokenTag AlexRawToken_STMT_ELSE   _ }
'elif'                { AlexTokenTag AlexRawToken_STMT_ELSE2  _ }
'if_op'               { AlexTokenTag AlexRawToken_STMT_IF2    _ }
'ForStatement'        { AlexTokenTag AlexRawToken_STMT_FOR    _ }
'BlockStatement'      { AlexTokenTag AlexRawToken_STMT_BLOCK  _ }
'return'              { AlexTokenTag AlexRawToken_STMT_RETURN _ }
'unless'              { AlexTokenTag AlexRawToken_STMT_UNLESS _ }
'ExpressionStatement' { AlexTokenTag AlexRawToken_STMT_EXP    _ }
'string_concat'       { AlexTokenTag AlexRawToken_string_concat _ }
'lambda'              { AlexTokenTag AlexRawToken_lambda _ }
'lambda_var'          { AlexTokenTag AlexRawToken_lambda_var _ }
-- last keywords first part

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

'<'   { AlexTokenTag AlexRawToken_OP_LT       _ }
'>'   { AlexTokenTag AlexRawToken_OP_GT       _ }
'>='  { AlexTokenTag AlexRawToken_OP_GEQ      _ }
'<<'  { AlexTokenTag AlexRawToken_OP_SHL      _ }
'=='  { AlexTokenTag AlexRawToken_OP_EQ       _ }
'+='  { AlexTokenTag AlexRawToken_OP_PLUSEQ   _ }
'!~'  { AlexTokenTag AlexRawToken_OP_NEQ      _ }
'!'   { AlexTokenTag AlexRawToken_OP_BANG     _ }
'='   { AlexTokenTag AlexRawToken_OP_ASSIGN   _ }
'.'   { AlexTokenTag AlexRawToken_OP_DOT      _ }
'+'   { AlexTokenTag AlexRawToken_OP_PLUS     _ }
'-'   { AlexTokenTag AlexRawToken_OP_MINUS    _ }
'*'   { AlexTokenTag AlexRawToken_OP_TIMES    _ }
'..'  { AlexTokenTag AlexRawToken_OP_DOTDOT   _ }
'::'  { AlexTokenTag AlexRawToken_OP_COLON2   _ }
'%'   { AlexTokenTag AlexRawToken_OP_PERCENT  _ }
'++'  { AlexTokenTag AlexRawToken_OP_PLUSPLUS _ }
'||'  { AlexTokenTag AlexRawToken_OP_OR       _ }
'&&'  { AlexTokenTag AlexRawToken_OP_AND      _ }
'||=' { AlexTokenTag AlexRawToken_OP_OR2      _ }
'and' { AlexTokenTag AlexRawToken_OP_AND2     _ }

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
ornull(a):      a { Just $1 } | 'null' { Nothing } 

-- ******************
-- *                *
-- * optional rules *
-- *                *
-- ******************
optional(a): { Nothing } | a { Just $1 }

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program:
'{'
    'type' ':' 'program' ','
    'location' ':' location ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Ast.Root
    {
        Ast.filename = getFilename $1,
        Ast.stmts = rights (catMaybes $12)
    }
}

-- ************
-- *          *
-- * location *
-- *          *
-- ************
location: '[' INT ',' INT ',' INT ',' INT ']'
{
    Location
    {
        Location.filename = getFilename $1,
        lineStart = fromIntegral (tokIntValue $2),
        colStart = fromIntegral (tokIntValue $4),
        lineEnd = fromIntegral (tokIntValue $6),
        colEnd = fromIntegral (tokIntValue $8)
    }
}

-- ************
-- *          *
-- * token ID *
-- *          *
-- ************
tokenID:
ID        { unquote (tokIDValue $1) } |
'id'      { "id"                    } |
'self'    { "self"                  } |
'true'    { "true"                  } |
'type'    { "type"                  } |
'call'    { "call"                  } |
'false'   { "false"                 } |
'args'    { "args"                  } |
'value'   { "value"                 } |
'object'  { "object"                } |
'binary'  { "binary"                } |
'message' { "message"               } |
'contents' { "contents"             } |
'name'    { "name"                  } |
'.'       { "."                     } |
'='       { "="                     } |
'+'       { "+"                     } |
'start'   { "start"                 } |
'index'   { "index"                 } |
'update'  { "update"                } |
'params'  { "params"                } |
'label'   { "label"                 } |
'block'   { "block"                 } |
'options' { "options"               } |
'class'   { "class"                 }

-- *******************
-- *                 *
-- * identifier_type *
-- *                 *
-- *******************
identifier_type:
'const' { Nothing } |
'ident' { Nothing } |
'ivar'  { Nothing } |
'gvar'  { Nothing } |
'kw'    { Nothing }

-- **************
-- *            *
-- * identifier *
-- *            *
-- **************
identifier:
'{'
    'type' ':' identifier_type ','
    'location' ':' location ','
    'value' ':' tokenID ','
    'comments' ':' comments 
'}'
{
    Token.Named
    {
        Token.content = $12,
        Token.location = $8
    }
}

-- *********
-- *       *
-- * param *
-- *       *
-- *********
param:
'{'
    'type' ':' 'ident' ','
    'name' ':' tokenID ','
    'location' ':' location
'}'
{
    Token.Named
    {
        Token.content = $8,
        Token.location = $12
    } 
}

-- ***********
-- *         *
-- * exp_var *
-- *         *
-- ***********
exp_var: var { Ast.ExpVar (Ast.ExpVarContent $1) }

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop:
'{'
    'type' ':' 'binary' ','
    'location' ':' location ','
    'left' ':' exp ','
    'operator' ':' actual_op ','
    'right' ':' exp ','
    'comments' ':' comments
'}'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
       Ast.expBinopLeft = $12,
       Ast.expBinopRight = $20,
       Ast.expBinopOperator = $16,
       Ast.expBinopLocation = $8
    }
}

-- ************
-- *          *
-- * exp_unop *
-- *          *
-- ************
exp_unop:
'{'
    'type' ':' 'unary' ','
    'location' ':' location ','
    'operator' ':' actual_op ','
    'statement' ':' exp ','
    'comments' ':' comments
'}'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
       Ast.expBinopLeft = $16,
       Ast.expBinopRight = $16,
       Ast.expBinopOperator = $12,
       Ast.expBinopLocation = $8
    }
}

var_simple_type:
'var_field' { Nothing } |
'const_ref' { Nothing } |
'var_ref'   { Nothing }

var_simple_content:
'value'    { Nothing } |
'constant' { Nothing }

-- **************
-- *            *
-- * var_simple *
-- *            *
-- **************
var_simple:
'{'
    'type' ':' var_simple_type ','
    'location' ':' location ','
    var_simple_content ':' identifier ','
    'comments' ':' comments
'}'
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $12
}

-- *************
-- *           *
-- * var_field *
-- *           *
-- *************
var_field:
'{'
    'type' ':' 'field' ','
    'location' ':' location ','
    'parent' ':' identifier_wrapper ','
    'operator' ':' operator ','
    'name' ':' identifier ','
    'comments' ':' comments
'}'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $12,
        Ast.varFieldName = Token.FieldName $20,
        Ast.varFieldLocation = $8
    }
}

var_subscript_type:
'aref_field' { Nothing } |
'aref'       { Nothing }

-- *****************
-- *               *
-- * var_subscript *
-- *               *
-- *****************
var_subscript:
'{'
    'type' ':' var_subscript_type ','
    'location' ':' location ','
    'collection' ':' exp ','
    'index' ':' exp ','
    'comments' ':' comments
'}'
{
    Ast.VarSubscript $ Ast.VarSubscriptContent
    {
        Ast.varSubscriptLhs = $12,
        Ast.varSubscriptIdx = $16,
        Ast.varSubscriptLocation = $8
    }
}

var_parented_type:
'const_path_ref' { Nothing } |
'top_const_ref'  { Nothing }

-- ****************
-- *              *
-- * var_parented *
-- *              *
-- ****************
var_parented:
'{'
    'type' ':' var_parented_type ','
    'location' ':' location ','
    optional(parent)
    'constant' ':' identifier ','
    'comments' ':' comments
'}'
{
    let e' = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $13 
    in Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = case $10 of { Just e -> e; Nothing -> e' },
        Ast.varFieldName = Token.FieldName $13,
        Ast.varFieldLocation = $8
    }
}

-- ************
-- *          *
-- * variable *
-- *          *
-- ************
var:
var_simple    { $1 } |
var_field     { $1 } |
var_parented  { $1 } |
var_subscript { $1 }

-- ***************
-- *             *
-- * string_part *
-- *             *
-- ***************
string_part_type_1:
'{'
    'type' ':' 'tstring_content' ','
    'location' ':' location ','
    'value' ':' tokenID ','
    'comments' ':' comments
'}'
{
    Ast.ExpStr $ Ast.ExpStrContent $ Token.ConstStr
    {
        Token.constStrValue = $12,
        Token.constStrLocation = $8
    }
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
string_part_type_2:
'{'
    'type' ':' 'string_embexpr' ','
    'location' ':' location ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    case $12 of
        [] -> Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 888 $8
        (stmt:_) -> case stmt of
            (Just (Right (Ast.StmtExp e))) -> e
            _ -> Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 888 $8
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
string_part:
string_part_type_1 { $1 } |
string_part_type_2 { $1 }

-- *************
-- *           *
-- * exp_str_1 *
-- *           *
-- *************
fstring:
'{'
    'type' ':' 'string_literal' ','
    'location' ':' location ','
    'parts' ':' parts ','
    'comments' ':' comments
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "fstring",
            Token.location = $8
        },
        Ast.args = $12,
        Ast.expCallLocation = $8
    }
}

-- *************
-- *           *
-- * exp_str_2 *
-- *           *
-- *************
exp_str:
'{'
    'type' ':' 'symbol_literal' ','
    'location' ':' location ','
    'value' ':' identifier ','
    'comments' ':' comments
'}'
{
    Ast.ExpStr $ Ast.ExpStrContent $ Token.ConstStr
    {
        Token.constStrValue = (Token.content $12),
        Token.constStrLocation = $8
    }
}

-- *******
-- *     *
-- * key *
-- *     *
-- *******
key_type:
'label'          { Nothing } |
'string_literal' { Nothing }

-- *******
-- *     *
-- * key *
-- *     *
-- *******
key_parts: 'parts' ':' '[' commalistof(string_part) ']' ',' { Nothing }

-- *******
-- *     *
-- * key *
-- *     *
-- *******
key_value: 'value' ':' ID ',' { Nothing }

-- *******
-- *     *
-- * key *
-- *     *
-- *******
key:
'{'
    'type' ':' key_type ','
    'location' ':' location ','
    optional(key_parts)
    optional(key_value)
    'comments' ':' comments
'}'
{
    Nothing
}

-- *********
-- *       *
-- * assoc *
-- *       *
-- *********
assoc_type_1:
'{'
    'type' ':' 'assoc' ','
    'location' ':' location ','
    'key' ':' key ','
    'value' ':' exp ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- *********
-- *       *
-- * assoc *
-- *       *
-- *********
assoc_type_2:
'{'
    'type' ':' 'assoc_splat' ','
    'location' ':' location ','
    'value' ':' exp ','
    'comments' ':' comments
'}'
{
    Nothing
}

assoc:
assoc_type_1 { Nothing } |
assoc_type_2 { Nothing }

-- ************
-- *          *
-- * exp_dict *
-- *          *
-- ************
exp_dict_1:
'{'
    'type' ':' 'bare_assoc_hash' ','
    'location' ':' location ','
    'assocs' ':' '[' commalistof(assoc) ']' ','
    'comments' ':' comments
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 9999,
        Token.constIntLocation = $8
    }
}

-- **********
-- *        *
-- * assocs *
-- *        *
-- **********
assocs: 'assocs' ':' '[' commalistof(assoc) ']' ',' { $4 }

-- ************
-- *          *
-- * exp_dict *
-- *          *
-- ************
exp_dict_2:
'{'
    'type' ':' 'hash' ','
    'location' ':' location ','
    optional(assocs)
    'comments' ':' comments
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 9999,
        Token.constIntLocation = $8
    }
}

-- ************
-- *          *
-- * exp_dict *
-- *          *
-- ************
exp_dict:
exp_dict_1 { $1 } |
exp_dict_2 { $1 }

-- *********
-- *       *
-- * parts *
-- *       *
-- *********
parts: '[' ']' { [] } | '[' commalistof(exp) ']' { $2 }

-- ********
-- *      *
-- * args *
-- *      *
-- ********
args:
'{'
    'type' ':' 'args' ','
    'location' ':' location ','
    'parts' ':' parts ','
    'comments' ':' comments
'}'
{
    $12
}

-- *****************
-- *               *
-- * array_content *
-- *               *
-- *****************
array_content: args { $1 } | 'null' { [] }

-- *************
-- *           *
-- * exp_array *
-- *           *
-- *************
exp_array:
'{'
    'type' ':' 'array' ','
    'location' ':' location ','
    'contents' ':' array_content ','
    'comments' ':' comments
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "listify",
            Token.location = $8
        },
        Ast.args = $12,
        Ast.expCallLocation = $8
    }
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
arg_star:
'{'
    'type' ':' 'arg_star' ','
    'location' ':' location ','
    'value' ':' exp ','
    'comments' ':' comments
'}'
{
    $12
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp_vcall:
'{'
    'type' ':' 'vcall' ','
    'location' ':' location ','
    'value' ':' identifier ','
    'comments' ':' comments
'}'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $12
}

-- *********
-- *       *
-- * yield *
-- *       *
-- *********
exp_yield:
'{'
    'type' ':' 'yield' ','
    'location' ':' location ','
    'arguments' ':' 'null' ','
    'comments' ':' comments
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 555,
        Token.constIntLocation = $8
    }
}

-- *********
-- *       *
-- * paren *
-- *       *
-- *********
exp_paren:
'{'
    'type' ':' 'paren' ','
    'location' ':' location ','
    'contents' ':' stmts ','
    'comments' ':' comments
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 555,
        Token.constIntLocation = $8
    }  
}

-- *************
-- *           *
-- * subscript *
-- *           *
-- *************
subscript:
'{'
    'type' ':' 'aref' ','
    'location' ':' location ','
    'collection' ':' exp ','
    'index' ':' exp ','
    'comments' ':' comments
'}'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSubscript $ Ast.VarSubscriptContent
    {
        Ast.varSubscriptLhs = $12,
        Ast.varSubscriptIdx = $16,
        Ast.varSubscriptLocation = $8
    }
}

-- ************
-- *          *
-- * exp_args *
-- *          *
-- ************
exp_args:
'{'
    'type' ':' 'args' ','
    'location' ':' location ','
    'parts' ':' '[' commalistof(exp) ']' ','
    'comments' ':' comments
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "expify",
            Token.location = $8
        },
        Ast.args = $13,
        Ast.expCallLocation = $8
    }
}

-- *************
-- *           *
-- * exp_regex *
-- *           *
-- *************
exp_regex:
'{'
    'type' ':' 'regexp_literal' ','
    'location' ':' location ','
    'parts' ':' '[' commalistof(string_part) ']' ','
    'options' ':' ID ','
    'comments' ':' comments
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 222,
        Token.constIntLocation = $8
    }
}

-- ***********
-- *         *
-- * backref *
-- *         *
-- ***********
backref:
'{'
    'type' ':' 'backref' ','
    'location' ':' location ','
    'value' ':' ID ','
    'comments' ':' comments
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 333,
        Token.constIntLocation = $8
    }
}

-- ********
-- *      *
-- * gvar *
-- *      *
-- ********
exp_gvar:
'{'
    'type' ':' 'gvar' ','
    'location' ':' location ','
    'value' ':' ID ','
    'comments' ':' comments
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 333,
        Token.constIntLocation = $8
    }
}

-- ********
-- *      *
-- * dyna *
-- *      *
-- ********
exp_dyna:
'{'
    'type' ':' 'dyna_symbol' ','
    'location' ':' location ','
    'parts' ':' '[' commalistof(string_part) ']' ','
    'comments' ':' comments
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 333,
        Token.constIntLocation = $8
    }
}

arg_block:
'{'
    'type' ':' 'arg_block' ','
    'location' ':' location ','
    'value' ':' exp ','
    'comments' ':' comments
'}'
{
    $12
}

-- ***********
-- *         *
-- * exp_int *
-- *         *
-- ***********
exp_int:
'{'
    'type' ':' 'int' ','
    'location' ':' location ','
    'value' ':' ID ','
    'comments' ':' comments
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = read (unquote (tokIDValue $12)),
        Token.constIntLocation = $8
    }
}

-- **********
-- *        *
-- * exp_if *
-- *        *
-- **********
exp_if:
'{'
    'type' ':' 'if_op' ','
    'location' ':' location ','
    'predicate' ':' exp ','
    'truthy' ':' exp ','
    'falsy' ':' exp ','
    'comments' ':' comments
'}'
{
    $12
}

exp_nil:
'{'
    'type' ':' 'kw' ','
    'location' ':' location ','
    'value' ':' ID ','
    'comments' ':' comments
'}'
{
    Ast.ExpNull $ Ast.ExpNullContent $ Token.ConstNull $8
}

str_concat:
'{'
    'type' ':' 'string_concat' ','
    'location' ':' location ','
    'left' ':' exp ','
    'right' ':' exp ','
    'comments' ':' comments
'}'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $12,
        Ast.expBinopRight = $16,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $8
    }
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
str_concat { $1 } |
exp_block  { $1 } |
exp_str    { $1 } |
exp_nil    { $1 } |
exp_if     { $1 } |
exp_int    { $1 } |
backref    { $1 } |
exp_regex  { $1 } |
arg_block  { $1 } |
exp_var    { $1 } |
exp_gvar   { $1 } |
exp_dyna   { $1 } |
exp_call   { $1 } |
exp_vcall  { $1 } |
exp_dict   { $1 } |
exp_array  { $1 } |
fstring    { $1 } |
exp_unop   { $1 } |
exp_args   { $1 } |
exp_binop  { $1 } |
exp_paren  { $1 } |
exp_yield  { $1 } |
exp_cmd    { $1 } |
arg_star   { $1 } |
string_part {$1 } |
exp_cmdcall {$1 }

-- **************
-- *            *
-- * collection *
-- *            *
-- **************
collection:
'{'
    'type' ':' 'range' ','
    'location' ':' location ','
    'left' ':' exp ','
    'operator' ':' operator ','
    'right' ':' exp ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- ************
-- *          *
-- * stmt_for *
-- *          *
-- ************
stmt_for:
'{'
    'type' ':' 'ForStatement' ','
    'location' ':' location ','
    'index' ':' var ','
    'collection' ':' collection ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- ************
-- *          *
-- * operator *
-- *          *
-- ************
actual_op:
'..'  { Ast.PLUS    } |
'::'  { Ast.PLUS    } |
'=='  { Ast.PLUS    } |
'and' { Ast.PLUS    } |
'+='  { Ast.PLUS    } |
'&&'  { Ast.PLUS    } |
'||'  { Ast.PLUS    } |
'||=' { Ast.PLUS    } |
'+'   { Ast.PLUS    } |
'*'   { Ast.TIMES   } |
'%'   { Ast.PERCENT } |
'-'   { Ast.MINUS   } |
'<'   { Ast.PLUS    } |
'>'   { Ast.PLUS    } |
'>='  { Ast.PLUS    } |
'<<'  { Ast.PLUS    } |
'='   { Ast.PLUS    } |
'!~'  { Ast.PLUS    } |
'.'   { Ast.PLUS    } |
'!'   { Ast.PLUS    }

-- ************
-- *          *
-- * operator *
-- *          *
-- ************
operator_name:
'op'     { Nothing } |
'period' { Nothing }

-- ************
-- *          *
-- * operator *
-- *          *
-- ************
operator:
'{'
    'type' ':' operator_name ','
    'location' ':' location ','
    'value' ':' actual_op ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- *************
-- *           *
-- * arguments *
-- *           *
-- *************
arguments_type_1:
'{'
    'type' ':' 'args' ','
    'location' ':' location ','
    'parts' ':' '[' commalistof(exp) ']' ','
    'comments' ':' comments
'}'
{
    $13
}

-- *************
-- *           *
-- * arguments *
-- *           *
-- *************
arguments_type_2:
'{'
    'type' ':' 'args' ','
    'location' ':' location ','
    'parts' ':' '[' ']' ','
    'comments' ':' comments 
'}'
{
    []
}

-- *************
-- *           *
-- * arguments *
-- *           *
-- *************
arguments_type_3:
'{'
    'type' ':' 'arg_paren' ','
    'location' ':' location ','
    'arguments' ':' arguments ','
    'comments' ':' comments 
'}'
{
    $12
}

-- *************
-- *           *
-- * arguments *
-- *           *
-- *************
arguments_type_4: 'null' { [] }

-- *************
-- *           *
-- * arguments *
-- *           *
-- *************
arguments:
arguments_type_1 { $1 } |
arguments_type_2 { $1 } |
arguments_type_3 { $1 } |
arguments_type_4 { $1 }


-- *********************
-- *                   *
-- * arguments_wrapper *
-- *                   *
-- *********************
arguments_wrapper:
'{'
    'type' ':' 'arg_paren' ','
    'location' ':' location ','
    'arguments' ':' arguments ','
    'comments' ':' comments
'}'
{
    $12
}

-- ***************
-- *             *
-- * stmt_assign *
-- *             *
-- ***************
stmt_assign_type_2:
'{'
    'type' ':' 'opassign' ','
    'location' ':' location ','
    'target' ':' var ','
    'operator' ':' operator ','
    'value' ':' exp ','
    'comments' ':' comments 
'}'
{
    Just $ Right $ Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = $12,
        Ast.stmtAssignRhs = Ast.ExpBinop $ Ast.ExpBinopContent {
            expBinopLeft = Ast.ExpVar (Ast.ExpVarContent $12),
            expBinopRight = $20,
            expBinopOperator = Ast.PLUS,
            expBinopLocation = $8
        }
    }
}

-- ***************
-- *             *
-- * stmt_assign *
-- *             *
-- ***************
stmt_assign_type_1:
'{'
    'type' ':' 'assign' ','
    'location' ':' location ','
    'target' ':' var ','
    'value' ':' exp ','
    'comments' ':' comments 
'}'
{
    Just $ Right $ Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = $12,
        Ast.stmtAssignRhs = $16
    }
}

mlhs:
'{'
    'type' ':' 'mlhs' ','
    'location' ':' location ','
    'parts' ':' parts ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_assign *
-- *             *
-- ***************
stmt_assign_type_3:
'{'
    'type' ':' 'massign' ','
    'location' ':' location ','
    'target' ':' mlhs ','
    'value' ':' exp ','
    'comments' ':' comments 
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_assign *
-- *             *
-- ***************
stmt_assign:
stmt_assign_type_1 { $1 } |
stmt_assign_type_2 { $1 } |
stmt_assign_type_3 { $1 }

else_type:
'else' { Nothing } |
'elif' { Nothing }

predicate: 'predicate' ':' exp ',' { Nothing }

consequent: 'consequent' ':' stmt_else ',' { Nothing }

-- *************
-- *           *
-- * stmt_else *
-- *           *
-- *************
stmt_else:
'{'
    'type' ':' else_type ','
    'location' ':' location ','
    optional(predicate)
    'stmts' ':' stmts ','
    optional(consequent)
    'comments' ':' comments
'}'
{
    Nothing
}

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
else_part: 'consequent' ':' stmt_else ',' { Nothing }

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if_type_1:
'{'
    'type' ':' 'if' ','
    'location' ':' location ','
    'predicate' ':' exp ','
    'stmts' ':' stmts ','
    optional(else_part)
    'comments' ':' comments
'}'
{
    Just $ Right $ Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $12,
        Ast.stmtIfBody = rights (catMaybes $16),
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $8
    }
}

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if2:
'{'
    'type' ':' 'if' ','
    'location' ':' location ','
    'predicate' ':' stmt_assign ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Just $ Right $ Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 888 $8,
        Ast.stmtIfBody = rights (catMaybes $16),
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $8
    }
}


-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if:
stmt_if_type_1 { $1 }

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call_without_args:
'{'
    'type' ':' 'call' ','
    'location' ':' location ','
    'receiver' ':' exp ','
    'operator' ':' operator ','
    'message' ':' identifier ','
    'comments' ':' comments
'}'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $12,
        Ast.varFieldName = Token.FieldName $20,
        Ast.varFieldLocation = $8
    }
}

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call_with_args:
'{'
    'type' ':' 'call' ','
    'location' ':' location ','
    'receiver' ':' exp ','
    'operator' ':' operator ','
    'message' ':' identifier ','
    'arguments' ':' arguments_wrapper ','
    'comments' ':' comments
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarField $ Ast.VarFieldContent
        {
            Ast.varFieldLhs = $12,
            Ast.varFieldName = Token.FieldName $20,
            Ast.varFieldLocation = Location {
                Location.filename = Location.filename $8,
                lineStart = Location.lineStart $8,
                colStart = Location.colStart $8,
                lineEnd = Location.lineEnd (Token.location $20),
                colEnd = Location.colEnd (Token.location $20)
            }
        },
        Ast.args = $24,
        Ast.expCallLocation = $8
    }
}

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call_without_rcvr:
'{'
    'type' ':' 'call' ','
    'location' ':' location ','
    'receiver' ':' 'null' ','
    'operator' ':' 'null' ','
    'message' ':' identifier ','
    'arguments' ':' arguments ','
    'comments' ':' comments
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $20,
        Ast.args = $24,
        Ast.expCallLocation = $8
    }
}

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call:
exp_call_with_args    { $1 } |
exp_call_without_rcvr { $1 } |
exp_call_without_args { $1 }

-- ****************
-- *              *
-- * stmt_comment *
-- *              *
-- ****************
stmt_comment:
'{'
    'type' ':' 'comment' ','
    'location' ':' location ','
    'value' ':' ID
'}'
{
    Nothing
}

-- ************
-- *          *
-- * constant *
-- *          *
-- ************
constant_or_value:
'constant'  { Nothing } |
'value'     { Nothing }

-- ***************************
-- *                         *
-- * identifier_wrapper_kind *
-- *                         *
-- ***************************
identifier_wrapper_kind:
'const_ref'      { Nothing } |
'const_path_ref' { Nothing } |
'var_ref'        { Nothing }

-- **********
-- *        *
-- * parent *
-- *        *
-- **********
parent: 'parent' ':' exp ',' { $3 }

-- **********************
-- *                    *
-- * identifier_wrapper *
-- *                    *
-- **********************
identifier_wrapper:
'{'
    'type' ':' identifier_wrapper_kind ','
    'location' ':' location ','
    optional(parent)
    constant_or_value ':' identifier ','
    'comments' ':' comments
'}'
{
    case $10 of
        Nothing -> $13
        Just exp -> case exp of
            (Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent v)))) -> Token.getVarNameToken v
            _ -> $13
}

-- **************
-- *            *
-- * superclass *
-- *            *
-- **************
superclass: 'superclass' ':' identifier_wrapper ',' { $3 }

-- **************
-- *            *
-- * stmt_class *
-- *            *
-- **************
stmt_class:
'{'
    'type' ':' 'class' ','
    'location' ':' location ','
    'constant' ':' var ','
    optional(superclass)
    'bodystmt' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    let
        allStmts = rights (catMaybes $17)
        justFuncStmts = \d -> case d of { (Ast.StmtFunc f) -> Just f; _ -> Nothing }
        funcStmts = catMaybes $ Data.List.map justFuncStmts allStmts
        methods = Data.Map.fromList (methodify (Token.ClassName (nameify $12)) $14 funcStmts)
    in Just $ Right $ Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName (nameify $12),
        Ast.stmtClassSupers = case $14 of { Nothing -> [] ; Just oneSuper -> [ Token.SuperName oneSuper ] },
        Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
        Ast.stmtClassMethods = Ast.Methods methods
    }
}

-- ***************
-- *             *
-- * exp_command *
-- *             *
-- ***************
exp_cmd:
'{'
    'type' ':' 'command' ','
    'location' ':' location ','
    'message' ':' identifier ','
    'arguments' ':' arguments ','
    'comments' ':' comments
'}'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $12
}

-- ***************
-- *             *
-- * exp_cmdcall *
-- *             *
-- ***************
exp_cmdcall_1:
'{'
    'type' ':' 'command_call' ','
    'location' ':' location ','
    'receiver' ':' exp ','
    'operator' ':' operator ','
    'message' ':' identifier ','
    'arguments' ':' args ','
    'comments' ':' comments
'}'
{
    $12
}

-- ***************
-- *             *
-- * exp_cmdcall *
-- *             *
-- ***************
exp_cmdcall_2:
'{'
    'type' ':' 'command_call' ','
    'location' ':' location ','
    'receiver' ':' exp ','
    'operator' ':' operator ','
    'message' ':' identifier ','
    'arguments' ':' args ','
    'block' ':' block ','
    'comments' ':' comments
'}'
{
    $12
}

-- ***************
-- *             *
-- * exp_cmdcall *
-- *             *
-- ***************
exp_cmdcall:
exp_cmdcall_1 { $1 } |
exp_cmdcall_2 { $1 }

-- ***************
-- *             *
-- * stmt_method *
-- *             *
-- ***************
stmt_method_type_1:
'{'
    'type' ':' 'def' ','
    'location' ':' location ','
    'target' ':' exp_var ','
    'operator' ':' operator ','
    'name' ':' identifier ','
    'params' ':' params ','
    'bodystmt' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    Just $ Right $ Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Just (varme (Token.Named "any" $8)),
        Ast.stmtFuncName = Token.FuncName $20,
        Ast.stmtFuncParams = $24,
        Ast.stmtFuncBody = rights (catMaybes $28),
        Ast.stmtFuncLocation = $8,
        Ast.stmtFuncAnnotations = []
    }
}

-- ***************
-- *             *
-- * stmt_method *
-- *             *
-- ***************
stmt_method_type_2:
'{'
    'type' ':' 'def' ','
    'location' ':' location ','
    'target' ':' 'null' ','
    'operator' ':' 'null' ','
    'name' ':' identifier ','
    'params' ':' params ','
    'bodystmt' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    Just $ Right $ Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Just (varme (Token.Named "any" $8)),
        Ast.stmtFuncName = Token.FuncName $20,
        Ast.stmtFuncParams = $24,
        Ast.stmtFuncBody = rights (catMaybes $28),
        Ast.stmtFuncLocation = $8,
        Ast.stmtFuncAnnotations = []
    }
}

-- ***************
-- *             *
-- * stmt_method *
-- *             *
-- ***************
stmt_method:
stmt_method_type_1 { $1 } |
stmt_method_type_2 { $1 }

-- ************
-- *          *
-- * stmt_exp *
-- *          *
-- ************
stmt_exp: exp { Just $ Right (Ast.StmtExp $1) }

-- ***************
-- *             *
-- * stmt_sclass *
-- *             *
-- ***************
stmt_sclass:
'{'
    'type' ':' 'sclass' ','
    'location' ':' location ','
    'target' ':' identifier_wrapper ','
    'bodystmt' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return:
'{'
    'type' ':' 'return' ','
    'location' ':' location ','
    'arguments' ':' arguments ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_unless *
-- *             *
-- ***************
stmt_unless:
'{'
    'type' ':' 'unless' ','
    'location' ':' location ','
    'predicate' ':' exp ','
    'stmts' ':' stmts ',' 
    'comments' ':' comments
'}'
{
    Nothing
}

-- *************
-- *           *
-- * block_var *
-- *           *
-- *************
block_var: 
'{'
    'type' ':' 'block_var' ','
    'location' ':' location ','
    'params' ':' params ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- *********************
-- *                   *
-- * block_var_wrapper *
-- *                   *
-- *********************
block_var_wrapper: 'block_var' ':' block_var ',' { $3 }

-- *********
-- *       *
-- * block *
-- *       *
-- *********
block:
'{'
    'type' ':' 'block' ','
    'location' ':' location ','
    optional(block_var_wrapper)
    'bodystmt' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    Nothing
}

exp_block:
'{'
    'type' ':' 'method_add_block' ','
    'location' ':' location ','
    'call' ':' exp ',' 
    'block' ':' block ','
    'comments' ':' comments
'}'
{
    $12
}

-- ***************
-- *             *
-- * stmt_module *
-- *             *
-- ***************
stmt_module:
'{'
    'type' ':' 'module' ','
    'location' ':' location ','
    'constant' ':' identifier_wrapper ','
    'bodystmt' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    Just $ Right $ Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 999 $8,
        Ast.stmtIfBody = modulize $12 (rights (catMaybes $16)),
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $8
    }
}

-- *************
-- *           *
-- * stmt_void *
-- *           *
-- *************
stmt_void:
'{'
    'type' ':' 'void_stmt' ','
    'location' ':' location ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- *************
-- *           *
-- * stmt_next *
-- *           *
-- *************
stmt_next:
'{'
    'type' ':' 'next' ','
    'location' ':' location ','
    'arguments' ':' args ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- **************
-- *            *
-- * stmt_begin *
-- *            *
-- **************
stmt_begin:
'{'
    'type' ':' 'begin' ','
    'location' ':' location ','
    'bodystmt' ':' bodystmt','
    'comments' ':' comments
'}'
{
    Just $ Right $ Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 1 $8,
        Ast.stmtIfBody = rights (catMaybes $12),
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $8
    }
}

-- **************
-- *            *
-- * stmt_break *
-- *            *
-- **************
stmt_break:
'{'
    'type' ':' 'break' ','
    'location' ':' location ','
    'arguments' ':' arguments ','
    'comments' ':' comments
'}'
{
    Nothing
}

lambda_block:
'{'
    'type' ':' 'blockarg' ','
    'location' ':' location ','
    'name' ':' identifier ','
    'comments' ':' comments
'}'
{
    Nothing
}

lambda_params_tag:
'{'
    'type' ':' 'params' ','
    'location' ':' location ','
    'block' ':' lambda_block ','
    'comments' ':' comments
'}'
{
    Nothing
}

lambda_content:
'{'
    'type' ':' 'lambda_var' ','
    'location' ':' location ','
    'params' ':' lambda_params_tag ','
    'comments' ':' comments
'}'
{
    Nothing
}

lambda_params:
'{'
    'type' ':' 'paren' ','
    'location' ':' location ','
    'contents' ':' lambda_content ','
    'comments' ':' comments
'}'
{
    Nothing
}

stmt_lambda:
'{'
    'type' ':' 'lambda' ','
    'location' ':' location ','
    'params' ':' lambda_params ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Just $ Right $ Ast.StmtExp $ Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        Ast.expLambdaParams = [],
        Ast.expLambdaBody = [],
        Ast.expLambdaLocation = $8
    }
}

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_lambda  { $1 } |
stmt_if      { $1 } |
stmt_if2     { $1 } |
stmt_for     { $1 } |
stmt_exp     { $1 } |
stmt_assign  { $1 } |
stmt_begin   { $1 } |
stmt_break   { $1 } |
stmt_void    { $1 } |
stmt_next    { $1 } |
stmt_class   { $1 } |
stmt_sclass  { $1 } |
stmt_method  { $1 } |
stmt_module  { $1 } |
stmt_return  { $1 } |
stmt_unless  { $1 } |
stmt_comment { $1 }

-- *********
-- *       *
-- * stmts *
-- *       *
-- *********
stmts:
'{'
    'type' ':' 'stmts' ','
    'location' ':' location ','
    'body' ':' '[' commalistof(stmt) ']' ','
    'comments' ':' comments
'}'
{
    $13
}

-- ************
-- *          *
-- * contents *
-- *          *
-- ************
optional_pair: '[' identifier ',' exp ']' { $2 }

-- *************
-- *           *
-- * optionals *
-- *           *
-- *************
optionals: 'optionals' ':' '[' commalistof(optional_pair) ']' ',' { $4 } 

rest: 'keyword_rest' ':' rest_params ',' { Nothing }

rest_params:
'{'
    'type' ':' 'kwrest_param' ','
    'location' ':' location ','
    'name' ':' identifier ','
    'comments' ':' comments
'}'
{
    Nothing
}

block_param:
'block' ':'
'{'
    'type' ':' 'blockarg' ','
    'location' ':' location ','
    'name' ':' identifier ','
    'comments' ':' comments
'}' ','
{
    Nothing
}

keyword_1:
'{'
    'type' ':' 'label' ','
    'location' ':' location ','
    'value' ':' tokenID ','
    'comments' ':' comments
'}'
{
    Nothing
}

keyword_2:
'{'
    'type' ':' 'var_ref' ','
    'location' ':' location ','
    'value' ':' exp ','
    'comments' ':' comments
'}'
{
    Nothing
}


keyword:
keyword_1 { Nothing } |
keyword_2 { Nothing }


keywords: 'keywords' ':' '[' '[' commalistof(ornull(keyword)) ']' ']' ',' { Nothing }

-- ************
-- *          *
-- * contents *
-- *          *
-- ************
contents:
'{'
    'type' ':' 'params' ','
    'location' ':' location ','
    optional(requireds)
    optional(keywords)
    optional(optionals)
    optional(rest)
    optional(block_param)
    'comments' ':' comments
'}'
{
    let
        params1 = case $10 of { Nothing -> [] ; Just ps -> ps }
        params2 = case $12 of { Nothing -> [] ; Just ps -> ps }
        params3 = params1 ++ params2
    in
        Data.List.map paramify params3
}

-- **********
-- *        *
-- * params *
-- *        *
-- **********
params_type_1:
'{'
    'type' ':' 'paren' ','
    'location' ':' location ','
    'contents' ':' contents ','
    'comments' ':' comments
'}'
{
    $12
}

-- ***********
-- *         *
-- * comment *
-- *         *
-- ***********
comment:
'{'
    'type' ':' 'comment' ','
    'location' ':' location ','
    'value' ':' ID
'}'
{
    Nothing
}

-- ************
-- *          *
-- * comments *
-- *          *
-- ************
comments: '[' ']' { Nothing } | '[' commalistof(comment) ']' { Nothing }

-- *************
-- *           *
-- * requireds *
-- *           *
-- *************
requireds: 'requireds' ':' '[' commalistof(identifier) ']' ',' { $4 }

-- **********
-- *        *
-- * params *
-- *        *
-- **********
params_type_2:
'{'
    'type' ':' 'params' ','
    'location' ':' location ','
    optional(requireds)
    'comments' ':' comments
'}'
{
    Data.List.map paramify (case $10 of { Nothing -> [] ; Just ps -> ps })
}

-- **********
-- *        *
-- * params *
-- *        *
-- **********
params:
params_type_1 { $1 } |
params_type_2 { $1 }

-- **********
-- *        *
-- * rescue *
-- *        *
-- **********
variable:
'{'
    'type' ':' 'var_field' ','
    'location' ':' location ','
    'value' ':' identifier ','
    'comments' ':' comments
'}'
{
    Nothing
} |
'null' { Nothing }

-- **************
-- *            *
-- * exceptions *
-- *            *
-- **************
exceptions:
'null' { Nothing } |
exp    { Nothing }

-- **********
-- *        *
-- * rescue *
-- *        *
-- **********
exception:
'{'
    'type' ':' 'rescue_ex' ','
    'location' ':' location ','
    'exceptions' ':' exceptions ','
    'variable' ':' variable ','
    'comments' ':' comments
'}'
{
    Nothing
}

rescue_exception: 'exception' ':' exception ',' { Nothing }

-- **********
-- *        *
-- * rescue *
-- *        *
-- **********
rescue:
'{'
    'type' ':' 'rescue' ','
    'location' ':' location ','
    optional(rescue_exception)
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- *****************
-- *               *
-- * rescue_clause *
-- *               *
-- *****************
rescue_clause: 'rescue_clause' ':' rescue ',' { Nothing }

-- *****************
-- *               *
-- * rescue_clause *
-- *               *
-- *****************
else_clause: 'else_clause' ':' stmts ',' { Nothing }

-- ************
-- *          *
-- * bodystmt *
-- *          *
-- ************
bodystmt_type_1:
'{'
    'type' ':' 'bodystmt' ','
    'location' ':' location ','
    'stmts' ':' stmts ','
    optional(rescue_clause)
    optional(else_clause)
    'comments' ':' comments
'}'
{
    $12
}

-- ************
-- *          *
-- * bodystmt *
-- *          *
-- ************
bodystmt_type_2:
'{'
    'type' ':' 'stmts' ','
    'location' ':' location ','
    'body' ':' '[' commalistof(stmt) ']' ','
    'comments' ':' comments
'}'
{
    $13
}

-- ************
-- *          *
-- * bodystmt *
-- *          *
-- ************
bodystmt:
bodystmt_type_1 { $1 } |
bodystmt_type_2 { $1 }

{

paramify :: Token.Named -> Ast.Param
paramify token = let
    paramName = Token.ParamName token
    nominalType = Just (varme (Token.Named "any" (Token.location token)))
    in Ast.Param paramName nominalType 156

nameify :: Ast.Var -> Token.Named
nameify (Ast.VarSimple    v) = nameifyVarSimple    v
nameify (Ast.VarField     v) = nameifyVarField     v
nameify (Ast.VarSubscript v) = nameifyVarSubscript v

varme :: Token.Named -> Ast.Var
varme = Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

nameifyVarSimple :: Ast.VarSimpleContent -> Token.Named
nameifyVarSimple = Token.getVarNameToken . Ast.varName

nameifyVarField :: Ast.VarFieldContent -> Token.Named
nameifyVarField = Token.getFieldNameToken . Ast.varFieldName

nameifyVarSubscript :: Ast.VarSubscriptContent -> Token.Named
nameifyVarSubscript v = Token.Named "popo" (Ast.varSubscriptLocation v)

modulize :: Token.Named -> [ Ast.Stmt ] -> [ Ast.Stmt ]
modulize moduleName = Data.List.map (modulize' moduleName)

modulize' :: Token.Named -> Ast.Stmt -> Ast.Stmt
modulize' moduleName (Ast.StmtClass c) = modulizeStmtClass c moduleName
modulize' moduleName (Ast.StmtFunc  f) = modulizeStmtFunc  f moduleName
modulize' moduleName (Ast.StmtIf    s) = modulizeStmtIf    s moduleName
modulize' moduleName stmt = stmt

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
methodify c Nothing = Data.List.map (methodify' c Nothing)
methodify c (Just s) = Data.List.map (methodify' c (Just s))

methodify' :: Token.ClassName -> Maybe Token.Named -> Ast.StmtFuncContent -> (Token.MethodName, Ast.StmtMethodContent)
methodify' c s f = methodify'' c (case s of { Nothing -> Nothing; Just n -> Just (Token.SuperName n) }) f

methodify'' :: Token.ClassName -> Maybe Token.SuperName -> Ast.StmtFuncContent -> (Token.MethodName, Ast.StmtMethodContent)
methodify'' c s f = let m = Token.MethodName $ Token.getFuncNameToken (Ast.stmtFuncName f) in (m, Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = (Ast.stmtFuncReturnType f),
    Ast.stmtMethodName = Token.MethodName $ Token.getFuncNameToken (Ast.stmtFuncName f),
    Ast.stmtMethodParams = (Ast.stmtFuncParams f),
    Ast.stmtMethodBody = (Ast.stmtFuncBody f),
    Ast.stmtMethodAnnotations = (Ast.stmtFuncAnnotations f),
    Ast.stmtMethodLocation = (Ast.stmtFuncLocation f),
    Ast.hostingClassName = c,
    Ast.hostingClassSupers = case s of { Nothing -> []; Just oneSuperClassName -> [ oneSuperClassName ] },
    Ast.hostingClassAnnotations = []
})

unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

-- ***********
-- *         *
-- * lexwrap *
-- *         *
-- ***********
lexwrap :: (AlexTokenTag -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

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

