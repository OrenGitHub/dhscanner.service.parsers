{
{-# OPTIONS -Werror=missing-fields #-}

module RbParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import qualified Ast
import RbLexer
import Location
import qualified RbParserActions as Actions
import qualified Token
import qualified Common

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************

}

-- ***********************
-- *                     *
-- * API function: parse *
-- *                     *
-- ***********************
%name parse

-- any shift/reduce or reduce/reduce conflict fails the build
%expect 0

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
'float'                 { AlexTokenTag AlexRawToken_FLOAT           _ }
'op'                    { AlexTokenTag AlexRawToken_OP              _ }
'end'                   { AlexTokenTag AlexRawToken_END             _ }
'raw'                   { AlexTokenTag AlexRawToken_RAW             _ }
'self'                  { AlexTokenTag AlexRawToken_SELF            _ }
'gvar'                  { AlexTokenTag AlexRawToken_GVAR            _ }
'begin'                 { AlexTokenTag AlexRawToken_BEGIN           _ }
'break'                 { AlexTokenTag AlexRawToken_BREAK           _ }
'case'                  { AlexTokenTag AlexRawToken_CASE            _ }
'when'                  { AlexTokenTag AlexRawToken_WHEN            _ }
'keyword'               { AlexTokenTag AlexRawToken_KEYWORD         _ }
'call'                  { AlexTokenTag AlexRawToken_CALL            _ }
'vcall'                 { AlexTokenTag AlexRawToken_VCALL           _ }
'defined'               { AlexTokenTag AlexRawToken_DEFINED         _ }
'superclass'            { AlexTokenTag AlexRawToken_SUPER           _ }
'super'                 { AlexTokenTag AlexRawToken_SUPER2         _ }
'zsuper'                { AlexTokenTag AlexRawToken_ZSUPER         _ }
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
'heredoc'               { AlexTokenTag AlexRawToken_HEREDOC         _ }
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
'mlhs_paren'            { AlexTokenTag AlexRawToken_LEFT3           _ }
'mrhs'                  { AlexTokenTag AlexRawToken_MRHS           _ }
'arg_block'             { AlexTokenTag AlexRawToken_ARG_BLOCK       _ }
'keyword_rest'          { AlexTokenTag AlexRawToken_REST            _ }
'kwrest_param'          { AlexTokenTag AlexRawToken_REST2           _ }
'rest'                  { AlexTokenTag AlexRawToken_REST3           _ }
'rest_param'            { AlexTokenTag AlexRawToken_REST_PARAM      _ }
'dyna_symbol'           { AlexTokenTag AlexRawToken_DYNA            _ }
'exception'             { AlexTokenTag AlexRawToken_EXCEPTION       _ }
'exceptions'            { AlexTokenTag AlexRawToken_EXCEPTIONS      _ }
'rescue'                { AlexTokenTag AlexRawToken_RESCUE          _ }
'rescue_clause'         { AlexTokenTag AlexRawToken_RESCUE2         _ }
'else_clause'           { AlexTokenTag AlexRawToken_RESCUE4         _ }
'ensure'                { AlexTokenTag AlexRawToken_ENSURE          _ }
'ensure_clause'         { AlexTokenTag AlexRawToken_ENSURE_CLAUSE   _ }
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
'elements'              { AlexTokenTag AlexRawToken_ELEMENTS        _ }
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
'array'                  { AlexTokenTag AlexRawToken_ARRAY           _ }
'qwords'                 { AlexTokenTag AlexRawToken_QWORDS          _ }
'qsymbols'               { AlexTokenTag AlexRawToken_QSYMBOLS        _ }
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
'cvar'                  { AlexTokenTag AlexRawToken_CVAR            _ }
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
'while'               { AlexTokenTag AlexRawToken_STMT_WHILE  _ }
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
'<='  { AlexTokenTag AlexRawToken_OP_LEQ      _ }
'<<'  { AlexTokenTag AlexRawToken_OP_SHL      _ }
'=='  { AlexTokenTag AlexRawToken_OP_EQ       _ }
'==='  { AlexTokenTag AlexRawToken_OP_EQ3      _ }
'!='  { AlexTokenTag AlexRawToken_OP_NE       _ }
'=~'  { AlexTokenTag AlexRawToken_OP_EQMATCH  _ }
'+='  { AlexTokenTag AlexRawToken_OP_PLUSEQ   _ }
'-='  { AlexTokenTag AlexRawToken_OP_MINUSEQ  _ }
'!~'  { AlexTokenTag AlexRawToken_OP_NEQ      _ }
'!'   { AlexTokenTag AlexRawToken_OP_BANG     _ }
'='   { AlexTokenTag AlexRawToken_OP_ASSIGN   _ }
'.'   { AlexTokenTag AlexRawToken_OP_DOT      _ }
'&.'  { AlexTokenTag AlexRawToken_OP_AMP_DOT  _ }
'+'   { AlexTokenTag AlexRawToken_OP_PLUS     _ }
'-'   { AlexTokenTag AlexRawToken_OP_MINUS    _ }
'*'   { AlexTokenTag AlexRawToken_OP_TIMES    _ }
'/'   { AlexTokenTag AlexRawToken_OP_DIV      _ }
'**'  { AlexTokenTag AlexRawToken_OP_POW      _ }
'..'  { AlexTokenTag AlexRawToken_OP_DOTDOT   _ }
'...' { AlexTokenTag AlexRawToken_OP_DOTDOTDOT _ }
'::'  { AlexTokenTag AlexRawToken_OP_COLON2   _ }
'%'   { AlexTokenTag AlexRawToken_OP_PERCENT  _ }
'++'  { AlexTokenTag AlexRawToken_OP_PLUSPLUS _ }
'||'  { AlexTokenTag AlexRawToken_OP_OR       _ }
'|'   { AlexTokenTag AlexRawToken_OP_PIPE    _ }
'&'   { AlexTokenTag AlexRawToken_OP_AMP     _ }
'&&'  { AlexTokenTag AlexRawToken_OP_AND      _ }
'||=' { AlexTokenTag AlexRawToken_OP_OR2      _ }
'&='  { AlexTokenTag AlexRawToken_OP_AMP_EQ   _ }
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

-- direct dhscanner subtree creation: Ast.Root
root:
'{'
    'type' ':' 'program' ','
    'location' ':' location ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Actions.root (getFilename $1) $12
}

-- a Location
location: '[' INT ',' INT ',' INT ',' INT ']'
{
    Actions.mkLocation (getFilename $1) (tokIntValue $2) (tokIntValue $4) (tokIntValue $6) (tokIntValue $8)
}

-- ************
-- *          *
-- * token ID *
-- *          *
-- ************
tokenID:
ID        { Actions.unquote (tokIDValue $1) } |
'id'      { "id"                    } |
'self'    { "self"                  } |
'cvar'    { "cvar"                  } |
'true'    { "true"                  } |
'type'    { "type"                  } |
'call'    { "call"                  } |
'raw'     { "raw"                   } |
'heredoc' { "heredoc"               } |
'false'   { "false"                 } |
'args'    { "args"                  } |
'value'   { "value"                 } |
'object'  { "object"                } |
'rest'    { "rest"                  } |
'binary'  { "binary"                } |
'message' { "message"               } |
'command' { "command"               } |
'receiver' { "receiver"             } |
'operator' { "operator"               } |
'contents' { "contents"             } |
'parent'  { "parent"                } |
'period'  { "period"                } |
'name'    { "name"                  } |
'case'    { "case"                  } |
'begin'   { "begin"                } |
'end'     { "end"                  } |
'when'    { "when"                  } |
'else'    { "else"                  } |
'ensure'  { "ensure"                } |
'keyword' { "keyword"               } |
'keywords' { "keywords"             } |
'defined' { "defined"               } |
'.'       { "."                     } |
'..'      { ".."                    } |
'...'     { "..."                   } |
'==='     { "==="                   } |
'='       { "="                     } |
'+'       { "+"                     } |
'-'       { "-"                     } |
'*'       { "*"                     } |
'**'      { "**"                    } |
'%'       { "%"                     } |
'/'       { "/"                     } |
'::'      { "::"                    } |
'|'       { "|"                     } |
'&'       { "&"                     } |
'!'       { "!"                     } |
'<'       { "<"                     } |
'>'       { ">"                     } |
'start'   { "start"                 } |
'index'   { "index"                 } |
'collection' { "collection"         } |
'line'    { "line"                  } |
'location' { "location"             } |
'update'  { "update"                } |
'params'  { "params"                } |
'label'   { "label"                 } |
'block'   { "block"                 } |
'options' { "options"               } |
'elements'              { "elements"             } |
'parts'   { "parts"                 } |
'qwords'  { "qwords"                } |
'qsymbols' { "qsymbols"             } |
'prefix'  { "prefix"               } |
'key'     { "key"                  } |
'target'  { "target"               } |
'test'    { "test"                 } |
'body'    { "body"                 } |
'class'   { "class"                 } |
'array'   { "array"                 } |
'hash'    { "hash"                  } |
'int'     { "int"                   } |
'float'   { "float"                 } |
'comment' { "comment"               } |
'field'   { "field"                 } |
'lambda'  { "lambda"                } |
'range'   { "range"                 } |
'arguments'            { "arguments"             } |
'column'                { "column"                } |
'next'    { "next"                  } |
'ForStatement' { "for"                  } |
'argument' { "argument" } |
'exception' { "exception" } |
'async'    { "async"                 } |
'assign'   { "assign"                } |
'backref'  { "backref"               } |
'constant' { "constant" } |
'comments'              { "comments"              }

-- *******************
-- *                 *
-- * identifier_type *
-- *                 *
-- *******************
identifier_type:
'const' { Nothing } |
'ident' { Nothing } |
'ivar'  { Nothing } |
'cvar'  { Nothing } |
'gvar'  { Nothing } |
'kw'    { Nothing } |
'op'    { Nothing }

-- a Token.Named
identifier:
'{'
    'type' ':' identifier_type ','
    'location' ':' location ','
    'value' ':' tokenID ','
    'comments' ':' comments 
'}'
{
    Actions.named $12 $8
}

-- a Token.Named
param:
'{'
    'type' ':' 'ident' ','
    'name' ':' tokenID ','
    'location' ':' location
'}'
{
    Actions.named $8 $12
}

-- ***********
-- *         *
-- * expVar *
-- direct dhscanner subtree creation: Ast.ExpVar
expVar: var { Actions.expVar $1 }

-- direct dhscanner subtree creation: Ast.ExpBinop
expBinop:
'{'
    'type' ':' 'binary' ','
    'location' ':' location ','
    'left' ':' exp ','
    'operator' ':' actual_op ','
    'right' ':' exp ','
    'comments' ':' comments
'}'
{
    Actions.expBinop $8 $12 $16 $20
}

-- instrumented as dhscanner Ast.ExpBinop
expUnop:
'{'
    'type' ':' 'unary' ','
    'location' ':' location ','
    'operator' ':' actual_op ','
    'statement' ':' exp ','
    'comments' ':' comments
'}'
{
    Actions.expUnop $8 $12 $16
}

var_simple_type:
'var_field' { Nothing } |
'const_ref' { Nothing } |
'var_ref'   { Nothing }

var_simple_content:
'value'    { Nothing } |
'constant' { Nothing }

-- direct dhscanner subtree creation: Ast.VarSimple
varSimple:
'{'
    'type' ':' var_simple_type ','
    'location' ':' location ','
    var_simple_content ':' identifier ','
    'comments' ':' comments
'}'
{
    Actions.varSimple $12
}

-- direct dhscanner subtree creation: Ast.VarField
varField:
'{'
    'type' ':' 'field' ','
    'location' ':' location ','
    'parent' ':' exp ','
    'operator' ':' operator ','
    'name' ':' identifier ','
    'comments' ':' comments
'}'
{
    Actions.varField $8 $12 $20
}

var_subscript_type:
'aref_field' { Nothing } |
'aref'       { Nothing }

-- direct dhscanner subtree creation: Ast.VarSubscript
varSubscript:
'{'
    'type' ':' var_subscript_type ','
    'location' ':' location ','
    'collection' ':' exp ','
    'index' ':' exp ','
    'comments' ':' comments
'}'
{
    Actions.varSubscript $8 $12 $16
}

var_parented_type:
'const_path_ref' { Nothing } |
'top_const_ref'  { Nothing }

-- direct dhscanner subtree creation: Ast.VarField
varParented:
'{'
    'type' ':' var_parented_type ','
    'location' ':' location ','
    optional(parent)
    'constant' ':' identifier ','
    'comments' ':' comments
'}'
{
    Actions.varParented $8 $10 $13
}

-- ************
-- *          *
-- * variable *
-- *          *
-- ************
var:
varSimple    { $1 } |
varField     { $1 } |
varParented  { $1 } |
varSubscript { $1 }

-- direct dhscanner subtree creation: Ast.ExpStr
stringPartContent:
'{'
    'type' ':' 'tstring_content' ','
    'location' ':' location ','
    'value' ':' tokenID ','
    'comments' ':' comments
'}'
{
    Actions.expStr $8 $12
}

-- instrumented as dhscanner Ast.Exp ( the embedded expression )
stringPartEmbexpr:
'{'
    'type' ':' 'string_embexpr' ','
    'location' ':' location ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Actions.stringEmbexpr $8 $12
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
string_part:
stringPartContent { $1 } |
stringPartEmbexpr { $1 }

-- instrumented as dhscanner Ast.ExpCall ( legacy callee `fstring` )
fstring:
'{'
    'type' ':' 'string_literal' ','
    'location' ':' location ','
    'parts' ':' parts ','
    'comments' ':' comments
'}'
{
    Actions.legacyNamedCall "fstring" $8 $12
}

-- instrumented as dhscanner Ast.ExpCall ( legacy callee `fstring` )
expHeredoc:
'{'
    'type' ':' 'heredoc' ','
    'location' ':' location ','
    'parts' ':' parts ','
    'comments' ':' comments
'}'
{
    Actions.legacyNamedCall "fstring" $8 $12
}

-- direct dhscanner subtree creation: Ast.ExpStr
expSymbol:
'{'
    'type' ':' 'symbol_literal' ','
    'location' ':' location ','
    'value' ':' identifier ','
    'comments' ':' comments
'}'
{
    Actions.expSymbol $8 $12
}

-- *******
-- *     *
-- * key *
-- *     *
-- *******
key_type:
'label'          { Nothing } |
'string_literal' { Nothing } |
'symbol_literal' { Nothing } |
'dyna_symbol'    { Nothing }

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
key_value:
'value' ':' ID ',' { Nothing } |
'value' ':' identifier ',' { Nothing }

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

-- allow keys that are variable references (e.g., some_var => value)
key_var_ref:
'{'
    'type' ':' 'var_ref' ','
    'location' ':' location ','
    'value' ':' identifier ','
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

-- assoc with a var_ref key
assoc_type_1_varref:
'{'
    'type' ':' 'assoc' ','
    'location' ':' location ','
    'key' ':' key_var_ref ','
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

assoc_type_3:
'{'
    'type' ':' 'assoc' ','
    'location' ':' location ','
    'key' ':' key ','
    'comments' ':' comments
'}'
{
    Nothing
}

assoc:
assoc_type_1 { Nothing } |
assoc_type_1_varref { Nothing } |
assoc_type_2 { Nothing } |
assoc_type_3 { Nothing }

-- instrumented as dhscanner Ast.ExpInt placeholder
expDictBare:
'{'
    'type' ':' 'bare_assoc_hash' ','
    'location' ':' location ','
    'assocs' ':' '[' commalistof(assoc) ']' ','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 9999 $8
}

-- **********
-- *        *
-- * assocs *
-- *        *
-- **********
assocs: 'assocs' ':' '[' commalistof(assoc) ']' ',' { $4 }

-- instrumented as dhscanner Ast.ExpInt placeholder
expDictHash:
'{'
    'type' ':' 'hash' ','
    'location' ':' location ','
    optional(assocs)
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 9999 $8
}

-- ************
-- *          *
-- * exp_dict *
-- *          *
-- ************
exp_dict:
expDictBare { $1 } |
expDictHash { $1 }

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

-- instrumented as dhscanner Ast.ExpCall ( legacy callee `listify` )
expArray:
'{'
    'type' ':' 'array' ','
    'location' ':' location ','
    'contents' ':' array_content ','
    'comments' ':' comments
'}'
{
    Actions.legacyNamedCall "listify" $8 $12
}

-- instrumented as dhscanner Ast.ExpCall ( legacy callee `listify` )
expQwordsNonEmpty:
'{'
    'type' ':' 'qwords' ','
    'location' ':' location ','
    'elements' ':' '[' commalistof(string_part) ']' ','
    'comments' ':' comments
'}'
{
    Actions.legacyNamedCall "listify" $8 $13
}

-- instrumented as dhscanner Ast.ExpCall ( legacy callee `listify` )
expQwordsEmpty:
'{'
    'type' ':' 'qwords' ','
    'location' ':' location ','
    'elements' ':' '[' ']' ','
    'comments' ':' comments
'}'
{
    Actions.legacyNamedCall "listify" $8 []
}

exp_qwords:
expQwordsNonEmpty { $1 } |
expQwordsEmpty    { $1 }

-- instrumented as dhscanner Ast.ExpCall ( legacy callee `listify` )
expQsymbolsNonEmpty:
'{'
    'type' ':' 'qsymbols' ','
    'location' ':' location ','
    'elements' ':' '[' commalistof(string_part) ']' ','
    'comments' ':' comments
'}'
{
    Actions.legacyNamedCall "listify" $8 $13
}

-- instrumented as dhscanner Ast.ExpCall ( legacy callee `listify` )
expQsymbolsEmpty:
'{'
    'type' ':' 'qsymbols' ','
    'location' ':' location ','
    'elements' ':' '[' ']' ','
    'comments' ':' comments
'}'
{
    Actions.legacyNamedCall "listify" $8 []
}

exp_qsymbols:
expQsymbolsNonEmpty { $1 } |
expQsymbolsEmpty    { $1 }

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

-- direct dhscanner subtree creation: Ast.ExpVar
expVcall:
'{'
    'type' ':' 'vcall' ','
    'location' ':' location ','
    'value' ':' identifier ','
    'comments' ':' comments
'}'
{
    Actions.expvarify $12
}

-- instrumented as dhscanner Ast.ExpInt placeholder
expYield:
'{'
    'type' ':' 'yield' ','
    'location' ':' location ','
    'arguments' ':' 'null' ','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 555 $8
}

-- allow yield with arguments wrapped in a paren node
yieldArgumentsParen:
'{'
    'type' ':' 'paren' ','
    'location' ':' location ','
    'contents' ':' arguments ','
    'comments' ':' comments
'}'
{
    $12
}

-- instrumented as dhscanner Ast.ExpInt placeholder (yield with args)
expYieldArgs:
'{'
    'type' ':' 'yield' ','
    'location' ':' location ','
    'arguments' ':' yieldArgumentsParen ','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 555 $8
}

-- instrumented as dhscanner Ast.ExpInt placeholder
expParen:
'{'
    'type' ':' 'paren' ','
    'location' ':' location ','
    'contents' ':' stmts ','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 555 $8
}

-- direct dhscanner subtree creation: Ast.VarSubscript
subscript:
'{'
    'type' ':' 'aref' ','
    'location' ':' location ','
    'collection' ':' exp ','
    'index' ':' exp ','
    'comments' ':' comments
'}'
{
    Actions.expVar (Actions.varSubscript $8 $12 $16)
}

-- instrumented as dhscanner Ast.ExpCall ( legacy callee `expify` )
expArgs:
'{'
    'type' ':' 'args' ','
    'location' ':' location ','
    'parts' ':' '[' commalistof(exp) ']' ','
    'comments' ':' comments
'}'
{
    Actions.legacyNamedCall "expify" $8 $13
}

-- instrumented as dhscanner Ast.ExpInt placeholder
expRegex:
'{'
    'type' ':' 'regexp_literal' ','
    'location' ':' location ','
    'parts' ':' '[' commalistof(string_part) ']' ','
    'options' ':' ID ','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 222 $8
}

-- instrumented as dhscanner Ast.ExpInt placeholder
backref:
'{'
    'type' ':' 'backref' ','
    'location' ':' location ','
    'value' ':' ID ','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 333 $8
}

-- instrumented as dhscanner Ast.ExpInt placeholder
expGvar:
'{'
    'type' ':' 'gvar' ','
    'location' ':' location ','
    'value' ':' ID ','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 333 $8
}

-- instrumented as dhscanner Ast.ExpInt placeholder
expDyna:
'{'
    'type' ':' 'dyna_symbol' ','
    'location' ':' location ','
    'parts' ':' '[' commalistof(string_part) ']' ','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 333 $8
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

-- direct dhscanner subtree creation: Ast.ExpInt
expInt:
'{'
    'type' ':' 'int' ','
    'location' ':' location ','
    'value' ':' ID ','
    'comments' ':' comments
'}'
{
    Actions.expInt $8 (tokIDValue $12)
}

-- direct dhscanner subtree creation: Ast.ExpInt
expFloat:
'{'
    'type' ':' 'float' ','
    'location' ':' location ','
    'value' ':' ID ','
    'comments' ':' comments
'}'
{
    Actions.expInt $8 (tokIDValue $12)
}

-- **********
-- *        *
-- * exp_if *
-- *        *
-- **********
expDefined:
'{'
    'type' ':' 'defined' ','
    'location' ':' location ','
    'value' ':' exp ','
    'comments' ':' comments
'}'
{
    $12
}

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

-- instrumented as dhscanner Ast.ExpInt placeholder
expIf:
'{'
    'type' ':' 'if' ','
    'location' ':' location ','
    'predicate' ':' exp ','
    'stmts' ':' stmts ','
    optional(else_part)
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 888 $8
}

-- a generic keyword tag node we can ignore
kw_tag:
'{'
    'type' ':' 'kw' ','
    'location' ':' location ','
    'value' ':' tokenID ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- else branch within a case/when chain
caseElse:
'{'
    'type' ':' 'else' ','
    'location' ':' location ','
    'keyword' ':' kw_tag ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Nothing
}
|
'{'
    'type' ':' 'else' ','
    'location' ':' location ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Nothing
}

when_consequent: 'consequent' ':' whenish ',' { Nothing }

when_keyword: 'keyword' ':' kw_tag ',' { Nothing }

whenish:
whenNode { Nothing } |
caseElse { Nothing }

-- a when clause inside a case
whenNode:
'{'
    'type' ':' 'when' ','
    'location' ':' location ','
    optional(when_keyword)
    'arguments' ':' arguments ','
    'stmts' ':' stmts ','
    optional(when_consequent)
    'comments' ':' comments
'}'
{
    Nothing
}

-- instrumented as dhscanner Ast.ExpInt placeholder
expCase:
'{'
    'type' ':' 'case' ','
    'location' ':' location ','
    'keyword' ':' kw_tag ','
    'value' ':' ornull(exp) ','
    'consequent' ':' whenish ','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 777 $8
}

-- direct dhscanner subtree creation: Ast.ExpNull
expNil:
'{'
    'type' ':' 'kw' ','
    'location' ':' location ','
    'value' ':' tokenID ','
    'comments' ':' comments
'}'
{
    Actions.expNull $8
}

-- instrumented as dhscanner Ast.ExpBinop
strConcat:
'{'
    'type' ':' 'string_concat' ','
    'location' ':' location ','
    'left' ':' exp ','
    'right' ':' exp ','
    'comments' ':' comments
'}'
{
    Actions.strConcat $8 $12 $16
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
-- every exp. stmtExp wraps expBase only : if / assign / begin as a
-- statement go through their stmt twins ( stmtIfPlain, stmtAssignPlain, stmtBegin )
exp:
expBase   { $1 } |
expIf     { $1 } |
expAssign { $1 } |
expBegin  { $1 }

expBase:
strConcat   { $1 } |
exp_block   { $1 } |
expSymbol   { $1 } |
expNil      { $1 } |
exp_if      { $1 } |
expCase     { $1 } |
expFloat   { $1 } |
expInt      { $1 } |
backref     { $1 } |
expRegex    { $1 } |
arg_block   { $1 } |
expVar      { $1 } |
expGvar     { $1 } |
expDyna     { $1 } |
exp_call    { $1 } |
expVcall    { $1 } |
exp_dict    { $1 } |
expArray    { $1 } |
exp_qwords  { $1 } |
exp_qsymbols  { $1 } |
fstring     { $1 } |
expHeredoc  { $1 } |
expLambda   { $1 } |
expUnop     { $1 } |
expDefined  { $1 } |
expArgs     { $1 } |
expBinop    { $1 } |
expParen    { $1 } |
expYield    { $1 } |
expYieldArgs { $1 } |
expSuper   { $1 } |
expZSuper  { $1 } |
expCmdWithBlock { $1 } |
expCmd      { $1 } |
arg_star    { $1 } |
string_part { $1 } |
expRange   { $1 } |
exp_cmdcall { $1 }

-- **************
-- *            *
-- * collection *
-- *            *
-- **************
-- instrumented as dhscanner Ast.ExpBinop
expRange:
'{'
    'type' ':' 'range' ','
    'location' ':' location ','
    'left' ':' exp ','
    'operator' ':' operator ','
    'right' ':' exp ','
    'comments' ':' comments
'}'
{
    Actions.expBinop $8 $12 $16 $20
}
|
'{'
    'type' ':' 'range' ','
    'location' ':' location ','
    'left' ':' exp ','
    'operator' ':' operator ','
    'comments' ':' comments
'}'
{
    Actions.expBinop $8 $12 $16 $12
}
|
'{'
    'type' ':' 'range' ','
    'location' ':' location ','
    'operator' ':' operator ','
    'right' ':' exp ','
    'comments' ':' comments
'}'
{
    Actions.expBinop $8 $16 $12 $16
}

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
'...' { Ast.PLUS    } |
'::'  { Ast.PLUS    } |
'=='  { Ast.PLUS    } |
'===' { Ast.PLUS    } |
'!='  { Ast.PLUS    } |
'=~'  { Ast.PLUS    } |
'and' { Ast.PLUS    } |
'+='  { Ast.PLUS    } |
'-='  { Ast.PLUS    } |
'&&'  { Ast.PLUS    } |
'||'  { Ast.PLUS    } |
'|'   { Ast.PLUS    } |
'&'   { Ast.PLUS    } |
'||=' { Ast.PLUS    } |
'&='  { Ast.PLUS    } |
'+'   { Ast.PLUS    } |
'*'   { Ast.TIMES   } |
'/'   { Ast.TIMES   } |
'**'  { Ast.TIMES   } |
'%'   { Ast.PERCENT } |
'-'   { Ast.MINUS   } |
'<'   { Ast.PLUS    } |
'>'   { Ast.PLUS    } |
'>='  { Ast.PLUS    } |
'<='  { Ast.PLUS    } |
'<<'  { Ast.PLUS    } |
'='   { Ast.PLUS    } |
'!~'  { Ast.PLUS    } |
'.'   { Ast.PLUS    } |
'&.'  { Ast.PLUS    } |
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
    $12
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

-- direct dhscanner subtree creation: Ast.StmtAssign
stmtOpAssign:
'{'
    'type' ':' 'opassign' ','
    'location' ':' location ','
    'target' ':' var ','
    'operator' ':' operator ','
    'value' ':' exp ','
    'comments' ':' comments 
'}'
{
    Actions.stmtOpAssign $8 $12 $20
}

-- direct dhscanner subtree creation: Ast.StmtAssign
stmtAssignPlain:
'{'
    'type' ':' 'assign' ','
    'location' ':' location ','
    'target' ':' var ','
    'value' ':' exp ','
    'comments' ':' comments 
'}'
{
    Actions.stmtAssign $12 $16
}

-- instrumented as dhscanner Ast.Exp ( assignment expression lowers to RHS )
expAssign:
'{'
    'type' ':' 'assign' ','
    'location' ':' location ','
    'target' ':' var ','
    'value' ':' exp ','
    'comments' ':' comments 
'}'
{
    $16
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

-- instrumented as dhscanner Ast.ExpInt placeholder (mrhs used as expression in mass assignment)
expMrhs:
'{'
    'type' ':' 'mrhs' ','
    'location' ':' location ','
    'parts' ':' parts ','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 9999 $8
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
|
'{'
    'type' ':' 'massign' ','
    'location' ':' location ','
    'target' ':' mlhs ','
    'value' ':' expMrhs ','
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
stmtAssignPlain    { $1 } |
stmtOpAssign       { $1 } |
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

-- direct dhscanner subtree creation: Ast.StmtIf
stmtIfPlain:
'{'
    'type' ':' 'if' ','
    'location' ':' location ','
    'predicate' ':' exp ','
    'stmts' ':' stmts ','
    optional(else_part)
    'comments' ':' comments
'}'
{
    Actions.stmtIf $8 $12 $16
}

-- if / unless / while `x op= e` ( a plain `x = e` predicate is an exp : expAssign )
assignPredicate:
stmtOpAssign       { $1 } |
stmt_assign_type_3 { $1 }

-- direct dhscanner subtree creation: Ast.StmtIf
stmtIfAssign:
'{'
    'type' ':' 'if' ','
    'location' ':' location ','
    'predicate' ':' assignPredicate ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Actions.stmtIfAssign $8 $16
}


-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if:
stmtIfPlain { $1 }

-- direct dhscanner subtree creation: Ast.VarField
expFieldAccess:
'{'
    'type' ':' 'call' ','
    'location' ':' location ','
    'receiver' ':' exp ','
    'operator' ':' operator ','
    'message' ':' identifier ','
    'comments' ':' comments
'}'
{
    Actions.fieldAccess $8 $12 $20
}

-- direct dhscanner subtree creation: Ast.ExpCall
expMethodCall:
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
    Actions.methodCall $8 $12 $20 $24
}

-- direct dhscanner subtree creation: Ast.ExpCall
expCallNoRcvr:
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
    Actions.call $8 $20 $24
}

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call:
expMethodCall  { $1 } |
expCallNoRcvr  { $1 } |
expFieldAccess { $1 }

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
'top_const_ref'  { Nothing } |
'var_ref'        { Nothing }

-- **********
-- *        *
-- * parent *
-- *        *
-- **********
parent: 'parent' ':' exp ',' { $3 }

-- a Token.Named
identifierWrapper:
'{'
    'type' ':' identifier_wrapper_kind ','
    'location' ':' location ','
    optional(parent)
    constant_or_value ':' identifier ','
    'comments' ':' comments
'}'
{
    Actions.identifierWrapper $10 $13
}

-- **************
-- *            *
-- * superclass *
-- *            *
-- **************
superclass: 'superclass' ':' identifierWrapper ',' { $3 } |
            'superclass' ':' superclassArefValue ',' { $3 }

superclassArefValue:
'{'
    'type' ':' 'aref' ','
    'location' ':' location ','
    'collection' ':' identifierWrapper ','
    'index' ':' exp ','
    'comments' ':' comments
'}'
{
    $12
}

-- direct dhscanner subtree creation: Ast.StmtClass
stmtClass:
'{'
    'type' ':' 'class' ','
    'location' ':' location ','
    'constant' ':' var ','
    optional(superclass)
    'bodystmt' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    Actions.stmtClass $12 $14 $17
}

-- direct dhscanner subtree creation: Ast.ExpCall
expSuper:
'{'
    'type' ':' 'super' ','
    'location' ':' location ','
    'arguments' ':' arguments ','
    'comments' ':' comments
'}'
{
    Actions.superCall $8 $12
}

-- direct dhscanner subtree creation: Ast.ExpCall
expZSuper:
'{'
    'type' ':' 'zsuper' ','
    'location' ':' location ','
    'comments' ':' comments
'}'
{
    Actions.superCall $8 []
}

-- direct dhscanner subtree creation: Ast.ExpVar
expCmd:
'{'
    'type' ':' 'command' ','
    'location' ':' location ','
    'message' ':' identifier ','
    'arguments' ':' arguments ','
    'comments' ':' comments
'}'
{
    Actions.expvarify $12
}

-- direct dhscanner subtree creation: Ast.ExpVar
expCmdWithBlock:
'{'
    'type' ':' 'command' ','
    'location' ':' location ','
    'message' ':' identifier ','
    'arguments' ':' arguments ','
    'block' ':' block ','
    'comments' ':' comments
'}'
{
    Actions.expvarify $12
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

-- direct dhscanner subtree creation: Ast.StmtFunc
stmtMethodWithTarget:
'{'
    'type' ':' 'def' ','
    'location' ':' location ','
    'target' ':' expVar ','
    'operator' ':' operator ','
    'name' ':' identifier ','
    'params' ':' params ','
    'bodystmt' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    Actions.stmtFunc $8 $20 $24 $28
}

-- direct dhscanner subtree creation: Ast.StmtFunc
stmtMethodPlain:
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
    Actions.stmtFunc $8 $20 $24 $28
}

-- ***************
-- *             *
-- * stmt_method *
-- *             *
-- ***************
stmt_method:
stmtMethodWithTarget { $1 } |
stmtMethodPlain      { $1 }

-- ************
-- *          *
-- * stmtExp *
-- direct dhscanner subtree creation: Ast.StmtExp
stmtExp: expBase { Actions.stmtExp $1 }

-- ***************
-- *             *
-- * stmt_sclass *
-- *             *
-- ***************
stmt_sclass:
'{'
    'type' ':' 'sclass' ','
    'location' ':' location ','
    'target' ':' identifierWrapper ','
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
|
'{'
    'type' ':' 'unless' ','
    'location' ':' location ','
    'predicate' ':' assignPredicate ','
    'stmts' ':' stmts ',' 
    'comments' ':' comments
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_while  *
-- *             *
-- ***************
stmt_while:
'{'
    'type' ':' 'while' ','
    'location' ':' location ','
    'predicate' ':' exp ','
    'stmts' ':' stmts ',' 
    'comments' ':' comments
'}'
{
    Nothing
}
|
'{'
    'type' ':' 'while' ','
    'location' ':' location ','
    'predicate' ':' assignPredicate ','
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

-- instrumented as dhscanner Ast.StmtIf
stmtModule:
'{'
    'type' ':' 'module' ','
    'location' ':' location ','
    'constant' ':' identifierWrapper ','
    'bodystmt' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    Actions.stmtModule $8 $12 $16
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

-- instrumented as dhscanner Ast.StmtIf
stmtBegin:
'{'
    'type' ':' 'begin' ','
    'location' ':' location ','
    'bodystmt' ':' bodystmt','
    'comments' ':' comments
'}'
{
    Actions.stmtBegin $8 $12
}

-- instrumented as dhscanner Ast.ExpInt placeholder
expBegin:
'{'
    'type' ':' 'begin' ','
    'location' ':' location ','
    'bodystmt' ':' bodystmt','
    'comments' ':' comments
'}'
{
    Actions.placeholderInt 1 $8
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

lambda_params_tag:
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

-- direct dhscanner subtree creation: Ast.ExpLambda
-- direct dhscanner subtree creation: Ast.ExpLambda
expLambda:
'{'
    'type' ':' 'lambda' ','
    'location' ':' location ','
    'params' ':' lambda_content ','
    'stmts' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    Actions.expLambda $8
}
|
'{'
    'type' ':' 'lambda' ','
    'location' ':' location ','
    'params' ':' lambda_params ','
    'stmts' ':' bodystmt ','
    'comments' ':' comments
'}'
{
    Actions.expLambda $8
}


-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if      { $1 } |
stmtIfAssign { $1 } |
stmt_for     { $1 } |
stmtExp      { $1 } |
stmt_assign  { $1 } |
stmtBegin    { $1 } |
stmt_break   { $1 } |
stmt_void    { $1 } |
stmt_next    { $1 } |
stmtClass    { $1 } |
stmt_sclass  { $1 } |
stmt_method  { $1 } |
stmtModule   { $1 } |
stmt_return  { $1 } |
stmt_unless  { $1 } |
stmt_while   { $1 } |
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

rest: 'keyword_rest' ':' rest_params ',' { Nothing } |
      'rest'          ':' rest_params ',' { Nothing }

rest_param_type:
'kwrest_param' { Nothing } |
'rest_param'   { Nothing }

rest_params:
'{'
    'type' ':' rest_param_type ','
    'location' ':' location ','
    'name' ':' identifier ','
    'comments' ':' comments
'}'
{
    Nothing
}
|
'{'
    'type' ':' rest_param_type ','
    'location' ':' location ','
    'name' ':' 'null' ','
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



keyword_5:
exp { Nothing }

keyword:
keyword_1 { Nothing } |
keyword_5 { Nothing }

keyword_pair: '[' commalistof(ornull(keyword)) ']' { Nothing }

keywords: 'keywords' ':' '[' commalistof(keyword_pair) ']' ',' { Nothing }

-- keywords and optionals, in either order ( each at most once ) : the optionals
kwOpts:
                               { Nothing } |
keywords  optional(optionals)  { $2      } |
optionals optional(keywords)   { Just $1 }

-- zero, one or two of rest / keyword_rest
rests:
          { Nothing } |
rest      { Nothing } |
rest rest { Nothing }

-- a list of Ast.Param
contents:
'{'
    'type' ':' 'params' ','
    'location' ':' location ','
    optional(requireds)
    kwOpts
    rests
    optional(block_param)
    'comments' ':' comments
'}'
{
    Actions.params $10 $11
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
required_item: identifier { $1 } |
               mlhs_paren_item { $1 }

mlhs_idents:
'{'
    'type' ':' 'mlhs' ','
    'location' ':' location ','
    'parts' ':' '[' commalistof(identifier) ']' ','
    'comments' ':' comments
'}'
{
    $13
}

mlhs_paren_item:
'{'
    'type' ':' 'mlhs_paren' ','
    'location' ':' location ','
    'contents' ':' mlhs_idents ','
    'comments' ':' comments
'}'
{
    Actions.mlhsParenFirst $8 $12
}

requireds: 'requireds' ':' '[' commalistof(required_item) ']' ',' { $4 }

-- a list of Ast.Param
paramsBare:
'{'
    'type' ':' 'params' ','
    'location' ':' location ','
    optional(requireds)
    optional(keywords)
    optional(optionals)
    'comments' ':' comments
'}'
{
    Actions.params $10 $12
}

-- **********
-- *        *
-- * params *
-- *        *
-- **********
params:
params_type_1 { $1 } |
paramsBare    { $1 }

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
exceptions_mrhs:
'{'
    'type' ':' 'mrhs' ','
    'location' ':' location ','
    'parts' ':' parts ','
    'comments' ':' comments
'}'
{
    Nothing
}

exceptions:
'null' { Nothing } |
exp    { Nothing } |
exceptions_mrhs { Nothing }

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

-- allow chained rescue clauses within a rescue node
rescue_consequent: 'consequent' ':' rescue ',' { Nothing }

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
    optional(rescue_consequent)
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

-- **********
-- *        *
-- * ensure *
-- *        *
-- **********
ensure:
'{'
    'type' ':' 'ensure' ','
    'location' ':' location ','
    'stmts' ':' stmts ','
    'comments' ':' comments
'}'
{
    Nothing
}

-- *****************
-- *               *
-- * ensure_clause *
-- *               *
-- *****************
ensure_clause: 'ensure_clause' ':' ensure ',' { Nothing }

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
    optional(ensure_clause)
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

