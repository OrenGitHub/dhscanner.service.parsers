{
{-# OPTIONS -Werror=missing-fields #-}

module PyParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import PyLexer
import Location
import qualified PyParserActions as Actions
import qualified Token
import qualified Common

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.Maybe
import Data.Either
import Data.List ( map )

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
%lexer { lexwrap } { AlexTokenTag TokenEOF _ _ }

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

'('    { AlexTokenTag AlexRawToken_LPAREN _ _ }
')'    { AlexTokenTag AlexRawToken_RPAREN _ _ }
'['    { AlexTokenTag AlexRawToken_LBRACK _ _ }
']'    { AlexTokenTag AlexRawToken_RBRACK _ _ }
'{'    { AlexTokenTag AlexRawToken_LBRACE _ _ }
'}'    { AlexTokenTag AlexRawToken_RBRACE _ _ }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

':'    { AlexTokenTag AlexRawToken_COLON  _ _ }
','    { AlexTokenTag AlexRawToken_COMMA  _ _ }
'-'    { AlexTokenTag AlexRawToken_HYPHEN _ _ }

-- *********************
-- *                   *
-- * reserved keywords *
-- *                   *
-- *********************

'id'                        { AlexTokenTag AlexRawToken_KWID            _ _ }
'left'                      { AlexTokenTag AlexRawToken_LEFT            _ _ }
'iter'                      { AlexTokenTag AlexRawToken_ITER            _ _ }
'Dict'                      { AlexTokenTag AlexRawToken_DICT            _ _ }
'keys'                      { AlexTokenTag AlexRawToken_KEYS            _ _ }
'right'                     { AlexTokenTag AlexRawToken_RIGHT           _ _ }
'op'                        { AlexTokenTag AlexRawToken_OPERATOR        _ _ }
'ops'                       { AlexTokenTag AlexRawToken_OPERATOR2       _ _ }
'decorator_list'            { AlexTokenTag AlexRawToken_DECORATORS      _ _ }
'optional_vars'             { AlexTokenTag AlexRawToken_WITH_VARS       _ _ }
'type_params'               { AlexTokenTag AlexRawToken_TYPE_PARAMS     _ _ }
'type_ignores'              { AlexTokenTag AlexRawToken_TYPE_IGNORES    _ _ }
'comparators'               { AlexTokenTag AlexRawToken_COMPARE2        _ _ }
'BoolOp'                    { AlexTokenTag AlexRawToken_COMPARE3        _ _ }
'Compare'                   { AlexTokenTag AlexRawToken_COMPARE         _ _ }
'operand'                   { AlexTokenTag AlexRawToken_OPERAND         _ _ }
'Return'                    { AlexTokenTag AlexRawToken_STMT_RETURN     _ _ }
'returns'                   { AlexTokenTag AlexRawToken_STMT_RETURN2    _ _ }
'Yield'                     { AlexTokenTag AlexRawToken_STMT_YIELD      _ _ }
'YieldFrom'                 { AlexTokenTag AlexRawToken_STMT_YIELDFROM  _ _ }
'Raise'                     { AlexTokenTag AlexRawToken_STMT_RAISE      _ _ }
'cause'                     { AlexTokenTag AlexRawToken_STMT_CAUSE      _ _ }
'Del'                       { AlexTokenTag AlexRawToken_STMT_DEL        _ _ }
'Delete'                    { AlexTokenTag AlexRawToken_STMT_DELETE     _ _ }
'Global'                    { AlexTokenTag AlexRawToken_STMT_GLOBAL     _ _ }
'Try'                       { AlexTokenTag AlexRawToken_STMT_TRY        _ _ }
'exc'                       { AlexTokenTag AlexRawToken_EXC             _ _ }
'With'                      { AlexTokenTag AlexRawToken_WITH            _ _ }
'AsyncWith'                 { AlexTokenTag AlexRawToken_WITH            _ _ }
'withitem'                  { AlexTokenTag AlexRawToken_WITH2           _ _ }
'context_expr'              { AlexTokenTag AlexRawToken_CTX_MANAGER     _ _ }
'items'                     { AlexTokenTag AlexRawToken_ITEMS           _ _ }
'List'                      { AlexTokenTag AlexRawToken_LIST            _ _ }
'Set'                       { AlexTokenTag AlexRawToken_SET             _ _ }
'SetComp'                   { AlexTokenTag AlexRawToken_SET_COMP        _ _ }
'ListComp'                  { AlexTokenTag AlexRawToken_LIST_COMP       _ _ }
'DictComp'                  { AlexTokenTag AlexRawToken_DICT_COMP       _ _ }
'GeneratorExp'              { AlexTokenTag AlexRawToken_GENERATOR_EXP   _ _ }
'Tuple'                     { AlexTokenTag AlexRawToken_TUPLE           _ _ }
'elt'                       { AlexTokenTag AlexRawToken_ELT             _ _ }
'elts'                      { AlexTokenTag AlexRawToken_ELTS            _ _ }
'False'                     { AlexTokenTag AlexRawToken_FALSE           _ _ }
'True'                      { AlexTokenTag AlexRawToken_TRUE            _ _ }
'Ellipsis'                  { AlexTokenTag AlexRawToken_ELLIPSIS        _ _ }
'Constant'                  { AlexTokenTag AlexRawToken_EXPR_CONST      _ _ }
'Continue'                  { AlexTokenTag AlexRawToken_STMT_CONTINUE   _ _ }
'Break'                     { AlexTokenTag AlexRawToken_STMT_BREAK      _ _ }
'Pass'                      { AlexTokenTag AlexRawToken_STMT_PASS       _ _ }
'Not'                       { AlexTokenTag AlexRawToken_NOT             _ _ }
'NotEq'                     { AlexTokenTag AlexRawToken_NOTEQ           _ _ }
'NotIn'                     { AlexTokenTag AlexRawToken_NOTIN           _ _ }
'Add'                       { AlexTokenTag AlexRawToken_ADD             _ _ }
'Pow'                       { AlexTokenTag AlexRawToken_POW             _ _ }
'FloorDiv'                  { AlexTokenTag AlexRawToken_FLOOR_DIV       _ _ }
'Mod'                       { AlexTokenTag AlexRawToken_MOD             _ _ }
'Div'                       { AlexTokenTag AlexRawToken_DIV             _ _ }
'Sub'                       { AlexTokenTag AlexRawToken_SUB             _ _ }
'USub'                      { AlexTokenTag AlexRawToken_USUB            _ _ }
'Invert'                    { AlexTokenTag AlexRawToken_INVERT          _ _ }
'Mult'                      { AlexTokenTag AlexRawToken_MULT            _ _ }
'Eq'                        { AlexTokenTag AlexRawToken_EQ              _ _ }
'Gt'                        { AlexTokenTag AlexRawToken_GT              _ _ }
'GtE'                       { AlexTokenTag AlexRawToken_GE              _ _ }
'LtE'                       { AlexTokenTag AlexRawToken_LE              _ _ }
'Lt'                        { AlexTokenTag AlexRawToken_LT              _ _ }
'In'                        { AlexTokenTag AlexRawToken_IN              _ _ }
'Is'                        { AlexTokenTag AlexRawToken_IS              _ _ }
'IsNot'                     { AlexTokenTag AlexRawToken_ISNOT           _ _ }
'Or'                        { AlexTokenTag AlexRawToken_OR              _ _ }
'BitOr'                     { AlexTokenTag AlexRawToken_OR2             _ _ }
'BitXor'                    { AlexTokenTag AlexRawToken_BITXOR          _ _ }
'And'                       { AlexTokenTag AlexRawToken_AND             _ _ }
'BitAnd'                    { AlexTokenTag AlexRawToken_AND2            _ _ }
'LShift'                    { AlexTokenTag AlexRawToken_LSHIFT          _ _ }
'RShift'                    { AlexTokenTag AlexRawToken_RSHIFT          _ _ }
'ctx'                       { AlexTokenTag AlexRawToken_CTX             _ _ }
'kwonlyargs'                { AlexTokenTag AlexRawToken_ARGS4           _ _ }
'posonlyargs'               { AlexTokenTag AlexRawToken_ARGS3           _ _ }
'arguments'                 { AlexTokenTag AlexRawToken_ARGS2           _ _ }
'arg'                       { AlexTokenTag AlexRawToken_ARG             _ _ }
'vararg'                    { AlexTokenTag AlexRawToken_VARARG          _ _ }
'args'                      { AlexTokenTag AlexRawToken_ARGS            _ _ }
'attr'                      { AlexTokenTag AlexRawToken_ATTR            _ _ }
'Attribute'                 { AlexTokenTag AlexRawToken_ATTR2           _ _ }
'Starred'                   { AlexTokenTag AlexRawToken_STARRED         _ _ }
'Subscript'                 { AlexTokenTag AlexRawToken_SUBSCRIPT       _ _ }
'slice'                     { AlexTokenTag AlexRawToken_SLICE           _ _ }
'lower'                     { AlexTokenTag AlexRawToken_LOWER           _ _ }
'step'                      { AlexTokenTag AlexRawToken_STEP            _ _ }
'upper'                     { AlexTokenTag AlexRawToken_UPPER           _ _ }
'Slice'                     { AlexTokenTag AlexRawToken_EXPR_SLICE      _ _ }
'func'                      { AlexTokenTag AlexRawToken_FUNC            _ _ }
'body'                      { AlexTokenTag AlexRawToken_BODY            _ _ }
'None'                      { AlexTokenTag AlexRawToken_NONE            _ _ }
'handlers'                  { AlexTokenTag AlexRawToken_HANDLERS        _ _ }
'type'                      { AlexTokenTag AlexRawToken_TYPE            _ _ }
'finalbody'                 { AlexTokenTag AlexRawToken_BODY2           _ _ }
'ExceptHandler'             { AlexTokenTag AlexRawToken_EXCEPT_HANDLER  _ _ }
'test'                      { AlexTokenTag AlexRawToken_TEST            _ _ }
'Name'                      { AlexTokenTag AlexRawToken_NAME2           _ _ }
'Call'                      { AlexTokenTag AlexRawToken_CALL            _ _ }
'Expr'                      { AlexTokenTag AlexRawToken_STMT_EXPR       _ _ }
'level'                     { AlexTokenTag AlexRawToken_LEVEL           _ _ }
'key'                       { AlexTokenTag AlexRawToken_KEY             _ _ }
'value'                     { AlexTokenTag AlexRawToken_VALUE           _ _ }
'kind'                      { AlexTokenTag AlexRawToken_KIND            _ _ }
'values'                    { AlexTokenTag AlexRawToken_VALUES          _ _ }
'name'                      { AlexTokenTag AlexRawToken_NAME            _ _ }
'asname'                    { AlexTokenTag AlexRawToken_ASNAME          _ _ }
'orelse'                    { AlexTokenTag AlexRawToken_ORELSE          _ _ }
'defaults'                  { AlexTokenTag AlexRawToken_DEFAULTS        _ _ }
'kwarg'                     { AlexTokenTag AlexRawToken_KWARG           _ _ }
'comprehension'             { AlexTokenTag AlexRawToken_COMPREHENSION   _ _ }
'generators'                { AlexTokenTag AlexRawToken_GENERATORS      _ _ }
'kw_defaults'               { AlexTokenTag AlexRawToken_KW_DEFAULTS     _ _ }
'target'                    { AlexTokenTag AlexRawToken_TARGET          _ _ }
'targets'                   { AlexTokenTag AlexRawToken_TARGETS         _ _ }
'names'                     { AlexTokenTag AlexRawToken_NAMES           _ _ }
'alias'                     { AlexTokenTag AlexRawToken_ALIAS           _ _ }
'keyword'                   { AlexTokenTag AlexRawToken_KEYWORD         _ _ }
'keywords'                  { AlexTokenTag AlexRawToken_KEYWORDS        _ _ }
'Import'                    { AlexTokenTag AlexRawToken_IMPORT          _ _ }
'conversion'                { AlexTokenTag AlexRawToken_CONVERSION      _ _ }
'JoinedStr'                 { AlexTokenTag AlexRawToken_FSTRING         _ _ }
'ImportFrom'                { AlexTokenTag AlexRawToken_IMPORTF         _ _ }
'format_spec'               { AlexTokenTag AlexRawToken_FORMAT_SPEC     _ _ }
'FormattedValue'            { AlexTokenTag AlexRawToken_FORMATTED_VAL   _ _ }
'Load'                      { AlexTokenTag AlexRawToken_LOAD            _ _ }
'Store'                     { AlexTokenTag AlexRawToken_STORE           _ _ }
'is_async'                  { AlexTokenTag AlexRawToken_IS_ASYNC        _ _ }
'simple'                    { AlexTokenTag AlexRawToken_SIMPLE          _ _ }
'Assign'                    { AlexTokenTag AlexRawToken_ASSIGN          _ _ }
'Await'                     { AlexTokenTag AlexRawToken_AWAIT           _ _ }
'NamedExpr'                 { AlexTokenTag AlexRawToken_NAMED_EXPR      _ _ }
'Assert'                    { AlexTokenTag AlexRawToken_ASSERT          _ _ }
'Lambda'                    { AlexTokenTag AlexRawToken_LAMBDA          _ _ }
'AugAssign'                 { AlexTokenTag AlexRawToken_ASSIGN2         _ _ }
'AnnAssign'                 { AlexTokenTag AlexRawToken_ASSIGN3         _ _ }
'TypeAlias'                 { AlexTokenTag AlexRawToken_TYPE_ALIAS      _ _ }
'TypeVar'                   { AlexTokenTag AlexRawToken_TYPE_VAR        _ _ }
'Match'                     { AlexTokenTag AlexRawToken_MATCH           _ _ }
'match_case'                { AlexTokenTag AlexRawToken_MATCH_CASE      _ _ }
'MatchValue'                { AlexTokenTag AlexRawToken_MATCH_VALUE     _ _ }
'MatchSingleton'            { AlexTokenTag AlexRawToken_MATCH_SINGLETON _ _ }
'MatchSequence'             { AlexTokenTag AlexRawToken_MATCH_SEQUENCE  _ _ }
'MatchMapping'              { AlexTokenTag AlexRawToken_MATCH_MAPPING   _ _ }
'MatchClass'                { AlexTokenTag AlexRawToken_MATCH_CLASS     _ _ }
'MatchStar'                 { AlexTokenTag AlexRawToken_MATCH_STAR      _ _ }
'MatchAs'                   { AlexTokenTag AlexRawToken_MATCH_AS        _ _ }
'MatchOr'                   { AlexTokenTag AlexRawToken_MATCH_OR        _ _ }
'subject'                   { AlexTokenTag AlexRawToken_SUBJECT         _ _ }
'cases'                     { AlexTokenTag AlexRawToken_CASES           _ _ }
'pattern'                   { AlexTokenTag AlexRawToken_PATTERN         _ _ }
'patterns'                  { AlexTokenTag AlexRawToken_PATTERNS        _ _ }
'guard'                     { AlexTokenTag AlexRawToken_GUARD           _ _ }
'cls'                       { AlexTokenTag AlexRawToken_CLS             _ _ }
'kwd_attrs'                 { AlexTokenTag AlexRawToken_KWD_ATTRS       _ _ }
'kwd_patterns'              { AlexTokenTag AlexRawToken_KWD_PATTERNS    _ _ }
'rest'                      { AlexTokenTag AlexRawToken_REST            _ _ }
'annotation'                { AlexTokenTag AlexRawToken_ANNOTATION      _ _ }
'Module'                    { AlexTokenTag AlexRawToken_MODULE          _ _ }
'module'                    { AlexTokenTag AlexRawToken_MODULE2         _ _ }
'msg' { AlexTokenTag AlexRawToken_msg _ _ }
-- last keywords first part

-- ************
-- *          *
-- * location *
-- *          *
-- ************

'lineno'                    { AlexTokenTag AlexRawToken_LINE            _ _ }
'col_offset'                { AlexTokenTag AlexRawToken_COL             _ _ }
'end_lineno'                { AlexTokenTag AlexRawToken_ELINE           _ _ }
'end_col_offset'            { AlexTokenTag AlexRawToken_ECOL            _ _ }

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************
'UnaryOp'                   { AlexTokenTag AlexRawToken_EXPR_UNOP       _ _ }
'BinOp'                     { AlexTokenTag AlexRawToken_EXPR_BINOP      _ _ }


-- **************
-- *            *
-- * statements *
-- *            *
-- **************

'If'                        { AlexTokenTag AlexRawToken_STMT_IF         _ _ }
'ifs'                       { AlexTokenTag AlexRawToken_STMT_IFS        _ _ }
'While'                     { AlexTokenTag AlexRawToken_STMT_WHILE      _ _ }
'IfExp'                     { AlexTokenTag AlexRawToken_EXPR_IF         _ _ }
'For'                       { AlexTokenTag AlexRawToken_FOR             _ _ }
'AsyncFor'                  { AlexTokenTag AlexRawToken_FOR             _ _ }
'FunctionDef'               { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ _ }
'AsyncFunctionDef'          { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ _ }
'ClassDef'                  { AlexTokenTag AlexRawToken_STMT_CLASS      _ _ }
'bases'                     { AlexTokenTag AlexRawToken_SUPERS          _ _ }

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

'<'  { AlexTokenTag AlexRawToken_OP_LT       _ _ }
'==' { AlexTokenTag AlexRawToken_OP_EQ       _ _ }
'='  { AlexTokenTag AlexRawToken_OP_ASSIGN   _ _ }
'*'  { AlexTokenTag AlexRawToken_OP_TIMES    _ _ }

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************

INT    { AlexTokenTag (AlexRawToken_INT  i) _ _ }
ID     { AlexTokenTag (AlexRawToken_ID  id) _ _ }

-- *************************
-- *                       *
-- * grammar specification *
-- *                       *
-- *************************
%%

-- ************
-- *          *
-- * optional *
-- *          *
-- ************
optional(a): { Nothing } | a { Just $1 }

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
commalistof(a): a { [$1] } | a ',' commalistof(a) { $1:$3 }

-- ****************************
-- *                          *
-- * possibly_empty_listof(a) *
-- *                          *
-- ****************************
possibly_empty_listof(a): '[' ']' { [] } | '[' commalistof(a) ']' { $2 }

-- **********************
-- *                    *
-- * nonempty_listof(a) *
-- *                    *
-- **********************
nonempty_listof(a): '[' commalistof(a) ']' { $2 }

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program:
'Module'
'('
    'body' '=' stmts ','
    'type_ignores' '=' '[' ']'
')'
{
    Ast.Root
    {
        Ast.filename = getFilename $1,
        Ast.stmts = $5
    }
}

-- ********
-- *      *
-- * args *
-- *      *
-- ********
args:
'[' ']'                  { [] } |
'[' commalistof(arg) ']' { $2 }

-- *******
-- *     *
-- * arg *
-- *     *
-- *******
arg: exp { $1 }

-- ******
-- *    *
-- * op *
-- *    *
-- ******
op:
'Not'  '(' ')' { Nothing } |
'NotEq' '(' ')' { Nothing } |
'NotIn' '(' ')' { Nothing } |
'Add'  '(' ')' { Nothing } |
'Pow'  '(' ')' { Nothing } |
'Mod'  '(' ')' { Nothing } |
'Div'  '(' ')' { Nothing } |
'USub' '(' ')' { Nothing } |
'Sub'  '(' ')' { Nothing } |
'Mult' '(' ')' { Nothing } |
'Eq'   '(' ')' { Nothing } |
'Gt'   '(' ')' { Nothing } |
'LtE'  '(' ')' { Nothing } |
'GtE'  '(' ')' { Nothing } |
'Lt'   '(' ')' { Nothing } |
'In'   '(' ')' { Nothing } |
'Is'   '(' ')' { Nothing } |
'IsNot' '(' ')' { Nothing } |
'And'  '(' ')' { Nothing } |
'BitAnd' '(' ')' { Nothing } |
'LShift' '(' ')' { Nothing } |
'RShift' '(' ')' { Nothing } |
'FloorDiv' '(' ')' { Nothing } |
'BitOr' '(' ')' { Nothing } |
'BitXor' '(' ')' { Nothing } |
'Or'   '(' ')' { Nothing }

-- ************
-- *          *
-- * exp_unop *
-- *          *
-- ************
exp_unop:
'UnaryOp'
'('
    'op' '=' op ','
    'operand' '=' exp ','
    loc
')'
{
    $9
} |
'UnaryOp'
'('
    'op' '=' 'Invert' '(' ')' ','
    'operand' '=' exp ','
    loc
')'
{
    -- ~x ( e.g. django's ~Q(...) ) -> <dhscanner-instrumentation>[invert](x)
    Actions.instrumentationCall "invert" $13 [$11]
}

-- **************
-- *            *
-- * var_simple *
-- *            *
-- **************
var_simple:
name
{
    Ast.VarSimple $ Ast.VarSimpleContent
    {
        Ast.varName = Token.VarName $1
    }
}

-- *************
-- *           *
-- * var_field *
-- *           *
-- *************
var_field:
'Attribute'
'('
    'value' '=' exp','
    'attr' '=' ID ','
    'ctx' '=' ctx ','
    loc
')'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $5,
        Ast.varFieldName = Token.FieldName $ Token.Named
        {
            Token.content = unquote (tokIDValue $9),
            Token.location = $15
        },
        Ast.varFieldLocation = $15
    }
}

-- *****************
-- *               *
-- * var_subscript *
-- *               *
-- *****************
var_subscript:
'Subscript'
'('
    'value' '=' exp ','
    'slice' '=' exp ','
    'ctx' '=' ctx ','
    loc
')'
{
    Ast.VarSubscript $ Ast.VarSubscriptContent
    {
        Ast.varSubscriptLhs = $5,
        Ast.varSubscriptIdx = $9,
        Ast.varSubscriptLocation = $15
    }
}

-- *******
-- *     *
-- * var *
-- *     *
-- *******
var:
var_simple    { $1 } |
var_field     { $1 } |
var_subscript { $1 }

-- ***********
-- *         *
-- * exp_var *
-- *         *
-- ***********
exp_var: var { Ast.ExpVar (Ast.ExpVarContent $1) }

exp_str_kind:
'kind' '=' tokenID ',' { Nothing }

-- ***********
-- *         *
-- * exp_str *
-- *         *
-- ***********
exp_str:
'Constant'
'('
    'value' '=' ID ','
    optional(exp_str_kind)
    loc
')'
{
    Ast.ExpStr $ Ast.ExpStrContent
    {
        Ast.expStrValue = Token.ConstStr
        {
            Token.constStrValue = unquote (tokIDValue $5),
            Token.constStrLocation = $8
        }
    }
}

-- ***********
-- *         *
-- * exp_int *
-- *         *
-- ***********
exp_int:
'Constant'
'('
    'value' '=' INT ','
    loc
')'
{
    Ast.ExpInt $ Ast.ExpIntContent
    {
        Ast.expIntValue = Token.ConstInt
        {
            Token.constIntValue = tokIntValue $5,
            Token.constIntLocation = $7
        }
    }
}

-- ************
-- *          *
-- * exp_none *
-- *          *
-- ************
exp_none:
'Constant'
'('
    'value' '=' 'None' ','
    loc
')'
{
    Ast.ExpInt $ Ast.ExpIntContent
    {
        Ast.expIntValue = Token.ConstInt
        {
            Token.constIntValue = 0,
            Token.constIntLocation = $7
        }
    }
}

-- ******************
-- *                *
-- * exp_bool_false *
-- *                *
-- ******************
exp_bool_false:
'Constant'
'('
    'value' '=' 'False' ','
    loc
')'
{
    Ast.ExpBool $ Ast.ExpBoolContent
    {
        Ast.expBoolValue = Token.ConstBool
        {
            Token.constBoolValue = False,
            Token.constBoolLocation = $7
        }
    }
}

-- *****************
-- *               *
-- * exp_bool_true *
-- *               *
-- *****************
exp_bool_true:
'Constant'
'('
    'value' '=' 'True' ','
    loc
')'
{
    Ast.ExpBool $ Ast.ExpBoolContent
    {
        Ast.expBoolValue = Token.ConstBool
        {
            Token.constBoolValue = True,
            Token.constBoolLocation = $7
        }
    }
}

-- ************
-- *          *
-- * exp_bool *
-- *          *
-- ************
exp_bool:
exp_bool_true  { $1 } |
exp_bool_false { $1 }

-- **************
-- *            *
-- * conversion *
-- *            *
-- **************
conversion:
'-' INT { Nothing } |
INT     { Nothing }

format_spec: 'format_spec' '=' exp ',' { Nothing }

-- *******************
-- *                 *
-- * formatted_value *
-- *                 *
-- *******************
formatted_value:
'FormattedValue'
'('
    'value' '=' exp ','
    'conversion' '=' conversion ','
    optional(format_spec)
    loc
')'
{
    $5
}

-- ****************
-- *              *
-- * fstring elem *
-- *              *
-- ****************
fstring_elem:
exp_str         { $1 } |
formatted_value { $1 }

-- ***************
-- *             *
-- * exp_fstring *
-- *             *
-- ***************
exp_fstring:
'JoinedStr'
'('
    'values' '=' '[' commalistof(fstring_elem) ']' ','
    loc
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent
        {
            Ast.actualExpVar = Ast.VarSimple $ Ast.VarSimpleContent
            {
                Ast.varName = Token.VarName $ Token.Named
                {
                    Token.content = "fstring",
                    Token.location = $9
                }
            }
        },
        Ast.args = $6,
        Ast.expCallLocation = $9
    }
}

-- ***************
-- *             *
-- * exp_relop_1 *
-- *             *
-- ***************
exp_relop_1:
'Compare'
'('
    'left' '=' exp ','
    'ops' '=' '[' commalistof(op) ']' ','
    'comparators' '=' '[' commalistof(exp) ']' ','
    loc
')'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $5,
        Ast.expBinopRight = case $16 of { [] -> $5; (rhs:_) -> rhs },
        Ast.expBinopOperator = Ast.PLUS, -- FIXME
        Ast.expBinopLocation = $19
    }
}

-- ***************
-- *             *
-- * exp_relop_2 *
-- *             *
-- ***************
exp_relop_2:
'BoolOp'
'('
    'op' '=' op ','
    'values' '=' '[' commalistof(exp) ']' ','
    loc
')'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = case $10 of
        {
            [] -> dummyExp $13;
            [e] -> e;
            [lhs,rhs] -> lhs;
            (lhs:rhs:_) -> lhs
        },
        Ast.expBinopRight = case $10 of
        {
            [] -> dummyExp $13;
            [e] -> e;
            [lhs,rhs] -> rhs;
            (lhs:rhs:_) -> rhs
        },
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $13
    }  
}

-- *************
-- *           *
-- * exp_relop *
-- *           *
-- *************
exp_relop:
exp_relop_1 { $1 } |
exp_relop_2 { $1 }

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop:
'BinOp'
'('
    'left' '=' exp ','
    'op' '=' op ','
    'right' '=' exp ','
    loc
')'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $5,
        Ast.expBinopRight = $13,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $15
    }
}

-- ************
-- *          *
-- * exp_list *
-- *          *
-- ************
exp_list:
'List'
'('
    'elts' '=' possibly_empty_listof(exp) ','
    'ctx' '=' ctx ','
    loc
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent
        {
            Ast.actualExpVar = Ast.VarSimple $ Ast.VarSimpleContent
            {
                Ast.varName = Token.VarName $ Token.Named
                {
                    Token.content = "listify",
                    Token.location = $11
                }
            }
        },
        Ast.args = $5,
        Ast.expCallLocation = $11
    }
}

-- *************
-- *           *
-- * exp_field *
-- *           *
-- *************
exp_field:
'Attribute'
'('
    'value' '=' exp_str ','
    'attr' '=' ID ','
    'ctx' '=' ctx ','
    loc
')'
{
    $5
}

slice_lower: 'lower' '=' exp ',' { $3 }
slice_step:  'step'  '=' exp ',' { $3 }
slice_upper: 'upper' '=' exp ',' { $3 }

-- *************
-- *           *
-- * exp_slice *
-- *           *
-- *************
exp_slice:
'Slice'
'('
    optional(slice_lower)
    optional(slice_upper)
    optional(slice_step)
    loc
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = expmyname (Token.Named "slicify" $6),
        Ast.args = [],
        Ast.expCallLocation = $6
    }
}

-- **********
-- *        *
-- * exp_if *
-- *        *
-- **********
exp_if:
'IfExp'
'('
    'test' '=' exp ','
    'body' '=' exp ','
    'orelse' '=' exp ','
    loc
')'
{
    $5
}

-- ********
-- *      *
-- * exps *
-- *      *
-- ********
exps: possibly_empty_listof(exp) { $1 }

none: 'None' { Nothing }

dict_key:
exp  { Nothing } |
none { Nothing }

dict_keys: possibly_empty_listof(dict_key) { $1 }

-- ************
-- *          *
-- * exp_dict *
-- *          *
-- ************
exp_dict:
'Dict'
'('
    'keys' '=' dict_keys ','
    'values' '=' exps ','
    loc
')'
{
    Ast.ExpInt $ Ast.ExpIntContent
    {
        Ast.expIntValue = Token.ConstInt
        {
            Token.constIntValue = 999,
            Token.constIntLocation = $11
        }
    } 
}

-- ****************
-- *              *
-- * exp_ellipsis *
-- *              *
-- ****************
exp_ellipsis:
'Constant'
'('
    'value' '=' 'Ellipsis' ','
    loc
')'
{
    Ast.ExpInt $ Ast.ExpIntContent
    {
        Ast.expIntValue = Token.ConstInt
        {
            Token.constIntValue = 999,
            Token.constIntLocation = $7
        }
    }
}

-- *************
-- *           *
-- * exp_tuple *
-- *           *
-- *************
exp_tuple:
'Tuple'
'('
    'elts' '=' possibly_empty_listof(exp) ','
    'ctx' '=' ctx ','
    loc
')'
{
    case $5 of {
        [] -> Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 888 $11;
        (x:_) -> x
    }
}

comprehension:
'comprehension'
'('
    'target' '=' exp ','
    'iter' '=' exp ','
    'ifs' '=' possibly_empty_listof(exp) ','
    'is_async' '=' INT
')'
{
    Nothing
}

exp_setcomp:
'SetComp'
'('
    'elt' '=' exp ','
    'generators' '=' nonempty_listof(comprehension) ','
    loc
')'
{
    $5
}


exp_listcomp:
'ListComp'
'('
    'elt' '=' exp ','
    'generators' '=' nonempty_listof(comprehension) ','
    loc
')'
{
    $5
}

exp_dictcomp:
'DictComp'
'('
    'key' '=' exp ','
    'value' '=' exp ','
    'generators' '=' nonempty_listof(comprehension) ','
    loc
')'
{
    $5
}

generator:
'GeneratorExp'
'('
    'elt' '=' exp ','
    'generators' '=' nonempty_listof(comprehension) ','
    loc
')'
{
    $5
}

exp_set:
'Set'
'('
    'elts' '=' possibly_empty_listof(exp) ','
    loc
')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 999,
        Token.constIntLocation = $7
    }
}

exp_lambda:
'Lambda'
'('
    'args' '=' params ','
    'body' '=' exp ','
    loc 
')'
{
    $9
}

exp_starred:
'Starred'
'('
    'value' '=' exp ','
    'ctx' '=' ctx ','
    loc
')'
{
    $5
}

exp_namedexpr:
'NamedExpr'
'('
    'target' '=' exp ','
    'value' '=' exp ','
    loc
')'
{
    case $5 of
        (Ast.ExpVar (Ast.ExpVarContent v)) -> Ast.ExpAssign $ Ast.ExpAssignContent
            {
                Ast.expAssignLhs = v,
                Ast.expAssignRhs = $9,
                Ast.expAssignLocation = $11
            }
        _ -> $9
}

exp_await:
'Await'
'('
    'value' '=' exp ','
    loc
')'
{
    $5
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_if        { $1 } |
exp_str       { $1 } |
exp_int       { $1 } |
exp_none      { $1 } |
exp_yield     { $1 } |
exp_yield_from { $1 } |
exp_starred   { $1 } |
exp_dict      { $1 } |
exp_var       { $1 } |
exp_bool      { $1 } |
exp_set       { $1 } |
exp_list      { $1 } |
generator     { $1 } |
exp_setcomp   { $1 } |
exp_listcomp  { $1 } |
exp_dictcomp  { $1 } |
exp_tuple     { $1 } |
exp_lambda    { $1 } |
exp_unop      { $1 } |
exp_slice     { $1 } |
exp_relop     { $1 } |
exp_namedexpr { $1 } |
exp_await     { $1 } |
exp_binop     { $1 } |
exp_call      { Ast.ExpCall $1 } |
exp_fstring   { $1 } |
exp_ellipsis  { $1 }

keyword_arg: 'arg' '=' tokenID ',' { $3 }

-- ***********
-- *         *
-- * keyword *
-- *         *
-- ***********
keyword:
'keyword'
'('
    optional(keyword_arg)
    'value' '=' exp ','
    loc
')'
{
    Ast.ExpKwArg $ Ast.ExpKwArgContent
    {
        Ast.expKwArgName = case $3 of { Just kw -> kw; _ -> "" },
        Ast.expKwArgValue = $6,
        Ast.expKwArgLocation = $8
    }
}

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************
keywords:
'[' ']'                      { [] } | 
'[' commalistof(keyword) ']' { $2 }

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call:
'Call'
'('
    'func' '=' exp ','
    'args' '=' args ','
    'keywords' '=' keywords ','
    loc
')'
{
    Ast.ExpCallContent
    {
        Ast.callee = $5,
        Ast.args = $9 ++ $13,
        Ast.expCallLocation = $15
    }
}

-- *********
-- *       *
-- * stmts *
-- *       *
-- *********
stmts: '[' ']' { [] } | '[' commalistof(stmt) ']' { $2 }

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if:
'If'
'('
    'test' '=' exp ','
    'body' '=' stmts ','
    'orelse' '=' stmts ','
    loc 
')'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $5,
        Ast.stmtIfBody = $9,
        Ast.stmtElseBody = $13,
        Ast.stmtIfLocation = $15
    }
}

-- **************
-- *            *
-- * stmt_while *
-- *            *
-- **************
stmt_while:
'While'
'('
    'test' '=' exp ','
    'body' '=' stmts ','
    'orelse' '=' stmts ','
    loc 
')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $5,
        Ast.stmtWhileBody = $9,
        Ast.stmtWhileLocation = $15
    }
}


-- ****************
-- *              *
-- * return_value *
-- *              *
-- ****************
return_value: 'value' '=' exp ',' { $3 }

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return: 'Return' '(' optional(return_value) loc ')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = $3,
        Ast.stmtReturnLocation = $4
    }
}

-- *******************
-- *                 *
-- * stmt_aug_assign *
-- *                 *
-- *******************
stmt_aug_assign:
'AugAssign'
'('
    'target' '=' var ','
    'op' '=' op ','
    'value' '=' exp ','
    loc
')'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = $5,
        Ast.stmtAssignRhs = $13
    }
}

-- *************
-- *           *
-- * with_vars *
-- *           *
-- *************
with_vars: ',' 'optional_vars' '=' name { $4 }

-- *************
-- *           *
-- * with_item *
-- *           *
-- *************
with_item:
'withitem'
'('
    'context_expr' '=' exp
    optional(with_vars)
')'
{
    case $6 of {
        Nothing -> Ast.StmtExp $5;
        Just name -> Ast.StmtAssign $ Ast.StmtAssignContent
            {
                Ast.stmtAssignLhs = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName name,
                Ast.stmtAssignRhs = $5
            }
    }
}

-- *************
-- *           *
-- * stmt_with *
-- *           *
-- *************
stmt_with:
'With'
'('
    'items' '=' '[' commalistof(with_item) ']' ','
    'body' '=' stmts ','
    loc
')'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool True $13,
        Ast.stmtIfBody = $6 ++ $11,
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $13
    }
}

-- instrumented as dhscanner Ast.StmtBlock ( subject assignment + if / elif / else chain )
stmtMatch:
'Match'
'('
    'subject' '=' exp ','
    'cases' '=' nonempty_listof(matchCase) ','
    loc
')'
{
    Actions.stmtMatch $11 $5 $9
}

matchGuard: 'guard' '=' exp ',' { $3 }

matchCase:
'match_case'
'('
    'pattern' '=' pattern ','
    optional(matchGuard)
    'body' '=' stmts
')'
{
    Actions.MatchCase $5 $7 $10
}

patterns: possibly_empty_listof(pattern) { $1 }

pattern:
patternValue     { $1 } |
patternSingleton { $1 } |
patternSequence  { $1 } |
patternMapping   { $1 } |
patternClass     { $1 } |
patternStar      { $1 } |
patternAs        { $1 } |
patternOr        { $1 }

patternValue: 'MatchValue' '(' 'value' '=' exp ',' loc ')' { Actions.PatternValue $7 $5 }

singleton:
'None'  { Actions.expNone } |
'True'  { Actions.expBool True } |
'False' { Actions.expBool False }

patternSingleton: 'MatchSingleton' '(' 'value' '=' singleton ',' loc ')' { Actions.PatternValue $7 ($5 $7) }

patternSequence: 'MatchSequence' '(' 'patterns' '=' patterns ',' loc ')' { Actions.PatternSequence $7 $5 }

patternRest: 'rest' '=' tokenID ',' { $3 }

patternMapping:
'MatchMapping'
'('
    'keys' '=' exps ','
    'patterns' '=' patterns ','
    optional(patternRest)
    loc
')'
{
    Actions.PatternMapping $12 (zip $5 $9) $11
}

patternClass:
'MatchClass'
'('
    'cls' '=' exp ','
    'patterns' '=' patterns ','
    'kwd_attrs' '=' possibly_empty_listof(tokenID) ','
    'kwd_patterns' '=' patterns ','
    loc
')'
{
    Actions.PatternClass $19 $5 $9 (zip $13 $17)
}

patternName: 'name' '=' tokenID ',' { $3 }

patternStar: 'MatchStar' '(' optional(patternName) loc ')' { Actions.PatternStar $4 $3 }

patternAsPattern: 'pattern' '=' pattern ',' { $3 }

patternAs: 'MatchAs' '(' optional(patternAsPattern) optional(patternName) loc ')' { Actions.PatternAs $5 $3 $4 }

patternOr: 'MatchOr' '(' 'patterns' '=' patterns ',' loc ')' { Actions.PatternOr $7 $5 }

-- instrumented as dhscanner Ast.StmtClass ( + a [default-method] method )
stmtClass:
'ClassDef'
'('
    'name' '=' tokenID ','
    'bases' '=' possibly_empty_listof(var) ','
    'keywords' '=' keywords ','
    'body' '=' stmts ','
    'decorator_list' '=' exps ','
    'type_params' '=' possibly_empty_listof(type_param) ','
    loc
')'
{
    Actions.stmtClass $27 $5 $9 $17
}

ann_assign_simple: 'simple' '=' INT ',' { Nothing }
ann_assign_exp: 'value' '=' exp ',' { $3 }

-- *******************
-- *                 *
-- * stmt_ann_assign *
-- *                 *
-- *******************
stmt_ann_assign:
'AnnAssign'
'('
    'target' '=' var ','
    'annotation' '=' exp ','
    optional(ann_assign_exp)
    ann_assign_simple
    loc
')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = Nothing,
        Ast.stmtReturnLocation = $13
    }
}

stmt_typealias:
'TypeAlias'
'('
    'name' '=' name ','
    'type_params' '=' '[' ']' ','
    'value' '=' exp ','
    loc
')'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $5,
        Ast.stmtAssignRhs = $14
    }
}

-- ******************
-- *                *
-- * exception_type *
-- *                *
-- ******************
exception_type: 'type' '=' exp ',' { $3 }


-- ******************
-- *                *
-- * exception_name *
-- *                *
-- ******************
exception_name: 'name' '=' ID ',' { $3 }

-- *********************
-- *                   *
-- * exception_handler *
-- *                   *
-- *********************
exception_handler:
'ExceptHandler'
'('
    optional(exception_type)
    optional(exception_name)
    'body' '=' stmts ','
    loc
')'
{
    Nothing
}

-- **********************
-- *                    *
-- * exception_handlers *
-- *                    *
-- **********************
exception_handlers: possibly_empty_listof(exception_handler) { $1 }

-- ************
-- *          *
-- * stmt_try *
-- *          *
-- ************
stmt_try:
'Try'
'('
    'body' '=' stmts ','
    'handlers' '=' exception_handlers ','
    'orelse' '=' stmts ','
    'finalbody' '=' stmts ','
    loc
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = $5,
        Ast.stmtBlockLocation = $19
    }
}

-- ************
-- *          *
-- * stmt_for *
-- *          *
-- ************
stmt_for:
'For'
'('
    'target' '=' exp ','
    'iter' '=' exp ','
    'body' '=' stmts ','
    'orelse' '=' stmts ','
    loc
')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $9,
        Ast.stmtWhileBody = $13,
        Ast.stmtWhileLocation = $19
    }
}

yield_exp: 'value' '=' exp ',' { $3 }

-- *************
-- *           *
-- * exp_yield *
-- *           *
-- *************
exp_yield:
'Yield'
'('
    optional(yield_exp)
    loc
')'
{
    dummyExp $4
}

exp_yield_from:
'YieldFrom'
'('
    'value' '=' exp ','
    loc
')'
{
    $5
}

-- ************
-- *          *
-- * stmt_exp *
-- *          *
-- ************
stmt_exp:
'Expr'
'('
    'value' '=' exp ','
    loc
')'
{
    Ast.StmtExp $5
}

nonempty_raise_part: 'exc'   '=' exp ',' { $3 }
nonempty_cause_part: 'cause' '=' exp ',' { $3 }

-- **************
-- *            *
-- * stmt_raise *
-- *            *
-- **************
stmt_raise:
'Raise'
'('
    optional(nonempty_raise_part)
    optional(nonempty_cause_part)
    loc
')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = $3,
        Ast.stmtReturnLocation = $5
    }
}

-- *****************
-- *               *
-- * stmt_continue *
-- *               *
-- *****************
stmt_continue:
'Continue'
'('
    loc
')'
{
    Ast.StmtContinue $ Ast.StmtContinueContent
    {
        Ast.stmtContinueLocation = $3
    }
}

assert_msg:
'msg' '=' exp ','
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_assert *
-- *             *
-- ***************
stmt_assert:
'Assert'
'('
    'test' '=' exp ','
    optional(assert_msg)
    loc
')'
{
    Ast.StmtContinue $ Ast.StmtContinueContent
    {
        Ast.stmtContinueLocation = $8
    }
}

-- *************
-- *           *
-- * stmt_pass *
-- *           *
-- *************
stmt_pass: 'Pass' '(' loc ')'
{
    Ast.StmtContinue $ Ast.StmtContinueContent
    {
        Ast.stmtContinueLocation = $3
    }
}

-- **************
-- *            *
-- * stmt_break *
-- *            *
-- **************
stmt_break:
'Break'
'('
    loc
')'
{
    Ast.StmtBreak $ Ast.StmtBreakContent
    {
        Ast.stmtBreakLocation = $3
    }
}

-- ***************
-- *             *
-- * stmt_delete *
-- *             *
-- ***************
stmt_delete:
'Delete'
'('
    'targets' '=' nonempty_listof(var) ','
    loc
')'
{
    Ast.StmtBreak $ Ast.StmtBreakContent
    {
        Ast.stmtBreakLocation = $7
    }
}

-- ***************
-- *             *
-- * stmt_global *
-- *             *
-- ***************
stmt_global:
'Global'
'('
    'names' '=' nonempty_listof(tokenID) ','
    loc
')'
{
    Ast.StmtBreak $ Ast.StmtBreakContent
    {
        Ast.stmtBreakLocation = $7
    }
}


-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if          { $1 } |
stmt_try         { $1 } |
stmt_exp         { $1 } |
stmt_for         { $1 } |
stmt_with        { $1 } |
stmtClass        { $1 } |
stmt_while       { $1 } |
stmt_break       { $1 } |
stmtImport       { $1 } |
stmt_raise       { $1 } |
stmt_delete      { $1 } |
stmt_return      { $1 } |
stmt_assign      { $1 } |
stmt_assert      { $1 } |
stmt_global      { $1 } |
stmt_function    { $1 } |
stmt_pass        { $1 } |
stmt_continue    { $1 } |
stmt_aug_assign  { $1 } |
stmt_ann_assign  { $1 } |
stmt_typealias    { $1 } |
stmtMatch        { $1 } |
stmt_import_from { $1 }

-- *************
-- *           *
-- * type_hint *
-- *           *
-- *************
type_param:
'TypeVar'
'('
    'name' '=' ID ','
    loc
')'
{
    Nothing
}

type_hint: 'annotation' '=' exp ',' { $3 }

-- *********
-- *       *
-- * param *
-- *       *
-- *********
param:
'arg'
'('
    'arg' '=' tokenID ','
    optional(type_hint)
    loc
')'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $ Token.Named
        {
            Token.content = $5,
            Token.location = $8 { Location.colEnd = (Location.colStart $8) + (fromIntegral (length $5)) }
        },
        Ast.paramNominalType = nominalTypeMe $7,
        Ast.paramSerialIdx = 156,
        Ast.paramAnnotations = []
    }
}

defaults:
'['                  ']' { Nothing } |
'[' commalistof(exp) ']' { Nothing }

kw_default: exp { Nothing } | none { Nothing }

vararg: 'vararg' '=' param ',' { $3 }
kwarg:  'kwarg'  '=' param ',' { $3 } 

-- **********
-- *        *
-- * params *
-- *        *
-- **********
params:
'arguments'
'('
    'posonlyargs' '=' '[' ']' ','
    'args' '=' possibly_empty_listof(param) ','
    optional(vararg)
    'kwonlyargs' '=' possibly_empty_listof(param) ','
    'kw_defaults' '=' possibly_empty_listof(kw_default) ','
    optional(kwarg)
    'defaults' '=' defaults
')'
{
    $10
}

function_def:
'FunctionDef'      { Nothing } |
'AsyncFunctionDef' { Nothing }

return_type_hint: 'returns' '=' exp ',' { Nothing }

-- *****************
-- *               *
-- * stmt_function *
-- *               *
-- *****************
stmt_function:
function_def
'('
    'name' '=' ID ','
    'args' '=' params ','
    'body' '=' stmts ','
    'decorator_list' '=' exps ','
    optional(return_type_hint) 
    'type_params' '=' possibly_empty_listof(type_param) ','
    loc
')'
{
    Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Just (varme (Token.Named "any" $24)),
        Ast.stmtFuncName = Token.FuncName (Token.Named (unquote (tokIDValue $5)) $24),
        Ast.stmtFuncParams = $9,
        Ast.stmtFuncBody = $13,
        Ast.stmtFuncAnnotations = $17,
        Ast.stmtFuncLocation = $24
    }
}

-- *******
-- *     *
-- * ctx *
-- *     *
-- *******
ctx:
'Store' '(' ')' { Nothing } |
'Del'   '(' ')' { Nothing } |
'Load'  '(' ')' { Nothing }

-- ********
-- *      *
-- * name *
-- *      *
-- ********
name:
'Name'
'('
    'id' '=' tokenID ','
    'ctx' '=' ctx ','
    loc
')'
{
    Token.Named
    {
        Token.content = $5,
        Token.location = $11
    }
}

-- ***************
-- *             *
-- * stmt_assign *
-- *             *
-- ***************
stmt_assign:
'Assign'
'('
    'targets' '=' nonempty_listof(exp) ','
    'value' '=' exp ','
    loc
')'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = case $5 of
            ((Ast.ExpVar (Ast.ExpVarContent v)):_) -> v
            _ -> Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named "dummyAssignVar" $11))),
        Ast.stmtAssignRhs = $9
    }
}

-- instrumented as dhscanner Ast.StmtBlock ( one Ast.StmtImport per imported name )
stmtImport:
'Import'
'('
    'names' '=' '[' commalistof(alias) ']' ','
    loc
')'
{
    Actions.stmtImport $9 $6
}

import_from_module: 'module' '=' tokenID ',' { $3 }


-- ********************
-- *                  *
-- * stmt_import_from *
-- *                  *
-- ********************
stmt_import_from:
'ImportFrom'
'('
    optional(import_from_module)
    'names' '=' '[' commalistof(alias) ']' ','
    'level' '=' INT ','
    loc
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = importify $14 $3 ((tokIntValue $12) > 0) (getAdditionalRepoInfo $1) $7,
        Ast.stmtBlockLocation = $14
    }
}

-- ***********
-- *         *
-- * tokenID *
-- *         *
-- ***********
tokenID: ID { unquote (tokIDValue $1) }

-- *********
-- *       *
-- * alias *
-- *       *
-- *********
alias:
'alias'
'('
    'name' '=' tokenID ','
    optional(asname)
    loc
')'
{
    ($5, $7)
}

-- **********
-- *        *
-- * asname *
-- *        *
-- **********
asname: 'asname' '=' ID ',' { unquote (tokIDValue $3) }

-- ************
-- *          *
-- * location *
-- *          *
-- ************
loc:
'lineno' '=' INT ','
'col_offset' '=' INT ','
'end_lineno' '=' INT ','
'end_col_offset' '=' INT
{
    Location
    {
        Location.filename = getFilename $1,
        lineStart = fromIntegral (tokIntValue $3),
        colStart = 1 + (fromIntegral (tokIntValue $7)),
        lineEnd = fromIntegral (tokIntValue $11),
        colEnd = 1 + (fromIntegral (tokIntValue $15))
    }
}

{

dummyExp :: Location -> Ast.Exp
dummyExp loc = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 888 loc

expmyname :: Token.Named -> Ast.Exp
expmyname = Ast.ExpVar . Ast.ExpVarContent . Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

importify :: Location -> (Maybe String) -> Bool -> Common.AdditionalRepoInfo -> [(String, Maybe String)] -> [ Ast.Stmt ]
importify loc Nothing localImportLevel _ = Data.List.map (importify' loc localImportLevel)
importify loc (Just src) localImportLevel repoInfo = Data.List.map (importify'' loc src localImportLevel repoInfo)

importify' :: Location -> Bool -> (String, Maybe String) -> Ast.Stmt
importify' loc localImportLevel (specific, Nothing) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent ""),
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific specific),
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = loc
}
importify' loc localImportLevel (specific, Just alias) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource  = ImportThirdParty (ImportThirdPartyContent ""),
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific specific),
    Ast.stmtImportAlias = Just (Ast.ImportAlias alias),
    Ast.stmtImportLocation = loc
}

importify'' :: Location -> String -> Bool -> Common.AdditionalRepoInfo -> (String, Maybe String) -> Ast.Stmt
importify'' loc src localImportLevel repoInfo (specific, Nothing) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = resolve (Location.filename loc) src localImportLevel repoInfo,
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific specific),
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = loc
}
importify'' loc src localImportLevel repoInfo (specific, Just alias) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = resolve (Location.filename loc) src localImportLevel repoInfo,
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific specific),
    Ast.stmtImportAlias = Just (Ast.ImportAlias alias),
    Ast.stmtImportLocation = loc
}

nominalTypeMe :: Maybe Ast.Exp -> Maybe Ast.Var
nominalTypeMe (Just (Ast.ExpVar (Ast.ExpVarContent v))) = Just v
nominalTypeMe _ = Nothing

varme :: Token.Named -> Ast.Var
varme = Ast.VarSimple . Ast.VarSimpleContent . Token.VarName


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

