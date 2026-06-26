{
{- _OPTIONS -Werror=missing-fields #-}

module PhpParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import PhpLexer
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
import Data.List ( map, foldl' )
import Data.Map ( empty, fromList )

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

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

':'    { AlexTokenTag AlexRawToken_COLON  _ }
'\\'   { AlexTokenTag AlexRawToken_SLASH  _ }
'-'    { AlexTokenTag AlexRawToken_HYPHEN _ }
'|'    { AlexTokenTag AlexRawToken_OR     _ }

-- *********************
-- *                   *
-- * reserved keywords *
-- *                   *
-- *********************

'Arg'                   { AlexTokenTag AlexRawToken_ARG             _ }
'var'                   { AlexTokenTag AlexRawToken_VAR             _ }
'args'                  { AlexTokenTag AlexRawToken_ARGS            _ }
'name'                  { AlexTokenTag AlexRawToken_NAME            _ }
'uses'                  { AlexTokenTag AlexRawToken_USES            _ }
'expr'                  { AlexTokenTag AlexRawToken_EXPR            _ }
'Name'                  { AlexTokenTag AlexRawToken_MAME            _ }
'null'                  { AlexTokenTag AlexRawToken_NULL            _ }
'alias'                 { AlexTokenTag AlexRawToken_ALIAS           _ }
'type'                  { AlexTokenTag AlexRawToken_TYPE            _ }
'left'                  { AlexTokenTag AlexRawToken_LEFT            _ }
'loop'                  { AlexTokenTag AlexRawToken_LOOP            _ }
'init'                  { AlexTokenTag AlexRawToken_INIT            _ }
'cond'                  { AlexTokenTag AlexRawToken_COND            _ }
'Const'                 { AlexTokenTag AlexRawToken_CONST           _ }
'exprs'                 { AlexTokenTag AlexRawToken_EXPRS           _ }
'value'                 { AlexTokenTag AlexRawToken_VALUE           _ }
'right'                 { AlexTokenTag AlexRawToken_RIGHT           _ }
'stmts'                 { AlexTokenTag AlexRawToken_STMTS           _ }
'array'                 { AlexTokenTag AlexRawToken_ARRAY           _ }
'Param'                 { AlexTokenTag AlexRawToken_PARAM           _ }
'UseItem'               { AlexTokenTag AlexRawToken_USE_ITEM        _ }
'Stmt_If'               { AlexTokenTag AlexRawToken_STMT_IF         _ }
'Stmt_Else'             { AlexTokenTag AlexRawToken_STMT_ELSE       _ }
'Stmt_Catch'            { AlexTokenTag AlexRawToken_STMT_CATCH      _ }
'Stmt_Do'               { AlexTokenTag AlexRawToken_STMT_DO         _ }
'Stmt_While'            { AlexTokenTag AlexRawToken_STMT_WHILE      _ }
'Stmt_ElseIf'           { AlexTokenTag AlexRawToken_STMT_ELIF       _ }
'Stmt_TryCatch'         { AlexTokenTag AlexRawToken_STMT_TRY_CATCH  _ }
'Stmt_For'              { AlexTokenTag AlexRawToken_STMT_FOR        _ }
'Stmt_Nop'              { AlexTokenTag AlexRawToken_STMT_NOP        _ }
'Stmt_Namespace'        { AlexTokenTag AlexRawToken_STMT_NAMESPACE  _ }
'Stmt_Trait'            { AlexTokenTag AlexRawToken_STMT_TRAIT      _ }
'Stmt_TraitUse'         { AlexTokenTag AlexRawToken_STMT_TRAITUSE   _ }
'Stmt_Switch'           { AlexTokenTag AlexRawToken_STMT_SWITCH     _ }
'Stmt_Case'             { AlexTokenTag AlexRawToken_STMT_CASE       _ }
'Stmt_Foreach'          { AlexTokenTag AlexRawToken_STMT_FOREACH    _ }
'Stmt_Echo'             { AlexTokenTag AlexRawToken_STMT_ECHO       _ }
'Stmt_Unset'            { AlexTokenTag AlexRawToken_STMT_UNSET      _ }
'Expr_Closure'          { AlexTokenTag AlexRawToken_EXPR_LAMBDA     _ }
'Expr_ErrorSuppress'    { AlexTokenTag AlexRawToken_ERROR_SUPPRESS  _ }
'Name_FullyQualified'   { AlexTokenTag AlexRawToken_FULLY_QUALIFIED _ }
'ClosureUse'            { AlexTokenTag AlexRawToken_CLOSURE_USE     _ }
'Expr_Instanceof'       { AlexTokenTag AlexRawToken_EXPR_INSTOF     _ }
'Expr_Cast_Double'      { AlexTokenTag AlexRawToken_EXPR_CAST       _ }
'Expr_Cast_Int'         { AlexTokenTag AlexRawToken_EXPR_CAST2      _ }
'Expr_Cast_Bool'        { AlexTokenTag AlexRawToken_EXPR_CAST4      _ }
'Expr_Cast_Object'      { AlexTokenTag AlexRawToken_EXPR_CAST5      _ }
'Expr_Cast_String'      { AlexTokenTag AlexRawToken_EXPR_CAST3      _ }
'Expr_New'              { AlexTokenTag AlexRawToken_EXPR_NEW        _ }
'Expr_Exit'             { AlexTokenTag AlexRawToken_EXPR_EXIT       _ }
'Expr_Include'          { AlexTokenTag AlexRawToken_EXPR_IMPORT     _ }
'Expr_Ternary'          { AlexTokenTag AlexRawToken_EXPR_TERNARY    _ }
'Expr_Throw'            { AlexTokenTag AlexRawToken_EXPR_THROW      _ }
'Expr_Variable'         { AlexTokenTag AlexRawToken_EXPR_VAR        _ }
'Expr_FuncCall'         { AlexTokenTag AlexRawToken_EXPR_CALL       _ }
'Expr_MethodCall'       { AlexTokenTag AlexRawToken_EXPR_MCALL      _ }
'Expr_StaticCall'       { AlexTokenTag AlexRawToken_EXPR_SCALL      _ }
'Stmt_Use'              { AlexTokenTag AlexRawToken_STMT_USE        _ }
'Stmt_Expression'       { AlexTokenTag AlexRawToken_STMT_EXPR       _ }
'Scalar_Int'            { AlexTokenTag AlexRawToken_SCALAR_INT      _ }
'Scalar_Float'          { AlexTokenTag AlexRawToken_SCALAR_FLOAT    _ }
'Scalar_InterpolatedString' { AlexTokenTag AlexRawToken_SCALAR_FSTRING _ }
'Scalar_MagicConst_Class' { AlexTokenTag AlexRawToken_SCALAR_CLASS  _ }
'Scalar_MagicConst_File' { AlexTokenTag AlexRawToken_SCALAR_FILE    _ }
'Scalar_MagicConst_Dir' { AlexTokenTag AlexRawToken_SCALAR_DIR      _ }
'Identifier'            { AlexTokenTag AlexRawToken_IDENTIFIER      _ }
'Stmt_Return'           { AlexTokenTag AlexRawToken_STMT_RETURN     _ }
'Stmt_Property'         { AlexTokenTag AlexRawToken_STMT_PROPERTY   _ }
'Stmt_Interface'        { AlexTokenTag AlexRawToken_STMT_INTERFACE  _ }
'Stmt_ClassMethod'      { AlexTokenTag AlexRawToken_STMT_CLASSMETH  _ }
'returnType'            { AlexTokenTag AlexRawToken_RETURN_TYPE     _ }
'Stmt_Class'            { AlexTokenTag AlexRawToken_STMT_CLASS      _ }
'Stmt_ClassConst'       { AlexTokenTag AlexRawToken_STMT_CLASS_CONST _ }
'Stmt_Continue'         { AlexTokenTag AlexRawToken_STMT_CONT       _ }
'Stmt_Break'            { AlexTokenTag AlexRawToken_STMT_BREAK      _ }
'Stmt_Static'           { AlexTokenTag AlexRawToken_STMT_STATIC     _ }
'StaticVar'             { AlexTokenTag AlexRawToken_STATIC_VAR      _ }
'Stmt_Global'           { AlexTokenTag AlexRawToken_STMT_GLOBAL     _ }
'Stmt_Function'         { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ }
'Expr_Assign'           { AlexTokenTag AlexRawToken_EXPR_ASSIGN     _ }
'Expr_AssignOp_Div'     { AlexTokenTag AlexRawToken_EXPR_ASSIGN4    _ }
'Expr_AssignOp_Plus'    { AlexTokenTag AlexRawToken_EXPR_ASSIGN2    _ }
'Expr_AssignOp_Minus'   { AlexTokenTag AlexRawToken_EXPR_ASSIGN5    _ }
'Expr_AssignOp_Concat'  { AlexTokenTag AlexRawToken_EXPR_ASSIGN3    _ }
'Expr_Array'            { AlexTokenTag AlexRawToken_EXPR_ARRAY      _ }
'Expr_List'             { AlexTokenTag AlexRawToken_EXPR_LIST       _ }
'Expr_Empty'            { AlexTokenTag AlexRawToken_EXPR_EMPTY      _ }
'ArrayItem'             { AlexTokenTag AlexRawToken_EXPR_ARRAY3     _ }
'Expr_ArrayDimFetch'    { AlexTokenTag AlexRawToken_EXPR_ARRAY2     _ }
'Expr_ClassConstFetch'  { AlexTokenTag AlexRawToken_EXPR_FETCH      _ }
'Expr_Isset'            { AlexTokenTag AlexRawToken_EXPR_ISSET      _ }
'Expr_ConstFetch'       { AlexTokenTag AlexRawToken_EXPR_CONST_GET  _ }
'Expr_PropertyFetch'    { AlexTokenTag AlexRawToken_EXPR_PROP_GET   _ }
'Expr_StaticPropertyFetch' { AlexTokenTag AlexRawToken_EXPR_PROP_GET2  _ }
'Expr_BooleanNot'       { AlexTokenTag AlexRawToken_EXPR_UNOP_NOT   _ }
'Expr_UnaryMinus'       { AlexTokenTag AlexRawToken_EXPR_UNOP_MINUS _ }
'Expr_PostInc'          { AlexTokenTag AlexRawToken_EXPR_POST_INC   _ }
'Expr_Print'            { AlexTokenTag AlexRawToken_EXPR_PRINT      _ }
'Expr_PostDec'          { AlexTokenTag AlexRawToken_EXPR_POST_DEC   _ }
'Expr_BinaryOp_Mul'     { AlexTokenTag AlexRawToken_EXPR_BINOP_MUL  _ }
'Expr_BinaryOp_BitwiseOr' { AlexTokenTag AlexRawToken_EXPR_BINOP_OR2  _ }
'Expr_BinaryOp_Div'     { AlexTokenTag AlexRawToken_EXPR_BINOP_DIV  _ }
'Expr_BinaryOp_Plus'    { AlexTokenTag AlexRawToken_EXPR_BINOP_PLUS _ }
'Expr_BinaryOp_Minus'   { AlexTokenTag AlexRawToken_EXPR_BINOP_MINUS _ }
'Expr_BinaryOp_Concat'  { AlexTokenTag AlexRawToken_EXPR_BINOP_CONCAT _ }
'Expr_BinaryOp_BooleanOr' { AlexTokenTag AlexRawToken_EXPR_BINOP_OR _ }
'Expr_BinaryOp_LogicalOr' { AlexTokenTag AlexRawToken_EXPR_BINOP_LOR _ }
'Expr_BinaryOp_BooleanAnd' { AlexTokenTag AlexRawToken_EXPR_BINOP_AND _ }
'Expr_BinaryOp_Smaller' { AlexTokenTag AlexRawToken_EXPR_BINOP_LT   _ }
'Expr_BinaryOp_SmallerOrEqual' { AlexTokenTag AlexRawToken_EXPR_BINOP_LEQ _ }
'Expr_BinaryOp_Equal'   { AlexTokenTag AlexRawToken_EXPR_BINOP_EQ   _ }
'Expr_BinaryOp_NotEqual'  { AlexTokenTag AlexRawToken_EXPR_BINOP_NEQ   _ }
'Expr_BinaryOp_Greater' { AlexTokenTag AlexRawToken_EXPR_BINOP_GT   _ }
'Expr_BinaryOp_GreaterOrEqual' { AlexTokenTag AlexRawToken_EXPR_BINOP_GEQ _ }
'Expr_BinaryOp_Coalesce' { AlexTokenTag AlexRawToken_EXPR_BINOP_CO   _ }
'Expr_BinaryOp_Identical' { AlexTokenTag AlexRawToken_EXPR_BINOP_IS _ }
'Expr_BinaryOp_NotIdentical' { AlexTokenTag AlexRawToken_EXPR_BINOP_ISNOT _ }
'Expr_NullsafePropertyFetch' { AlexTokenTag AlexRawToken_Expr_NullsafePropertyFetch _ }
'Expr_BinaryOp_LogicalAnd' { AlexTokenTag AlexRawToken_Expr_BinaryOp_LogicalAnd _ }
'attrGroups' { AlexTokenTag AlexRawToken_attrGroups _ }
'flags' { AlexTokenTag AlexRawToken_flags _ }
'extends' { AlexTokenTag AlexRawToken_extends _ }
'implements' { AlexTokenTag AlexRawToken_implements _ }
'false' { AlexTokenTag AlexRawToken_false _ }
'byRef' { AlexTokenTag AlexRawToken_byRef _ }
'params' { AlexTokenTag AlexRawToken_params _ }
'default' { AlexTokenTag AlexRawToken_default _ }
'variadic' { AlexTokenTag AlexRawToken_variadic _ }
'unpack' { AlexTokenTag AlexRawToken_unpack _ }
'key' { AlexTokenTag AlexRawToken_key _ }
'keyVar' { AlexTokenTag AlexRawToken_keyVar _ }
'valueVar' { AlexTokenTag AlexRawToken_valueVar _ }
'consts' { AlexTokenTag AlexRawToken_consts _ }
'static' { AlexTokenTag AlexRawToken_static _ }
'Expr_PreInc' { AlexTokenTag AlexRawToken_Expr_PreInc _ }
'Scalar_String' { AlexTokenTag AlexRawToken_Scalar_String _ }
'traits' { AlexTokenTag AlexRawToken_traits _ }
'adaptations' { AlexTokenTag AlexRawToken_adaptations _ }
'props' { AlexTokenTag AlexRawToken_props _ }
'VarLikeIdentifier' { AlexTokenTag AlexRawToken_VarLikeIdentifier _ }
'PropertyItem' { AlexTokenTag AlexRawToken_PropertyItem _ }
'Expr_BinaryOp_Pow' { AlexTokenTag AlexRawToken_Expr_BinaryOp_Pow _ }
'Expr_ArrowFunction' { AlexTokenTag AlexRawToken_Expr_ArrowFunction _ }
'parts' { AlexTokenTag AlexRawToken_parts _ }
'InterpolatedStringPart' { AlexTokenTag AlexRawToken_InterpolatedStringPart _ }
'true' { AlexTokenTag AlexRawToken_true _ }
'Expr_BinaryOp_BitwiseAnd' { AlexTokenTag AlexRawToken_Expr_BinaryOp_BitwiseAnd _ }
'Expr_BinaryOp_ShiftRight' { AlexTokenTag AlexRawToken_Expr_BinaryOp_ShiftRight _ }
'Expr_BinaryOp_ShiftLeft' { AlexTokenTag AlexRawToken_Expr_BinaryOp_ShiftLeft _ }
'Expr_AssignOp_BitwiseXor' { AlexTokenTag AlexRawToken_Expr_AssignOp_BitwiseXor _ }
'Expr_BinaryOp_BitwiseXor' { AlexTokenTag AlexRawToken_Expr_BinaryOp_BitwiseXor _ }
'Expr_AssignOp_ShiftRight' { AlexTokenTag AlexRawToken_Expr_AssignOp_ShiftRight _ }
'Expr_AssignOp_BitwiseAnd' { AlexTokenTag AlexRawToken_Expr_AssignOp_BitwiseAnd _ }
'Expr_AssignOp_BitwiseOr' { AlexTokenTag AlexRawToken_Expr_AssignOp_BitwiseOr _ }
'NullableType' { AlexTokenTag AlexRawToken_NullableType _ }
'MatchArm' { AlexTokenTag AlexRawToken_MatchArm _ }
'conds' { AlexTokenTag AlexRawToken_conds _ }
'body' { AlexTokenTag AlexRawToken_body _ }
'arms' { AlexTokenTag AlexRawToken_arms _ }
'Expr_Match' { AlexTokenTag AlexRawToken_Expr_Match _ }
'class' { AlexTokenTag AlexRawToken_class _ }
'Stmt_InlineHTML' { AlexTokenTag AlexRawToken_Stmt_InlineHTML _ }
'Expr_Cast_Array' { AlexTokenTag AlexRawToken_Expr_Cast_Array _ }
'Scalar_MagicConst_Function' { AlexTokenTag AlexRawToken_Scalar_MagicConst_Function _ }
'Stmt_GroupUse' { AlexTokenTag AlexRawToken_Stmt_GroupUse _ }
'prefix' { AlexTokenTag AlexRawToken_prefix _ }
'Scalar_MagicConst_Method' { AlexTokenTag AlexRawToken_Scalar_MagicConst_Method _ }
'Expr_PreDec' { AlexTokenTag AlexRawToken_Expr_PreDec _ }
'Expr_Clone' { AlexTokenTag AlexRawToken_Expr_Clone _ }
'Expr_BinaryOp_Spaceship' { AlexTokenTag AlexRawToken_Expr_BinaryOp_Spaceship _ }
'Expr_BinaryOp_Mod' { AlexTokenTag AlexRawToken_Expr_BinaryOp_Mod _ }
'Stmt_Block' { AlexTokenTag AlexRawToken_Stmt_Block _ }
'Expr_AssignOp_Mul' { AlexTokenTag AlexRawToken_Expr_AssignOp_Mul _ }
'Expr_Eval' { AlexTokenTag AlexRawToken_Expr_Eval _ }
'items' { AlexTokenTag AlexRawToken_items _ }
'dim' { AlexTokenTag AlexRawToken_dim _ }
'Stmt_Declare' { AlexTokenTag AlexRawToken_Stmt_Declare _ }
'DeclareItem' { AlexTokenTag AlexRawToken_DeclareItem _ }
'declares' { AlexTokenTag AlexRawToken_declares _ }
-- last keywords first part

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************

INT    { AlexTokenTag (AlexRawToken_INT    i) _ }
FLOAT  { AlexTokenTag (AlexRawToken_FLOAT  f) _ }
ID     { AlexTokenTag (AlexRawToken_ID    id) _ }
STR    { AlexTokenTag (AlexRawToken_STR    s) _ }

-- *************************
-- *                       *
-- * grammar specification *
-- *                       *
-- *************************
%%

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program: stmts
{
    Ast.Root
    {
        Ast.filename = "",
        Ast.stmts = $1
    }
}

tokenID:
ID      { tokIDValue $1 } |
'array' { "array"       } |
'static' { "static"     } |
'items' { "items"       } |
'value' { "value"       } |
'class' { "class"       } |
'params' { "params"     } |
'default' { "default"   } |
'true'  { "true"        } |
'init'  { "init"        } |
'false' { "false"       } |
'props' { "props"       } |
'prefix' { "prefix"     } |
'implements' { "implements" } |
'Name'  { "Name"        } |
'name'  { "name"        } |
'parts' { "parts"       } |
'body'  { "body"        } |
'flags' { "flags"       } |
'key'   { "key"         } |
'dim'   { "dim"         } |
'var'   { "var"         } |
'type'  { "type"        } |
'args'  { "args"        } |
'null'  { "null"        }

empty_array: 'array' '(' ')' { Nothing }

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
numbered(a): INT ':' a { $3 }
optional(a): { Nothing } | a { Just $1 }
listof(a): a { [$1] } | a listof(a) { $1:$2 }
ornull(a): 'null' { Nothing } | a { Just $1 }
arrayof(a): 'array' '(' listof(a) ')' { $3 }
possibly_empty_arrayof(a): empty_array { [] } | arrayof(a) { $1 }

-- *********
-- *       *
-- * stmts *
-- *       *
-- *********
stmts: possibly_empty_arrayof(numbered(stmt)) { $1 }

attrGroups: empty_array { Nothing }

-- *****************
-- *               *
-- * stmt_function *
-- *               *
-- *****************
stmt_function:
'Stmt_Function' loc
'('
    'attrGroups' ':' attrGroups
    'byRef' ':' byRef
    'name' ':' identifier
    'params' ':' params
    'returnType' ':' type
    'stmts' ':' stmts
')'
{
    Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Just (varme (Token.Named "any" $2)),
        Ast.stmtFuncName = Token.FuncName $12,
        Ast.stmtFuncParams = $15,
        Ast.stmtFuncBody = $21,
        Ast.stmtFuncAnnotations = [],
        Ast.stmtFuncLocation = $2
    }
}

-- *************
-- *           *
-- * stmt_cont *
-- *           *
-- *************
stmt_cont:
'Stmt_Continue' loc
'('
    ID ':' 'null'
')'
{
    Ast.StmtContinue (Ast.StmtContinueContent $2)
}

case:
'Stmt_Case' loc
'('
    'cond' ':' ornull(exp)
    'stmts' ':' stmts
')'
{
    Nothing
}

numbered_case: INT ':' case { Nothing }
cases: 'array' '(' listof(numbered_case) ')' { Nothing }

declare_item:
'DeclareItem' loc
'('
    'key' ':' identifier
    'value' ':' exp
')'
{
    Nothing
}

numbered_declare_item: INT ':' declare_item { Nothing }

declare_items: 'array' '(' listof(numbered_declare_item) ')' { Nothing }

stmt_declare:
'Stmt_Declare' loc
'('
    'declares' ':' declare_items
    'stmts' ':' ornull(stmts)
')'
{
    Ast.StmtContinue $ Ast.StmtContinueContent $2
}


-- ***************
-- *             *
-- * stmt_switch *
-- *             *
-- ***************
stmt_switch:
'Stmt_Switch' loc
'('
    'cond' ':' exp
    ID ':' cases
')'
{
    Ast.StmtExp $6
}

-- *************
-- *           *
-- * stmt_cont *
-- *           *
-- *************
stmt_break:
'Stmt_Break' loc
'('
    ID ':' 'null'
')'
{
    Ast.StmtBreak (Ast.StmtBreakContent $2)
}

-- ************
-- *          *
-- * stmt_nop *
-- *          *
-- ************
stmt_nop: 'Stmt_Nop' loc '(' ')'
{
    Ast.StmtBreak (Ast.StmtBreakContent $2)
}

-- ***************
-- *             *
-- * stmt_global *
-- *             *
-- ***************
stmt_global:
'Stmt_Global' loc
'('
    ID ':' exps
')'
{
    Ast.StmtBreak (Ast.StmtBreakContent $2)
}

-- *****************
-- *               *
-- * stmt_trycatch *
-- *               *
-- *****************
stmt_trycatch:
'Stmt_TryCatch' loc
'('
    'stmts' ':' stmts
    ID ':' stmts
    ID ':' 'null'
')'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
        {
            Token.constIntValue = 1,
            Token.constIntLocation = $2
        },
        Ast.stmtIfBody = $6,
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $2
    }
}

-- **************
-- *            *
-- * stmt_while *
-- *            *
-- **************
stmt_while:
'Stmt_While' loc
'('
    'cond' ':' exp
    'stmts' ':' stmts
')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $6,
        Ast.stmtWhileBody = $9,
        Ast.stmtWhileLocation = $2
    }
}

-- ***********
-- *         *
-- * stmt_do *
-- *         *
-- ***********
stmt_do:
'Stmt_Do' loc
'('
    'stmts' ':' stmts
    'cond' ':' exp
')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $9,
        Ast.stmtWhileBody = $6,
        Ast.stmtWhileLocation = $2
    }
}

Name_FullyQualified:
'Name_FullyQualified' loc
'('
    'name' ':' tokenID
')'
{
    Token.Named $6 $2
}

-- **************
-- *            *
-- * stmt_catch *
-- *            *
-- **************
stmt_catch:
'Stmt_Catch' loc
'('
    ID ':' arrayof(numbered(named))
    'var' ':' ornull(exp)
    'stmts' ':' stmts
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = $12,
        Ast.stmtBlockLocation = $2
    }
}

static_vardec:
'StaticVar' loc
'('
    'var' ':' var
    'default' ':' ornull(exp)
')'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = $6,
        Ast.stmtAssignRhs = case $9 of {
            Nothing -> Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 0 $2;
            Just exp -> exp
        }
    }
}

stmt_static_single:
static_vardec { $1 }

numbered_stmt_static:
INT ':' stmt_static_single { $3 }

-- ***************
-- *             *
-- * stmt_static *
-- *             *
-- ***************
stmt_static:
'Stmt_Static' loc
'('
    ID ':' 'array' '(' listof(numbered_stmt_static) ')'
')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
        {
            Token.constIntValue = 1,
            Token.constIntLocation = $2
        },
        Ast.stmtWhileBody = [],
        Ast.stmtWhileLocation = $2
    }
}

-- ******************
-- *                *
-- * stmt_namespace *
-- *                *
-- ******************
stmt_namespace:
'Stmt_Namespace' loc
'('
    'name' ':' Name
    'stmts' ':' stmts 
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = $9,
        Ast.stmtBlockLocation = $2
    }
}

stmt_traituse:
'Stmt_TraitUse' loc
'('
    'traits' ':' possibly_empty_arrayof(numbered_exp) 
    'adaptations' ':' possibly_empty_arrayof(exp) 
')'
{
    case $6 of {
        [] -> Ast.StmtContinue $ Ast.StmtContinueContent $2;
        (head:_) -> Ast.StmtExp head
    }
}

stmt_trait:
'Stmt_Trait' loc
'('
    'attrGroups' ':' attrGroups
    'name' ':' type
    'stmts' ':' stmts 
')'
{
    Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName $9,
        Ast.stmtClassSupers = [],
        Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
        Ast.stmtClassMethods = Ast.Methods $ Data.Map.fromList $ methodify (Token.ClassName $9) $12
    }
}

stmt_class_const:
'Stmt_ClassConst' loc
'('
    'attrGroups' ':' possibly_empty_arrayof(exp)
    'flags' ':' flags
    'type' ':' type
    'consts' ':' stmts 
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = $15,
        Ast.stmtBlockLocation = $2
    }
}

stmt_dec_const:
'Const' loc
'('
    'name' ':' identifier
    'value' ':' exp
')'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $6,
        Ast.stmtAssignRhs = $9
    }
}

stmt_html:
'Stmt_InlineHTML' loc
'('
    'value' ':' 'null'
')'
{
    Ast.StmtContinue $ Ast.StmtContinueContent $2
}

stmt_block:
'Stmt_Block' loc
'('
    'stmts' ':' stmts
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent $6 $2
}

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if        { $1 } | 
stmt_use       { $1 } |
stmt_for       { $1 } |
stmt_html      { $1 } |
stmt_while     { $1 } |
stmt_block     { $1 } |
stmt_do        { $1 } |
stmt_nop       { $1 } |
stmt_foreach   { $1 } |
stmt_exp       { $1 } |
stmt_echo      { $1 } |
stmt_unset     { $1 } |
stmt_trait     { $1 } |
stmt_traituse  { $1 } |
stmt_catch     { $1 } |
stmt_global    { $1 } |
stmt_switch    { $1 } |
stmt_class     { $1 } |
stmt_method    { $1 } |
stmt_data_member { $1 } |
stmt_interface { $1 } |
stmt_namespace { $1 } |
stmt_declare  { $1 } |
stmt_cont      { $1 } |
stmt_dec_const { $1 } |
stmt_class_const { $1 } |
stmt_break     { $1 } |
stmt_static    { $1 } |
stmt_trycatch  { $1 } |
stmt_function  { $1 } |
stmt_return    { $1 }

importee: tokenID { [$1] } | tokenID '\\' importee { $1:$3 }

name:
'Name' loc
'('
    'name' ':' importee
')'
{
    Data.List.foldl' (\x y -> x ++ "." ++ y) "" $6
}

use_item:
'UseItem' loc
'('
    'type' ':' stmt_use_type
    'name' ':' name
    'alias' ':' type
')'
{
    Ast.StmtImportContent
    {
        Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent $9),
        Ast.stmtImportSpecific = Nothing,
        Ast.stmtImportAlias = Nothing,
        Ast.stmtImportLocation = $2
    }
}

stmt_use_type: tokenID '(' INT ')' { Nothing }

-- ************
-- *          *
-- * stmt_use *
-- *          *
-- ************
stmt_use_1:
'Stmt_Use' loc
'('
    'type' ':' stmt_use_type
    'uses' ':' arrayof(numbered(use_item))
')'
{
    Ast.StmtBlock (Ast.StmtBlockContent (Data.List.map Ast.StmtImport $9) $2)
}

stmt_use_2:
'Stmt_GroupUse' loc
'('
    'type' ':' stmt_use_type
    'prefix' ':' name
    'uses' ':' arrayof(numbered(use_item))
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent (importify $2 $9 $12) $2
}

stmt_use:
stmt_use_1 { $1 } |
stmt_use_2 { $1 }

Name:
'Name' loc
'('
    'name' ':' tokenID
')'
{
    Token.Named
    {
        Token.content = $6,
        Token.location = $2
    }
}

-- **************
-- *            *
-- * identifier *
-- *            *
-- **************
identifier: 'Identifier' loc '(' 'name' ':' tokenID ')'
{
    Token.Named
    {
        Token.content = $6,
        Token.location = $2
    }
}
var_like_identifier:
'VarLikeIdentifier' loc
'('
    'name' ':' tokenID
')'
{
    Token.Named
    {
        Token.content = $6,
        Token.location = $2
    }
}

property_item:
'PropertyItem' loc
'('
    'name' ':' var_like_identifier
    'default' ':' ornull(exp)
')'
{
    Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = Token.VarName $6,
        Ast.stmtVardecNominalType = Just (varme (Token.Named "any" $2)),
        Ast.stmtVardecInitValue = $9,
        Ast.stmtVardecLocation = $2
    }
}

flag:
ID             { Nothing } |
INT            { Nothing } |
ID '(' INT ')' { Nothing }

flags:
flag { Nothing } |
flag '|' flags { Nothing }

stmt_data_member:
'Stmt_Property' loc
'('
    'attrGroups' ':' attrGroups
    'flags' ':' flags
    'type' ':' type
    'props' ':' arrayof(numbered(property_item))
    optional(hooks)
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent $15 $2
}

type:
tokenID { Token.Named $1 dummyLoc } |
identifier { $1 } |
'NullableType' loc '(' 'type' ':' type ')' { $6 } |
'Name_FullyQualified' loc '(' 'name' ':' tokenID ')' { Token.Named $6 $2 } |
Name { $1 }

param_default_value:
tokenID { Nothing } |
exp     { Nothing }

hooks:
ID ':' 'array' '(' ')' { Nothing }

bool:
'true'  { Nothing } |
'false' { Nothing }

variadic: bool { Nothing }

param:
'Param' loc
'('
    'attrGroups' ':' 'array' '(' ')'
    'flags' ':' INT
    'type' ':' type
    'byRef' ':' byRef
    'variadic' ':' variadic
    'var' ':' 'Expr_Variable' loc '(' 'name' ':' tokenID ')'
    'default' ':' param_default_value
    optional(hooks)
')'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $ Token.Named $28 $24,
        Ast.paramNominalType = Just (varme (Token.Named "any" $24)),
        paramSerialIdx = 15555
    }
}

numbered_param: INT ':' param { $3 }

params: 'array' '(' ')' { [] } | 'array' '(' listof(numbered_param) ')' { $3 }

byRef: bool { Nothing }

returnType: type { $1 }

stmt_method:
'Stmt_ClassMethod' loc
'('
    'attrGroups' ':' 'array' '(' ')'
    'flags' ':' flags
    'byRef' ':' byRef
    'name' ':' identifier
    'params' ':' params
    'returnType' ':' returnType
    'stmts' ':' ornull(stmts)
')'
{
    Ast.StmtMethod $ Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = Just (varme (Token.Named "any" $2)),
        Ast.stmtMethodName = Token.MethodName $17,
        Ast.stmtMethodParams = [],
        Ast.stmtMethodBody = case $26 of { Nothing -> []; Just _stmts -> _stmts },
        Ast.stmtMethodLocation = $2,
        Ast.hostingClassName = Token.ClassName (Token.Named "not_a_real_class_name" $2),
        Ast.hostingClassSupers = []
    }
}

stmt_interface:
'Stmt_Interface' loc
'('
    'attrGroups' ':' empty_array
    'name' ':' identifier
    'extends' ':' possibly_empty_arrayof(numbered(Name))
    'stmts' ':' stmts
')'
{
    Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName $9,
        Ast.stmtClassSupers = Data.List.map Token.SuperName $12,
        Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
        Ast.stmtClassMethods = Ast.Methods $ Data.Map.fromList $ methodify (Token.ClassName $9) $15
    }
} 

implements: named { $1 }

numbered_implements:
INT ':' implements { Nothing }

stmt_class:
'Stmt_Class' loc
'('
    'attrGroups' ':' attrGroups
    'flags' ':' flags
    'name' ':' identifier
    'extends' ':' type
    'implements' ':' possibly_empty_arrayof(numbered_implements)
    'stmts' ':' stmts 
')'
{
    Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName $12,
        Ast.stmtClassSupers = [Token.SuperName $15],
        Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
        Ast.stmtClassMethods = Ast.Methods $ Data.Map.fromList $ methodify (Token.ClassName $12) $21
    }
} 

-- ************
-- *          *
-- * stmt_exp *
-- *          *
-- ************
stmt_exp_1:
'Stmt_Expression' loc
'('
    'expr' ':' exp
')'
{
    Ast.StmtExp $6
}

-- ************
-- *          *
-- * stmt_exp *
-- *          *
-- ************
stmt_exp_2:
exp
{
    Ast.StmtExp $1
}

-- ************
-- *          *
-- * stmt_exp *
-- *          *
-- ************
stmt_exp:
stmt_exp_1 { $1 } |
stmt_exp_2 { $1 }

-- *************
-- *           *
-- * stmt_else *
-- *           *
-- *************
stmt_else:
'Stmt_Else' loc
'('
    'stmts' ':' stmts
')'
{
    $6
}

elseif:
'Stmt_ElseIf' loc
'('
    'cond' ':' exp
    'stmts' ':' stmts
')'
{
    $9
}

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if:
'Stmt_If' loc
'('
    'cond' ':' exp
    'stmts' ':' stmts
    ID ':' possibly_empty_arrayof(numbered(elseif))
    ID ':' ornull(stmt_else)
')'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $6,
        Ast.stmtIfBody = $9,
        Ast.stmtElseBody = case $15 of { Just s -> s; _ -> [] },
        Ast.stmtIfLocation = $2
    }
}

-- **************
-- *            *
-- * var_simple *
-- *            *
-- **************
var_simple:
'Expr_Variable' loc '(' 'name' ':' tokenID ')'
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = $6,
        Token.location = $2
    }
} |
'Name' loc '(' 'name' ':' tokenID ')'
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = $6,
        Token.location = $2
    }
} |
'Name_FullyQualified' loc '(' 'name' ':' tokenID ')'
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = $6,
        Token.location = $2
    }
}




-- **************
-- *            *
-- * var_field1 *
-- *            *
-- **************
var_field1:
'Expr_PropertyFetch' loc
'('
    'var' ':' exp
    'name' ':' identifier
')'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $6,
        Ast.varFieldName = Token.FieldName $9,
        Ast.varFieldLocation = $2
    }
}

named:
Name                { $1 } |
identifier          { $1 } |
exp_variable        { $1 } |
Name_FullyQualified { $1 }

-- **************
-- *            *
-- * var_field2 *
-- *            *
-- **************
var_field2:
'Expr_StaticPropertyFetch' loc
'('
    'class' ':' named
    'name' ':' var_like_identifier
')'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $6,
        Ast.varFieldName = Token.FieldName $9,
        Ast.varFieldLocation = $2
    }
}

-- **************
-- *            *
-- * var_field3 *
-- *            *
-- **************
var_field3:
'Expr_PropertyFetch' loc
'('
    'var' ':' exp
    'name' ':' exp
')'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $6,
        Ast.varFieldName = Token.FieldName (Token.Named "popo" $2),
        Ast.varFieldLocation = $2
    }
}

-- **************
-- *            *
-- * var_field4 *
-- *            *
-- **************
var_field4:
'Expr_ClassConstFetch' loc
'('
    'class' ':' exp
    'name' ':' identifier 
')'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $6,
        Ast.varFieldName = Token.FieldName $9,
        Ast.varFieldLocation = $2
    }
}

-- *************
-- *           *
-- * var_field *
-- *           *
-- *************
var_field:
var_field1 { $1 } |
var_field2 { $1 } |
var_field3 { $1 } |
var_field4 { $1 }

-- *****************
-- *               *
-- * var_subscript *
-- *               *
-- *****************
var_subscript:
'Expr_ArrayDimFetch' loc
'('
    'var' ':' exp
    'dim' ':' ornull(exp)
')'
{
    Ast.VarSubscript $ Ast.VarSubscriptContent
    {
        Ast.varSubscriptLhs = $6,
        Ast.varSubscriptIdx = case $9 of { Just idx -> idx; _ -> $6 },
        Ast.varSubscriptLocation = $2
    }
}

-- *******
-- *     *
-- * var *
-- *     *
-- *******
var:
var_simple    { $1 } |
var_list      { $1 } |
var_field     { $1 } |
var_subscript { $1 }

assign_op:
'Expr_Assign'          { Nothing } |
'Expr_AssignOp_Div'    { Nothing } |
'Expr_AssignOp_Plus'   { Nothing } |
'Expr_AssignOp_Mul'    { Nothing } |
'Expr_AssignOp_Minus'  { Nothing } |
'Expr_AssignOp_Concat' { Nothing } |
'Expr_AssignOp_ShiftRight' { Nothing } |
'Expr_AssignOp_BitwiseAnd' { Nothing } |
'Expr_AssignOp_BitwiseOr'  { Nothing } |
'Expr_AssignOp_BitwiseXor' { Nothing }

-- *************
-- *           *
-- * stmt_echo *
-- *           *
-- *************
stmt_echo:
'Stmt_Echo' loc
'('
    'exprs' ':' exps
')'
{
    Ast.StmtExp $ Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "echo",
            Token.location = $2
        },
        Ast.args = $6,
        Ast.expCallLocation = $2
    }
}

-- *************
-- *           *
-- * stmt_echo *
-- *           *
-- *************
stmt_unset:
'Stmt_Unset' loc
'('
    ID ':' exps
')'
{
    Ast.StmtExp $ Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "unset",
            Token.location = $2
        },
        Ast.args = $6,
        Ast.expCallLocation = $2
    }
}

exp_or_null:
'null' { Nothing } |
exp { Just $1 }

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return: 'Stmt_Return' loc '(' 'expr' ':' exp_or_null ')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = $6,
        Ast.stmtReturnLocation = $2
    }
}

-- ********
-- *      *
-- * exps *
-- *      *
-- ********
exps: 'array' '(' numbered_exps ')' { $3 }

-- *****************
-- *               *
-- * numbered_exps *
-- *               *
-- *****************
numbered_exps: numbered_exp numbered_exps { $1:$2 } | numbered_exp { [$1] }

-- ****************
-- *              *
-- * numbered_exp *
-- *              *
-- ****************
numbered_exp: INT ':' exp { $3 }

-- ***********
-- *         *
-- * exp_not *
-- *         *
-- ***********
exp_not:
'Expr_BooleanNot' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_exit *
-- *          *
-- ************
exp_exit:
'Expr_Exit' loc
'('
    'expr' ':' ornull(exp)
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "exit",
            Token.location = $2
        },
        Ast.args = [],
        Ast.expCallLocation = $2
    }
}

-- *************
-- *           *
-- * exp_isset *
-- *           *
-- *************
exp_isset:
'Expr_Isset' loc
'('
    ID ':' exps 
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "isset",
            Token.location = $2
        },
        Ast.args = $6,
        Ast.expCallLocation = $2
    }
}

-- ***************
-- *             *
-- * exp_ternary *
-- *             *
-- ***************
exp_ternary:
'Expr_Ternary' loc
'('
    'cond' ':' exp
    ID ':' ornull(exp)
    ID ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast1:
'Expr_Cast_Double' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast2:
'Expr_Cast_Int' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast3:
'Expr_Cast_String' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast4:
'Expr_Cast_Bool' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast5:
'Expr_Cast_Object' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast6:
'Expr_Cast_Array' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast:
exp_cast1 { $1 } |
exp_cast2 { $1 } |
exp_cast3 { $1 } |
exp_cast4 { $1 } |
exp_cast5 { $1 } |
exp_cast6 { $1 }

-- *************
-- *           *
-- * exp_empty *
-- *           *
-- *************
exp_empty:
'Expr_Empty' loc
'('
    'expr' ':' exp
')'
{
    $6
}

exp_fstring_element:
'InterpolatedStringPart' loc
'('
    'value' ':' STR
')'
{
    Ast.ExpStr $ Ast.ExpStrContent $ Token.ConstStr (tokStrValue $6) $2
}

-- ***************
-- *             *
-- * exp_fstring *
-- *             *
-- ***************
exp_fstring:
'Scalar_InterpolatedString' loc
'('
    'parts' ':' arrayof(numbered(exp))
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "fstring",
            Token.location = $2
        },
        Ast.args = $6,
        Ast.expCallLocation = $2
    }
}

unop_operator:
'Expr_UnaryMinus' { Nothing } |
'Expr_PostInc'    { Nothing } |
'Expr_PreDec'     { Nothing } |
'Expr_PostDec'    { Nothing }

-- ************
-- *          *
-- * exp_unop *
-- *          *
-- ************
exp_unop1:
unop_operator loc
'('
    'var' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_unop *
-- *          *
-- ************
exp_unop2:
unop_operator loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_unop *
-- *          *
-- ************
exp_unop:
exp_unop1 { $1 } |
exp_unop2 { $1 }

import_type:
ID '(' INT ')' { Nothing }

-- **************
-- *            *
-- * exp_import *
-- *            *
-- **************
exp_import:
'Expr_Include' loc
'('
    'expr' ':' exp
    'type' ':' import_type
')'
{
    $6
}

-- ************
-- *          *
-- * exp_file *
-- *          *
-- ************
exp_file: 'Scalar_MagicConst_File' loc '(' ')'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = "__FILE__",
        Token.location = $2
    }
}

-- ************
-- *          *
-- * exp_file *
-- *          *
-- ************
exp_dir: 'Scalar_MagicConst_Dir' loc '(' ')'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = "__DIR__",
        Token.location = $2
    }
}

-- ************
-- *          *
-- * exp_inst *
-- *          *
-- ************
exp_instof:
'Expr_Instanceof' loc
'('
    'expr' ':' exp
    'class' ':' named
')'
{
    $6
}

-- ****************
-- *              *
-- * err_suppress *
-- *              *
-- ****************
err_suppress:
'Expr_ErrorSuppress' loc
'('
    'expr' ':' exp
')'
{
    $6
}

exp_assign:
assign_op loc
'('
    'var' ':' var
    'expr' ':' exp
')'
{
    Ast.ExpAssign $ Ast.ExpAssignContent
    {
        Ast.expAssignLhs = $6,
        Ast.expAssignRhs = $9,
        Ast.expAssignLocation = $2
    }
}

-- *************
-- *           *
-- * exp_print *
-- *           *
-- *************
exp_print:
'Expr_Print' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- *************
-- *           *
-- * exp_throw *
-- *           *
-- *************
exp_throw:
'Expr_Throw' loc
'('
    'expr' ':' exp
')'
{
    $6
}

exp_kwclass:
'Scalar_MagicConst_Class' loc
'('
')'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = "class",
        Token.location = $2
    } 
}

exp_null_safe_prop_fetch:
'Expr_NullsafePropertyFetch' loc
'('
    'var' ':' var
    'name' ':' identifier
')'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = Ast.ExpVar (Ast.ExpVarContent $6),
        Ast.varFieldName = Token.FieldName $9,
        Ast.varFieldLocation = $2
    }
}


exp_preinc:
'Expr_PreInc' loc
'('
    'var' ':' var
')'
{
    Ast.ExpVar $ Ast.ExpVarContent $6
}

exp_arrow:
'Expr_ArrowFunction' loc
'('
    'attrGroups' ':' attrGroups
    'static' ':' static
    'byRef' ':' byRef
    'params' ':' params
    'returnType' ':' returnType
    'expr' ':' exp
')'
{
    Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        Ast.expLambdaParams = $15,
        Ast.expLambdaBody = [Ast.StmtExp $21],
        Ast.expLambdaLocation = $2
    }
}

match_arm:
'MatchArm' loc
'('
    'conds' ':' ornull(arrayof(numbered(exp)))
    'body' ':' exp
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "<instrumented-case>",
            Token.location = $2
        },
        Ast.args = $9:(case $6 of { Just es -> es; _ -> [] }),
        Ast.expCallLocation = $2
    }
}

exp_match:
'Expr_Match' loc
'('
    'cond' ':' exp
    'arms' ':' arrayof(numbered(match_arm))
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "<instrumented-switch>",
            Token.location = $2
        },
        Ast.args = $6:$9
    }
}

exp_magic_1:
'Scalar_MagicConst_Function' loc '(' ')'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "__FUNCTION__" $2
}

exp_magic_2:
'Scalar_MagicConst_Method' loc '(' ')'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "__METHOD__" $2
}

exp_magic:
exp_magic_1 { $1 } |
exp_magic_2 { $1 }

exp_clone:
'Expr_Clone' loc
'('
    'expr' ':' exp
')'
{
    $6
}

exp_eval:
'Expr_Eval' loc
'('
    'expr' ':' exp
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "eval",
            Token.location = $2
        },
        Ast.args = [$6],
        Ast.expCallLocation = $2
    }
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_int     { $1 } |
exp_float   { $1 } |
exp_match   { $1 } |
exp_clone   { $1 } |
exp_str     { $1 } |
exp_preinc  { $1 } |
exp_magic   { $1 } |
exp_assign  { $1 } |
exp_new     { $1 } |
exp_not     { $1 } |
exp_bool    { $1 } |
exp_eval    { $1 } |
exp_exit    { $1 } |
exp_cast    { $1 } |
exp_call    { Ast.ExpCall $1 } |
exp_binop   { $1 } |
exp_print   { $1 } |
exp_throw   { $1 } |
exp_dir     { $1 } |
exp_file    { $1 } |
exp_unop    { $1 } |
exp_isset   { $1 } |
exp_empty   { $1 } |
exp_null_safe_prop_fetch { $1 } |
exp_array   { $1 } |
exp_import  { $1 } |
err_suppress  { $1 } |
exp_lambda  { $1 } |
exp_arrow   { $1 } |
exp_ternary { $1 } |
exp_fstring { $1 } |
exp_fstring_element { $1 } |
exp_instof  { $1 } |
exp_var     { $1 } |
exp_kwclass { $1 }

array_item:
'ArrayItem' loc
'('
    'key' ':' ornull(exp)
    'value' ':' exp 
    'byRef' ':' byRef
    'unpack' ':' unpack
')'
{
    $9
}

-- *************
-- *           *
-- * exp_array *
-- *           *
-- *************
exp_array:
'Expr_Array' loc
'('
    'items' ':' possibly_empty_arrayof(numbered(array_item))
')'
{
    normalize_expr_array "arrayify" $2 $6
}

-- ************
-- *          *
-- * exp_list *
-- *          *
-- ************
var_list:
'Expr_List' loc
'('
    'items' ':' possibly_empty_arrayof(numbered(array_item))
')'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = Ast.ExpCall $ Ast.ExpCallContent
        { 
            Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
            {
                Token.content = "arrayify",
                Token.location = $2
            },
            Ast.args = $6,
            Ast.expCallLocation = $2
        },
        Ast.varFieldName = Token.FieldName $ Token.Named
        {
            Token.content = "arrayify",
            Token.location = $2
        },
        Ast.varFieldLocation = $2
    }
}

closure_use:
'ClosureUse' loc
'('
    'var' ':' var
    'byRef' ':' byRef
')'
{
    Nothing
}

numbered_closure_use: INT ':' closure_use { Nothing }

static: bool { Nothing }

-- **************
-- *            *
-- * exp_lambda *
-- *            *
-- **************
exp_lambda:
'Expr_Closure' loc
'('
    'attrGroups' ':' attrGroups
    'static' ':' static
    'byRef' ':' byRef
    'params' ':' params
    'uses' ':' possibly_empty_arrayof(numbered_closure_use)
    'returnType' ':' returnType
    'stmts' ':' stmts
')'
{
    Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        Ast.expLambdaParams = $15,
        Ast.expLambdaBody = $24,
        Ast.expLambdaLocation = $2
    }
}

-- ***********
-- *         *
-- * exp_int *
-- *         *
-- ***********
exp_int: 'Scalar_Int' loc '(' 'value' ':' INT ')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = tokIntValue $6,
        Token.constIntLocation = $2
    }
}

float_value:
FLOAT { Nothing } |
INT   { Nothing }

-- *************
-- *           *
-- * exp_float *
-- *           *
-- *************
exp_float: 'Scalar_Float' loc '(' 'value' ':' float_value ')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 4444,
        Token.constIntLocation = $2
    }
}

exp_str:
'Scalar_String' loc
'('
    'value' ':' STR
')'
{
    Ast.ExpStr $ Ast.ExpStrContent $ Token.ConstStr
    {
        Token.constStrValue = tokStrValue $6,
        Token.constStrLocation = $2
    }
   
}

-- ************
-- *          *
-- * exp_bool *
-- *          *
-- ************
exp_bool:
'Expr_ConstFetch' loc '(' 'name' ':' 'Name' loc '(' 'name' ':' tokenID ')' ')'
{
    Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool
    {
        Token.constBoolValue = True,
        Token.constBoolLocation = $2
    }
}
 
-- ***********
-- *         *
-- * exp_var *
-- *         *
-- ***********
exp_var: var { Ast.ExpVar (Ast.ExpVarContent $1) }

operator:
'Expr_BinaryOp_Smaller'      { Nothing } |
'Expr_BinaryOp_SmallerOrEqual' { Nothing } |
'Expr_BinaryOp_Equal'        { Nothing } |
'Expr_BinaryOp_NotEqual'     { Nothing } |
'Expr_BinaryOp_Mod'          { Nothing } |
'Expr_BinaryOp_Spaceship'    { Nothing } |
'Expr_BinaryOp_Mul'          { Nothing } |
'Expr_BinaryOp_Div'          { Nothing } |
'Expr_BinaryOp_Plus'         { Nothing } |
'Expr_BinaryOp_Minus'        { Nothing } |
'Expr_BinaryOp_Pow'          { Nothing } |
'Expr_BinaryOp_Greater'      { Nothing } |
'Expr_BinaryOp_GreaterOrEqual' { Nothing } |
'Expr_BinaryOp_Coalesce'     { Nothing } |
'Expr_BinaryOp_Concat'       { Nothing } |
'Expr_BinaryOp_Identical'    { Nothing } |
'Expr_BinaryOp_NotIdentical' { Nothing } |
'Expr_BinaryOp_LogicalAnd'   { Nothing } |
'Expr_BinaryOp_BooleanOr'    { Nothing } |
'Expr_BinaryOp_ShiftRight'   { Nothing } |
'Expr_BinaryOp_ShiftLeft'    { Nothing } |
'Expr_BinaryOp_BitwiseOr'    { Nothing } |
'Expr_BinaryOp_BitwiseAnd'   { Nothing } |
'Expr_BinaryOp_BitwiseXor'   { Nothing } |
'Expr_BinaryOp_LogicalOr'    { Nothing } |
'Expr_BinaryOp_BooleanAnd'   { Nothing }

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop:
operator loc
'('
    'left' ':' exp
    'right' ':' exp
')'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $6,
        Ast.expBinopRight = $9,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $2
    }
} 

-- ***********
-- *         *
-- * exp_new *
-- *         *
-- ***********
exp_new:
'Expr_New' loc
'('
    'class' ':' exp
    'args' ':' args
')' 
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarField $ Ast.VarFieldContent {
            Ast.varFieldLhs = $6,
            Ast.varFieldName = Token.FieldName $ Token.Named "__construct" $2,
            Ast.varFieldLocation = $2
        },
        Ast.args = $9,
        Ast.expCallLocation = $2
    }
}


-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call:
exp_func_call          { $1 } |
exp_method_call        { $1 } |
exp_static_method_call { $1 }

-- *****************
-- *               *
-- * exp_func_call *
-- *               *
-- *****************
exp_func_call:
'Expr_FuncCall' loc
'('
    'name' ':' exp
    'args' ':' args
')' 
{
    Ast.ExpCallContent
    {
        Ast.callee = $6,
        Ast.args = $9,
        Ast.expCallLocation = $2
    }
}

-- *******************
-- *                 *
-- * exp_method_call *
-- *                 *
-- *******************
exp_method_call:
'Expr_MethodCall' loc
'('
    'var' ':' exp
    'name' ':' named
    'args' ':' args
')' 
{
    Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarField $ Ast.VarFieldContent
        {
            Ast.varFieldLhs = $6,
            Ast.varFieldName = Token.FieldName $9,
            Ast.varFieldLocation = $2
        },
        Ast.args = $12,
        Ast.expCallLocation = $2
    }
}

exp_variable:
'Expr_Variable' loc
'('
    'name' ':' tokenID
')'
{
    Token.Named $6 $2
}

exp_static_call_name:
identifier   { $1 } |
exp_variable { $1 }

-- **************************
-- *                        *
-- * exp_static_method_call *
-- *                        *
-- **************************
exp_static_method_call:
'Expr_StaticCall' loc
'('
    'class' ':' exp
    'name' ':' exp_static_call_name
    'args' ':' args
')'
{
    Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarField $ Ast.VarFieldContent
        {
            Ast.varFieldLhs = $6,
            Ast.varFieldName = Token.FieldName $9,
            Ast.varFieldLocation = $2
        },
        Ast.args = $12,
        Ast.expCallLocation = $2
    }
}

-- ********
-- *      *
-- * args *
-- *      *
-- ********
args:
'array' '('                      ')' { [] } |
'array' '(' listof(numbered_arg) ')' { $3 }

-- ****************
-- *              *
-- * numbered_arg *
-- *              *
-- ****************
numbered_arg: INT ':' arg { $3 }

unpack: bool { Nothing }

argname:
tokenID    { Nothing } |
identifier { Nothing }

-- *******
-- *     *
-- * arg *
-- *     *
-- *******
arg:
'Arg' loc
'('
    'name' ':' argname
    'value' ':' exp
    'byRef' ':' byRef
    'unpack' ':' unpack
')'
{
    $9
}

stmt_for_loop:
exp { $1 } |
exps { head $1 } 

-- ************
-- *          *
-- * stmt_for *
-- *          *
-- ************
stmt_for:
'Stmt_For' loc
'('
    'init' ':' stmts
    'cond' ':' exps
    'loop' ':' stmt_for_loop
    'stmts' ':' stmts
')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $12,
        Ast.stmtWhileBody = $15,
        Ast.stmtWhileLocation = $2
    }
}

-- ****************
-- *              *
-- * stmt_foreach *
-- *              *
-- ****************
stmt_foreach:
'Stmt_Foreach' loc
'('
    'expr' ':' exp
    'keyVar' ':' ornull(exp)
    'byRef' ':' byRef 
    'valueVar' ':' exp
    'stmts' ':' stmts
')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $6,
        Ast.stmtWhileBody = $18,
        Ast.stmtWhileLocation = $2
    }
}


-- *******
-- *     *
-- * loc *
-- *     *
-- *******
loc: '[' INT ':' INT '-' INT ':' INT ']'
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

{

importify' :: Location -> String -> Ast.StmtImportContent -> Ast.Stmt
importify' loc src content = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent src),
    Ast.stmtImportSpecific = Nothing,
    Ast.stmtImportAlias = Ast.stmtImportAlias content,
    Ast.stmtImportLocation = loc
}

importify :: Location -> String -> [ Ast.StmtImportContent ] -> [ Ast.Stmt ]
importify loc f = Data.List.map (importify' loc f)

arr_snd :: Ast.Exp -> Maybe Token.Named
arr_snd e@(Ast.ExpStr (Ast.ExpStrContent (Token.ConstStr v loc))) = Just (Token.Named v loc)
arr_snd _ = Nothing

arr_helper' :: Location -> Ast.Exp -> Ast.Exp -> Maybe Ast.Exp
arr_helper' loc obj e2 = case (arr_snd e2) of {
    Nothing -> Nothing;
    Just field -> Just $ Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarField $ Ast.VarFieldContent {
        Ast.varFieldLhs = obj,
        Ast.varFieldName = Token.FieldName field,
        Ast.varFieldLocation = loc {
            Location.lineEnd = Location.lineEnd (Token.location field),
            Location.colEnd = Location.colEnd (Token.location field)
        }
    }
}

arr_fst :: Ast.Exp -> Maybe (Ast.Exp, Location)
arr_fst obj@(Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named _ l)))))) = Just (obj,l)
arr_fst _ = Nothing

arr_helper :: Ast.Exp -> Ast.Exp -> Maybe Ast.Exp
arr_helper e1 e2 = case (arr_fst e1) of { Just (obj,loc) -> arr_helper' loc obj e2; _ -> Nothing }

normalize_expr_array_method :: [Ast.Exp] -> Maybe Ast.Exp
normalize_expr_array_method [e1,e2] = arr_helper e1 e2
normalize_expr_array_method _ = Nothing

normalize_expr_array_normal :: String -> Location -> [Ast.Exp] -> Ast.Exp
normalize_expr_array_normal name loc args = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = callify (Token.Named name loc),
    Ast.args = args,
    Ast.expCallLocation = loc
}

normalize_expr_array :: String -> Location -> [Ast.Exp] -> Ast.Exp
normalize_expr_array name loc args = case (normalize_expr_array_method args) of {
    Nothing -> normalize_expr_array_normal name loc args;
    Just method -> method
}

varme :: Token.Named -> Ast.Var
varme = Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

callify :: Token.Named -> Ast.Exp
callify = Ast.ExpVar . Ast.ExpVarContent . Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

dummyLoc :: Location
dummyLoc = Location "" 0 0 0 0

-- *************
-- *           *
-- * methodify *
-- *           *
-- *************
methodify :: Token.ClassName -> [ Ast.Stmt ] -> [(Token.MethodName, Ast.StmtMethodContent)]
methodify c stmts = catMaybes $ Data.List.map (methodify' c) stmts

methodify' :: Token.ClassName -> Ast.Stmt -> Maybe (Token.MethodName, Ast.StmtMethodContent)
methodify' c (Ast.StmtMethod m) = Just (Ast.stmtMethodName m, m { Ast.hostingClassName = c } )
methodify' _ _ = Nothing

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
