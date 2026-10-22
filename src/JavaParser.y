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

-- ****************************
-- *                          *
-- * native ast json keys     *
-- *                          *
-- ****************************

'kind'        { AlexTokenTag AlexRawToken_KIND        _ }
'loc'         { AlexTokenTag AlexRawToken_LOC         _ }
'children'    { AlexTokenTag AlexRawToken_CHILDREN    _ }
'text'        { AlexTokenTag AlexRawToken_TEXT        _ }
'startLine'   { AlexTokenTag AlexRawToken_START_LINE  _ }
'startCol'    { AlexTokenTag AlexRawToken_START_COL   _ }
'endLine'     { AlexTokenTag AlexRawToken_END_LINE    _ }
'endCol'      { AlexTokenTag AlexRawToken_END_COL     _ }
'annotations' { AlexTokenTag AlexRawToken_annotations _ }
'parameters'  { AlexTokenTag AlexRawToken_parameters  _ }
'supers'      { AlexTokenTag AlexRawToken_supers      _ }
'type'        { AlexTokenTag AlexRawToken_TYPE        _ }
'init'        { AlexTokenTag AlexRawToken_INIT        _ }
'cond'        { AlexTokenTag AlexRawToken_COND        _ }
'body'        { AlexTokenTag AlexRawToken_BODY        _ }
'returnType'  { AlexTokenTag AlexRawToken_RETURN_TYPE _ }
'left'        { AlexTokenTag AlexRawToken_LEFT        _ }
'right'       { AlexTokenTag AlexRawToken_RIGHT       _ }

-- ****************************
-- *                          *
-- * native ast kind names    *
-- *                          *
-- ****************************

'COMPILATION_UNIT'     { AlexTokenTag AlexRawToken_COMPILATION_UNIT     _ }
'PACKAGE'              { AlexTokenTag AlexRawToken_PACKAGE              _ }
'IMPORT'               { AlexTokenTag AlexRawToken_IMPORT               _ }
'MEMBER_SELECT'        { AlexTokenTag AlexRawToken_MEMBER_SELECT        _ }
'CLASS'                { AlexTokenTag AlexRawToken_CLASS                _ }
'INTERFACE'            { AlexTokenTag AlexRawToken_INTERFACE            _ }
'ENUM'                 { AlexTokenTag AlexRawToken_ENUM                 _ }
'NEW_CLASS'            { AlexTokenTag AlexRawToken_NEW_CLASS            _ }
'ANNOTATION'           { AlexTokenTag AlexRawToken_ANNOTATION           _ }
'ASSIGNMENT'           { AlexTokenTag AlexRawToken_ASSIGNMENT           _ }
'STRING_LITERAL'       { AlexTokenTag AlexRawToken_STRING_LITERAL       _ }
'VARIABLE'             { AlexTokenTag AlexRawToken_VARIABLE             _ }
'METHOD'               { AlexTokenTag AlexRawToken_METHOD               _ }
'PARAMETER'            { AlexTokenTag AlexRawToken_PARAMETER            _ }
'PARAMETERIZED_TYPE'   { AlexTokenTag AlexRawToken_PARAMETERIZED_TYPE   _ }
'PRIMITIVE_TYPE'       { AlexTokenTag AlexRawToken_PRIMITIVE_TYPE       _ }
'INT_LITERAL'          { AlexTokenTag AlexRawToken_INT_LITERAL          _ }
'NULL_LITERAL'         { AlexTokenTag AlexRawToken_NULL_LITERAL         _ }
'BOOLEAN_LITERAL'      { AlexTokenTag AlexRawToken_BOOLEAN_LITERAL      _ }
'BLOCK'                { AlexTokenTag AlexRawToken_BLOCK                _ }
'RETURN'               { AlexTokenTag AlexRawToken_RETURN               _ }
'CONSTRUCTOR'          { AlexTokenTag AlexRawToken_CONSTRUCTOR          _ }
'IF'                   { AlexTokenTag AlexRawToken_IF                   _ }
'METHOD_INVOCATION'    { AlexTokenTag AlexRawToken_METHOD_INVOCATION    _ }
'EQUAL_TO'             { AlexTokenTag AlexRawToken_EQUAL_TO             _ }
'PLUS'                 { AlexTokenTag AlexRawToken_PLUS                 _ }
'MULTIPLY'             { AlexTokenTag AlexRawToken_MULTIPLY             _ }
'LOGICAL_COMPLEMENT'   { AlexTokenTag AlexRawToken_LOGICAL_COMPLEMENT   _ }
'EXPRESSION_STATEMENT' { AlexTokenTag AlexRawToken_EXPRESSION_STATEMENT _ }
'TRY'                  { AlexTokenTag AlexRawToken_TRY                  _ }
'CATCH'                { AlexTokenTag AlexRawToken_CATCH                _ }
'CONDITIONAL_EXPRESSION' { AlexTokenTag AlexRawToken_CONDITIONAL_EXPRESSION _ }
'IDENTIFIER'           { AlexTokenTag AlexRawToken_IDENTIFIER           _ }
'ANNOTATED_TYPE'                    { AlexTokenTag AlexRawToken_ANNOTATED_TYPE                    _ }
'TYPE_ANNOTATION'                   { AlexTokenTag AlexRawToken_TYPE_ANNOTATION                   _ }
'ARRAY_ACCESS'                      { AlexTokenTag AlexRawToken_ARRAY_ACCESS                      _ }
'ARRAY_TYPE'                        { AlexTokenTag AlexRawToken_ARRAY_TYPE                        _ }
'ASSERT'                            { AlexTokenTag AlexRawToken_ASSERT                            _ }
'BREAK'                             { AlexTokenTag AlexRawToken_BREAK                             _ }
'CASE'                              { AlexTokenTag AlexRawToken_CASE                              _ }
'CONTINUE'                          { AlexTokenTag AlexRawToken_CONTINUE                          _ }
'DO_WHILE_LOOP'                     { AlexTokenTag AlexRawToken_DO_WHILE_LOOP                     _ }
'ENHANCED_FOR_LOOP'                 { AlexTokenTag AlexRawToken_ENHANCED_FOR_LOOP                 _ }
'MEMBER_REFERENCE'                  { AlexTokenTag AlexRawToken_MEMBER_REFERENCE                  _ }
'FOR_LOOP'                          { AlexTokenTag AlexRawToken_FOR_LOOP                          _ }
'INSTANCE_OF'                       { AlexTokenTag AlexRawToken_INSTANCE_OF                       _ }
'TEMPLATE'                          { AlexTokenTag AlexRawToken_TEMPLATE                          _ }
'LABELED_STATEMENT'                 { AlexTokenTag AlexRawToken_LABELED_STATEMENT                 _ }
'MODIFIERS'                         { AlexTokenTag AlexRawToken_MODIFIERS                         _ }
'NEW_ARRAY'                         { AlexTokenTag AlexRawToken_NEW_ARRAY                         _ }
'LAMBDA_EXPRESSION'                 { AlexTokenTag AlexRawToken_LAMBDA_EXPRESSION                 _ }
'PARENTHESIZED'                     { AlexTokenTag AlexRawToken_PARENTHESIZED                     _ }
'ANY_PATTERN'                       { AlexTokenTag AlexRawToken_ANY_PATTERN                       _ }
'BINDING_PATTERN'                   { AlexTokenTag AlexRawToken_BINDING_PATTERN                   _ }
'DEFAULT_CASE_LABEL'                { AlexTokenTag AlexRawToken_DEFAULT_CASE_LABEL                _ }
'CONSTANT_CASE_LABEL'               { AlexTokenTag AlexRawToken_CONSTANT_CASE_LABEL               _ }
'PATTERN_CASE_LABEL'                { AlexTokenTag AlexRawToken_PATTERN_CASE_LABEL                _ }
'DECONSTRUCTION_PATTERN'            { AlexTokenTag AlexRawToken_DECONSTRUCTION_PATTERN            _ }
'EMPTY_STATEMENT'                   { AlexTokenTag AlexRawToken_EMPTY_STATEMENT                   _ }
'SWITCH'                            { AlexTokenTag AlexRawToken_SWITCH                            _ }
'SWITCH_EXPRESSION'                 { AlexTokenTag AlexRawToken_SWITCH_EXPRESSION                 _ }
'SYNCHRONIZED'                      { AlexTokenTag AlexRawToken_SYNCHRONIZED                      _ }
'THROW'                             { AlexTokenTag AlexRawToken_THROW                             _ }
'UNION_TYPE'                        { AlexTokenTag AlexRawToken_UNION_TYPE                        _ }
'INTERSECTION_TYPE'                 { AlexTokenTag AlexRawToken_INTERSECTION_TYPE                 _ }
'TYPE_CAST'                         { AlexTokenTag AlexRawToken_TYPE_CAST                         _ }
'TYPE_PARAMETER'                    { AlexTokenTag AlexRawToken_TYPE_PARAMETER                    _ }
'WHILE_LOOP'                        { AlexTokenTag AlexRawToken_WHILE_LOOP                        _ }
'POSTFIX_INCREMENT'                 { AlexTokenTag AlexRawToken_POSTFIX_INCREMENT                 _ }
'POSTFIX_DECREMENT'                 { AlexTokenTag AlexRawToken_POSTFIX_DECREMENT                 _ }
'PREFIX_INCREMENT'                  { AlexTokenTag AlexRawToken_PREFIX_INCREMENT                  _ }
'PREFIX_DECREMENT'                  { AlexTokenTag AlexRawToken_PREFIX_DECREMENT                  _ }
'UNARY_PLUS'                        { AlexTokenTag AlexRawToken_UNARY_PLUS                        _ }
'UNARY_MINUS'                       { AlexTokenTag AlexRawToken_UNARY_MINUS                       _ }
'BITWISE_COMPLEMENT'                { AlexTokenTag AlexRawToken_BITWISE_COMPLEMENT                _ }
'DIVIDE'                            { AlexTokenTag AlexRawToken_DIVIDE                            _ }
'REMAINDER'                         { AlexTokenTag AlexRawToken_REMAINDER                         _ }
'MINUS'                             { AlexTokenTag AlexRawToken_MINUS                             _ }
'LEFT_SHIFT'                        { AlexTokenTag AlexRawToken_LEFT_SHIFT                        _ }
'RIGHT_SHIFT'                       { AlexTokenTag AlexRawToken_RIGHT_SHIFT                       _ }
'UNSIGNED_RIGHT_SHIFT'              { AlexTokenTag AlexRawToken_UNSIGNED_RIGHT_SHIFT              _ }
'LESS_THAN'                         { AlexTokenTag AlexRawToken_LESS_THAN                         _ }
'GREATER_THAN'                      { AlexTokenTag AlexRawToken_GREATER_THAN                      _ }
'LESS_THAN_EQUAL'                   { AlexTokenTag AlexRawToken_LESS_THAN_EQUAL                   _ }
'GREATER_THAN_EQUAL'                { AlexTokenTag AlexRawToken_GREATER_THAN_EQUAL                _ }
'NOT_EQUAL_TO'                      { AlexTokenTag AlexRawToken_NOT_EQUAL_TO                      _ }
'AND'                               { AlexTokenTag AlexRawToken_AND                               _ }
'XOR'                               { AlexTokenTag AlexRawToken_XOR                               _ }
'OR'                                { AlexTokenTag AlexRawToken_OR                                _ }
'CONDITIONAL_AND'                   { AlexTokenTag AlexRawToken_CONDITIONAL_AND                   _ }
'CONDITIONAL_OR'                    { AlexTokenTag AlexRawToken_CONDITIONAL_OR                    _ }
'MULTIPLY_ASSIGNMENT'               { AlexTokenTag AlexRawToken_MULTIPLY_ASSIGNMENT               _ }
'DIVIDE_ASSIGNMENT'                 { AlexTokenTag AlexRawToken_DIVIDE_ASSIGNMENT                 _ }
'REMAINDER_ASSIGNMENT'              { AlexTokenTag AlexRawToken_REMAINDER_ASSIGNMENT              _ }
'PLUS_ASSIGNMENT'                   { AlexTokenTag AlexRawToken_PLUS_ASSIGNMENT                   _ }
'MINUS_ASSIGNMENT'                  { AlexTokenTag AlexRawToken_MINUS_ASSIGNMENT                  _ }
'LEFT_SHIFT_ASSIGNMENT'             { AlexTokenTag AlexRawToken_LEFT_SHIFT_ASSIGNMENT             _ }
'RIGHT_SHIFT_ASSIGNMENT'            { AlexTokenTag AlexRawToken_RIGHT_SHIFT_ASSIGNMENT            _ }
'UNSIGNED_RIGHT_SHIFT_ASSIGNMENT'   { AlexTokenTag AlexRawToken_UNSIGNED_RIGHT_SHIFT_ASSIGNMENT   _ }
'AND_ASSIGNMENT'                    { AlexTokenTag AlexRawToken_AND_ASSIGNMENT                    _ }
'XOR_ASSIGNMENT'                    { AlexTokenTag AlexRawToken_XOR_ASSIGNMENT                    _ }
'OR_ASSIGNMENT'                     { AlexTokenTag AlexRawToken_OR_ASSIGNMENT                     _ }
'LONG_LITERAL'                      { AlexTokenTag AlexRawToken_LONG_LITERAL                      _ }
'FLOAT_LITERAL'                     { AlexTokenTag AlexRawToken_FLOAT_LITERAL                     _ }
'DOUBLE_LITERAL'                    { AlexTokenTag AlexRawToken_DOUBLE_LITERAL                    _ }
'CHAR_LITERAL'                      { AlexTokenTag AlexRawToken_CHAR_LITERAL                      _ }
'UNBOUNDED_WILDCARD'                { AlexTokenTag AlexRawToken_UNBOUNDED_WILDCARD                _ }
'EXTENDS_WILDCARD'                  { AlexTokenTag AlexRawToken_EXTENDS_WILDCARD                  _ }
'SUPER_WILDCARD'                    { AlexTokenTag AlexRawToken_SUPER_WILDCARD                    _ }
'ERRONEOUS'                         { AlexTokenTag AlexRawToken_ERRONEOUS                         _ }
'ANNOTATION_TYPE'                   { AlexTokenTag AlexRawToken_ANNOTATION_TYPE                   _ }
'MODULE'                            { AlexTokenTag AlexRawToken_MODULE                            _ }
'EXPORTS'                           { AlexTokenTag AlexRawToken_EXPORTS                           _ }
'OPENS'                             { AlexTokenTag AlexRawToken_OPENS                             _ }
'PROVIDES'                          { AlexTokenTag AlexRawToken_PROVIDES                          _ }
'RECORD'                            { AlexTokenTag AlexRawToken_RECORD                            _ }
'REQUIRES'                          { AlexTokenTag AlexRawToken_REQUIRES                          _ }
'USES'                              { AlexTokenTag AlexRawToken_USES                              _ }
'OTHER'                             { AlexTokenTag AlexRawToken_OTHER                             _ }
'YIELD'                             { AlexTokenTag AlexRawToken_YIELD                             _ }

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
optional(a): { Nothing } | a { Just $1 }

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
        Ast.stmts = JavaParserActions.withSuperImports $13
    }
}

stmt:
stmt_package   { $1 } |
stmt_import    { $1 } |
stmt_class     { $1 } |
stmt_interface { $1 } |
stmt_enum      { $1 } |
stmt_empty     { $1 }

-- ****************
-- *              *
-- * stmt_empty   *
-- *              *
-- ****************
stmt_empty:
'{'
    'kind' ':' 'EMPTY_STATEMENT' ','
    'loc' ':' location ','
    'children' ':' '[' ']'
'}'
{
    JavaParserActions.stmtBlock $8 Nothing
}

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
    'annotations' ':' '[' optional(commalistof(annotation)) ']' ','
    'supers' ':' '[' optional(commalistof(type)) ']' ','
    'children' ':' '[' optional(commalistof(attribute)) ']'
'}'
{
    JavaParserActions.stmtClass $8 $12 $17 $23 $29
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
    'annotations' ':' '[' optional(commalistof(annotation)) ']' ','
    'supers' ':' '[' optional(commalistof(type)) ']' ','
    'children' ':' '[' optional(commalistof(attribute)) ']'
'}'
{
    JavaParserActions.stmtClass $8 $12 $17 $23 $29
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
    'annotations' ':' '[' optional(commalistof(annotation)) ']' ','
    'supers' ':' '[' optional(commalistof(type)) ']' ','
    'children' ':' '[' optional(commalistof(attribute)) ']'
'}'
{
    JavaParserActions.stmtClass $8 $12 $17 $23 $29
}

-- *************
-- *           *
-- * attribute *
-- *           *
-- *************
attribute:
stmt_vardec { $1 } |
stmt_ctor   { $1 } |
stmt_method { $1 } |
stmt_class  { $1 } |
stmt_enum   { $1 } |
stmt_block  { $1 }

stmt_vardec:
stmt_vardec_init          { $1 } |
stmt_vardec_inferred_init { $1 } |
stmt_vardec_no_init       { $1 }

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
    'annotations' ':' '[' optional(commalistof(annotation)) ']' ','
    'type' ':' type ','
    'init' ':' exp
'}'
{
    JavaParserActions.stmtVardec $8 $12 $22 (Just $26)
}

-- ****************************
-- *                          *
-- * stmt_vardec_inferred_init *
-- *                          *
-- ****************************
stmt_vardec_inferred_init:
'{'
    'kind' ':' 'VARIABLE' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'annotations' ':' '[' optional(commalistof(annotation)) ']' ','
    'init' ':' exp
'}'
{
    JavaParserActions.stmtVardec $8 $12 (JavaParserActions.primitiveType $8 "var") (Just $22)
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
    'annotations' ':' '[' optional(commalistof(annotation)) ']' ','
    'type' ':' type
'}'
{
    JavaParserActions.stmtVardec $8 $12 $22 Nothing
}

-- ********
-- *      *
-- * type *
-- *      *
-- ********
type:
simple_var            { $1 } |
field_var             { $1 } |
parameterized_type    { $1 } |
primitive_type        { $1 } |
array_type            { $1 } |
union_type            { $1 } |
extends_wildcard_type { $1 } |
super_wildcard_type   { $1 }

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
    'text' ':' tokenID
'}'
{
    JavaParserActions.primitiveType $8 $12
}

-- **************
-- *            *
-- * array_type *
-- *            *
-- **************
array_type:
'{'
    'kind' ':' 'ARRAY_TYPE' ','
    'loc' ':' location ','
    'children' ':' '[' type ']'
'}'
{
    JavaParserActions.arrayType $8 $13
}

-- **************
-- *            *
-- * union_type *
-- *            *
-- **************
union_type:
'{'
    'kind' ':' 'UNION_TYPE' ','
    'loc' ':' location ','
    'children' ':' '[' commalistof(type) ']'
'}'
{
    JavaParserActions.parameterizedType $8 $13
}

-- *************************
-- *                       *
-- * extends_wildcard_type *
-- *                       *
-- *************************
extends_wildcard_type:
'{'
    'kind' ':' 'EXTENDS_WILDCARD' ','
    'loc' ':' location ','
    'children' ':' '[' type ']'
'}'
{
    $13
}

-- ***********************
-- *                     *
-- * super_wildcard_type *
-- *                     *
-- ***********************
super_wildcard_type:
'{'
    'kind' ':' 'SUPER_WILDCARD' ','
    'loc' ':' location ','
    'children' ':' '[' type ']'
'}'
{
    $13
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
    'annotations' ':' '[' optional(commalistof(annotation)) ']' ','
    'type' ':' type
'}'
{
    JavaParserActions.parameter $8 $12 $17 $22
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
    'annotations' ':' '[' optional(commalistof(annotation)) ']' ','
    'parameters' ':' '[' optional(commalistof(parameter)) ']' ','
    'body' ':' '[' optional(stmt_block) ']'
'}'
{
    JavaParserActions.stmtCtor $8 $12 $17 $23 $29
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
    'annotations' ':' '[' optional(commalistof(annotation)) ']' ','
    'parameters' ':' '[' optional(commalistof(parameter)) ']' ','
    'returnType' ':' type ','
    'body' ':' '[' optional(stmt_block) ']'
'}'
{
    JavaParserActions.stmtMethod $8 $12 $17 $23 $28 $33
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
exp_char          { $1 } |
exp_int           { $1 } |
exp_null          { $1 } |
exp_bool          { $1 } |
exp_assign        { $1 } |
exp_call          { $1 } |
exp_new           { $1 } |
exp_new_array     { $1 } |
exp_binop         { $1 } |
exp_ternary        { $1 } |
exp_unop          { $1 } |
exp_lambda        { $1 } |
exp_instanceof    { $1 } |
exp_switch        { $1 } |
exp_member_reference { $1 }

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
    'text' ':' tokenID
'}'
{
    JavaParserActions.expStr $8 $12
}

-- ************
-- *          *
-- * exp_char *
-- *          *
-- ************
exp_char:
'{'
    'kind' ':' 'CHAR_LITERAL' ','
    'loc' ':' location ','
    'text' ':' tokenID
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
    'kind' ':' int_literal_kind ','
    'loc' ':' location ','
    'text' ':' tokenID
'}'
{
    JavaParserActions.expInt $8 $12
}

int_literal_kind:
'INT_LITERAL'    { () } |
'LONG_LITERAL'   { () } |
'DOUBLE_LITERAL' { () } |
'FLOAT_LITERAL'  { () }

-- ************
-- *          *
-- * exp_null *
-- *          *
-- ************
exp_null:
'{'
    'kind' ':' 'NULL_LITERAL' ','
    'loc' ':' location ','
    'text' ':' tokenID
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
    'text' ':' tokenID
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
    'children' ':' '[' commalistof(new_class_child) ']'
'}'
{
    JavaParserActions.expCall $8 $13
}

new_class_child:
exp                 { $1 } |
parameterized_type  { Ast.ExpVar (Ast.ExpVarContent $1) } |
anonymous_class_exp { $1 }

anonymous_class_exp:
'{'
    'kind' ':' 'CLASS' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'annotations' ':' '[' optional(commalistof(annotation)) ']' ','
    'supers' ':' '[' optional(commalistof(type)) ']' ','
    'children' ':' '[' optional(commalistof(attribute)) ']'
'}'
{
    JavaParserActions.instrumentationCall "anonymous_class" $8 []
}

-- ***************
-- *             *
-- * exp_new_array *
-- *             *
-- ***************
exp_new_array:
'{'
    'kind' ':' 'NEW_ARRAY' ','
    'loc' ':' location ','
    'children' ':' '[' optional(commalistof(new_array_child)) ']'
'}'
{
    JavaParserActions.instrumentationCall "new_array" $8 (fromMaybe [] $13)
}

new_array_child:
exp                { $1 } |
parameterized_type { Ast.ExpVar (Ast.ExpVarContent $1) } |
primitive_type     { Ast.ExpVar (Ast.ExpVarContent $1) } |
array_type         { Ast.ExpVar (Ast.ExpVarContent $1) }

-- **************
-- *            *
-- * exp_lambda *
-- *            *
-- **************
exp_lambda:
'{'
    'kind' ':' 'LAMBDA_EXPRESSION' ','
    'loc' ':' location ','
    'children' ':' '[' optional(commalistof(lambda_child)) ']'
'}'
{
    JavaParserActions.expLambda $8 $13
}

-- ************************
-- *                      *
-- * exp_member_reference *
-- *                      *
-- ************************
exp_member_reference:
'{'
    'kind' ':' 'MEMBER_REFERENCE' ','
    'loc' ':' location ','
    'children' ':' '[' commalistof(exp) ']'
'}'
{
    JavaParserActions.expMemberReference $8 $13
}

exp_switch:
'{'
    'kind' ':' 'SWITCH_EXPRESSION' ','
    'loc' ':' location ','
    'children' ':' '[' exp optional_switch_cases ']'
'}'
{
    $13
}

lambda_child:
exp              { Just $1 } |
parameter        { Nothing } |
lambda_parameter { Nothing } |
stmt_body        { Nothing }

lambda_parameter:
'{'
    'kind' ':' 'VARIABLE' ','
    'loc' ':' location ','
    'text' ':' tokenID ','
    'annotations' ':' '[' optional(commalistof(annotation)) ']'
'}'
{
    ()
}

-- ******************
-- *                *
-- * exp_instanceof *
-- *                *
-- ******************
exp_instanceof:
'{'
    'kind' ':' 'INSTANCE_OF' ','
    'loc' ':' location ','
    'children' ':' '[' exp ',' type ']'
'}'
{
    JavaParserActions.expBinop $8 $13 (Ast.ExpVar (Ast.ExpVarContent $15)) Ast.EQ
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
    'right' ':' exp
'}'
{
    JavaParserActions.expBinop $8 $12 $16 $4
}

binop_kind:
'EQUAL_TO'            { Ast.EQ    } |
'NOT_EQUAL_TO'        { Ast.EQ    } |
'GREATER_THAN'        { Ast.EQ    } |
'GREATER_THAN_EQUAL'  { Ast.EQ    } |
'LESS_THAN'           { Ast.EQ    } |
'LESS_THAN_EQUAL'     { Ast.EQ    } |
'PLUS'                { Ast.PLUS  } |
'MINUS'               { Ast.PLUS  } |
'REMAINDER'           { Ast.PLUS  } |
'MULTIPLY'            { Ast.TIMES } |
'DIVIDE'              { Ast.TIMES } |
'AND'                 { Ast.PLUS  } |
'LEFT_SHIFT'          { Ast.PLUS  } |
'UNSIGNED_RIGHT_SHIFT' { Ast.PLUS  } |
'CONDITIONAL_AND'     { Ast.PLUS  } |
'CONDITIONAL_OR'      { Ast.PLUS  }

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
    JavaParserActions.expTernary $8 $13 $15 $17
}

-- ************
-- *          *
-- * exp_unop *
-- *          *
-- ************
exp_unop:
exp_not               { $1 } |
exp_postfix_increment { $1 } |
exp_prefix_increment  { $1 } |
exp_unary_minus       { $1 }

exp_not:
'{'
    'kind' ':' 'LOGICAL_COMPLEMENT' ','
    'loc' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    JavaParserActions.expUnop "not" $8 $13
}

exp_postfix_increment:
'{'
    'kind' ':' postfix_update_kind ','
    'loc' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    JavaParserActions.expUnop $4 $8 $13
}

postfix_update_kind:
'POSTFIX_INCREMENT' { "postfix_increment" } |
'POSTFIX_DECREMENT' { "postfix_decrement" }

exp_prefix_increment:
'{'
    'kind' ':' 'PREFIX_INCREMENT' ','
    'loc' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    JavaParserActions.expUnop "prefix_increment" $8 $13
}

exp_unary_minus:
'{'
    'kind' ':' 'UNARY_MINUS' ','
    'loc' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    JavaParserActions.expUnop "unary_minus" $8 $13
}

-- *************
-- *           *
-- * stmt_body *
-- *           *
-- *************
stmt_body:
stmt_block        { $1 } |
stmt_exp          { $1 } |
stmt_return       { $1 } |
stmt_throw        { $1 } |
stmt_continue     { $1 } |
stmt_break        { $1 } |
stmt_if           { $1 } |
stmt_switch       { $1 } |
stmt_while        { $1 } |
stmt_do_while     { $1 } |
stmt_vardec       { $1 } |
stmt_enhanced_for { $1 } |
stmt_for          { $1 } |
stmt_try          { $1 }

-- **************
-- *            *
-- * stmt_block *
-- *            *
-- **************
stmt_block:
'{'
    'kind' ':' 'BLOCK' ','
    'loc' ':' location ','
    'children' ':' '[' optional(commalistof(stmt_body)) ']'
'}'
{
    JavaParserActions.stmtBlock $8 $13
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
    'children' ':' '[' optional(exp) ']'
'}'
{
    JavaParserActions.stmtReturn $8 $13
}

-- **************
-- *            *
-- * stmt_throw *
-- *            *
-- **************
stmt_throw:
'{'
    'kind' ':' 'THROW' ','
    'loc' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    JavaParserActions.stmtThrow $8 $13
}

-- ***************
-- *             *
-- * stmt_continue *
-- *             *
-- ***************
stmt_continue:
'{'
    'kind' ':' 'CONTINUE' ','
    'loc' ':' location ','
    'children' ':' '[' ']'
'}'
{
    JavaParserActions.stmtContinue $8
}

-- **************
-- *            *
-- * stmt_break *
-- *            *
-- **************
stmt_break:
'{'
    'kind' ':' 'BREAK' ','
    'loc' ':' location ','
    'children' ':' '[' ']'
'}'
{
    JavaParserActions.stmtBreak $8
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
    'children' ':' '[' optional(commalistof(stmt_body)) ']'
'}'
{
    JavaParserActions.stmtIf $8 $12 $16 $21
}

-- ***************
-- *             *
-- * stmt_switch *
-- *             *
-- ***************
stmt_switch:
'{'
    'kind' ':' 'SWITCH' ','
    'loc' ':' location ','
    'children' ':' '[' exp optional_switch_cases ']'
'}'
{
    JavaParserActions.stmtExp $13
}

optional_switch_cases:
{ () } |
',' commalistof(switch_case) { () }

switch_case:
'{'
    'kind' ':' 'CASE' ','
    'loc' ':' location ','
    'children' ':' '[' optional(commalistof(switch_case_child)) ']'
'}'
{
    ()
}

switch_case_child:
case_label { () } |
stmt_body  { () } |
exp        { () }

case_label:
'{'
    'kind' ':' case_label_kind ','
    'loc' ':' location ','
    'children' ':' '[' optional(commalistof(exp)) ']'
'}'
{
    ()
}

case_label_kind:
'CONSTANT_CASE_LABEL' { () } |
'DEFAULT_CASE_LABEL'  { () } |
'PATTERN_CASE_LABEL'  { () }

-- **************
-- *            *
-- * stmt_while *
-- *            *
-- **************
stmt_while:
'{'
    'kind' ':' 'WHILE_LOOP' ','
    'loc' ':' location ','
    'children' ':' '[' exp ',' stmt_body ']'
'}'
{
    JavaParserActions.stmtWhile $8 $13 $15
}

-- *****************
-- *               *
-- * stmt_do_while *
-- *               *
-- *****************
stmt_do_while:
'{'
    'kind' ':' 'DO_WHILE_LOOP' ','
    'loc' ':' location ','
    'children' ':' '[' stmt_body ',' exp ']'
'}'
{
    JavaParserActions.stmtWhile $8 $15 $13
}

-- *********************
-- *                   *
-- * stmt_enhanced_for *
-- *                   *
-- *********************
stmt_enhanced_for:
'{'
    'kind' ':' 'ENHANCED_FOR_LOOP' ','
    'loc' ':' location ','
    'children' ':' '[' stmt_vardec ',' exp ',' stmt_body ']'
'}'
{
    JavaParserActions.stmtEnhancedFor $8 $13 $15 $17
}

-- *****************
-- *               *
-- * stmt_for_init *
-- *               *
-- *****************
stmt_for_init:
stmt_vardec { $1 } |
stmt_exp    { $1 }

-- **********
-- *        *
-- * stmt_for *
-- *        *
-- **********
stmt_for:
'{'
    'kind' ':' 'FOR_LOOP' ','
    'loc' ':' location ','
    'children' ':' '[' stmt_for_init ',' exp ',' stmt_exp ',' stmt_body ']'
'}'
{
    JavaParserActions.stmtFor $8 $13 $15 $17 $19
} |
'{'
    'kind' ':' 'FOR_LOOP' ','
    'loc' ':' location ','
    'children' ':' '[' stmt_for_init ',' exp ',' stmt_block ']'
'}'
{
    JavaParserActions.stmtForNoUpdate $8 $13 $15 $17
} |
'{'
    'kind' ':' 'FOR_LOOP' ','
    'loc' ':' location ','
    'children' ':' '[' exp ',' stmt_exp ',' stmt_body ']'
'}'
{
    JavaParserActions.stmtForNoInit $8 $13 $15 $17
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
    'children' ':' '[' stmt_block ',' commalistof(try_part) ']'
'}'
{
    JavaParserActions.stmtTry $8 $13 $15
} |
'{'
    'kind' ':' 'TRY' ','
    'loc' ':' location ','
    'children' ':' '[' stmt_vardec ',' stmt_block optional_try_catches ']'
'}'
{
    JavaParserActions.stmtTryWithResource $8 $13 $15 $16
} |
'{'
    'kind' ':' 'TRY' ','
    'loc' ':' location ','
    'children' ':' '[' stmt_vardec ',' stmt_vardec ',' stmt_block optional_try_catches ']'
'}'
{
    JavaParserActions.stmtTryWithResources $8 [$13, $15] $17 $18
}

optional_try_catches:
{ Nothing } |
',' commalistof(try_part) { Just $2 }

try_part:
catch_part { $1 } |
stmt_block { $1 }

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
    'kind' ':' assignment_kind ','
    'loc' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    JavaParserActions.expAssign $8 $13 $15
}

assignment_kind:
'ASSIGNMENT'                       { () } |
'MULTIPLY_ASSIGNMENT'              { () } |
'DIVIDE_ASSIGNMENT'                { () } |
'REMAINDER_ASSIGNMENT'             { () } |
'PLUS_ASSIGNMENT'                  { () } |
'MINUS_ASSIGNMENT'                 { () } |
'LEFT_SHIFT_ASSIGNMENT'            { () } |
'RIGHT_SHIFT_ASSIGNMENT'           { () } |
'UNSIGNED_RIGHT_SHIFT_ASSIGNMENT'  { () } |
'AND_ASSIGNMENT'                   { () } |
'XOR_ASSIGNMENT'                   { () } |
'OR_ASSIGNMENT'                    { () }

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
simple_var    { $1} |
field_var     { $1} |
subscript_var { $1}

-- ***************
-- *             *
-- * subscript_var *
-- *             *
-- ***************
subscript_var:
'{'
    'kind' ':' 'ARRAY_ACCESS' ','
    'loc' ':' location ','
    'children' ':' '[' exp ',' exp ']'
'}'
{
    JavaParserActions.subscriptVar $8 $13 $15
}

-- **************
-- *            *
-- * simple_var *
-- *            *
-- **************
simple_var:
'{'
    'kind' ':' 'IDENTIFIER' ','
    'loc' ':' location ','
    'text' ':' tokenID
'}'
{
    JavaParserActions.simpleVar $8 $12
}

field_var_lhs:
exp            { $1 } |
primitive_type { Ast.ExpVar (Ast.ExpVarContent $1) }

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
    'children' ':' '[' field_var_lhs ']'
'}'
{
    JavaParserActions.fieldVar $8 $12 $17
}

-- ***********
-- *         *
-- * tokenID *
-- *         *
-- ***********
-- Every reserved Alex token is a valid Java identifier spelling. The
-- lexer steals those strings from the generic ID rule, so `text`
-- payloads must accept the full keyword set rather than grow one
-- collision at a time. When a new %token keyword is added, add it here.
tokenID:
ID                                      { tokIDValue $1 } |
'kind'                                  { "kind" } |
'loc'                                   { "loc" } |
'children'                              { "children" } |
'text'                                  { "text" } |
'startLine'                             { "startLine" } |
'startCol'                              { "startCol" } |
'endLine'                               { "endLine" } |
'endCol'                                { "endCol" } |
'annotations'                           { "annotations" } |
'parameters'                            { "parameters" } |
'supers'                                { "supers" } |
'type'                                  { "type" } |
'init'                                  { "init" } |
'cond'                                  { "cond" } |
'body'                                  { "body" } |
'returnType'                            { "returnType" } |
'left'                                  { "left" } |
'right'                                 { "right" } |
'CONTINUE'                              { "CONTINUE" } |
'GREATER_THAN'                          { "GREATER_THAN" } |
'LESS_THAN'                             { "LESS_THAN" } |
'OTHER'                                 { "OTHER" } |
'METHOD'                                { "METHOD" } |
'OR'                                    { "OR" }

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
