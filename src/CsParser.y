{
{-# OPTIONS -Werror=missing-fields #-}

module CsParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import CsLexer
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
import Data.List ( map )
import Data.Map ( fromList, empty )

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

'(' { AlexTokenTag AlexRawToken_LPAREN _ }
')' { AlexTokenTag AlexRawToken_RPAREN _ }
'[' { AlexTokenTag AlexRawToken_LBRACK _ }
']' { AlexTokenTag AlexRawToken_RBRACK _ }
'{' { AlexTokenTag AlexRawToken_LBRACE _ }
'}' { AlexTokenTag AlexRawToken_RBRACE _ }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

':' { AlexTokenTag AlexRawToken_COLON _ }
',' { AlexTokenTag AlexRawToken_COMMA _ }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

'kind' { AlexTokenTag AlexRawToken_KIND _ }
'null' { AlexTokenTag AlexRawToken_NULL _ }
'value' { AlexTokenTag AlexRawToken_VALUE _ }
'location' { AlexTokenTag AlexRawToken_LOCATION _ }
'children' { AlexTokenTag AlexRawToken_CHILDREN _ }
'CompilationUnit' { AlexTokenTag AlexRawToken_COMPILATION_UNIT _ }
'UsingDirective' { AlexTokenTag AlexRawToken_USING_DIRECTIVE _ }
'IdentifierName' { AlexTokenTag AlexRawToken_IDENTIFIER_NAME _ }
'QualifiedName' { AlexTokenTag AlexRawToken_QUALIFIED_NAME _ }
'NameEquals' { AlexTokenTag AlexRawToken_NAME_EQUALS _ }
'NamespaceDeclaration' { AlexTokenTag AlexRawToken_NAMESPACE_DECLARATION _ }
'ClassDeclaration' { AlexTokenTag AlexRawToken_CLASS_DECLARATION _ }
'SimpleBaseType' { AlexTokenTag AlexRawToken_SIMPLE_BASE_TYPE _ }
'BaseList' { AlexTokenTag AlexRawToken_BASE_LIST _ }
'ArgumentList' { AlexTokenTag AlexRawToken_ARGUMENT_LIST _ }
'ObjectCreationExpression' { AlexTokenTag AlexRawToken_OBJECT_CREATION_EXPRESSION _ }
'EqualsValueClause' { AlexTokenTag AlexRawToken_EQUALS_VALUE_CLAUSE _ }
'VariableDeclarator' { AlexTokenTag AlexRawToken_VARIABLE_DECLARATOR _ }
'TypeArgumentList' { AlexTokenTag AlexRawToken_TYPE_ARGUMENT_LIST _ }
'ObjectCreationExample' { AlexTokenTag AlexRawToken_OBJECT_CREATION_EXPRESSION _ }
'GenericName' { AlexTokenTag AlexRawToken_GENERIC_NAME _ }
'VariableDeclaration' { AlexTokenTag AlexRawToken_VARIABLE_DECLARATION _ }
'FieldDeclaration' { AlexTokenTag AlexRawToken_FIELD_DECLARATION _ }
'ElementAccessExpression' { AlexTokenTag AlexRawToken_ELEMENT_ACCESS_EXPRESSION _ }
'BracketedArgumentList' { AlexTokenTag AlexRawToken_BRACKETED_ARGUMENT_LIST _ }
'Argument' { AlexTokenTag AlexRawToken_ARGUMENT _ }
'NullableType' { AlexTokenTag AlexRawToken_NULLABLE_TYPE _ }
'MethodDeclaration' { AlexTokenTag AlexRawToken_METHOD_DECLARATION _ }
'ParameterList' { AlexTokenTag AlexRawToken_PARAMETER_LIST _ }
'Block' { AlexTokenTag AlexRawToken_BLOCK _ }
'IfStatement' { AlexTokenTag AlexRawToken_IF_STATEMENT _ }
'EqualsExpression' { AlexTokenTag AlexRawToken_EQUALS_EXPRESSION _ }
'NotEqualsExpression' { AlexTokenTag AlexRawToken_NOT_EQUALS_EXPRESSION _ }
'NullLiteralExpression' { AlexTokenTag AlexRawToken_NULL_LITERAL_EXPRESSION _ }
'WhileStatement' { AlexTokenTag AlexRawToken_WHILE_STATEMENT _ }
'InvocationExpression' { AlexTokenTag AlexRawToken_INVOCATION_EXPRESSION _ }
'SimpleMemberAccessExpression' { AlexTokenTag AlexRawToken_SIMPLE_MEMBER_ACCESS_EXPRESSION _ }
'ExpressionStatement' { AlexTokenTag AlexRawToken_EXPRESSION_STATEMENT _ }
'SimpleAssignmentExpression' { AlexTokenTag AlexRawToken_SIMPLE_ASSIGNMENT_EXPRESSION _ }
'IsPatternExpression' { AlexTokenTag AlexRawToken_IS_PATTERN_EXPRESSION _ }
'DeclarationPattern' { AlexTokenTag AlexRawToken_DECLARATION_PATTERN _ }
'SingleVariableDesignation' { AlexTokenTag AlexRawToken_SINGLE_VARIABLE_DESIGNATION _ }
'ElseClause' { AlexTokenTag AlexRawToken_ELSE_CLAUSE _ }
'BreakStatement' { AlexTokenTag AlexRawToken_BREAK_STATEMENT _ }
'ReturnStatement' { AlexTokenTag AlexRawToken_RETURN_STATEMENT _ }
'ConstructorDeclaration' { AlexTokenTag AlexRawToken_CONSTRUCTOR_DECLARATION _ }
'Parameter' { AlexTokenTag AlexRawToken_PARAMETER _ }
'ThisConstructorInitializer' { AlexTokenTag AlexRawToken_THIS_CONSTRUCTOR_INITIALIZER _ }
'ThisExpression' { AlexTokenTag AlexRawToken_THIS_EXPRESSION _ }
'PropertyDeclaration' { AlexTokenTag AlexRawToken_PROPERTY_DECLARATION _ }
'AccessorList' { AlexTokenTag AlexRawToken_ACCESSOR_LIST _ }
'GetAccessorDeclaration' { AlexTokenTag AlexRawToken_GET_ACCESSOR_DECLARATION _ }
'SetAccessorDeclaration' { AlexTokenTag AlexRawToken_SET_ACCESSOR_DECLARATION _ }
'PredefinedType' { AlexTokenTag AlexRawToken_PREDEFINED_TYPE _ }
'FalseLiteralExpression' { AlexTokenTag AlexRawToken_FALSE_LITERAL_EXPRESSION _ }
'TrueLiteralExpression' { AlexTokenTag AlexRawToken_TRUE_LITERAL_EXPRESSION _ }
'NumericLiteralExpression' { AlexTokenTag AlexRawToken_NUMERIC_LITERAL_EXPRESSION _ }
'SwitchStatement' { AlexTokenTag AlexRawToken_SWITCH_STATEMENT _ }
'SwitchSection' { AlexTokenTag AlexRawToken_SWITCH_SECTION _ }
'CaseSwitchLabel' { AlexTokenTag AlexRawToken_CASE_SWITCH_LABEL _ }
'DefaultSwitchLabel' { AlexTokenTag AlexRawToken_DEFAULT_SWITCH_LABEL _ }
'StringLiteralExpression' { AlexTokenTag AlexRawToken_STRING_LITERAL_EXPRESSION _ }
'ThrowStatement' { AlexTokenTag AlexRawToken_THROW_STATEMENT _ }
'LocalDeclarationStatement' { AlexTokenTag AlexRawToken_LOCAL_DECLARATION_STATEMENT _ }
'LogicalNotExpression' { AlexTokenTag AlexRawToken_LOGICAL_NOT_EXPRESSION _ }
'ParenthesizedExpression' { AlexTokenTag AlexRawToken_PARENTHESIZED_EXPRESSION _ }

-- ************
-- *          *
-- * location *
-- *          *
-- ************

'startLine' { AlexTokenTag AlexRawToken_START_LINE _ }
'startColumn' { AlexTokenTag AlexRawToken_START_COLUMN _ }
'endLine' { AlexTokenTag AlexRawToken_END_LINE _ }
'endColumn' { AlexTokenTag AlexRawToken_END_COLUMN _ }

-- ************
-- *          *
-- * integers *
-- *          *
-- ************

INT { AlexTokenTag (AlexRawToken_INT i) _ }
ID { AlexTokenTag (AlexRawToken_ID s) _ }

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
'{'
    'kind' ':' 'CompilationUnit' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(stmt) 
'}'
{
    Ast.Root
    {
        Ast.filename = getFilename $1,
        stmts = []
    }
}

tokenID:
ID { tokIDValue $1 } |
'null' { "null" }

identifier:
'{'
    'kind' ':' 'IdentifierName' ','
    'value' ':' optional(ID) ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Token.Named
    {
        Token.content = case $8 of { Nothing -> "<empty>"; Just name -> tokIDValue name },
        Token.location = $12
    }
}

var_simple_1:
identifier
{
    Ast.VarSimple $ Ast.VarSimpleContent
    {
        Ast.varName = Token.VarName $1
    }
}

var_simple_2:
'{'
    'kind' ':' 'SingleVariableDesignation' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = tokIDValue $8,
        Token.location = $12
    }
}

var_simple:
var_simple_1 { $1 } |
var_simple_2 { $1 }

var_field_1:
'{'
    'kind' ':' 'QualifiedName' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' identifier ']'
'}'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = Ast.ExpVar $ Ast.ExpVarContent $17,
        Ast.varFieldName = Token.FieldName $19,
        Ast.varFieldLocation = $12
    }
}

var_field_2:
'{'
    'kind' ':' 'SimpleMemberAccessExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' identifier ']'
'}'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $17,
        Ast.varFieldName = Token.FieldName $19,
        Ast.varFieldLocation = $12
    }
}

var_field:
var_field_1 { $1 } |
var_field_2 { $1 }

var:
var_simple { $1 } |
var_field { $1 }

stmt_using_1:
'{'
    'kind' ':' 'UsingDirective' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(var)
'}'
{
    Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent (stringify $16)),
        Ast.stmtImportSpecific = Nothing,
        Ast.stmtImportAlias = Nothing,
        Ast.stmtImportLocation = $12
    }
}

name_equals:
'{'
    'kind' ':' 'NameEquals' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    $17
}

stmt_using_2:
'{'
    'kind' ':' 'UsingDirective' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' name_equals ',' var ']'
'}'
{
    Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent (stringify [$19])),
        Ast.stmtImportSpecific = Just (Ast.ImportSpecific (Token.content $17)),
        Ast.stmtImportAlias = Nothing,
        Ast.stmtImportLocation = $12
    }
}

stmt_using:
stmt_using_1 { $1 } |
stmt_using_2 { $1 }

stmt_var:
var
{
    Ast.StmtExp (Ast.ExpVar (Ast.ExpVarContent $1))
}

stmt_class:
'{'
    'kind' ':' 'ClassDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(stmt)
'}'
{
    Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName (Token.Named "MyAwesomeClass" $12),
        Ast.stmtClassSupers = [],
        Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
        Ast.stmtClassMethods = Ast.Methods Data.Map.empty
    }
}

simple_base_type:
'{'
    'kind' ':' 'SimpleBaseType' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    $17
}

stmt_super:
'{'
    'kind' ':' 'BaseList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' simple_base_type ']'
'}'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "thisClassExtends" $12,
        Ast.stmtAssignRhs = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $17
    }
}

type_argument_list:
'{'
    'kind' ':' 'TypeArgumentList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    $17
}

generic_name:
'{'
    'kind' ':' 'GenericName' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' type_argument_list ']'
'}'
{
    $17
}

nullable_type:
'{'
    'kind' ':' 'NullableType' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominal_type ']'
'}'
{
    $17
}

predefined_type:
'{'
    'kind' ':' 'PredefinedType' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName (Token.Named "any" $12)
}

nominal_type:
var { $1 } |
nullable_type { $1 } |
predefined_type { $1 } |
generic_name { Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $1 }

argument_list_value:
'(' ')' { Nothing } |
'null'  { Nothing }

args:
'{'
    'kind' ':' 'ArgumentList' ','
    'value' ':' argument_list_value ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(exp_arg)
'}'
{
    []
}

exp_new:
'{'
    'kind' ':' 'ObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominal_type ',' args ']'
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $17,
        Ast.args = [],
        Ast.expCallLocation = $12
    }
}

array_values:
'{'
    'kind' ':' 'BracketedArgumentList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(exp)
'}'
{
    $16
}

exp_array:
'{'
    'kind' ':' 'ElementAccessExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ',' array_values ']'
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "arrayify" $12,
        Ast.args = [],
        Ast.expCallLocation = $12
    }
}

exp_var: var { Ast.ExpVar (Ast.ExpVarContent $1) }

exp_arg:
'{'
    'kind' ':' 'Argument' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

operator:
'EqualsExpression'    { Nothing } |
'IsPatternExpression' { Nothing } |
'NotEqualsExpression' { Nothing }

exp_binop:
'{'
    'kind' ':' operator ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' exp ']'
'}'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $17,
        Ast.expBinopRight = $19,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $12
    }
}

exp_null:
'{'
    'kind' ':' 'NullLiteralExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Ast.ExpNull (Ast.ExpNullContent (Token.ConstNull $12))
}

exp_call:
'{'
    'kind' ':' 'InvocationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' args ']'
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = $17,
        Ast.args = $19,
        Ast.expCallLocation = $12
    }
}

exp_casting:
'{'
    'kind' ':' 'DeclarationPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    $19
}

exp_this:
'{'
    'kind' ':' 'ThisExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "this" $12
}

exp_bool_true:
'{'
    'kind' ':' 'TrueLiteralExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool False $12
}

exp_bool_false:
'{'
    'kind' ':' 'FalseLiteralExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool False $12
}

exp_bool:
exp_bool_true { $1 } |
exp_bool_false { $1 }

exp_int:
'{'
    'kind' ':' 'NumericLiteralExpression' ','
    'value' ':' INT ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt (tokIntValue $15) $12
}

exp_str:
'{'
    'kind' ':' 'StringLiteralExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Ast.ExpStr $ Ast.ExpStrContent $ Token.ConstStr (tokIDValue $15) $12
}

exp_not:
'{'
    'kind' ':' 'LogicalNotExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

exp_paren:
'{'
    'kind' ':' 'ParenthesizedExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

exp:
exp_new { $1 } |
exp_arg { $1 } |
exp_var { $1 } |
exp_not { $1 } |
exp_int { $1 } |
exp_str { $1 } |
exp_this { $1 } |
exp_call { $1 } |
exp_null { $1 } |
exp_bool { $1 } |
exp_binop { $1 } |
exp_paren { $1 } |
exp_casting { $1 } |
exp_array { $1 }

equals_value_clause:
'{'
    'kind' ':' 'EqualsValueClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

vardec:
'{'
    'kind' ':' 'VariableDeclarator' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' '[' optional(equals_value_clause) ']'
'}'
{
    $17
}

stmt_vardec:
'{'
    'kind' ':' 'VariableDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominal_type ',' vardec ']'
'}'
{
    Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = Token.VarName (Token.Named "unknown" $12),
        Ast.stmtVardecNominalType = Nothing,
        Ast.stmtVardecInitValue = $19,
        Ast.stmtVardecLocation = $12 
    }
}

stmt_local_vardec:
'{'
    'kind' ':' 'LocalDeclarationStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmt_vardec ']'
'}'
{
    $17
}

stmt_data_member:
'{'
    'kind' ':' 'FieldDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmt_vardec ']'
'}'
{
    $17
}

accessor_kind:
'GetAccessorDeclaration' { Nothing } |
'SetAccessorDeclaration' { Nothing }

accessor:
'{'
    'kind' ':' accessor_kind ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Nothing
}

accessor_list:
'{'
    'kind' ':' 'AccessorList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(accessor)
'}'
{
    Nothing
}

stmt_property:
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominal_type ',' accessor_list ']'
'}'
{
    Ast.StmtMethod $ Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = Nothing,
        Ast.stmtMethodName = Token.MethodName (Token.Named "POPO" $12),
        Ast.stmtMethodParams = [],
        Ast.stmtMethodBody = [],
        Ast.stmtMethodAnnotations = [],
        Ast.stmtMethodLocation = $12,
        Ast.hostingClassName = Token.ClassName (Token.Named "MOMO" $12),
        Ast.hostingClassSupers = [],
        Ast.hostingClassAnnotations = []
    }
}

body_value:
'{' '}' { Nothing } |
'null'  { Nothing }

stmt_block:
'{'
    'kind' ':' 'Block' ','
    'value' ':' body_value ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(stmt)
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent $16 $12
}

parameter_list_value:
'(' ')' { Nothing } |
'null'  { Nothing }

parameter:
'{'
    'kind' ':' 'Parameter' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    $17
}

params:
'{'
    'kind' ':' 'ParameterList' ','
    'value' ':' parameter_list_value ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(parameter)
'}'
{
    []
}

return_type: nominal_type { $1 }

stmt_method:
'{'
    'kind' ':' 'MethodDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' return_type ',' params ',' stmt_block ']'
'}'
{
    Ast.StmtMethod $ Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = Nothing,
        Ast.stmtMethodName = Token.MethodName (Token.Named "POPO" $12),
        Ast.stmtMethodParams = $19,
        Ast.stmtMethodBody = [],
        Ast.stmtMethodAnnotations = [],
        Ast.stmtMethodLocation = $12,
        Ast.hostingClassName = Token.ClassName (Token.Named "MOMO" $12),
        Ast.hostingClassSupers = [],
        Ast.hostingClassAnnotations = []
    }
}

else:
','
'{'
    'kind' ':' 'ElseClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(stmt)
'}'
{
    $17
}

stmt_if:
'{'
    'kind' ':' 'IfStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' stmt_block optional(else) ']'
'}'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $17,
        Ast.stmtIfBody = [],
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $12
    }
}

stmt_while:
'{'
    'kind' ':' 'WhileStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' stmt_block ']'
'}'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $17,
        Ast.stmtWhileBody = [],
        Ast.stmtWhileLocation = $12
    }
}

stmt_wrapper:
'{'
    'kind' ':' 'ExpressionStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmt ']'
'}'
{
    $17
}

stmt_assign:
'{'
    'kind' ':' 'SimpleAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = $17,
        Ast.stmtAssignRhs = $19
    }
}

stmt_exp: exp { Ast.StmtExp $1 }

stmt_break:
'{'
    'kind' ':' 'BreakStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Ast.StmtBreak $ Ast.StmtBreakContent $12
}

stmt_return:
'{'
    'kind' ':' 'ReturnStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = Just $17,
        Ast.stmtReturnLocation = $12
    }
}

initializer:
'{'
    'kind' ':' 'ThisConstructorInitializer' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' args ']'
'}'
{
    []
}

stmt_ctor_1:
'{'
    'kind' ':' 'ConstructorDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' params ',' initializer ',' stmt_block ']'
'}'
{

    Ast.StmtMethod $ Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = Nothing,
        Ast.stmtMethodName = Token.MethodName (Token.Named "POPO" $12),
        Ast.stmtMethodParams = $17,
        Ast.stmtMethodBody = [],
        Ast.stmtMethodAnnotations = [],
        Ast.stmtMethodLocation = $12,
        Ast.hostingClassName = Token.ClassName (Token.Named "MOMO" $12),
        Ast.hostingClassSupers = [],
        Ast.hostingClassAnnotations = []
    }
}

stmt_ctor_2:
'{'
    'kind' ':' 'ConstructorDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' params ',' stmt_block ']'
'}'
{

    Ast.StmtMethod $ Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = Nothing,
        Ast.stmtMethodName = Token.MethodName (Token.Named "POPO" $12),
        Ast.stmtMethodParams = $17,
        Ast.stmtMethodBody = [],
        Ast.stmtMethodAnnotations = [],
        Ast.stmtMethodLocation = $12,
        Ast.hostingClassName = Token.ClassName (Token.Named "MOMO" $12),
        Ast.hostingClassSupers = [],
        Ast.hostingClassAnnotations = []
    }
}

stmt_ctor:
stmt_ctor_1 { $1 } |
stmt_ctor_2 { $1 }

case_label:
'{'
    'kind' ':' 'CaseSwitchLabel' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Nothing
}

normal_case:
'{'
    'kind' ':' 'SwitchSection' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' case_label ',' stmt ']'
'}'
{
    Nothing
}

default:
'{'
    'kind' ':' 'DefaultSwitchLabel' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Nothing
}

default_stmts:
stmt { [$1] } |
stmt ',' default_stmts { $1:$3 }

default_case:
'{'
    'kind' ':' 'SwitchSection' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' default ',' default_stmts ']'
'}'
{
    Nothing
}

case:
normal_case  { $1 } |
default_case { $1 }

cases:
case { [$1] } |
case ',' cases { $1:$3}

stmt_switch:
'{'
    'kind' ':' 'SwitchStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' cases ']'
'}'
{
    Ast.StmtExp $17
}

stmt_throw:
'{'
    'kind' ':' 'ThrowStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Ast.StmtExp $17
}

stmt:
stmt_if { $1 } |
stmt_exp { $1 } |
stmt_ctor { $1 } |
stmt_super { $1 } |
stmt_break { $1 } |
stmt_class { $1 } |
stmt_using { $1 } |
stmt_while { $1 } |
stmt_block { $1 } |
stmt_throw { $1 } |
stmt_switch { $1 } |
stmt_return { $1 } |
stmt_vardec { $1 } |
stmt_assign { $1 } |
stmt_method { $1 } |
stmt_wrapper { $1 } |
stmt_property { $1 } |
stmt_namespace { $1 } |
stmt_local_vardec { $1 } |
stmt_data_member { $1 }

stmt_namespace:
'{'
    'kind' ':' 'NamespaceDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(stmt)
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = $16,
        Ast.stmtBlockLocation = $12
    }
}

location:
'{'
    'startLine' ':' INT ','
    'startColumn' ':' INT ','
    'endLine' ':' INT ','
    'endColumn' ':' INT
'}'
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

dummyVar :: Location -> Ast.Var
dummyVar loc = Ast.VarSimple $ Ast.VarSimpleContent {
    Ast.varName = Token.VarName $ Token.Named {
        Token.content = "bestVarInTheWorld",
        Token.location = loc
    }
}

stringifyVarSimple :: Ast.VarSimpleContent -> String
stringifyVarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named v _))) = v

stringifyVarField :: Ast.VarFieldContent -> String
stringifyVarField (Ast.VarFieldContent (Ast.ExpVar (Ast.ExpVarContent v)) (Token.FieldName (Token.Named f _)) _) = (stringify' v) ++ "." ++ f

stringify' :: Ast.Var -> String	
stringify' (Ast.VarSimple v) = stringifyVarSimple v
stringify' (Ast.VarField v) = stringifyVarField v
stringify' _ = "MOISH"

stringify :: [ Ast.Var ] -> String
stringify = concatMap stringify' 

methodify :: Token.ClassName -> [ Ast.Var ] -> [ Ast.Stmt ] -> [(Token.MethodName, Ast.StmtMethodContent)]
methodify c vars stmts = catMaybes $ Data.List.map (methodify' c vars) stmts

methodify' :: Token.ClassName -> [ Ast.Var ] -> Ast.Stmt -> Maybe (Token.MethodName, Ast.StmtMethodContent)
methodify' c vars (Ast.StmtFunc f) = Just (methodify'' c vars f)
methodify' _ _ _ = Nothing

methodify'' :: Token.ClassName -> [ Ast.Var ] -> Ast.StmtFuncContent -> (Token.MethodName, Ast.StmtMethodContent)
methodify'' c vars f = let m = Token.MethodName $ Token.getFuncNameToken (Ast.stmtFuncName f) in (m, Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = (Ast.stmtFuncReturnType f),
    Ast.stmtMethodName = m,
    Ast.stmtMethodParams = (Ast.stmtFuncParams f),
    Ast.stmtMethodBody = (Ast.stmtFuncBody f),
    Ast.stmtMethodAnnotations = (Ast.stmtFuncAnnotations f),
    Ast.stmtMethodLocation = (Ast.stmtFuncLocation f),
    Ast.hostingClassName = c,
    Ast.hostingClassSupers = superify vars,
    Ast.hostingClassAnnotations = []
})

unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

simportify :: Location -> [(String, Maybe String)] -> [ Ast.Stmt ]
simportify loc args = Data.List.map (simportify' loc) args

simportify' :: Location -> (String, Maybe String) -> Ast.Stmt
simportify' loc (src, Nothing) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent src),
    Ast.stmtImportSpecific = Nothing,
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = loc
}
simportify' loc (src, Just alias) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent src),
    Ast.stmtImportSpecific = Nothing,
    Ast.stmtImportAlias = Just (Ast.ImportAlias alias),
    Ast.stmtImportLocation = loc
}

importify :: Location -> (Maybe String) -> [(String, Maybe String)] -> [ Ast.Stmt ] 
importify loc Nothing args = Data.List.map (importify' loc) args
importify loc (Just src) args = Data.List.map (importify'' loc src) args

importify' :: Location -> (String, Maybe String) -> Ast.Stmt
importify' loc (specific, Nothing) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent ""),
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific specific),
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = loc
}
importify' loc (specific, Just alias) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource  = ImportThirdParty (ImportThirdPartyContent ""),
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific specific),
    Ast.stmtImportAlias = Just (Ast.ImportAlias alias),
    Ast.stmtImportLocation = loc
}

importify'' :: Location -> String -> (String, Maybe String) -> Ast.Stmt
importify'' loc src (specific, Nothing) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent src),
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific specific),
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = loc
}
importify'' loc src (specific, Just alias) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = ImportThirdParty (ImportThirdPartyContent src),
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific specific),
    Ast.stmtImportAlias = Just (Ast.ImportAlias alias),
    Ast.stmtImportLocation = loc
}

superify' :: Ast.Var -> Token.SuperName
superify' (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v))) = Token.SuperName v
superify' (Ast.VarField (Ast.VarFieldContent _ (Token.FieldName v) _)) = Token.SuperName v
superify' (Ast.VarSubscript (Ast.VarSubscriptContent _ _ l)) = Token.SuperName (Token.Named "subscript" l)

superify :: [ Ast.Var ] -> [ Token.SuperName ]
superify = Data.List.map superify' 

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

