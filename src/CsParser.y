{
{-# OPTIONS -Werror=missing-fields #-}

module CsParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import qualified Ast
import CsLexer
import Location
import qualified CsParserActions as Actions
import qualified Common

}

-- ***********************
-- *                     *
-- * API function: parse *
-- *                     *
-- ***********************
%name parse

-- never ship a shift/reduce or reduce/reduce conflict
%expect 0

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
'AliasQualifiedName' { AlexTokenTag AlexRawToken_ALIAS_QUALIFIED_NAME _ }
'NameEquals' { AlexTokenTag AlexRawToken_NAME_EQUALS _ }
'NameColon' { AlexTokenTag AlexRawToken_NAME_COLON _ }
'NamespaceDeclaration' { AlexTokenTag AlexRawToken_NAMESPACE_DECLARATION _ }
'FileScopedNamespaceDeclaration' { AlexTokenTag AlexRawToken_FILE_SCOPED_NAMESPACE_DECLARATION _ }
'ClassDeclaration' { AlexTokenTag AlexRawToken_CLASS_DECLARATION _ }
'EmptyStatement' { AlexTokenTag AlexRawToken_EMPTY_STATEMENT _ }
'GotoDefaultStatement' { AlexTokenTag AlexRawToken_GOTO_DEFAULT_STATEMENT _ }
'GotoCaseStatement' { AlexTokenTag AlexRawToken_GOTO_CASE_STATEMENT _ }
'GotoStatement' { AlexTokenTag AlexRawToken_GOTO_STATEMENT _ }
'LabeledStatement' { AlexTokenTag AlexRawToken_LABELED_STATEMENT _ }
'ConversionOperatorDeclaration' { AlexTokenTag AlexRawToken_CONVERSION_OPERATOR_DECLARATION _ }
'OperatorDeclaration' { AlexTokenTag AlexRawToken_OPERATOR_DECLARATION _ }
'SlicePattern' { AlexTokenTag AlexRawToken_SLICE_PATTERN _ }
'ListPattern' { AlexTokenTag AlexRawToken_LIST_PATTERN _ }
'VarPattern' { AlexTokenTag AlexRawToken_VAR_PATTERN _ }
'PositionalPatternClause' { AlexTokenTag AlexRawToken_POSITIONAL_PATTERN_CLAUSE _ }
'DefaultExpression' { AlexTokenTag AlexRawToken_DEFAULT_EXPRESSION _ }
'GlobalStatement' { AlexTokenTag AlexRawToken_GLOBAL_STATEMENT _ }
'UnsignedRightShiftExpression' { AlexTokenTag AlexRawToken_UNSIGNED_RIGHT_SHIFT_EXPRESSION _ }
'RightShiftExpression' { AlexTokenTag AlexRawToken_RIGHT_SHIFT_EXPRESSION _ }
'LeftShiftExpression' { AlexTokenTag AlexRawToken_LEFT_SHIFT_EXPRESSION _ }
'ExclusiveOrExpression' { AlexTokenTag AlexRawToken_EXCLUSIVE_OR_EXPRESSION _ }
'BitwiseAndExpression' { AlexTokenTag AlexRawToken_BITWISE_AND_EXPRESSION _ }
'InterpolationAlignmentClause' { AlexTokenTag AlexRawToken_INTERPOLATION_ALIGNMENT_CLAUSE _ }
'ExtensionBlockDeclaration' { AlexTokenTag AlexRawToken_EXTENSION_BLOCK_DECLARATION _ }
'ModuloAssignmentExpression' { AlexTokenTag AlexRawToken_MODULO_ASSIGNMENT_EXPRESSION _ }
'RightShiftAssignmentExpression' { AlexTokenTag AlexRawToken_RIGHT_SHIFT_ASSIGNMENT_EXPRESSION _ }
'LeftShiftAssignmentExpression' { AlexTokenTag AlexRawToken_LEFT_SHIFT_ASSIGNMENT_EXPRESSION _ }
'ExclusiveOrAssignmentExpression' { AlexTokenTag AlexRawToken_EXCLUSIVE_OR_ASSIGNMENT_EXPRESSION _ }
'AndAssignmentExpression' { AlexTokenTag AlexRawToken_AND_ASSIGNMENT_EXPRESSION _ }
'OrAssignmentExpression' { AlexTokenTag AlexRawToken_OR_ASSIGNMENT_EXPRESSION _ }
'StackAllocArrayCreationExpression' { AlexTokenTag AlexRawToken_STACK_ALLOC_ARRAY_CREATION_EXPRESSION _ }
'PreDecrementExpression' { AlexTokenTag AlexRawToken_PRE_DECREMENT_EXPRESSION _ }
'PreIncrementExpression' { AlexTokenTag AlexRawToken_PRE_INCREMENT_EXPRESSION _ }
'DoStatement' { AlexTokenTag AlexRawToken_DO_STATEMENT _ }
'ParenthesizedPattern' { AlexTokenTag AlexRawToken_PARENTHESIZED_PATTERN _ }
'AttributeTargetSpecifier' { AlexTokenTag AlexRawToken_ATTRIBUTE_TARGET_SPECIFIER _ }
'EventFieldDeclaration' { AlexTokenTag AlexRawToken_EVENT_FIELD_DECLARATION _ }
'DivideAssignmentExpression' { AlexTokenTag AlexRawToken_DIVIDE_ASSIGNMENT_EXPRESSION _ }
'MultiplyAssignmentExpression' { AlexTokenTag AlexRawToken_MULTIPLY_ASSIGNMENT_EXPRESSION _ }
'SubtractAssignmentExpression' { AlexTokenTag AlexRawToken_SUBTRACT_ASSIGNMENT_EXPRESSION _ }
'ElementBindingExpression' { AlexTokenTag AlexRawToken_ELEMENT_BINDING_EXPRESSION _ }
'ModuloExpression' { AlexTokenTag AlexRawToken_MODULO_EXPRESSION _ }
'ComplexElementInitializerExpression' { AlexTokenTag AlexRawToken_COMPLEX_ELEMENT_INITIALIZER_EXPRESSION _ }
'ImplicitArrayCreationExpression' { AlexTokenTag AlexRawToken_IMPLICIT_ARRAY_CREATION_EXPRESSION _ }
'ArrayInitializerExpression' { AlexTokenTag AlexRawToken_ARRAY_INITIALIZER_EXPRESSION _ }
'QueryContinuation' { AlexTokenTag AlexRawToken_QUERY_CONTINUATION _ }
'GroupClause' { AlexTokenTag AlexRawToken_GROUP_CLAUSE _ }
'SelectClause' { AlexTokenTag AlexRawToken_SELECT_CLAUSE _ }
'DescendingOrdering' { AlexTokenTag AlexRawToken_DESCENDING_ORDERING _ }
'AscendingOrdering' { AlexTokenTag AlexRawToken_ASCENDING_ORDERING _ }
'OrderByClause' { AlexTokenTag AlexRawToken_ORDER_BY_CLAUSE _ }
'WhereClause' { AlexTokenTag AlexRawToken_WHERE_CLAUSE _ }
'LetClause' { AlexTokenTag AlexRawToken_LET_CLAUSE _ }
'JoinIntoClause' { AlexTokenTag AlexRawToken_JOIN_INTO_CLAUSE _ }
'JoinClause' { AlexTokenTag AlexRawToken_JOIN_CLAUSE _ }
'QueryBody' { AlexTokenTag AlexRawToken_QUERY_BODY _ }
'FromClause' { AlexTokenTag AlexRawToken_FROM_CLAUSE _ }
'QueryExpression' { AlexTokenTag AlexRawToken_QUERY_EXPRESSION _ }
'TypePattern' { AlexTokenTag AlexRawToken_TYPE_PATTERN _ }
'DiscardPattern' { AlexTokenTag AlexRawToken_DISCARD_PATTERN _ }
'SwitchExpressionArm' { AlexTokenTag AlexRawToken_SWITCH_EXPRESSION_ARM _ }
'SwitchExpression' { AlexTokenTag AlexRawToken_SWITCH_EXPRESSION _ }
'InterfaceDeclaration' { AlexTokenTag AlexRawToken_INTERFACE_DECLARATION _ }
'EnumDeclaration' { AlexTokenTag AlexRawToken_ENUM_DECLARATION _ }
'EnumMemberDeclaration' { AlexTokenTag AlexRawToken_ENUM_MEMBER_DECLARATION _ }
'RecordDeclaration' { AlexTokenTag AlexRawToken_RECORD_DECLARATION _ }
'RecordStructDeclaration' { AlexTokenTag AlexRawToken_RECORD_STRUCT_DECLARATION _ }
'StructDeclaration' { AlexTokenTag AlexRawToken_STRUCT_DECLARATION _ }
'PrimaryConstructorBaseType' { AlexTokenTag AlexRawToken_PRIMARY_CONSTRUCTOR_BASE_TYPE _ }
'AttributeArgument' { AlexTokenTag AlexRawToken_ATTRIBUTE_ARGUMENT _ }
'AttributeArgumentList' { AlexTokenTag AlexRawToken_ATTRIBUTE_ARGUMENT_LIST _ }
'Attribute' { AlexTokenTag AlexRawToken_ATTRIBUTE _ }
'AttributeList' { AlexTokenTag AlexRawToken_ATTRIBUTE_LIST _ }
'SimpleBaseType' { AlexTokenTag AlexRawToken_SIMPLE_BASE_TYPE _ }
'BaseList' { AlexTokenTag AlexRawToken_BASE_LIST _ }
'ArgumentList' { AlexTokenTag AlexRawToken_ARGUMENT_LIST _ }
'ObjectCreationExpression' { AlexTokenTag AlexRawToken_OBJECT_CREATION_EXPRESSION _ }
'ArrayCreationExpression' { AlexTokenTag AlexRawToken_ARRAY_CREATION_EXPRESSION _ }
'ImplicitObjectCreationExpression' { AlexTokenTag AlexRawToken_IMPLICIT_OBJECT_CREATION_EXPRESSION _ }
'ObjectInitializerExpression' { AlexTokenTag AlexRawToken_OBJECT_INITIALIZER_EXPRESSION _ }
'CollectionInitializerExpression' { AlexTokenTag AlexRawToken_COLLECTION_INITIALIZER_EXPRESSION _ }
'WithExpression' { AlexTokenTag AlexRawToken_WITH_EXPRESSION _ }
'WithInitializerExpression' { AlexTokenTag AlexRawToken_WITH_INITIALIZER_EXPRESSION _ }
'AnonymousObjectCreationExpression' { AlexTokenTag AlexRawToken_ANONYMOUS_OBJECT_CREATION_EXPRESSION _ }
'AnonymousObjectMemberDeclarator' { AlexTokenTag AlexRawToken_ANONYMOUS_OBJECT_MEMBER_DECLARATOR _ }
'CollectionExpression' { AlexTokenTag AlexRawToken_COLLECTION_EXPRESSION _ }
'ExpressionElement' { AlexTokenTag AlexRawToken_EXPRESSION_ELEMENT _ }
'SpreadElement' { AlexTokenTag AlexRawToken_SPREAD_ELEMENT _ }
'EqualsValueClause' { AlexTokenTag AlexRawToken_EQUALS_VALUE_CLAUSE _ }
'VariableDeclarator' { AlexTokenTag AlexRawToken_VARIABLE_DECLARATOR _ }
'TypeArgumentList' { AlexTokenTag AlexRawToken_TYPE_ARGUMENT_LIST _ }
'OmittedTypeArgument' { AlexTokenTag AlexRawToken_OMITTED_TYPE_ARGUMENT _ }
'TypeParameterList' { AlexTokenTag AlexRawToken_TYPE_PARAMETER_LIST _ }
'TypeParameter' { AlexTokenTag AlexRawToken_TYPE_PARAMETER _ }
'TypeParameterConstraintClause' { AlexTokenTag AlexRawToken_TYPE_PARAMETER_CONSTRAINT_CLAUSE _ }
'TypeConstraint' { AlexTokenTag AlexRawToken_TYPE_CONSTRAINT _ }
'ClassConstraint' { AlexTokenTag AlexRawToken_CLASS_CONSTRAINT _ }
'StructConstraint' { AlexTokenTag AlexRawToken_STRUCT_CONSTRAINT _ }
'ConstructorConstraint' { AlexTokenTag AlexRawToken_CONSTRUCTOR_CONSTRAINT _ }
'GenericName' { AlexTokenTag AlexRawToken_GENERIC_NAME _ }
'VariableDeclaration' { AlexTokenTag AlexRawToken_VARIABLE_DECLARATION _ }
'FieldDeclaration' { AlexTokenTag AlexRawToken_FIELD_DECLARATION _ }
'ElementAccessExpression' { AlexTokenTag AlexRawToken_ELEMENT_ACCESS_EXPRESSION _ }
'ImplicitElementAccess' { AlexTokenTag AlexRawToken_IMPLICIT_ELEMENT_ACCESS _ }
'BracketedArgumentList' { AlexTokenTag AlexRawToken_BRACKETED_ARGUMENT_LIST _ }
'Argument' { AlexTokenTag AlexRawToken_ARGUMENT _ }
'NullableType' { AlexTokenTag AlexRawToken_NULLABLE_TYPE _ }
'ArrayType' { AlexTokenTag AlexRawToken_ARRAY_TYPE _ }
'ArrayRankSpecifier' { AlexTokenTag AlexRawToken_ARRAY_RANK_SPECIFIER _ }
'OmittedArraySizeExpression' { AlexTokenTag AlexRawToken_OMITTED_ARRAY_SIZE_EXPRESSION _ }
'MethodDeclaration' { AlexTokenTag AlexRawToken_METHOD_DECLARATION _ }
'LocalFunctionStatement' { AlexTokenTag AlexRawToken_LOCAL_FUNCTION_STATEMENT _ }
'DelegateDeclaration' { AlexTokenTag AlexRawToken_DELEGATE_DECLARATION _ }
'ExplicitInterfaceSpecifier' { AlexTokenTag AlexRawToken_EXPLICIT_INTERFACE_SPECIFIER _ }
'ParameterList' { AlexTokenTag AlexRawToken_PARAMETER_LIST _ }
'BracketedParameterList' { AlexTokenTag AlexRawToken_BRACKETED_PARAMETER_LIST _ }
'Block' { AlexTokenTag AlexRawToken_BLOCK _ }
'IfStatement' { AlexTokenTag AlexRawToken_IF_STATEMENT _ }
'EqualsExpression' { AlexTokenTag AlexRawToken_EQUALS_EXPRESSION _ }
'IsExpression' { AlexTokenTag AlexRawToken_IS_EXPRESSION _ }
'AsExpression' { AlexTokenTag AlexRawToken_AS_EXPRESSION _ }
'GreaterThanExpression' { AlexTokenTag AlexRawToken_GREATER_THAN_EXPRESSION _ }
'GreaterThanOrEqualExpression' { AlexTokenTag AlexRawToken_GREATER_THAN_OR_EQUAL_EXPRESSION _ }
'LessThanExpression' { AlexTokenTag AlexRawToken_LESS_THAN_EXPRESSION _ }
'LessThanOrEqualExpression' { AlexTokenTag AlexRawToken_LESS_THAN_OR_EQUAL_EXPRESSION _ }
'NotEqualsExpression' { AlexTokenTag AlexRawToken_NOT_EQUALS_EXPRESSION _ }
'LogicalAndExpression' { AlexTokenTag AlexRawToken_LOGICAL_AND_EXPRESSION _ }
'LogicalOrExpression' { AlexTokenTag AlexRawToken_LOGICAL_OR_EXPRESSION _ }
'BitwiseOrExpression' { AlexTokenTag AlexRawToken_BITWISE_OR_EXPRESSION _ }
'CoalesceExpression' { AlexTokenTag AlexRawToken_COALESCE_EXPRESSION _ }
'AddExpression' { AlexTokenTag AlexRawToken_ADD_EXPRESSION _ }
'MultiplyExpression' { AlexTokenTag AlexRawToken_MULTIPLY_EXPRESSION _ }
'SubtractExpression' { AlexTokenTag AlexRawToken_SUBTRACT_EXPRESSION _ }
'DivideExpression' { AlexTokenTag AlexRawToken_DIVIDE_EXPRESSION _ }
'CoalesceAssignmentExpression' { AlexTokenTag AlexRawToken_COALESCE_ASSIGNMENT_EXPRESSION _ }
'AddAssignmentExpression' { AlexTokenTag AlexRawToken_ADD_ASSIGNMENT_EXPRESSION _ }
'ConditionalAccessExpression' { AlexTokenTag AlexRawToken_CONDITIONAL_ACCESS_EXPRESSION _ }
'ConditionalExpression' { AlexTokenTag AlexRawToken_CONDITIONAL_EXPRESSION _ }
'NullLiteralExpression' { AlexTokenTag AlexRawToken_NULL_LITERAL_EXPRESSION _ }
'DefaultLiteralExpression' { AlexTokenTag AlexRawToken_DEFAULT_LITERAL_EXPRESSION _ }
'WhileStatement' { AlexTokenTag AlexRawToken_WHILE_STATEMENT _ }
'ForStatement' { AlexTokenTag AlexRawToken_FOR_STATEMENT _ }
'TryStatement' { AlexTokenTag AlexRawToken_TRY_STATEMENT _ }
'UsingStatement' { AlexTokenTag AlexRawToken_USING_STATEMENT _ }
'LockStatement' { AlexTokenTag AlexRawToken_LOCK_STATEMENT _ }
'ForEachStatement' { AlexTokenTag AlexRawToken_FOR_EACH_STATEMENT _ }
'ForEachVariableStatement' { AlexTokenTag AlexRawToken_FOR_EACH_VARIABLE_STATEMENT _ }
'InvocationExpression' { AlexTokenTag AlexRawToken_INVOCATION_EXPRESSION _ }
'MemberBindingExpression' { AlexTokenTag AlexRawToken_MEMBER_BINDING_EXPRESSION _ }
'SimpleMemberAccessExpression' { AlexTokenTag AlexRawToken_SIMPLE_MEMBER_ACCESS_EXPRESSION _ }
'ExpressionStatement' { AlexTokenTag AlexRawToken_EXPRESSION_STATEMENT _ }
'SimpleAssignmentExpression' { AlexTokenTag AlexRawToken_SIMPLE_ASSIGNMENT_EXPRESSION _ }
'IsPatternExpression' { AlexTokenTag AlexRawToken_IS_PATTERN_EXPRESSION _ }
'DeclarationExpression' { AlexTokenTag AlexRawToken_DECLARATION_EXPRESSION _ }
'DeclarationPattern' { AlexTokenTag AlexRawToken_DECLARATION_PATTERN _ }
'NotPattern' { AlexTokenTag AlexRawToken_NOT_PATTERN _ }
'RecursivePattern' { AlexTokenTag AlexRawToken_RECURSIVE_PATTERN _ }
'OrPattern' { AlexTokenTag AlexRawToken_OR_PATTERN _ }
'AndPattern' { AlexTokenTag AlexRawToken_AND_PATTERN _ }
'ConstantPattern' { AlexTokenTag AlexRawToken_CONSTANT_PATTERN _ }
'PropertyPatternClause' { AlexTokenTag AlexRawToken_PROPERTY_PATTERN_CLAUSE _ }
'Subpattern' { AlexTokenTag AlexRawToken_SUBPATTERN _ }
'RelationalPattern' { AlexTokenTag AlexRawToken_RELATIONAL_PATTERN _ }
'SingleVariableDesignation' { AlexTokenTag AlexRawToken_SINGLE_VARIABLE_DESIGNATION _ }
'ParenthesizedVariableDesignation' { AlexTokenTag AlexRawToken_PARENTHESIZED_VARIABLE_DESIGNATION _ }
'DiscardDesignation' { AlexTokenTag AlexRawToken_DISCARD_DESIGNATION _ }
'ElseClause' { AlexTokenTag AlexRawToken_ELSE_CLAUSE _ }
'CatchClause' { AlexTokenTag AlexRawToken_CATCH_CLAUSE _ }
'CatchDeclaration' { AlexTokenTag AlexRawToken_CATCH_DECLARATION _ }
'CatchFilterClause' { AlexTokenTag AlexRawToken_CATCH_FILTER_CLAUSE _ }
'FinallyClause' { AlexTokenTag AlexRawToken_FINALLY_CLAUSE _ }
'WhenClause' { AlexTokenTag AlexRawToken_WHEN_CLAUSE _ }
'BreakStatement' { AlexTokenTag AlexRawToken_BREAK_STATEMENT _ }
'ContinueStatement' { AlexTokenTag AlexRawToken_CONTINUE_STATEMENT _ }
'ReturnStatement' { AlexTokenTag AlexRawToken_RETURN_STATEMENT _ }
'YieldBreakStatement' { AlexTokenTag AlexRawToken_YIELD_BREAK_STATEMENT _ }
'YieldReturnStatement' { AlexTokenTag AlexRawToken_YIELD_RETURN_STATEMENT _ }
'ConstructorDeclaration' { AlexTokenTag AlexRawToken_CONSTRUCTOR_DECLARATION _ }
'Parameter' { AlexTokenTag AlexRawToken_PARAMETER _ }
'BaseConstructorInitializer' { AlexTokenTag AlexRawToken_BASE_CONSTRUCTOR_INITIALIZER _ }
'ThisConstructorInitializer' { AlexTokenTag AlexRawToken_THIS_CONSTRUCTOR_INITIALIZER _ }
'ThisExpression' { AlexTokenTag AlexRawToken_THIS_EXPRESSION _ }
'BaseExpression' { AlexTokenTag AlexRawToken_BASE_EXPRESSION _ }
'PropertyDeclaration' { AlexTokenTag AlexRawToken_PROPERTY_DECLARATION _ }
'IndexerDeclaration' { AlexTokenTag AlexRawToken_INDEXER_DECLARATION _ }
'AccessorList' { AlexTokenTag AlexRawToken_ACCESSOR_LIST _ }
'GetAccessorDeclaration' { AlexTokenTag AlexRawToken_GET_ACCESSOR_DECLARATION _ }
'SetAccessorDeclaration' { AlexTokenTag AlexRawToken_SET_ACCESSOR_DECLARATION _ }
'InitAccessorDeclaration' { AlexTokenTag AlexRawToken_INIT_ACCESSOR_DECLARATION _ }
'PredefinedType' { AlexTokenTag AlexRawToken_PREDEFINED_TYPE _ }
'FalseLiteralExpression' { AlexTokenTag AlexRawToken_FALSE_LITERAL_EXPRESSION _ }
'TrueLiteralExpression' { AlexTokenTag AlexRawToken_TRUE_LITERAL_EXPRESSION _ }
'NumericLiteralExpression' { AlexTokenTag AlexRawToken_NUMERIC_LITERAL_EXPRESSION _ }
'SwitchStatement' { AlexTokenTag AlexRawToken_SWITCH_STATEMENT _ }
'SwitchSection' { AlexTokenTag AlexRawToken_SWITCH_SECTION _ }
'CaseSwitchLabel' { AlexTokenTag AlexRawToken_CASE_SWITCH_LABEL _ }
'CasePatternSwitchLabel' { AlexTokenTag AlexRawToken_CASE_PATTERN_SWITCH_LABEL _ }
'DefaultSwitchLabel' { AlexTokenTag AlexRawToken_DEFAULT_SWITCH_LABEL _ }
'StringLiteralExpression' { AlexTokenTag AlexRawToken_STRING_LITERAL_EXPRESSION _ }
'CharacterLiteralExpression' { AlexTokenTag AlexRawToken_CHARACTER_LITERAL_EXPRESSION _ }
'InterpolatedStringExpression' { AlexTokenTag AlexRawToken_INTERPOLATED_STRING_EXPRESSION _ }
'InterpolatedStringText' { AlexTokenTag AlexRawToken_INTERPOLATED_STRING_TEXT _ }
'Interpolation' { AlexTokenTag AlexRawToken_INTERPOLATION _ }
'InterpolationFormatClause' { AlexTokenTag AlexRawToken_INTERPOLATION_FORMAT_CLAUSE _ }
'ThrowStatement' { AlexTokenTag AlexRawToken_THROW_STATEMENT _ }
'ThrowExpression' { AlexTokenTag AlexRawToken_THROW_EXPRESSION _ }
'LocalDeclarationStatement' { AlexTokenTag AlexRawToken_LOCAL_DECLARATION_STATEMENT _ }
'LogicalNotExpression' { AlexTokenTag AlexRawToken_LOGICAL_NOT_EXPRESSION _ }
'PostIncrementExpression' { AlexTokenTag AlexRawToken_POST_INCREMENT_EXPRESSION _ }
'PostDecrementExpression' { AlexTokenTag AlexRawToken_POST_DECREMENT_EXPRESSION _ }
'ParenthesizedExpression' { AlexTokenTag AlexRawToken_PARENTHESIZED_EXPRESSION _ }
'CastExpression' { AlexTokenTag AlexRawToken_CAST_EXPRESSION _ }
'SimpleLambdaExpression' { AlexTokenTag AlexRawToken_SIMPLE_LAMBDA_EXPRESSION _ }
'ParenthesizedLambdaExpression' { AlexTokenTag AlexRawToken_PARENTHESIZED_LAMBDA_EXPRESSION _ }
'ArrowExpressionClause' { AlexTokenTag AlexRawToken_ARROW_EXPRESSION_CLAUSE _ }
'AwaitExpression' { AlexTokenTag AlexRawToken_AWAIT_EXPRESSION _ }
'TypeOfExpression' { AlexTokenTag AlexRawToken_TYPE_OF_EXPRESSION _ }
'TupleExpression' { AlexTokenTag AlexRawToken_TUPLE_EXPRESSION _ }
'TupleType' { AlexTokenTag AlexRawToken_TUPLE_TYPE _ }
'TupleElement' { AlexTokenTag AlexRawToken_TUPLE_ELEMENT _ }
'SuppressNullableWarningExpression' { AlexTokenTag AlexRawToken_SUPPRESS_NULLABLE_WARNING_EXPRESSION _ }
'RangeExpression' { AlexTokenTag AlexRawToken_RANGE_EXPRESSION _ }
'IndexExpression' { AlexTokenTag AlexRawToken_INDEX_EXPRESSION _ }
'UnaryMinusExpression' { AlexTokenTag AlexRawToken_UNARY_MINUS_EXPRESSION _ }

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

-- ********************
-- *                  *
-- * prefixlistof(a)  *
-- *                  *
-- ********************
-- `a , a , .. ,` in front of a sibling of another kind. Left recursive on
-- purpose : the parser only ever shifts on the next '{', so a prefix of
-- attribute lists before a type never conflicts ( `commalistof(a) ',' b`
-- would : after `a` it can't tell a ',' of the list from the one before b )
prefixlistof(a): a ',' { [$1] } | prefixlistof(a) a ',' { $1 ++ [$2] }

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
-- direct dhscanner subtree creation: Ast.Root
program:
'{'
    'kind' ':' 'CompilationUnit' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(rootMember)
'}'
{
    Actions.root $12 $16
}

-- a native `value` : a quoted string, or null
tokenID:
ID { tokIDValue $1 } |
'null' { "null" }

identifier:
'{'
    'kind' ':' 'IdentifierName' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.named $12 (tokIDValue $8)
}

-- direct dhscanner subtree creation: Ast.VarSimple
varSimple:
identifier { Actions.varSimple $1 } |
designation { $1 }

-- direct dhscanner subtree creation: Ast.VarSimple
designation:
'{'
    'kind' ':' 'SingleVariableDesignation' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.simpleVar $12 (tokIDValue $8)
}

-- extract first variable from a deconstruction like (x, _)
parenthesizedDesignation:
'{'
    'kind' ':' 'ParenthesizedVariableDesignation' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' designation ',' discardDesignation ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'ParenthesizedVariableDesignation' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' designation ',' designation ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'ParenthesizedVariableDesignation' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' designation ',' anyDesignation ',' commalistof(anyDesignation) ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'ParenthesizedVariableDesignation' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' discardDesignation ',' designation ']'
'}'
{
    $19
}
|
'{'
    'kind' ':' 'ParenthesizedVariableDesignation' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' discardDesignation ',' designation ',' commalistof(anyDesignation) ']'
'}'
{
    $19
}

discardDesignation:
'{'
    'kind' ':' 'DiscardDesignation' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    ()
}

-- helper: consume either a named or discard designation (value not used)
anyDesignation:
'{'
    'kind' ':' 'SingleVariableDesignation' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    ()
}
|
'{'
    'kind' ':' 'DiscardDesignation' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    ()
}

-- `var (x, _)` on the LHS of a deconstruction assignment: return x as Var
declarationDeconstructVar:
'{'
    'kind' ':' 'DeclarationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ',' parenthesizedDesignation ']'
'}'
{
    $19
}

-- direct dhscanner subtree creation: Ast.VarField
varQualifiedName:
'{'
    'kind' ':' 'QualifiedName' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' identifier ']'
'}'
{
    Actions.qualifiedName $12 $17 $19
}
|
'{'
    'kind' ':' 'QualifiedName' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' genericName ']'
'}'
{
    Actions.qualifiedGeneric $12 $17 $19
}

-- direct dhscanner subtree creation: Ast.VarField
varAliasQualifiedName:
'{'
    'kind' ':' 'AliasQualifiedName' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' identifier ']'
'}'
{
    Actions.qualifiedName $12 $17 $19
}

-- direct dhscanner subtree creation: Ast.VarField
varMemberAccess:
'{'
    'kind' ':' 'SimpleMemberAccessExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' identifier ']'
'}'
{
    Actions.fieldVar $12 $17 $19
}

-- direct dhscanner subtree creation: Ast.VarField
varMemberAccessGeneric:
'{'
    'kind' ':' 'SimpleMemberAccessExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' genericName ']'
'}'
{
    Actions.fieldVarFromVar $12 $17 $19
}

-- direct dhscanner subtree creation: Ast.VarSubscript
varSubscript:
'{'
    'kind' ':' 'ElementAccessExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' bracketedArgs ']'
'}'
{
    Actions.subscriptVar $12 $17 $19
}

bracketedArgs:
'{'
    'kind' ':' 'BracketedArgumentList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(expArg)
'}'
{
    $16
}

var:
varSimple { $1 } |
varQualifiedName { $1 } |
varAliasQualifiedName { $1 } |
varMemberAccess { $1 } |
varMemberAccessGeneric { $1 } |
genericName { $1 } |
varSubscript { $1 }

-- direct dhscanner subtree creation: Ast.StmtImport
stmtUsing:
'{'
    'kind' ':' 'UsingDirective' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ']'
'}'
{
    Actions.stmtUsing $12 $17
}

-- direct dhscanner subtree creation: Ast.StmtImport
stmtUsingAlias:
'{'
    'kind' ':' 'UsingDirective' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nameEquals ',' var ']'
'}'
{
    Actions.stmtUsingAlias $12 $17 $19
}

nameEquals:
'{'
    'kind' ':' 'NameEquals' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    $17
}

nameColon:
'{'
    'kind' ':' 'NameColon' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    $17
}

-- direct dhscanner subtree creation: Ast.StmtClass
stmtClass:
'{'
    'kind' ':' classKind ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(classChild)
'}'
{
    Actions.stmtClass $12 (tokIDValue $8) $16
}

-- records and interfaces are classes too
classKind:
'ClassDeclaration' { Nothing } |
'RecordDeclaration' { Nothing } |
'RecordStructDeclaration' { Nothing } |
'StructDeclaration' { Nothing } |
'InterfaceDeclaration' { Nothing }

-- one child of a class, in any order : every alternative starts with its
-- own kind, so a new one ( TypeParameterList, .. ) never conflicts
classChild:
'{'
    'kind' ':' 'ExtensionBlockDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' typeParameterList ',' params ',' commalistof(stmt) ']'
'}'
{
    Actions.classExtension $12 $19 $21
} |
'{'
    'kind' ':' 'ExtensionBlockDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' params ',' commalistof(stmt) ']'
'}'
{
    Actions.classExtension $12 $17 $19
} |
attributeList { Actions.classAttributes $1 } |
params { Actions.classParams $1 } |
baseList { Actions.classSupers $1 } |
typeParameterList { Actions.classTypeParams $1 } |
typeParameterConstraintClause { Actions.classTypeParams $1 } |
stmt { Actions.classMember $1 }

typeParameterList:
'{'
    'kind' ':' 'TypeParameterList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(typeParameter)
'}'
{
    ()
}

typeParameter:
'{'
    'kind' ':' 'TypeParameter' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(attributeList)
'}'
{
    ()
}

typeParameterConstraintClause:
'{'
    'kind' ':' 'TypeParameterConstraintClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(typeParameterConstraintChild)
'}'
{
    ()
}

typeParameterConstraintChild:
identifier { () } |
typeConstraint { () } |
'{'
    'kind' ':' 'ClassConstraint' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    ()
}
|
'{'
    'kind' ':' 'StructConstraint' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    ()
}
|
'{'
    'kind' ':' 'ConstructorConstraint' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    ()
}

typeConstraint:
'{'
    'kind' ':' 'TypeConstraint' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(nominalType)
'}'
{
    ()
}

-- `[A, B(x)]` : its attributes
attributeList:
'{'
    'kind' ':' 'AttributeList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(attribute)
'}'
{
    $16
}
|
'{'
    'kind' ':' 'AttributeList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' attributeTargetSpecifier ',' commalistof(attribute) ']'
'}'
{
    $19
}

-- instrumented as dhscanner Ast.ExpCall ( callee = the attribute name )
attribute:
'{'
    'kind' ':' 'Attribute' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ']'
'}'
{
    Actions.attributeNoArgs $12 $17
} |
'{'
    'kind' ':' 'Attribute' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' attributeArgs ']'
'}'
{
    Actions.attribute $12 $17 $19
}

attributeArgs:
'{'
    'kind' ':' 'AttributeArgumentList' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(attributeArg)
'}'
{
    $16
}

-- `x` or `Name = x` ( an Ast.ExpKwArg )
attributeArg:
'{'
    'kind' ':' 'AttributeArgument' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
} |
'{'
    'kind' ':' 'AttributeArgument' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nameEquals ',' exp ']'
'}'
{
    Actions.kwArg $12 $17 $19
} |
'{'
    'kind' ':' 'AttributeArgument' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nameColon ',' exp ']'
'}'
{
    Actions.kwArg $12 $17 $19
}

-- a type, possibly after attribute lists ( a method's return type, a parameter's type )
annotatedType:
nominalType { Actions.unannotated $1 } |
prefixlistof(attributeList) nominalType { Actions.annotated $1 $2 }

baseList:
'{'
    'kind' ':' 'BaseList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(baseType)
'}'
{
    $16
}

baseType:
'{'
    'kind' ':' 'SimpleBaseType' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ']'
'}'
{
    $17
} |
'{'
    'kind' ':' 'PrimaryConstructorBaseType' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' args ']'
'}'
{
    Actions.primaryConstructorBase $17 $19
}

omittedTypeArg:
'{'
    'kind' ':' 'OmittedTypeArgument' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.simpleVar $12 "?"
}

typeArg:
nominalType { $1 } |
omittedTypeArg { $1 }

typeArgumentList:
'{'
    'kind' ':' 'TypeArgumentList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(typeArg)
'}'
{
    $16
}

-- direct dhscanner subtree creation: Ast.VarSimple
genericName:
'{'
    'kind' ':' 'GenericName' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' typeArgumentList ']'
'}'
{
    Actions.genericName $12 (tokIDValue $8) $17
}

nullableType:
'{'
    'kind' ':' 'NullableType' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ']'
'}'
{
    $17
}

omittedArraySizeExpression:
'{'
    'kind' ':' 'OmittedArraySizeExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    ()
}

arraySizeExpression:
omittedArraySizeExpression { () } |
exp { () }

arrayRankSpecifier:
'{'
    'kind' ':' 'ArrayRankSpecifier' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(arraySizeExpression)
'}'
{
    ()
}

arrayType:
'{'
    'kind' ':' 'ArrayType' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' commalistof(arrayRankSpecifier) ']'
'}'
{
    $17
}

tupleElement:
'{'
    'kind' ':' 'TupleElement' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' '[' nominalType ']'
'}'
{
    $17
}

-- instrumented as a simple nominal type "tuple" (elements ignored)
tupleType:
'{'
    'kind' ':' 'TupleType' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(tupleElement)
'}'
{
    Actions.tupleType $12 $16
}

-- direct dhscanner subtree creation: Ast.VarSimple
predefinedType:
'{'
    'kind' ':' 'PredefinedType' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.predefinedType $12 (tokIDValue $8)
}

nominalType:
var { $1 } |
nullableType { $1 } |
predefinedType { $1 } |
arrayType { $1 } |
tupleType { $1 }

args:
'{'
    'kind' ':' 'ArgumentList' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(expArg)
'}'
{
    $16
}

-- instrumented as dhscanner Ast.ExpCall ( callee = the constructed type )
expNew:
'{'
    'kind' ':' 'ObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' args ']'
'}'
{
    Actions.expNew $12 $17 $19
}
|
'{'
    'kind' ':' 'ObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ']'
'}'
{
    Actions.expNewNoArgs $12 $17
}
|
'{'
    'kind' ':' 'ObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' objectInitializer ']'
'}'
{
    Actions.expNewInit $12 $17 $19
}
|
'{'
    'kind' ':' 'ObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' args ',' objectInitializer ']'
'}'
{
    Actions.expNewArgsInit $12 $17 $19 $21
}

|
'{'
    'kind' ':' 'ObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' collectionInitializer ']'
'}'
{
    Actions.expNewInit $12 $17 $19
}
|
'{'
    'kind' ':' 'ObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' args ',' collectionInitializer ']'
'}'
{
    Actions.expNewArgsInit $12 $17 $19 $21
}

-- instrumented as dhscanner Ast.ExpCall ( tag [new_implicit] )
exp_implicit_new:
'{'
    'kind' ':' 'ImplicitObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' args ']'
'}'
{
    Actions.instrumentationCall "new_implicit" $12 $17
}
|
'{'
    'kind' ':' 'ImplicitObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' args ',' objectInitializer ']'
'}'
{
    Actions.implicitNewArgsInit $12 $17 $19
}
|
'{'
    'kind' ':' 'ImplicitObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' args ',' collectionInitializer ']'
'}'
{
    Actions.implicitNewArgsInit $12 $17 $19
}

-- treat array creation like object creation: call on the array type with no args
exp_array_creation:
'{'
    'kind' ':' 'ArrayCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' arrayType ']'
'}'
{
    Actions.expNewNoArgs $12 $17
}
|
'{'
    'kind' ':' 'ArrayCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' arrayType ',' arrayInitializer ']'
'}'
{
    Actions.expNew $12 $17 [$19]
}

-- instrumented as dhscanner Ast.ExpCall ( tag [dictify], one Ast.ExpKwArg per member )
objectInitializer:
'{'
    'kind' ':' 'ObjectInitializerExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(initAssignment)
'}'
{
    Actions.dictify $12 $16
}

-- instrumented as dhscanner Ast.ExpCall ( tag [collection] )
collectionInitializer:
'{'
    'kind' ':' 'CollectionInitializerExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(exp)
'}'
{
    Actions.collection $12 $16
}

withInitializer:
'{'
    'kind' ':' 'WithInitializerExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(initAssignment)
'}'
{
    Actions.dictify $12 $16
}

implicitElementAccess:
'{'
    'kind' ':' 'ImplicitElementAccess' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' bracketedArgs ']'
'}'
{
    $17
}

initAssignment:
'{'
    'kind' ':' 'SimpleAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ',' exp ']'
'}'
{
    Actions.kwArg $12 $17 $19
}
|
'{'
    'kind' ':' 'SimpleAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' implicitElementAccess ',' exp ']'
'}'
{
    Actions.instrumentationCall "indexer" $12 ($17 ++ [$19])
}

anonymousObjectMember:
'{'
    'kind' ':' 'AnonymousObjectMemberDeclarator' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nameEquals ',' exp ']'
'}'
{
    Actions.kwArg $12 $17 $19
}
|
'{'
    'kind' ':' 'AnonymousObjectMemberDeclarator' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    Actions.kwArgSelf $12 $17
}
|
'{'
    'kind' ':' 'AnonymousObjectMemberDeclarator' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' varMemberAccess ']'
'}'
{
    Actions.kwArgSelfFromVar $12 $17
}

-- instrumented as dhscanner Ast.ExpCall ( tag [dictify], one Ast.ExpKwArg per member )
exp_anonymous_object:
'{'
    'kind' ':' 'AnonymousObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(anonymousObjectMember)
'}'
{
    Actions.dictify $12 $16
}

-- direct dhscanner subtree creation: Ast.ExpVar
expVar: var { Actions.expVar $1 }

expArg:
'{'
    'kind' ':' 'Argument' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'Argument' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nameColon ',' exp ']'
'}'
{
    Actions.kwArg $12 $17 $19
}

operator:
'EqualsExpression'    { Actions.opEq } |
'IsExpression' { Actions.opEq } |
'GreaterThanExpression' { Actions.opEq } |
'GreaterThanOrEqualExpression' { Actions.opEq } |
'LessThanExpression' { Actions.opEq } |
'LessThanOrEqualExpression' { Actions.opEq } |
'IsPatternExpression' { Actions.opEq } |
'LogicalAndExpression' { Actions.opPlus } |
'LogicalOrExpression' { Actions.opPlus } |
'BitwiseOrExpression' { Actions.opPlus } |
'CoalesceExpression' { Actions.opPlus } |
'AddExpression' { Actions.opPlus } |
'MultiplyExpression' { Actions.opTimes } |
'SubtractExpression' { Actions.opMinus } |
'DivideExpression' { Actions.opDivide } |
'ModuloExpression' { Actions.opPercent } |
'BitwiseAndExpression' { Actions.opPlus } |
'ExclusiveOrExpression' { Actions.opPlus } |
'LeftShiftExpression' { Actions.opTimes } |
'RightShiftExpression' { Actions.opDivide } |
'UnsignedRightShiftExpression' { Actions.opDivide } |
'NotEqualsExpression' { Actions.opNeq }

-- direct dhscanner subtree creation: Ast.ExpBinop
expBinop:
'{'
    'kind' ':' operator ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' exp ']'
'}'
{
    Actions.expBinop $12 $17 $19 $4
}

-- direct dhscanner subtree creation: Ast.ExpNull
expNull:
'{'
    'kind' ':' 'NullLiteralExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.expNull $12
}

-- direct dhscanner subtree creation: Ast.ExpNull
expDefault:
'{'
    'kind' ':' 'DefaultLiteralExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.expNull $12
}
|
'{'
    'kind' ':' 'DefaultExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ']'
'}'
{
    Actions.expNull $12
}

-- direct dhscanner subtree creation: Ast.ExpCall
expCall:
'{'
    'kind' ':' 'InvocationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' args ']'
'}'
{
    Actions.expCall $12 $17 $19
}

-- `x is T y` : the designated variable
exp_casting:
'{'
    'kind' ':' 'DeclarationPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' designation ']'
'}'
{
    Actions.expVar $19
}
|
'{'
    'kind' ':' 'DeclarationPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' discardDesignation ']'
'}'
{
    Actions.expVar $17
}

-- `out var x` : transparent, returns the designated variable
exp_declaration:
'{'
    'kind' ':' 'DeclarationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' designation ']'
'}'
{
    Actions.expVar $19
}

-- pattern negation wrapper: transparent
exp_not_pattern:
'{'
    'kind' ':' 'NotPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp_pattern ']'
'}'
{
    $17
}

propertyPatternClause:
'{'
    'kind' ':' 'PropertyPatternClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(subpattern)
'}'
{
    $16
}
|
'{'
    'kind' ':' 'PropertyPatternClause' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    []
}

subpattern:
'{'
    'kind' ':' 'Subpattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nameColon ',' exp_pattern ']'
'}'
{
    Actions.kwArg $12 $17 $19
}
|
'{'
    'kind' ':' 'Subpattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp_pattern ']'
'}'
{
    $17
}

relationalPattern:
'{'
    'kind' ':' 'RelationalPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

constantPattern:
'{'
    'kind' ':' 'ConstantPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

-- instrumented as dhscanner Ast.ExpCall ( tag [pattern] )
exp_recursive_pattern:
'{'
    'kind' ':' 'RecursivePattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' patternClause ']'
'}'
{
    Actions.instrumentationCall "pattern" $12 $17
}
|
'{'
    'kind' ':' 'RecursivePattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' patternClause ']'
'}'
{
    Actions.instrumentationCall "pattern" $12 (Actions.expVar $17 : $19)
}
|
'{'
    'kind' ':' 'RecursivePattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' patternClause ',' designation ']'
'}'
{
    Actions.instrumentationCall "pattern" $12 ($17 ++ [ Actions.expVar $19 ])
}
|
'{'
    'kind' ':' 'RecursivePattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' patternClause ',' designation ']'
'}'
{
    Actions.instrumentationCall "pattern" $12 (Actions.expVar $17 : ($19 ++ [ Actions.expVar $21 ]))
}

exp_or_pattern:
'{'
    'kind' ':' 'OrPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp_pattern ',' exp_pattern ']'
'}'
{
    Actions.instrumentationCall "pattern" $12 [$17, $19]
}

exp_and_pattern:
'{'
    'kind' ':' 'AndPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp_pattern ',' exp_pattern ']'
'}'
{
    Actions.instrumentationCall "pattern" $12 [$17, $19]
}

exp_pattern:
varPattern { $1 } |
listPattern { $1 } |
slicePattern { $1 } |
parenthesizedPattern { $1 } |
discardPattern { $1 } |
typePattern { $1 } |
exp_casting { $1 } |
relationalPattern { $1 } |
constantPattern { $1 } |
exp_recursive_pattern { $1 } |
exp_not_pattern { $1 } |
exp_and_pattern { $1 } |
exp_or_pattern { $1 }

-- direct dhscanner subtree creation: Ast.ExpVar
expThis:
'{'
    'kind' ':' 'ThisExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.expThis $12
}

-- direct dhscanner subtree creation: Ast.ExpVar
expBase:
'{'
    'kind' ':' 'BaseExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.expBase $12
}

-- direct dhscanner subtree creation: Ast.ExpBool
expTrue:
'{'
    'kind' ':' 'TrueLiteralExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.expTrue $12
}

-- direct dhscanner subtree creation: Ast.ExpBool
expFalse:
'{'
    'kind' ':' 'FalseLiteralExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.expFalse $12
}

-- direct dhscanner subtree creation: Ast.ExpInt
expInt:
'{'
    'kind' ':' 'NumericLiteralExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.expInt $12 (tokIDValue $8)
}

-- direct dhscanner subtree creation: Ast.ExpStr
expStr:
'{'
    'kind' ':' 'StringLiteralExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.expStr $12 (tokIDValue $8)
}

-- direct dhscanner subtree creation: Ast.ExpStr
expChar:
'{'
    'kind' ':' 'CharacterLiteralExpression' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.expStr $12 (tokIDValue $8)
}

-- parts of an interpolated string: plain text or an interpolated expression
interpolatedText:
'{'
    'kind' ':' 'InterpolatedStringText' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.expStr $12 (tokIDValue $8)
}

interpolationFormatClause:
'{'
    'kind' ':' 'InterpolationFormatClause' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    ()
}

interpolation:
'{'
    'kind' ':' 'Interpolation' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'Interpolation' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' interpolationFormatClause ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'Interpolation' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' interpolationAlignmentClause ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'Interpolation' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' interpolationAlignmentClause ',' interpolationFormatClause ']'
'}'
{
    $17
}

interpolatedContent:
interpolatedText { $1 } |
interpolation { $1 }

-- instrumented as dhscanner Ast.ExpCall ( tag [fstring] )
exp_interpolated_string:
'{'
    'kind' ':' 'InterpolatedStringExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(interpolatedContent)
'}'
{
    Actions.instrumentationCall "fstring" $12 $16
}

collectionElement:
'{'
    'kind' ':' 'ExpressionElement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'SpreadElement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

-- instrumented as dhscanner Ast.ExpCall ( tag [collection] )
exp_collection:
'{'
    'kind' ':' 'CollectionExpression' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(collectionElement)
'}'
{
    Actions.collection $12 $16
}

-- instrumented as dhscanner Ast.ExpCall ( tag [tuple] )
exp_tuple_expression:
'{'
    'kind' ':' 'TupleExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(expArg)
'}'
{
    Actions.instrumentationCall "tuple" $12 $16
}

-- `!e` : transparent
-- instrumented as dhscanner Ast.ExpCall ( tag [with] )
exp_with:
'{'
    'kind' ':' 'WithExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' withInitializer ']'
'}'
{
    Actions.instrumentationCall "with" $12 [$17, $19]
}

-- instrumented as dhscanner Ast.ExpCall ( tag [index] )
exp_index_expression:
'{'
    'kind' ':' 'IndexExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.instrumentationCall "index" $12 [$17]
}

-- instrumented as dhscanner Ast.ExpCall ( tag [range] )
exp_range_expression:
'{'
    'kind' ':' 'RangeExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' exp ']'
'}'
{
    Actions.instrumentationCall "range" $12 [$17, $19]
}
|
'{'
    'kind' ':' 'RangeExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.instrumentationCall "range" $12 [$17]
}

-- instrumented as dhscanner Ast.ExpCall ( tag [coalesce_assign] )
exp_coalesce_assign:
'{'
    'kind' ':' 'CoalesceAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.coalesceAssign $12 $17 $19
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

exp_unary_minus:
'{'
    'kind' ':' 'UnaryMinusExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.unaryMinus $12 $17
}

-- `i++` : transparent
exp_post_increment:
'{'
    'kind' ':' 'PostIncrementExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

-- `i--` : transparent
exp_post_decrement:
'{'
    'kind' ':' 'PostDecrementExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

-- `( e )` : transparent
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
|
'{'
    'kind' ':' 'ParenthesizedExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' expAssign ']'
'}'
{
    $17
}

-- `await e` : transparent
exp_await:
'{'
    'kind' ':' 'AwaitExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

-- `e!` : transparent
exp_suppress_nullable_warning:
'{'
    'kind' ':' 'SuppressNullableWarningExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

-- `(T) e` : transparent
exp_cast:
'{'
    'kind' ':' 'CastExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' exp ']'
'}'
{
    $19
}

-- `e as T` : transparent
exp_as:
'{'
    'kind' ':' 'AsExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' nominalType ']'
'}'
{
    $17
}

-- MemberBindingExpression: callee after ?. or in conditional access
exp_member_binding:
'{'
    'kind' ':' 'MemberBindingExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    Actions.expvarify $17
}
|
'{'
    'kind' ':' 'MemberBindingExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' genericName ']'
'}'
{
    Actions.expVar $17
}

-- `a?.b` : transparent (drop null-propagation), return the WhenNotNull branch
exp_conditional_access:
'{'
    'kind' ':' 'ConditionalAccessExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' exp ']'
'}'
{
    Actions.conditionalAccess $17 $19
}

-- instrumented as dhscanner Ast.ExpCall ( tag [ternary] )
exp_conditional:
'{'
    'kind' ':' 'ConditionalExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' exp ',' exp ']'
'}'
{
    Actions.instrumentationCall "ternary" $12 [$17, $19, $21]
}

-- instrumented as dhscanner Ast.ExpCall ( tag [typeof] )
exp_typeof:
'{'
    'kind' ':' 'TypeOfExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ']'
'}'
{
    Actions.typeof $12 $17
}

-- direct dhscanner subtree creation: Ast.ExpLambda
exp_parenthesized_lambda:
'{'
    'kind' ':' 'ParenthesizedLambdaExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' params ',' stmt ']'
'}'
{
    Actions.parenthesizedLambda $12 $17 $19
}

exp_simple_lambda:
'{'
    'kind' ':' 'SimpleLambdaExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' parameter ',' stmt ']'
'}'
{
    Actions.simpleLambda $12 $17 $19
}

-- `_` : instrumented as dhscanner Ast.ExpCall ( tag [discard] )
discardPattern:
'{'
    'kind' ':' 'DiscardPattern' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.discardPattern $12
}

-- `not string` : the type
typePattern:
'{'
    'kind' ':' 'TypePattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ']'
'}'
{
    Actions.expVar $17
}

-- `x switch { p => e, .. }` : instrumented as dhscanner Ast.ExpCall ( tag [switch] )
expSwitch:
'{'
    'kind' ':' 'SwitchExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' commalistof(switchArm) ']'
'}'
{
    Actions.expSwitch $12 $17 $19
}

-- `p when c => e` : instrumented as dhscanner Ast.ExpCall ( tag [arm] )
switchArm:
'{'
    'kind' ':' 'SwitchExpressionArm' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp_pattern ',' exp ']'
'}'
{
    Actions.switchArm $12 $17 Nothing $19
}
|
'{'
    'kind' ':' 'SwitchExpressionArm' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp_pattern ',' whenClause ',' exp ']'
'}'
{
    Actions.switchArm $12 $17 (Just $19) $21
}

-- `from x in xs .. select e` : instrumented as dhscanner Ast.ExpCall ( tag [query] ),
-- one instrumented call per clause, range variables kept next to their source
expQuery:
'{'
    'kind' ':' 'QueryExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' fromClause ',' queryBody ']'
'}'
{
    Actions.expQuery $12 $17 $19
}

queryBody:
'{'
    'kind' ':' 'QueryBody' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(queryClause)
'}'
{
    $16
}

queryClause:
fromClause { $1 } |
'{'
    'kind' ':' 'JoinClause' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' exp ',' exp ',' exp ']'
'}'
{
    Actions.queryJoin $12 (tokIDValue $8) $17 $19 $21 Nothing
}
|
'{'
    'kind' ':' 'JoinClause' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' exp ',' exp ',' exp ',' joinInto ']'
'}'
{
    Actions.queryJoin $12 (tokIDValue $8) $17 $19 $21 (Just $23)
}
|
'{'
    'kind' ':' 'LetClause' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.queryBinding "let" $12 (tokIDValue $8) $17
}
|
'{'
    'kind' ':' 'WhereClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.queryClause "where" $12 [$17]
}
|
'{'
    'kind' ':' 'OrderByClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(ordering)
'}'
{
    Actions.queryClause "orderby" $12 $16
}
|
'{'
    'kind' ':' 'SelectClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.queryClause "select" $12 [$17]
}
|
'{'
    'kind' ':' 'GroupClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' exp ']'
'}'
{
    Actions.queryClause "group" $12 [$17, $19]
}
|
'{'
    'kind' ':' 'QueryContinuation' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' queryBody ']'
'}'
{
    Actions.queryContinuation $12 (tokIDValue $8) $17
}

fromClause:
'{'
    'kind' ':' 'FromClause' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.queryBinding "from" $12 (tokIDValue $8) $17
}

joinInto:
'{'
    'kind' ':' 'JoinIntoClause' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.named $12 (tokIDValue $8)
}

ordering:
'{'
    'kind' ':' 'AscendingOrdering' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'DescendingOrdering' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

-- `{ a, b }` : instrumented as dhscanner Ast.ExpCall ( tag [collection] )
arrayInitializer:
'{'
    'kind' ':' 'ArrayInitializerExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(exp)
'}'
{
    Actions.collection $12 $16
}

-- `new[] { a, b }`
expImplicitArray:
'{'
    'kind' ':' 'ImplicitArrayCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' arrayInitializer ']'
'}'
{
    $17
}

-- `{ k, v }` in a collection initializer : instrumented as dhscanner Ast.ExpCall ( tag [element] )
complexElement:
'{'
    'kind' ':' 'ComplexElementInitializerExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(exp)
'}'
{
    Actions.complexElement $12 $16
}

-- `?[i]` of `o?[i]` : rebased onto o by Actions.conditionalAccess
elementBinding:
'{'
    'kind' ':' 'ElementBindingExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' bracketedArgs ']'
'}'
{
    Actions.elementBinding $12 $17
}

attributeTargetSpecifier:
'{'
    'kind' ':' 'AttributeTargetSpecifier' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    ()
}

-- a file's top level : statements, and [assembly: ..] attribute lists ( kept, instrumented )
rootMember:
stmt { $1 } |
'{'
    'kind' ':' 'GlobalStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmt ']'
'}'
{
    $17
} |
attributeList { Actions.rootAttributes $1 }

-- `( p )` pattern : transparent
parenthesizedPattern:
'{'
    'kind' ':' 'ParenthesizedPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp_pattern ']'
'}'
{
    $17
}

-- `x = y` used as an expression : `( x = y )`, `a = b = c`
expAssign:
'{'
    'kind' ':' 'SimpleAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.expAssign $12 $17 $19
}
|
'{'
    'kind' ':' 'SimpleAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' expAssign ']'
'}'
{
    Actions.expAssign $12 $17 $19
}

-- `++i` / `--i` : transparent
exp_pre_increment:
'{'
    'kind' ':' 'PreIncrementExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'PreDecrementExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

-- `stackalloc T[n]` : like array creation
exp_stackalloc:
'{'
    'kind' ':' 'StackAllocArrayCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' arrayType ']'
'}'
{
    Actions.expNewNoArgs $12 $17
}
|
'{'
    'kind' ':' 'StackAllocArrayCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' arrayType ',' arrayInitializer ']'
'}'
{
    Actions.expNew $12 $17 [$19]
}

-- `do s while (c)` : instrumented as the while loop over the same body
stmtDo:
'{'
    'kind' ':' 'DoStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmt ',' exp ']'
'}'
{
    Actions.stmtDo $12 $17 $19
}

-- instrumented lowering: x |= y  ==>  x = x + y ( bitwise ops are + like `a | b` ), x %= y  ==>  x = x % y
stmtBitwiseAssign:
'{'
    'kind' ':' 'OrAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.bitwiseAssign $12 $17 $19
}
|
'{'
    'kind' ':' 'AndAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.bitwiseAssign $12 $17 $19
}
|
'{'
    'kind' ':' 'ExclusiveOrAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.bitwiseAssign $12 $17 $19
}
|
'{'
    'kind' ':' 'LeftShiftAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.bitwiseAssign $12 $17 $19
}
|
'{'
    'kind' ':' 'RightShiftAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.bitwiseAssign $12 $17 $19
}
|
'{'
    'kind' ':' 'ModuloAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.moduloAssign $12 $17 $19
}

interpolationAlignmentClause:
'{'
    'kind' ':' 'InterpolationAlignmentClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    ()
}

patternClause:
propertyPatternClause { $1 } |
positionalPatternClause { $1 }

positionalPatternClause:
'{'
    'kind' ':' 'PositionalPatternClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(subpattern)
'}'
{
    $16
}

-- `var x` : the variable
varPattern:
'{'
    'kind' ':' 'VarPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' designation ']'
'}'
{
    Actions.expVar $17
}

-- `[1, .., var last]` : instrumented as dhscanner Ast.ExpCall ( tag [list_pattern] )
listPattern:
'{'
    'kind' ':' 'ListPattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(exp_pattern)
'}'
{
    Actions.listPattern $12 $16
}

-- `..` / `.. var rest` : instrumented as dhscanner Ast.ExpCall ( tag [slice] )
slicePattern:
'{'
    'kind' ':' 'SlicePattern' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.slicePattern $12
}
|
'{'
    'kind' ':' 'SlicePattern' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp_pattern ']'
'}'
{
    $17
}

-- `static C operator +(C a, C b)` / `implicit operator string(C c)` : a method
stmtOperator:
'{'
    'kind' ':' 'OperatorDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' params ',' methodBody ']'
'}'
{
    Actions.stmtOperator $12 $19 $17 $21
}
|
'{'
    'kind' ':' 'ConversionOperatorDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' params ',' methodBody ']'
'}'
{
    Actions.stmtOperator $12 $19 $17 $21
}

-- `label: s` : the statement ( the label is dropped )
stmtLabeled:
'{'
    'kind' ':' 'LabeledStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' stmt ']'
'}'
{
    $17
}

-- `goto l` / `goto case x` / `goto default` : instrumented as dhscanner Ast.ExpCall ( tag [goto] )
stmtGoto:
'{'
    'kind' ':' 'GotoStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.stmtGoto $12 $17
}
|
'{'
    'kind' ':' 'GotoCaseStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.stmtGoto $12 $17
}
|
'{'
    'kind' ':' 'GotoDefaultStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.stmtGotoDefault $12
}

-- `;`
stmtEmpty:
'{'
    'kind' ':' 'EmptyStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.stmtEmpty $12
}

exp:
varPattern { $1 } |
listPattern { $1 } |
slicePattern { $1 } |
exp_pre_increment { $1 } |
exp_stackalloc { $1 } |
expImplicitArray { $1 } |
arrayInitializer { $1 } |
collectionInitializer { $1 } |
complexElement { $1 } |
elementBinding { $1 } |
expSwitch { $1 } |
expQuery { $1 } |
expNew { $1 } |
exp_implicit_new { $1 } |
exp_array_creation { $1 } |
expArg { $1 } |
expVar { $1 } |
predefinedType { Actions.expVar $1 } |
exp_not { $1 } |
exp_unary_minus { $1 } |
expInt { $1 } |
expStr { $1 } |
expChar { $1 } |
expThis { $1 } |
expBase { $1 } |
expCall { $1 } |
expNull { $1 } |
expDefault { $1 } |
expTrue { $1 } |
expFalse { $1 } |
expBinop { $1 } |
exp_paren { $1 } |
exp_await { $1 } |
exp_suppress_nullable_warning { $1 } |
exp_post_increment { $1 } |
exp_post_decrement { $1 } |
exp_cast { $1 } |
exp_as { $1 } |
exp_conditional_access { $1 } |
exp_conditional { $1 } |
exp_typeof { $1 } |
exp_collection { $1 } |
exp_tuple_expression { $1 } |
exp_with { $1 } |
exp_anonymous_object { $1 } |
exp_simple_lambda { $1 } |
exp_parenthesized_lambda { $1 } |
exp_casting { $1 } |
exp_declaration { $1 } |
exp_not_pattern { $1 } |
exp_recursive_pattern { $1 } |
exp_and_pattern { $1 } |
exp_or_pattern { $1 } |
constantPattern { $1 } |
exp_interpolated_string { $1 } |
exp_index_expression { $1 } |
exp_range_expression { $1 } |
exp_coalesce_assign { $1 } |
exp_member_binding { $1 } |
exp_throw { $1 } |
objectInitializer { $1 } |
relationalPattern { $1 } |
discardPattern { $1 } |
typePattern { $1 } |
parenthesizedPattern { $1 }

equalsValueClause:
'{'
    'kind' ':' 'EqualsValueClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

-- a method body : a block, or `=> expr` ( returned ) / `=> x = y`
methodBody:
stmtBlock { $1 } |
'{'
    'kind' ':' 'ArrowExpressionClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.arrowBody $12 $17
}
|
'{'
    'kind' ':' 'ArrowExpressionClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtAssign ']'
'}'
{
    Actions.arrowStmtBody $12 $17
}

arrowExpressionClause:
'{'
    'kind' ':' 'ArrowExpressionClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'ArrowExpressionClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtAssign ']'
'}'
{
    Actions.expNull $12
}

-- one declarator of `T a = 1, b`
declarator:
'{'
    'kind' ':' 'VariableDeclarator' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' optional(equalsValueClause) ']'
'}'
{
    Actions.declarator $12 (tokIDValue $8) $17
}

-- direct dhscanner subtree creation: Ast.StmtVardec
stmtVardec:
'{'
    'kind' ':' 'VariableDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' commalistof(declarator) ']'
'}'
{
    Actions.stmtVardec $12 $17 $19
}

stmtLocalVardec:
'{'
    'kind' ':' 'LocalDeclarationStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtVardec ']'
'}'
{
    $17
}

stmtField:
'{'
    'kind' ':' 'FieldDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtVardec ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'FieldDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) stmtVardec ']'
'}'
{
    $18
}
|
'{'
    'kind' ':' 'EventFieldDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtVardec ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'EventFieldDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) stmtVardec ']'
'}'
{
    $18
}

accessor_kind:
'GetAccessorDeclaration' { Nothing } |
'SetAccessorDeclaration' { Nothing } |
'InitAccessorDeclaration' { Nothing }

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
|
'{'
    'kind' ':' accessor_kind ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' arrowExpressionClause ']'
'}'
{
    Nothing
}
|
'{'
    'kind' ':' accessor_kind ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtBlock ']'
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

-- instrumented as dhscanner Ast.StmtVardec ( a data member, accessors dropped )
stmtProperty:
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' accessor_list ']'
'}'
{
    Actions.stmtProperty $12 (tokIDValue $8) $17
} |
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) nominalType ',' accessor_list ']'
'}'
{
    Actions.stmtProperty $12 (tokIDValue $8) $18
} |
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' explicitInterfaceSpecifier ',' accessor_list ']'
'}'
{
    Actions.stmtProperty $12 (tokIDValue $8) $17
} |
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) nominalType ',' explicitInterfaceSpecifier ',' accessor_list ']'
'}'
{
    Actions.stmtProperty $12 (tokIDValue $8) $18
} |
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' accessor_list ',' equalsValueClause ']'
'}'
{
    Actions.stmtProperty $12 (tokIDValue $8) $17
} |
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) nominalType ',' accessor_list ',' equalsValueClause ']'
'}'
{
    Actions.stmtProperty $12 (tokIDValue $8) $18
}

|
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' arrowExpressionClause ']'
'}'
{
    Actions.stmtProperty $12 (tokIDValue $8) $17
}
|
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) nominalType ',' arrowExpressionClause ']'
'}'
{
    Actions.stmtProperty $12 (tokIDValue $8) $18
}

|
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' nominalType ',' explicitInterfaceSpecifier ',' arrowExpressionClause ']'
'}'
{
    Actions.stmtProperty $12 (tokIDValue $8) $17
}
|
'{'
    'kind' ':' 'PropertyDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) nominalType ',' explicitInterfaceSpecifier ',' arrowExpressionClause ']'
'}'
{
    Actions.stmtProperty $12 (tokIDValue $8) $18
}

stmtIndexer:
'{'
    'kind' ':' 'IndexerDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' bracketedParams ',' accessor_list ']'
'}'
{
    Actions.stmtBlock $12 []
}
|
'{'
    'kind' ':' 'IndexerDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' bracketedParams ',' arrowExpressionClause ']'
'}'
{
    Actions.stmtBlock $12 []
}

-- direct dhscanner subtree creation: Ast.StmtBlock
stmtBlock:
'{'
    'kind' ':' 'Block' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(stmt)
'}'
{
    Actions.stmtBlock $12 $16
}

-- direct dhscanner subtree creation: Ast.Param
parameter:
'{'
    'kind' ':' 'Parameter' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.parameterNameOnly $12 (tokIDValue $8)
} |
'{'
    'kind' ':' 'Parameter' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ']'
'}'
{
    Actions.parameter $12 (tokIDValue $8) $17
} |
'{'
    'kind' ':' 'Parameter' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' equalsValueClause ']'
'}'
{
    Actions.parameter $12 (tokIDValue $8) $17
}

params:
'{'
    'kind' ':' 'ParameterList' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(parameter)
'}'
{
    Actions.params $16
}

bracketedParams:
'{'
    'kind' ':' 'BracketedParameterList' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(parameter)
'}'
{
    Actions.params $16
}

explicitInterfaceSpecifier:
'{'
    'kind' ':' 'ExplicitInterfaceSpecifier' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(nominalType)
'}'
{
    ()
}

-- direct dhscanner subtree creation: Ast.StmtMethod
stmtMethod:
'{'
    'kind' ':' 'MethodDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' params ',' methodBody ']'
'}'
{
    Actions.stmtMethod $12 (tokIDValue $8) $19 $17 $21
}
|
'{'
    'kind' ':' 'MethodDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' typeParameterList ',' params ',' methodBody ']'
'}'
{
    Actions.stmtMethod $12 (tokIDValue $8) $21 $17 $23
}
|
'{'
    'kind' ':' 'MethodDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' typeParameterList ',' params ']'
'}'
{
    Actions.stmtMethodNoBody $12 (tokIDValue $8) $21 $17
}
|
'{'
    'kind' ':' 'MethodDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' params ']'
'}'
{
    Actions.stmtMethodNoBody $12 (tokIDValue $8) $19 $17
}
|
'{'
    'kind' ':' 'MethodDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' explicitInterfaceSpecifier ',' params ',' methodBody ']'
'}'
{
    Actions.stmtMethod $12 (tokIDValue $8) $21 $17 $23
}
|
'{'
    'kind' ':' 'MethodDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' explicitInterfaceSpecifier ',' params ']'
'}'
{
    Actions.stmtMethodNoBody $12 (tokIDValue $8) $21 $17
}
|
'{'
    'kind' ':' 'MethodDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' typeParameterList ',' params ',' prefixlistof(typeParameterConstraintClause) methodBody ']'
'}'
{
    Actions.stmtMethod $12 (tokIDValue $8) $21 $17 $24
}
|
'{'
    'kind' ':' 'MethodDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' typeParameterList ',' params ',' prefixlistof(typeParameterConstraintClause) typeParameterConstraintClause ']'
'}'
{
    Actions.stmtMethodNoBody $12 (tokIDValue $8) $21 $17
}
|
'{'
    'kind' ':' 'MethodDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' typeParameterList ',' params ',' typeParameterConstraintClause ']'
'}'
{
    Actions.stmtMethodNoBody $12 (tokIDValue $8) $21 $17
}

-- direct dhscanner subtree creation: Ast.StmtMethod
stmtLocalFunction:
'{'
    'kind' ':' 'LocalFunctionStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' params ',' methodBody ']'
'}'
{
    Actions.stmtMethod $12 (tokIDValue $8) $19 $17 $21
}
|
'{'
    'kind' ':' 'LocalFunctionStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' typeParameterList ',' params ',' methodBody ']'
'}'
{
    Actions.stmtMethod $12 (tokIDValue $8) $21 $17 $23
}
|
'{'
    'kind' ':' 'LocalFunctionStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' typeParameterList ',' params ',' prefixlistof(typeParameterConstraintClause) methodBody ']'
'}'
{
    Actions.stmtMethod $12 (tokIDValue $8) $21 $17 $24
}

-- direct dhscanner subtree creation: Ast.StmtMethod
stmtDelegate:
'{'
    'kind' ':' 'DelegateDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' params ']'
'}'
{
    Actions.stmtMethodNoBody $12 (tokIDValue $8) $19 $17
}
|
'{'
    'kind' ':' 'DelegateDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' typeParameterList ',' params ']'
'}'
{
    Actions.stmtMethodNoBody $12 (tokIDValue $8) $21 $17
}

elsePart:
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

-- direct dhscanner subtree creation: Ast.StmtIf
stmtIf:
'{'
    'kind' ':' 'IfStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' stmt optional(elsePart) ']'
'}'
{
    Actions.stmtIf $12 $17 $19 $20
}

-- direct dhscanner subtree creation: Ast.StmtWhile
stmtWhile:
'{'
    'kind' ':' 'WhileStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' stmt ']'
'}'
{
    Actions.stmtWhile $12 $17 $19
}

-- instrumented as dhscanner Ast.StmtBlock
stmtFor:
'{'
    'kind' ':' 'ForStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' stmt ']'
'}'
{
    Actions.stmtFor $12 [$17] $19
}
|
'{'
    'kind' ':' 'ForStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' exp ',' stmt ']'
'}'
{
    Actions.stmtFor $12 [$17, $19] $21
}
|
'{'
    'kind' ':' 'ForStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtVardec ',' stmt ']'
'}'
{
    Actions.stmtForFromVardec $12 $17 [] $19
}
|
'{'
    'kind' ':' 'ForStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtVardec ',' exp ',' stmt ']'
'}'
{
    Actions.stmtForFromVardec $12 $17 [$19] $21
}
|
'{'
    'kind' ':' 'ForStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtVardec ',' exp ',' exp ',' stmt ']'
'}'
{
    Actions.stmtForFromVardec $12 $17 [$19, $21] $23
}
|
'{'
    'kind' ':' 'ForStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtVardec ',' exp ',' expAssign ',' stmt ']'
'}'
{
    Actions.stmtForFromVardec $12 $17 [$19, $21] $23
}

catchDeclaration:
'{'
    'kind' ':' 'CatchDeclaration' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(nominalType)
'}'
{
    ()
}

catchFilterClause:
'{'
    'kind' ':' 'CatchFilterClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    ()
}

catchClause:
'{'
    'kind' ':' 'CatchClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtBlock ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'CatchClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' catchDeclaration ',' stmtBlock ']'
'}'
{
    $19
}
|
'{'
    'kind' ':' 'CatchClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' catchFilterClause ',' stmtBlock ']'
'}'
{
    $19
}
|
'{'
    'kind' ':' 'CatchClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' catchDeclaration ',' catchFilterClause ',' stmtBlock ']'
'}'
{
    $21
}

finallyClause:
'{'
    'kind' ':' 'FinallyClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtBlock ']'
'}'
{
    $17
}

tryChild:
stmtBlock { $1 } |
catchClause { $1 } |
finallyClause { $1 }

-- instrumented as dhscanner Ast.StmtBlock
stmtTry:
'{'
    'kind' ':' 'TryStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(tryChild)
'}'
{
    Actions.stmtBlock $12 $16
}

-- instrumented as dhscanner Ast.StmtBlock
stmtUsingStatement:
'{'
    'kind' ':' 'UsingStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' stmt ']'
'}'
{
    Actions.stmtUsingStatement $12 $17 $19
}
|
'{'
    'kind' ':' 'UsingStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmtVardec ',' stmt ']'
'}'
{
    Actions.stmtUsingStatementFromVardec $12 $17 $19
}

-- instrumented as dhscanner Ast.StmtBlock
-- instrumented as dhscanner Ast.StmtBlock
stmtLock:
'{'
    'kind' ':' 'LockStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' stmt ']'
'}'
{
    Actions.stmtLock $12 $17 $19
}

stmtForEach:
'{'
    'kind' ':' 'ForEachStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' annotatedType ',' exp ',' stmt ']'
'}'
{
    Actions.stmtForEach $12 (tokIDValue $8) $17 $19 $21
}

-- instrumented as dhscanner Ast.StmtBlock
stmtForEachVariable:
'{'
    'kind' ':' 'ForEachVariableStatement' ','
    'value' ':' tokenID ','
    'location' ':' location ','
    'children' ':' '[' declarationDeconstructVar ',' exp ',' stmt ']'
'}'
{
    Actions.stmtForEachVar $12 $17 $19 $21
}

-- `e;` : the expression ( or assignment ) itself
stmtExpression:
'{'
    'kind' ':' 'ExpressionStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmt ']'
'}'
{
    $17
}

-- direct dhscanner subtree creation: Ast.StmtAssign
stmtAssign:
'{'
    'kind' ':' 'SimpleAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.stmtAssign $17 $19
}
|
'{'
    'kind' ':' 'SimpleAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' declarationDeconstructVar ',' exp ']'
'}'
{
    Actions.stmtAssign $17 $19
}
|
'{'
    'kind' ':' 'SimpleAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp_tuple_expression ',' exp ']'
'}'
{
    Actions.tupleAssign $12 $17 $19
}
|
'{'
    'kind' ':' 'SimpleAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' expAssign ']'
'}'
{
    Actions.stmtAssign $17 $19
}

-- instrumented lowering: x += y  ==>  x = x + y
-- instrumented lowering: x -= y  ==>  x = x - y ( same for *= /= )
stmtCompoundAssign:
'{'
    'kind' ':' 'SubtractAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.subtractAssign $12 $17 $19
}
|
'{'
    'kind' ':' 'MultiplyAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.multiplyAssign $12 $17 $19
}
|
'{'
    'kind' ':' 'DivideAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.divideAssign $12 $17 $19
}

stmtAddAssign:
'{'
    'kind' ':' 'AddAssignmentExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' exp ']'
'}'
{
    Actions.addAssign $12 $17 $19
}

-- direct dhscanner subtree creation: Ast.StmtExp
stmtExp: exp { Actions.stmtExp $1 }

-- direct dhscanner subtree creation: Ast.StmtBreak
stmtBreak:
'{'
    'kind' ':' 'BreakStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.stmtBreak $12
}

-- instrumented as dhscanner Ast.StmtExp ( tag [continue] )
stmtContinue:
'{'
    'kind' ':' 'ContinueStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.stmtContinue $12
}

-- direct dhscanner subtree creation: Ast.StmtReturn
stmtReturn:
'{'
    'kind' ':' 'ReturnStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.stmtReturn $12 $17
}
|
'{'
    'kind' ':' 'ReturnStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.stmtReturnVoid $12
}

-- instrumented as dhscanner Ast.StmtExp ( tag [yield] )
stmtYieldReturn:
'{'
    'kind' ':' 'YieldReturnStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.stmtYieldReturn $12 $17
}

-- instrumented as dhscanner Ast.StmtExp ( tag [yield_break] )
stmtYieldBreak:
'{'
    'kind' ':' 'YieldBreakStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.stmtYieldBreak $12
}

-- instrumented as dhscanner Ast.StmtExp ( the call `this(args)` )
ctorInitializer:
'{'
    'kind' ':' 'ThisConstructorInitializer' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' args ']'
'}'
{
    Actions.thisInitializer $12 $17
}

-- instrumented as dhscanner Ast.StmtExp ( the call `base(args)` )
baseCtorInitializer:
'{'
    'kind' ':' 'BaseConstructorInitializer' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' args ']'
'}'
{
    Actions.baseInitializer $12 $17
}

-- direct dhscanner subtree creation: Ast.StmtMethod
stmtCtor:
'{'
    'kind' ':' 'ConstructorDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' params ',' ctorInitializer ',' stmtBlock ']'
'}'
{
    Actions.stmtCtorInit $12 (tokIDValue $8) $17 $19 $21
} |
'{'
    'kind' ':' 'ConstructorDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' params ',' baseCtorInitializer ',' stmtBlock ']'
'}'
{
    Actions.stmtCtorInit $12 (tokIDValue $8) $17 $19 $21
} |
'{'
    'kind' ':' 'ConstructorDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' params ',' methodBody ']'
'}'
{
    Actions.stmtCtor $12 (tokIDValue $8) $17 $19
}

|
'{'
    'kind' ':' 'ConstructorDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) params ',' ctorInitializer ',' stmtBlock ']'
'}'
{
    Actions.stmtCtorInit $12 (tokIDValue $8) $18 $20 $22
}
|
'{'
    'kind' ':' 'ConstructorDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) params ',' baseCtorInitializer ',' stmtBlock ']'
'}'
{
    Actions.stmtCtorInit $12 (tokIDValue $8) $18 $20 $22
}
|
'{'
    'kind' ':' 'ConstructorDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) params ',' methodBody ']'
'}'
{
    Actions.stmtCtor $12 (tokIDValue $8) $18 $20
}

whenClause:
'{'
    'kind' ':' 'WhenClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

caseLabel:
'{'
    'kind' ':' 'CaseSwitchLabel' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'CasePatternSwitchLabel' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}
|
'{'
    'kind' ':' 'CasePatternSwitchLabel' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' whenClause ']'
'}'
{
    $17
}

defaultLabel:
'{'
    'kind' ':' 'DefaultSwitchLabel' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Nothing
}

switchSection:
'{'
    'kind' ':' 'SwitchSection' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(caseLabel) commalistof(stmt) ']'
'}'
{
    Actions.caseSectionMulti $17 $18
} |
'{'
    'kind' ':' 'SwitchSection' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' defaultLabel ',' commalistof(stmt) ']'
'}'
{
    Actions.defaultSection $19
} |
'{'
    'kind' ':' 'SwitchSection' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(caseLabel) defaultLabel ',' commalistof(stmt) ']'
'}'
{
    Actions.caseSectionMulti $17 $20
}

-- instrumented as dhscanner Ast.StmtBlock ( scrutinee, labels, bodies )
stmtSwitch:
'{'
    'kind' ':' 'SwitchStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ',' commalistof(switchSection) ']'
'}'
{
    Actions.stmtSwitch $12 $17 $19
}

-- instrumented as dhscanner Ast.ExpCall ( tag [throw] )
stmtThrow:
'{'
    'kind' ':' 'ThrowStatement' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.stmtThrow $12 $17
}
|
'{'
    'kind' ':' 'ThrowStatement' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Actions.stmtRethrow $12
}

-- instrumented as dhscanner Ast.ExpCall ( tag [throw] )
exp_throw:
'{'
    'kind' ':' 'ThrowExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    Actions.instrumentationCall "throw" $12 [$17]
}

-- instrumented as dhscanner Ast.StmtExp ( tag [enum] )
stmtEnum:
'{'
    'kind' ':' 'EnumDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(enumMember)
'}'
{
    Actions.stmtEnum $12 (tokIDValue $8) $16
}
|
'{'
    'kind' ':' 'EnumDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) commalistof(enumMember) ']'
'}'
{
    Actions.stmtEnum $12 (tokIDValue $8) $18
}

enumMember:
'{'
    'kind' ':' 'EnumMemberDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    tokIDValue $8
}
|
'{'
    'kind' ':' 'EnumMemberDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' equalsValueClause ']'
'}'
{
    tokIDValue $8
}
|
'{'
    'kind' ':' 'EnumMemberDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) equalsValueClause ']'
'}'
{
    tokIDValue $8
}
|
'{'
    'kind' ':' 'EnumMemberDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' attributeList ']'
'}'
{
    tokIDValue $8
}
|
'{'
    'kind' ':' 'EnumMemberDeclaration' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' prefixlistof(attributeList) attributeList ']'
'}'
{
    tokIDValue $8
}

stmt:
stmtIf { $1 } |
stmtExp { $1 } |
stmtCtor { $1 } |
stmtBreak { $1 } |
stmtContinue { $1 } |
stmtClass { $1 } |
stmtEnum { $1 } |
stmtUsing { $1 } |
stmtUsingAlias { $1 } |
stmtWhile { $1 } |
stmtFor { $1 } |
stmtTry { $1 } |
stmtUsingStatement { $1 } |
stmtLock { $1 } |
stmtForEachVariable { $1 } |
stmtForEach { $1 } |
stmtBlock { $1 } |
stmtThrow { $1 } |
stmtSwitch { $1 } |
stmtYieldReturn { $1 } |
stmtYieldBreak { $1 } |
stmtReturn { $1 } |
stmtVardec { $1 } |
stmtAssign { $1 } |
stmtAddAssign { $1 } |
stmtOperator { $1 } |
stmtLabeled { $1 } |
stmtGoto { $1 } |
stmtEmpty { $1 } |
stmtBitwiseAssign { $1 } |
stmtDo { $1 } |
stmtCompoundAssign { $1 } |
stmtLocalFunction { $1 } |
stmtMethod { $1 } |
stmtExpression { $1 } |
stmtProperty { $1 } |
stmtIndexer { $1 } |
stmtNamespace { $1 } |
stmtLocalVardec { $1 } |
stmtDelegate { $1 } |
stmtField { $1 }

-- instrumented as dhscanner Ast.StmtBlock ( flattened into the root )
stmtNamespace:
'{'
    'kind' ':' 'NamespaceDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var optional(namespaceMembers) ']'
'}'
{
    Actions.stmtNamespace $12 $17 $18
} |
'{'
    'kind' ':' 'FileScopedNamespaceDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var optional(namespaceMembers) ']'
'}'
{
    Actions.stmtNamespace $12 $17 $18
}

-- the usings and members after a namespace's name
namespaceMembers: ',' commalistof(stmt) { $2 }

location:
'{'
    'startLine' ':' INT ','
    'startColumn' ':' INT ','
    'endLine' ':' INT ','
    'endColumn' ':' INT
'}'
{
    Actions.location (getFilename $1) (tokIntValue $4) (tokIntValue $8) (tokIntValue $12) (tokIntValue $16)
}

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
