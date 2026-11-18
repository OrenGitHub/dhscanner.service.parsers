{

{-# OPTIONS -w  #-}

module CsLexer

where

import Prelude hiding (lex)
import Control.Monad ( liftM )
import Location
import Common
}

-- ***********
-- *         *
-- * wrapper *
-- *         *
-- ***********
%wrapper "monadUserState"

-- ***************
-- *             *
-- * parentheses *
-- *             *
-- ***************

@LPAREN = \(
@RPAREN = \)
@LBRACK = \[
@RBRACK = \]
@LBRACE = \{
@RBRACE = \}

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

@COLON  = ":"
@COMMA  = ","

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@KIND = kind
@NULL = null
@VALUE = value
@LOCATION = location
@START_LINE = startLine
@START_COLUMN = startColumn
@END_LINE = endLine
@END_COLUMN = endColumn
@CHILDREN = children
@COMPILATION_UNIT = CompilationUnit
@USING_DIRECTIVE = UsingDirective
@IDENTIFIER_NAME = IdentifierName
@QUALIFIED_NAME = QualifiedName
@ALIAS_QUALIFIED_NAME = AliasQualifiedName
@NAME_EQUALS = NameEquals
@NAME_COLON = NameColon
@NAMESPACE_DECLARATION = NamespaceDeclaration
@FILE_SCOPED_NAMESPACE_DECLARATION = FileScopedNamespaceDeclaration
@CLASS_DECLARATION = ClassDeclaration
@EMPTY_STATEMENT = EmptyStatement
@GOTO_DEFAULT_STATEMENT = GotoDefaultStatement
@GOTO_CASE_STATEMENT = GotoCaseStatement
@GOTO_STATEMENT = GotoStatement
@LABELED_STATEMENT = LabeledStatement
@CONVERSION_OPERATOR_DECLARATION = ConversionOperatorDeclaration
@OPERATOR_DECLARATION = OperatorDeclaration
@SLICE_PATTERN = SlicePattern
@LIST_PATTERN = ListPattern
@VAR_PATTERN = VarPattern
@POSITIONAL_PATTERN_CLAUSE = PositionalPatternClause
@DEFAULT_EXPRESSION = DefaultExpression
@GLOBAL_STATEMENT = GlobalStatement
@UNSIGNED_RIGHT_SHIFT_EXPRESSION = UnsignedRightShiftExpression
@RIGHT_SHIFT_EXPRESSION = RightShiftExpression
@LEFT_SHIFT_EXPRESSION = LeftShiftExpression
@EXCLUSIVE_OR_EXPRESSION = ExclusiveOrExpression
@BITWISE_AND_EXPRESSION = BitwiseAndExpression
@INTERPOLATION_ALIGNMENT_CLAUSE = InterpolationAlignmentClause
@EXTENSION_BLOCK_DECLARATION = ExtensionBlockDeclaration
@MODULO_ASSIGNMENT_EXPRESSION = ModuloAssignmentExpression
@RIGHT_SHIFT_ASSIGNMENT_EXPRESSION = RightShiftAssignmentExpression
@LEFT_SHIFT_ASSIGNMENT_EXPRESSION = LeftShiftAssignmentExpression
@EXCLUSIVE_OR_ASSIGNMENT_EXPRESSION = ExclusiveOrAssignmentExpression
@AND_ASSIGNMENT_EXPRESSION = AndAssignmentExpression
@OR_ASSIGNMENT_EXPRESSION = OrAssignmentExpression
@STACK_ALLOC_ARRAY_CREATION_EXPRESSION = StackAllocArrayCreationExpression
@PRE_DECREMENT_EXPRESSION = PreDecrementExpression
@PRE_INCREMENT_EXPRESSION = PreIncrementExpression
@DO_STATEMENT = DoStatement
@PARENTHESIZED_PATTERN = ParenthesizedPattern
@ATTRIBUTE_TARGET_SPECIFIER = AttributeTargetSpecifier
@EVENT_FIELD_DECLARATION = EventFieldDeclaration
@DIVIDE_ASSIGNMENT_EXPRESSION = DivideAssignmentExpression
@MULTIPLY_ASSIGNMENT_EXPRESSION = MultiplyAssignmentExpression
@SUBTRACT_ASSIGNMENT_EXPRESSION = SubtractAssignmentExpression
@ELEMENT_BINDING_EXPRESSION = ElementBindingExpression
@MODULO_EXPRESSION = ModuloExpression
@COMPLEX_ELEMENT_INITIALIZER_EXPRESSION = ComplexElementInitializerExpression
@IMPLICIT_ARRAY_CREATION_EXPRESSION = ImplicitArrayCreationExpression
@ARRAY_INITIALIZER_EXPRESSION = ArrayInitializerExpression
@QUERY_CONTINUATION = QueryContinuation
@GROUP_CLAUSE = GroupClause
@SELECT_CLAUSE = SelectClause
@DESCENDING_ORDERING = DescendingOrdering
@ASCENDING_ORDERING = AscendingOrdering
@ORDER_BY_CLAUSE = OrderByClause
@WHERE_CLAUSE = WhereClause
@LET_CLAUSE = LetClause
@JOIN_INTO_CLAUSE = JoinIntoClause
@JOIN_CLAUSE = JoinClause
@QUERY_BODY = QueryBody
@FROM_CLAUSE = FromClause
@QUERY_EXPRESSION = QueryExpression
@TYPE_PATTERN = TypePattern
@DISCARD_PATTERN = DiscardPattern
@SWITCH_EXPRESSION_ARM = SwitchExpressionArm
@SWITCH_EXPRESSION = SwitchExpression
@INTERFACE_DECLARATION = InterfaceDeclaration
@ENUM_DECLARATION = EnumDeclaration
@ENUM_MEMBER_DECLARATION = EnumMemberDeclaration
@RECORD_DECLARATION = RecordDeclaration
@RECORD_STRUCT_DECLARATION = RecordStructDeclaration
@STRUCT_DECLARATION = StructDeclaration
@PRIMARY_CONSTRUCTOR_BASE_TYPE = PrimaryConstructorBaseType
@ATTRIBUTE_ARGUMENT = AttributeArgument
@ATTRIBUTE_ARGUMENT_LIST = AttributeArgumentList
@ATTRIBUTE = Attribute
@ATTRIBUTE_LIST = AttributeList
@SIMPLE_BASE_TYPE = SimpleBaseType
@BASE_LIST = BaseList
@ARGUMENT_LIST = ArgumentList
@OBJECT_CREATION_EXPRESSION = ObjectCreationExpression
@ARRAY_CREATION_EXPRESSION = ArrayCreationExpression
@IMPLICIT_OBJECT_CREATION_EXPRESSION = ImplicitObjectCreationExpression
@OBJECT_INITIALIZER_EXPRESSION = ObjectInitializerExpression
@COLLECTION_INITIALIZER_EXPRESSION = CollectionInitializerExpression
@WITH_EXPRESSION = WithExpression
@WITH_INITIALIZER_EXPRESSION = WithInitializerExpression
@ANONYMOUS_OBJECT_CREATION_EXPRESSION = AnonymousObjectCreationExpression
@ANONYMOUS_OBJECT_MEMBER_DECLARATOR = AnonymousObjectMemberDeclarator
@COLLECTION_EXPRESSION = CollectionExpression
@EXPRESSION_ELEMENT = ExpressionElement
@SPREAD_ELEMENT = SpreadElement
@EQUALS_VALUE_CLAUSE = EqualsValueClause
@VARIABLE_DECLARATOR = VariableDeclarator
@TYPE_ARGUMENT_LIST = TypeArgumentList
@OMITTED_TYPE_ARGUMENT = OmittedTypeArgument
@TYPE_PARAMETER_LIST = TypeParameterList
@TYPE_PARAMETER = TypeParameter
@TYPE_PARAMETER_CONSTRAINT_CLAUSE = TypeParameterConstraintClause
@TYPE_CONSTRAINT = TypeConstraint
@CLASS_CONSTRAINT = ClassConstraint
@STRUCT_CONSTRAINT = StructConstraint
@CONSTRUCTOR_CONSTRAINT = ConstructorConstraint
@GENERIC_NAME = GenericName
@VARIABLE_DECLARATION = VariableDeclaration
@FIELD_DECLARATION = FieldDeclaration
@ELEMENT_ACCESS_EXPRESSION = ElementAccessExpression
@IMPLICIT_ELEMENT_ACCESS = ImplicitElementAccess
@BRACKETED_ARGUMENT_LIST = BracketedArgumentList
@ARGUMENT = Argument
@NULLABLE_TYPE = NullableType
@ARRAY_TYPE = ArrayType
@ARRAY_RANK_SPECIFIER = ArrayRankSpecifier
@OMITTED_ARRAY_SIZE_EXPRESSION = OmittedArraySizeExpression
@METHOD_DECLARATION = MethodDeclaration
@LOCAL_FUNCTION_STATEMENT = LocalFunctionStatement
@DELEGATE_DECLARATION = DelegateDeclaration
@EXPLICIT_INTERFACE_SPECIFIER = ExplicitInterfaceSpecifier
@PARAMETER_LIST = ParameterList
@BRACKETED_PARAMETER_LIST = BracketedParameterList
@BLOCK = Block
@IF_STATEMENT = IfStatement
@EQUALS_EXPRESSION = EqualsExpression
@IS_EXPRESSION = IsExpression
@AS_EXPRESSION = AsExpression
@GREATER_THAN_EXPRESSION = GreaterThanExpression
@GREATER_THAN_OR_EQUAL_EXPRESSION = GreaterThanOrEqualExpression
@LESS_THAN_EXPRESSION = LessThanExpression
@LESS_THAN_OR_EQUAL_EXPRESSION = LessThanOrEqualExpression
@NOT_EQUALS_EXPRESSION = NotEqualsExpression
@LOGICAL_AND_EXPRESSION = LogicalAndExpression
@LOGICAL_OR_EXPRESSION = LogicalOrExpression
@BITWISE_OR_EXPRESSION = BitwiseOrExpression
@COALESCE_EXPRESSION = CoalesceExpression
@ADD_EXPRESSION = AddExpression
@MULTIPLY_EXPRESSION = MultiplyExpression
@SUBTRACT_EXPRESSION = SubtractExpression
@DIVIDE_EXPRESSION = DivideExpression
@COALESCE_ASSIGNMENT_EXPRESSION = CoalesceAssignmentExpression
@ADD_ASSIGNMENT_EXPRESSION = AddAssignmentExpression
@CONDITIONAL_ACCESS_EXPRESSION = ConditionalAccessExpression
@CONDITIONAL_EXPRESSION = ConditionalExpression
@RANGE_EXPRESSION = RangeExpression
@INDEX_EXPRESSION = IndexExpression
@NULL_LITERAL_EXPRESSION = NullLiteralExpression
@DEFAULT_LITERAL_EXPRESSION = DefaultLiteralExpression
@WHILE_STATEMENT = WhileStatement
@FOR_STATEMENT = ForStatement
@TRY_STATEMENT = TryStatement
@USING_STATEMENT = UsingStatement
@LOCK_STATEMENT = LockStatement
@FOR_EACH_STATEMENT = ForEachStatement
@FOR_EACH_VARIABLE_STATEMENT = ForEachVariableStatement
@INVOCATION_EXPRESSION = InvocationExpression
@MEMBER_BINDING_EXPRESSION = MemberBindingExpression
@SIMPLE_MEMBER_ACCESS_EXPRESSION = SimpleMemberAccessExpression
@EXPRESSION_STATEMENT = ExpressionStatement
@SIMPLE_ASSIGNMENT_EXPRESSION = SimpleAssignmentExpression
@IS_PATTERN_EXPRESSION = IsPatternExpression
@DECLARATION_EXPRESSION = DeclarationExpression
@DECLARATION_PATTERN = DeclarationPattern
@NOT_PATTERN = NotPattern
@RECURSIVE_PATTERN = RecursivePattern
@OR_PATTERN = OrPattern
@AND_PATTERN = AndPattern
@CONSTANT_PATTERN = ConstantPattern
@PROPERTY_PATTERN_CLAUSE = PropertyPatternClause
@SUBPATTERN = Subpattern
@RELATIONAL_PATTERN = RelationalPattern
@SINGLE_VARIABLE_DESIGNATION = SingleVariableDesignation
@PARENTHESIZED_VARIABLE_DESIGNATION = ParenthesizedVariableDesignation
@DISCARD_DESIGNATION = DiscardDesignation
@ELSE_CLAUSE = ElseClause
@CATCH_CLAUSE = CatchClause
@CATCH_DECLARATION = CatchDeclaration
@CATCH_FILTER_CLAUSE = CatchFilterClause
@FINALLY_CLAUSE = FinallyClause
@WHEN_CLAUSE = WhenClause
@BREAK_STATEMENT = BreakStatement
@CONTINUE_STATEMENT = ContinueStatement
@RETURN_STATEMENT = ReturnStatement
@YIELD_BREAK_STATEMENT = YieldBreakStatement
@YIELD_RETURN_STATEMENT = YieldReturnStatement
@CONSTRUCTOR_DECLARATION = ConstructorDeclaration
@PARAMETER = Parameter
@BASE_CONSTRUCTOR_INITIALIZER = BaseConstructorInitializer
@THIS_CONSTRUCTOR_INITIALIZER = ThisConstructorInitializer
@THIS_EXPRESSION = ThisExpression
@BASE_EXPRESSION = BaseExpression
@PROPERTY_DECLARATION = PropertyDeclaration
@INDEXER_DECLARATION = IndexerDeclaration
@ACCESSOR_LIST = AccessorList
@GET_ACCESSOR_DECLARATION = GetAccessorDeclaration
@SET_ACCESSOR_DECLARATION = SetAccessorDeclaration
@INIT_ACCESSOR_DECLARATION = InitAccessorDeclaration
@PREDEFINED_TYPE = PredefinedType
@FALSE_LITERAL_EXPRESSION = FalseLiteralExpression
@TRUE_LITERAL_EXPRESSION = TrueLiteralExpression
@NUMERIC_LITERAL_EXPRESSION = NumericLiteralExpression
@CHARACTER_LITERAL_EXPRESSION = CharacterLiteralExpression
@STRING_LITERAL_EXPRESSION = StringLiteralExpression
@INTERPOLATED_STRING_EXPRESSION = InterpolatedStringExpression
@INTERPOLATED_STRING_TEXT = InterpolatedStringText
@INTERPOLATION = Interpolation
@INTERPOLATION_FORMAT_CLAUSE = InterpolationFormatClause
@SWITCH_STATEMENT = SwitchStatement
@SWITCH_SECTION = SwitchSection
@CASE_SWITCH_LABEL = CaseSwitchLabel
@CASE_PATTERN_SWITCH_LABEL = CasePatternSwitchLabel
@DEFAULT_SWITCH_LABEL = DefaultSwitchLabel
@THROW_STATEMENT = ThrowStatement
@THROW_EXPRESSION = ThrowExpression
@LOCAL_DECLARATION_STATEMENT = LocalDeclarationStatement
@LOGICAL_NOT_EXPRESSION = LogicalNotExpression
@POST_INCREMENT_EXPRESSION = PostIncrementExpression
@POST_DECREMENT_EXPRESSION = PostDecrementExpression
@PARENTHESIZED_EXPRESSION = ParenthesizedExpression
@CAST_EXPRESSION = CastExpression
@SIMPLE_LAMBDA_EXPRESSION = SimpleLambdaExpression
@PARENTHESIZED_LAMBDA_EXPRESSION = ParenthesizedLambdaExpression
@ARROW_EXPRESSION_CLAUSE = ArrowExpressionClause
@AWAIT_EXPRESSION = AwaitExpression
@TYPE_OF_EXPRESSION = TypeOfExpression
@TUPLE_EXPRESSION = TupleExpression
@TUPLE_TYPE = TupleType
@TUPLE_ELEMENT = TupleElement
@SUPPRESS_NULLABLE_WARNING_EXPRESSION = SuppressNullableWarningExpression
@UNARY_MINUS_EXPRESSION = UnaryMinusExpression

-- ************
-- *          *
-- * integers *
-- *          *
-- ************
@DIGIT = 0-9
@LETTER = [a-zA-Z_]
@LETTER_OR_DIGIT = @LETTER | @DIGIT
@INT = @DIGIT+ 
-- every native `value` is a sanitised quoted string ( fronts/cs Program.cs Quote )
@ID = \"[^\"]*\"

-- ***************
-- *             *
-- * identifiers *
-- *             *
-- ***************

-- ***************
-- *             *
-- * white space *
-- *             *
-- ***************
@WHITE_SPACE = $white+

-- **********
-- *        *
-- * tokens *
-- *        *
-- **********
tokens :-

-- ***************
-- *             *
-- * parentheses *
-- *             *
-- ***************

@LPAREN { lex' AlexRawToken_LPAREN }
@RPAREN { lex' AlexRawToken_RPAREN }
@LBRACK { lex' AlexRawToken_LBRACK }
@RBRACK { lex' AlexRawToken_RBRACK }
@LBRACE { lex' AlexRawToken_LBRACE }
@RBRACE { lex' AlexRawToken_RBRACE }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

@COLON { lex' AlexRawToken_COLON }
@COMMA { lex' AlexRawToken_COMMA }

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@KIND { lex' AlexRawToken_KIND }
@NULL { lex' AlexRawToken_NULL }
@VALUE { lex' AlexRawToken_VALUE }
@LOCATION { lex' AlexRawToken_LOCATION }
@START_LINE { lex' AlexRawToken_START_LINE }
@START_COLUMN { lex' AlexRawToken_START_COLUMN }
@END_LINE { lex' AlexRawToken_END_LINE }
@END_COLUMN { lex' AlexRawToken_END_COLUMN }
@CHILDREN { lex' AlexRawToken_CHILDREN }
@COMPILATION_UNIT { lex' AlexRawToken_COMPILATION_UNIT }
@USING_DIRECTIVE { lex' AlexRawToken_USING_DIRECTIVE }
@IDENTIFIER_NAME { lex' AlexRawToken_IDENTIFIER_NAME }
@QUALIFIED_NAME { lex' AlexRawToken_QUALIFIED_NAME }
@ALIAS_QUALIFIED_NAME { lex' AlexRawToken_ALIAS_QUALIFIED_NAME }
@NAME_EQUALS { lex' AlexRawToken_NAME_EQUALS }
@NAME_COLON { lex' AlexRawToken_NAME_COLON }
@NAMESPACE_DECLARATION { lex' AlexRawToken_NAMESPACE_DECLARATION }
@FILE_SCOPED_NAMESPACE_DECLARATION { lex' AlexRawToken_FILE_SCOPED_NAMESPACE_DECLARATION }
@CLASS_DECLARATION { lex' AlexRawToken_CLASS_DECLARATION }
@EMPTY_STATEMENT { lex' AlexRawToken_EMPTY_STATEMENT }
@GOTO_DEFAULT_STATEMENT { lex' AlexRawToken_GOTO_DEFAULT_STATEMENT }
@GOTO_CASE_STATEMENT { lex' AlexRawToken_GOTO_CASE_STATEMENT }
@GOTO_STATEMENT { lex' AlexRawToken_GOTO_STATEMENT }
@LABELED_STATEMENT { lex' AlexRawToken_LABELED_STATEMENT }
@CONVERSION_OPERATOR_DECLARATION { lex' AlexRawToken_CONVERSION_OPERATOR_DECLARATION }
@OPERATOR_DECLARATION { lex' AlexRawToken_OPERATOR_DECLARATION }
@SLICE_PATTERN { lex' AlexRawToken_SLICE_PATTERN }
@LIST_PATTERN { lex' AlexRawToken_LIST_PATTERN }
@VAR_PATTERN { lex' AlexRawToken_VAR_PATTERN }
@POSITIONAL_PATTERN_CLAUSE { lex' AlexRawToken_POSITIONAL_PATTERN_CLAUSE }
@DEFAULT_EXPRESSION { lex' AlexRawToken_DEFAULT_EXPRESSION }
@GLOBAL_STATEMENT { lex' AlexRawToken_GLOBAL_STATEMENT }
@UNSIGNED_RIGHT_SHIFT_EXPRESSION { lex' AlexRawToken_UNSIGNED_RIGHT_SHIFT_EXPRESSION }
@RIGHT_SHIFT_EXPRESSION { lex' AlexRawToken_RIGHT_SHIFT_EXPRESSION }
@LEFT_SHIFT_EXPRESSION { lex' AlexRawToken_LEFT_SHIFT_EXPRESSION }
@EXCLUSIVE_OR_EXPRESSION { lex' AlexRawToken_EXCLUSIVE_OR_EXPRESSION }
@BITWISE_AND_EXPRESSION { lex' AlexRawToken_BITWISE_AND_EXPRESSION }
@INTERPOLATION_ALIGNMENT_CLAUSE { lex' AlexRawToken_INTERPOLATION_ALIGNMENT_CLAUSE }
@EXTENSION_BLOCK_DECLARATION { lex' AlexRawToken_EXTENSION_BLOCK_DECLARATION }
@MODULO_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_MODULO_ASSIGNMENT_EXPRESSION }
@RIGHT_SHIFT_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_RIGHT_SHIFT_ASSIGNMENT_EXPRESSION }
@LEFT_SHIFT_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_LEFT_SHIFT_ASSIGNMENT_EXPRESSION }
@EXCLUSIVE_OR_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_EXCLUSIVE_OR_ASSIGNMENT_EXPRESSION }
@AND_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_AND_ASSIGNMENT_EXPRESSION }
@OR_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_OR_ASSIGNMENT_EXPRESSION }
@STACK_ALLOC_ARRAY_CREATION_EXPRESSION { lex' AlexRawToken_STACK_ALLOC_ARRAY_CREATION_EXPRESSION }
@PRE_DECREMENT_EXPRESSION { lex' AlexRawToken_PRE_DECREMENT_EXPRESSION }
@PRE_INCREMENT_EXPRESSION { lex' AlexRawToken_PRE_INCREMENT_EXPRESSION }
@DO_STATEMENT { lex' AlexRawToken_DO_STATEMENT }
@PARENTHESIZED_PATTERN { lex' AlexRawToken_PARENTHESIZED_PATTERN }
@ATTRIBUTE_TARGET_SPECIFIER { lex' AlexRawToken_ATTRIBUTE_TARGET_SPECIFIER }
@EVENT_FIELD_DECLARATION { lex' AlexRawToken_EVENT_FIELD_DECLARATION }
@DIVIDE_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_DIVIDE_ASSIGNMENT_EXPRESSION }
@MULTIPLY_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_MULTIPLY_ASSIGNMENT_EXPRESSION }
@SUBTRACT_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_SUBTRACT_ASSIGNMENT_EXPRESSION }
@ELEMENT_BINDING_EXPRESSION { lex' AlexRawToken_ELEMENT_BINDING_EXPRESSION }
@MODULO_EXPRESSION { lex' AlexRawToken_MODULO_EXPRESSION }
@COMPLEX_ELEMENT_INITIALIZER_EXPRESSION { lex' AlexRawToken_COMPLEX_ELEMENT_INITIALIZER_EXPRESSION }
@IMPLICIT_ARRAY_CREATION_EXPRESSION { lex' AlexRawToken_IMPLICIT_ARRAY_CREATION_EXPRESSION }
@ARRAY_INITIALIZER_EXPRESSION { lex' AlexRawToken_ARRAY_INITIALIZER_EXPRESSION }
@QUERY_CONTINUATION { lex' AlexRawToken_QUERY_CONTINUATION }
@GROUP_CLAUSE { lex' AlexRawToken_GROUP_CLAUSE }
@SELECT_CLAUSE { lex' AlexRawToken_SELECT_CLAUSE }
@DESCENDING_ORDERING { lex' AlexRawToken_DESCENDING_ORDERING }
@ASCENDING_ORDERING { lex' AlexRawToken_ASCENDING_ORDERING }
@ORDER_BY_CLAUSE { lex' AlexRawToken_ORDER_BY_CLAUSE }
@WHERE_CLAUSE { lex' AlexRawToken_WHERE_CLAUSE }
@LET_CLAUSE { lex' AlexRawToken_LET_CLAUSE }
@JOIN_INTO_CLAUSE { lex' AlexRawToken_JOIN_INTO_CLAUSE }
@JOIN_CLAUSE { lex' AlexRawToken_JOIN_CLAUSE }
@QUERY_BODY { lex' AlexRawToken_QUERY_BODY }
@FROM_CLAUSE { lex' AlexRawToken_FROM_CLAUSE }
@QUERY_EXPRESSION { lex' AlexRawToken_QUERY_EXPRESSION }
@TYPE_PATTERN { lex' AlexRawToken_TYPE_PATTERN }
@DISCARD_PATTERN { lex' AlexRawToken_DISCARD_PATTERN }
@SWITCH_EXPRESSION_ARM { lex' AlexRawToken_SWITCH_EXPRESSION_ARM }
@SWITCH_EXPRESSION { lex' AlexRawToken_SWITCH_EXPRESSION }
@INTERFACE_DECLARATION { lex' AlexRawToken_INTERFACE_DECLARATION }
@ENUM_DECLARATION { lex' AlexRawToken_ENUM_DECLARATION }
@ENUM_MEMBER_DECLARATION { lex' AlexRawToken_ENUM_MEMBER_DECLARATION }
@RECORD_DECLARATION { lex' AlexRawToken_RECORD_DECLARATION }
@RECORD_STRUCT_DECLARATION { lex' AlexRawToken_RECORD_STRUCT_DECLARATION }
@STRUCT_DECLARATION { lex' AlexRawToken_STRUCT_DECLARATION }
@PRIMARY_CONSTRUCTOR_BASE_TYPE { lex' AlexRawToken_PRIMARY_CONSTRUCTOR_BASE_TYPE }
@ATTRIBUTE_ARGUMENT { lex' AlexRawToken_ATTRIBUTE_ARGUMENT }
@ATTRIBUTE_ARGUMENT_LIST { lex' AlexRawToken_ATTRIBUTE_ARGUMENT_LIST }
@ATTRIBUTE { lex' AlexRawToken_ATTRIBUTE }
@ATTRIBUTE_LIST { lex' AlexRawToken_ATTRIBUTE_LIST }
@SIMPLE_BASE_TYPE { lex' AlexRawToken_SIMPLE_BASE_TYPE }
@BASE_LIST { lex' AlexRawToken_BASE_LIST }
@ARGUMENT_LIST { lex' AlexRawToken_ARGUMENT_LIST }
@OBJECT_CREATION_EXPRESSION { lex' AlexRawToken_OBJECT_CREATION_EXPRESSION }
@ARRAY_CREATION_EXPRESSION { lex' AlexRawToken_ARRAY_CREATION_EXPRESSION }
@IMPLICIT_OBJECT_CREATION_EXPRESSION { lex' AlexRawToken_IMPLICIT_OBJECT_CREATION_EXPRESSION }
@OBJECT_INITIALIZER_EXPRESSION { lex' AlexRawToken_OBJECT_INITIALIZER_EXPRESSION }
@COLLECTION_INITIALIZER_EXPRESSION { lex' AlexRawToken_COLLECTION_INITIALIZER_EXPRESSION }
@WITH_EXPRESSION { lex' AlexRawToken_WITH_EXPRESSION }
@WITH_INITIALIZER_EXPRESSION { lex' AlexRawToken_WITH_INITIALIZER_EXPRESSION }
@ANONYMOUS_OBJECT_CREATION_EXPRESSION { lex' AlexRawToken_ANONYMOUS_OBJECT_CREATION_EXPRESSION }
@ANONYMOUS_OBJECT_MEMBER_DECLARATOR { lex' AlexRawToken_ANONYMOUS_OBJECT_MEMBER_DECLARATOR }
@COLLECTION_EXPRESSION { lex' AlexRawToken_COLLECTION_EXPRESSION }
@EXPRESSION_ELEMENT { lex' AlexRawToken_EXPRESSION_ELEMENT }
@SPREAD_ELEMENT { lex' AlexRawToken_SPREAD_ELEMENT }
@EQUALS_VALUE_CLAUSE { lex' AlexRawToken_EQUALS_VALUE_CLAUSE }
@VARIABLE_DECLARATOR { lex' AlexRawToken_VARIABLE_DECLARATOR }
@TYPE_ARGUMENT_LIST { lex' AlexRawToken_TYPE_ARGUMENT_LIST }
@OMITTED_TYPE_ARGUMENT { lex' AlexRawToken_OMITTED_TYPE_ARGUMENT }
@TYPE_PARAMETER_LIST { lex' AlexRawToken_TYPE_PARAMETER_LIST }
@TYPE_PARAMETER { lex' AlexRawToken_TYPE_PARAMETER }
@TYPE_PARAMETER_CONSTRAINT_CLAUSE { lex' AlexRawToken_TYPE_PARAMETER_CONSTRAINT_CLAUSE }
@TYPE_CONSTRAINT { lex' AlexRawToken_TYPE_CONSTRAINT }
@CLASS_CONSTRAINT { lex' AlexRawToken_CLASS_CONSTRAINT }
@STRUCT_CONSTRAINT { lex' AlexRawToken_STRUCT_CONSTRAINT }
@CONSTRUCTOR_CONSTRAINT { lex' AlexRawToken_CONSTRUCTOR_CONSTRAINT }
@GENERIC_NAME { lex' AlexRawToken_GENERIC_NAME }
@VARIABLE_DECLARATION { lex' AlexRawToken_VARIABLE_DECLARATION }
@FIELD_DECLARATION { lex' AlexRawToken_FIELD_DECLARATION }
@ELEMENT_ACCESS_EXPRESSION { lex' AlexRawToken_ELEMENT_ACCESS_EXPRESSION }
@IMPLICIT_ELEMENT_ACCESS { lex' AlexRawToken_IMPLICIT_ELEMENT_ACCESS }
@BRACKETED_ARGUMENT_LIST { lex' AlexRawToken_BRACKETED_ARGUMENT_LIST }
@ARGUMENT { lex' AlexRawToken_ARGUMENT }
@NULLABLE_TYPE { lex' AlexRawToken_NULLABLE_TYPE }
@ARRAY_TYPE { lex' AlexRawToken_ARRAY_TYPE }
@ARRAY_RANK_SPECIFIER { lex' AlexRawToken_ARRAY_RANK_SPECIFIER }
@OMITTED_ARRAY_SIZE_EXPRESSION { lex' AlexRawToken_OMITTED_ARRAY_SIZE_EXPRESSION }
@METHOD_DECLARATION { lex' AlexRawToken_METHOD_DECLARATION }
@LOCAL_FUNCTION_STATEMENT { lex' AlexRawToken_LOCAL_FUNCTION_STATEMENT }
@DELEGATE_DECLARATION { lex' AlexRawToken_DELEGATE_DECLARATION }
@EXPLICIT_INTERFACE_SPECIFIER { lex' AlexRawToken_EXPLICIT_INTERFACE_SPECIFIER }
@PARAMETER_LIST { lex' AlexRawToken_PARAMETER_LIST }
@BRACKETED_PARAMETER_LIST { lex' AlexRawToken_BRACKETED_PARAMETER_LIST }
@BLOCK { lex' AlexRawToken_BLOCK }
@IF_STATEMENT { lex' AlexRawToken_IF_STATEMENT }
@EQUALS_EXPRESSION { lex' AlexRawToken_EQUALS_EXPRESSION }
@IS_EXPRESSION { lex' AlexRawToken_IS_EXPRESSION }
@AS_EXPRESSION { lex' AlexRawToken_AS_EXPRESSION }
@GREATER_THAN_EXPRESSION { lex' AlexRawToken_GREATER_THAN_EXPRESSION }
@GREATER_THAN_OR_EQUAL_EXPRESSION { lex' AlexRawToken_GREATER_THAN_OR_EQUAL_EXPRESSION }
@LESS_THAN_EXPRESSION { lex' AlexRawToken_LESS_THAN_EXPRESSION }
@LESS_THAN_OR_EQUAL_EXPRESSION { lex' AlexRawToken_LESS_THAN_OR_EQUAL_EXPRESSION }
@NOT_EQUALS_EXPRESSION { lex' AlexRawToken_NOT_EQUALS_EXPRESSION }
@LOGICAL_AND_EXPRESSION { lex' AlexRawToken_LOGICAL_AND_EXPRESSION }
@LOGICAL_OR_EXPRESSION { lex' AlexRawToken_LOGICAL_OR_EXPRESSION }
@BITWISE_OR_EXPRESSION { lex' AlexRawToken_BITWISE_OR_EXPRESSION }
@COALESCE_EXPRESSION { lex' AlexRawToken_COALESCE_EXPRESSION }
@ADD_EXPRESSION { lex' AlexRawToken_ADD_EXPRESSION }
@MULTIPLY_EXPRESSION { lex' AlexRawToken_MULTIPLY_EXPRESSION }
@SUBTRACT_EXPRESSION { lex' AlexRawToken_SUBTRACT_EXPRESSION }
@DIVIDE_EXPRESSION { lex' AlexRawToken_DIVIDE_EXPRESSION }
@COALESCE_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_COALESCE_ASSIGNMENT_EXPRESSION }
@ADD_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_ADD_ASSIGNMENT_EXPRESSION }
@CONDITIONAL_ACCESS_EXPRESSION { lex' AlexRawToken_CONDITIONAL_ACCESS_EXPRESSION }
@CONDITIONAL_EXPRESSION { lex' AlexRawToken_CONDITIONAL_EXPRESSION }
@RANGE_EXPRESSION { lex' AlexRawToken_RANGE_EXPRESSION }
@INDEX_EXPRESSION { lex' AlexRawToken_INDEX_EXPRESSION }
@NULL_LITERAL_EXPRESSION { lex' AlexRawToken_NULL_LITERAL_EXPRESSION }
@DEFAULT_LITERAL_EXPRESSION { lex' AlexRawToken_DEFAULT_LITERAL_EXPRESSION }
@WHILE_STATEMENT { lex' AlexRawToken_WHILE_STATEMENT }
@FOR_STATEMENT { lex' AlexRawToken_FOR_STATEMENT }
@TRY_STATEMENT { lex' AlexRawToken_TRY_STATEMENT }
@USING_STATEMENT { lex' AlexRawToken_USING_STATEMENT }
@LOCK_STATEMENT { lex' AlexRawToken_LOCK_STATEMENT }
@FOR_EACH_STATEMENT { lex' AlexRawToken_FOR_EACH_STATEMENT }
@FOR_EACH_VARIABLE_STATEMENT { lex' AlexRawToken_FOR_EACH_VARIABLE_STATEMENT }
@INVOCATION_EXPRESSION { lex' AlexRawToken_INVOCATION_EXPRESSION }
@MEMBER_BINDING_EXPRESSION { lex' AlexRawToken_MEMBER_BINDING_EXPRESSION }
@SIMPLE_MEMBER_ACCESS_EXPRESSION { lex' AlexRawToken_SIMPLE_MEMBER_ACCESS_EXPRESSION }
@EXPRESSION_STATEMENT { lex' AlexRawToken_EXPRESSION_STATEMENT }
@SIMPLE_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_SIMPLE_ASSIGNMENT_EXPRESSION }
@IS_PATTERN_EXPRESSION { lex' AlexRawToken_IS_PATTERN_EXPRESSION }
@DECLARATION_EXPRESSION { lex' AlexRawToken_DECLARATION_EXPRESSION }
@DECLARATION_PATTERN { lex' AlexRawToken_DECLARATION_PATTERN }
@NOT_PATTERN { lex' AlexRawToken_NOT_PATTERN }
@RECURSIVE_PATTERN { lex' AlexRawToken_RECURSIVE_PATTERN }
@OR_PATTERN { lex' AlexRawToken_OR_PATTERN }
@AND_PATTERN { lex' AlexRawToken_AND_PATTERN }
@CONSTANT_PATTERN { lex' AlexRawToken_CONSTANT_PATTERN }
@PROPERTY_PATTERN_CLAUSE { lex' AlexRawToken_PROPERTY_PATTERN_CLAUSE }
@SUBPATTERN { lex' AlexRawToken_SUBPATTERN }
@RELATIONAL_PATTERN { lex' AlexRawToken_RELATIONAL_PATTERN }
@SINGLE_VARIABLE_DESIGNATION { lex' AlexRawToken_SINGLE_VARIABLE_DESIGNATION }
@PARENTHESIZED_VARIABLE_DESIGNATION { lex' AlexRawToken_PARENTHESIZED_VARIABLE_DESIGNATION }
@DISCARD_DESIGNATION { lex' AlexRawToken_DISCARD_DESIGNATION }
@ELSE_CLAUSE { lex' AlexRawToken_ELSE_CLAUSE }
@CATCH_CLAUSE { lex' AlexRawToken_CATCH_CLAUSE }
@CATCH_DECLARATION { lex' AlexRawToken_CATCH_DECLARATION }
@CATCH_FILTER_CLAUSE { lex' AlexRawToken_CATCH_FILTER_CLAUSE }
@FINALLY_CLAUSE { lex' AlexRawToken_FINALLY_CLAUSE }
@WHEN_CLAUSE { lex' AlexRawToken_WHEN_CLAUSE }
@BREAK_STATEMENT { lex' AlexRawToken_BREAK_STATEMENT }
@CONTINUE_STATEMENT { lex' AlexRawToken_CONTINUE_STATEMENT }
@RETURN_STATEMENT { lex' AlexRawToken_RETURN_STATEMENT }
@YIELD_BREAK_STATEMENT { lex' AlexRawToken_YIELD_BREAK_STATEMENT }
@YIELD_RETURN_STATEMENT { lex' AlexRawToken_YIELD_RETURN_STATEMENT }
@CONSTRUCTOR_DECLARATION { lex' AlexRawToken_CONSTRUCTOR_DECLARATION }
@PARAMETER { lex' AlexRawToken_PARAMETER }
@BASE_CONSTRUCTOR_INITIALIZER { lex' AlexRawToken_BASE_CONSTRUCTOR_INITIALIZER }
@THIS_CONSTRUCTOR_INITIALIZER { lex' AlexRawToken_THIS_CONSTRUCTOR_INITIALIZER }
@THIS_EXPRESSION { lex' AlexRawToken_THIS_EXPRESSION }
@BASE_EXPRESSION { lex' AlexRawToken_BASE_EXPRESSION }
@PROPERTY_DECLARATION { lex' AlexRawToken_PROPERTY_DECLARATION }
@INDEXER_DECLARATION { lex' AlexRawToken_INDEXER_DECLARATION }
@ACCESSOR_LIST { lex' AlexRawToken_ACCESSOR_LIST }
@GET_ACCESSOR_DECLARATION { lex' AlexRawToken_GET_ACCESSOR_DECLARATION }
@SET_ACCESSOR_DECLARATION { lex' AlexRawToken_SET_ACCESSOR_DECLARATION }
@INIT_ACCESSOR_DECLARATION { lex' AlexRawToken_INIT_ACCESSOR_DECLARATION }
@PREDEFINED_TYPE { lex' AlexRawToken_PREDEFINED_TYPE }
@FALSE_LITERAL_EXPRESSION { lex' AlexRawToken_FALSE_LITERAL_EXPRESSION }
@TRUE_LITERAL_EXPRESSION { lex' AlexRawToken_TRUE_LITERAL_EXPRESSION }
@NUMERIC_LITERAL_EXPRESSION { lex' AlexRawToken_NUMERIC_LITERAL_EXPRESSION }
@CHARACTER_LITERAL_EXPRESSION { lex' AlexRawToken_CHARACTER_LITERAL_EXPRESSION }
@STRING_LITERAL_EXPRESSION { lex' AlexRawToken_STRING_LITERAL_EXPRESSION }
@INTERPOLATED_STRING_EXPRESSION { lex' AlexRawToken_INTERPOLATED_STRING_EXPRESSION }
@INTERPOLATED_STRING_TEXT { lex' AlexRawToken_INTERPOLATED_STRING_TEXT }
@INTERPOLATION { lex' AlexRawToken_INTERPOLATION }
@INTERPOLATION_FORMAT_CLAUSE { lex' AlexRawToken_INTERPOLATION_FORMAT_CLAUSE }
@SWITCH_STATEMENT { lex' AlexRawToken_SWITCH_STATEMENT }
@SWITCH_SECTION { lex' AlexRawToken_SWITCH_SECTION }
@CASE_SWITCH_LABEL { lex' AlexRawToken_CASE_SWITCH_LABEL }
@CASE_PATTERN_SWITCH_LABEL { lex' AlexRawToken_CASE_PATTERN_SWITCH_LABEL }
@DEFAULT_SWITCH_LABEL { lex' AlexRawToken_DEFAULT_SWITCH_LABEL }
@THROW_STATEMENT { lex' AlexRawToken_THROW_STATEMENT }
@THROW_EXPRESSION { lex' AlexRawToken_THROW_EXPRESSION }
@LOCAL_DECLARATION_STATEMENT { lex' AlexRawToken_LOCAL_DECLARATION_STATEMENT }
@LOGICAL_NOT_EXPRESSION { lex' AlexRawToken_LOGICAL_NOT_EXPRESSION }
@POST_INCREMENT_EXPRESSION { lex' AlexRawToken_POST_INCREMENT_EXPRESSION }
@POST_DECREMENT_EXPRESSION { lex' AlexRawToken_POST_DECREMENT_EXPRESSION }
@PARENTHESIZED_EXPRESSION { lex' AlexRawToken_PARENTHESIZED_EXPRESSION }
@CAST_EXPRESSION { lex' AlexRawToken_CAST_EXPRESSION }
@SIMPLE_LAMBDA_EXPRESSION { lex' AlexRawToken_SIMPLE_LAMBDA_EXPRESSION }
@PARENTHESIZED_LAMBDA_EXPRESSION { lex' AlexRawToken_PARENTHESIZED_LAMBDA_EXPRESSION }
@ARROW_EXPRESSION_CLAUSE { lex' AlexRawToken_ARROW_EXPRESSION_CLAUSE }
@AWAIT_EXPRESSION { lex' AlexRawToken_AWAIT_EXPRESSION }
@TYPE_OF_EXPRESSION { lex' AlexRawToken_TYPE_OF_EXPRESSION }
@TUPLE_EXPRESSION { lex' AlexRawToken_TUPLE_EXPRESSION }
@TUPLE_TYPE { lex' AlexRawToken_TUPLE_TYPE }
@TUPLE_ELEMENT { lex' AlexRawToken_TUPLE_ELEMENT }
@SUPPRESS_NULLABLE_WARNING_EXPRESSION { lex' AlexRawToken_SUPPRESS_NULLABLE_WARNING_EXPRESSION }
@UNARY_MINUS_EXPRESSION { lex' AlexRawToken_UNARY_MINUS_EXPRESSION }

-- ***************************
-- *                         *
-- * whitespace ? do nothing *
-- *                         *
-- ***************************

@WHITE_SPACE ;

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************
@INT { lex (AlexRawToken_INT . round . read) }
@ID { lex (AlexRawToken_ID . unquote) }
-- anything else ( a kind with no token yet ) fails at its first char
. { lexicalError }

{

-- | According to [the docs][1] (emphasis mine):
--
-- > `AlexUserState` /must/ be defined in the user's program
--
-- [1]: https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
data AlexUserState = AlexUserState { filepath :: FilePath, additional_repo_info :: Common.AdditionalRepoInfo } deriving ( Show )

-- | According to [the docs][1] (emphasis mine):
--
-- > a call to an initialization function (`alexInitUserState`) ...
-- > must also be defined in the user's program
--
-- [1]: https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" (Common.AdditionalRepoInfo [] [] Nothing [])

-- | getter of the AlexUserState
-- this is w.r.t to alexGetUserState :: Alex AlexUserState
-- according to [the docs][1]
--
-- [1]: https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
getFilePath :: Alex FilePath
getFilePath = filepath <$> alexGetUserState

-- | setter of the AlexUserState
-- this is w.r.t to alexSetUserState :: AlexUserState -> Alex ()
-- according to [the docs][1]
--
-- [1]: https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
setFilePath :: FilePath -> Alex ()
setFilePath fp = do
  state <- alexGetUserState
  alexSetUserState (AlexUserState { filepath = fp, additional_repo_info = additional_repo_info state })

-- *********
-- *       *
-- * Token *
-- *       *
-- *********
data AlexTokenTag
   = AlexTokenTag
     {
         tokenRaw :: AlexRawToken,
         tokenLoc :: Location
     }
     deriving ( Show )

-- *************
-- *           *
-- * Raw Token *
-- *           *
-- *************
data AlexRawToken

     -- ********************
     -- *                  *
     -- * integers and ids *
     -- *                  *
     -- ********************

     = AlexRawToken_INT Int
     | AlexRawToken_ID String

     -- ************
     -- *          *
     -- * keywords *
     -- *          *
     -- ************

     | AlexRawToken_KIND
     | AlexRawToken_NULL
     | AlexRawToken_VALUE
     | AlexRawToken_LOCATION
     | AlexRawToken_START_LINE
     | AlexRawToken_START_COLUMN
     | AlexRawToken_END_LINE
     | AlexRawToken_END_COLUMN
     | AlexRawToken_CHILDREN
     | AlexRawToken_COMPILATION_UNIT
     | AlexRawToken_USING_DIRECTIVE
     | AlexRawToken_IDENTIFIER_NAME
     | AlexRawToken_QUALIFIED_NAME
     | AlexRawToken_ALIAS_QUALIFIED_NAME
     | AlexRawToken_NAME_EQUALS
     | AlexRawToken_NAME_COLON
     | AlexRawToken_NAMESPACE_DECLARATION
     | AlexRawToken_FILE_SCOPED_NAMESPACE_DECLARATION
     | AlexRawToken_CLASS_DECLARATION
     | AlexRawToken_EMPTY_STATEMENT
     | AlexRawToken_GOTO_DEFAULT_STATEMENT
     | AlexRawToken_GOTO_CASE_STATEMENT
     | AlexRawToken_GOTO_STATEMENT
     | AlexRawToken_LABELED_STATEMENT
     | AlexRawToken_CONVERSION_OPERATOR_DECLARATION
     | AlexRawToken_OPERATOR_DECLARATION
     | AlexRawToken_SLICE_PATTERN
     | AlexRawToken_LIST_PATTERN
     | AlexRawToken_VAR_PATTERN
     | AlexRawToken_POSITIONAL_PATTERN_CLAUSE
     | AlexRawToken_DEFAULT_EXPRESSION
     | AlexRawToken_GLOBAL_STATEMENT
     | AlexRawToken_UNSIGNED_RIGHT_SHIFT_EXPRESSION
     | AlexRawToken_RIGHT_SHIFT_EXPRESSION
     | AlexRawToken_LEFT_SHIFT_EXPRESSION
     | AlexRawToken_EXCLUSIVE_OR_EXPRESSION
     | AlexRawToken_BITWISE_AND_EXPRESSION
     | AlexRawToken_INTERPOLATION_ALIGNMENT_CLAUSE
     | AlexRawToken_EXTENSION_BLOCK_DECLARATION
     | AlexRawToken_MODULO_ASSIGNMENT_EXPRESSION
     | AlexRawToken_RIGHT_SHIFT_ASSIGNMENT_EXPRESSION
     | AlexRawToken_LEFT_SHIFT_ASSIGNMENT_EXPRESSION
     | AlexRawToken_EXCLUSIVE_OR_ASSIGNMENT_EXPRESSION
     | AlexRawToken_AND_ASSIGNMENT_EXPRESSION
     | AlexRawToken_OR_ASSIGNMENT_EXPRESSION
     | AlexRawToken_STACK_ALLOC_ARRAY_CREATION_EXPRESSION
     | AlexRawToken_PRE_DECREMENT_EXPRESSION
     | AlexRawToken_PRE_INCREMENT_EXPRESSION
     | AlexRawToken_DO_STATEMENT
     | AlexRawToken_PARENTHESIZED_PATTERN
     | AlexRawToken_ATTRIBUTE_TARGET_SPECIFIER
     | AlexRawToken_EVENT_FIELD_DECLARATION
     | AlexRawToken_DIVIDE_ASSIGNMENT_EXPRESSION
     | AlexRawToken_MULTIPLY_ASSIGNMENT_EXPRESSION
     | AlexRawToken_SUBTRACT_ASSIGNMENT_EXPRESSION
     | AlexRawToken_ELEMENT_BINDING_EXPRESSION
     | AlexRawToken_MODULO_EXPRESSION
     | AlexRawToken_COMPLEX_ELEMENT_INITIALIZER_EXPRESSION
     | AlexRawToken_IMPLICIT_ARRAY_CREATION_EXPRESSION
     | AlexRawToken_ARRAY_INITIALIZER_EXPRESSION
     | AlexRawToken_QUERY_CONTINUATION
     | AlexRawToken_GROUP_CLAUSE
     | AlexRawToken_SELECT_CLAUSE
     | AlexRawToken_DESCENDING_ORDERING
     | AlexRawToken_ASCENDING_ORDERING
     | AlexRawToken_ORDER_BY_CLAUSE
     | AlexRawToken_WHERE_CLAUSE
     | AlexRawToken_LET_CLAUSE
     | AlexRawToken_JOIN_INTO_CLAUSE
     | AlexRawToken_JOIN_CLAUSE
     | AlexRawToken_QUERY_BODY
     | AlexRawToken_FROM_CLAUSE
     | AlexRawToken_QUERY_EXPRESSION
     | AlexRawToken_TYPE_PATTERN
     | AlexRawToken_DISCARD_PATTERN
     | AlexRawToken_SWITCH_EXPRESSION_ARM
     | AlexRawToken_SWITCH_EXPRESSION
     | AlexRawToken_INTERFACE_DECLARATION
     | AlexRawToken_ENUM_DECLARATION
     | AlexRawToken_ENUM_MEMBER_DECLARATION
     | AlexRawToken_RECORD_DECLARATION
     | AlexRawToken_RECORD_STRUCT_DECLARATION
     | AlexRawToken_STRUCT_DECLARATION
     | AlexRawToken_PRIMARY_CONSTRUCTOR_BASE_TYPE
     | AlexRawToken_ATTRIBUTE_ARGUMENT
     | AlexRawToken_ATTRIBUTE_ARGUMENT_LIST
     | AlexRawToken_ATTRIBUTE
     | AlexRawToken_ATTRIBUTE_LIST
     | AlexRawToken_SIMPLE_BASE_TYPE
     | AlexRawToken_BASE_LIST
     | AlexRawToken_ARGUMENT_LIST
     | AlexRawToken_OBJECT_CREATION_EXPRESSION
     | AlexRawToken_ARRAY_CREATION_EXPRESSION
     | AlexRawToken_IMPLICIT_OBJECT_CREATION_EXPRESSION
     | AlexRawToken_OBJECT_INITIALIZER_EXPRESSION
     | AlexRawToken_COLLECTION_INITIALIZER_EXPRESSION
     | AlexRawToken_WITH_EXPRESSION
     | AlexRawToken_WITH_INITIALIZER_EXPRESSION
     | AlexRawToken_ANONYMOUS_OBJECT_CREATION_EXPRESSION
     | AlexRawToken_ANONYMOUS_OBJECT_MEMBER_DECLARATOR
     | AlexRawToken_COLLECTION_EXPRESSION
     | AlexRawToken_EXPRESSION_ELEMENT
     | AlexRawToken_SPREAD_ELEMENT
     | AlexRawToken_EQUALS_VALUE_CLAUSE
     | AlexRawToken_VARIABLE_DECLARATOR
     | AlexRawToken_TYPE_ARGUMENT_LIST
     | AlexRawToken_OMITTED_TYPE_ARGUMENT
     | AlexRawToken_TYPE_PARAMETER_LIST
     | AlexRawToken_TYPE_PARAMETER
     | AlexRawToken_TYPE_PARAMETER_CONSTRAINT_CLAUSE
     | AlexRawToken_TYPE_CONSTRAINT
     | AlexRawToken_CLASS_CONSTRAINT
     | AlexRawToken_STRUCT_CONSTRAINT
     | AlexRawToken_CONSTRUCTOR_CONSTRAINT
     | AlexRawToken_GENERIC_NAME
     | AlexRawToken_VARIABLE_DECLARATION
     | AlexRawToken_FIELD_DECLARATION
     | AlexRawToken_ELEMENT_ACCESS_EXPRESSION
     | AlexRawToken_IMPLICIT_ELEMENT_ACCESS
     | AlexRawToken_BRACKETED_ARGUMENT_LIST
     | AlexRawToken_ARGUMENT
     | AlexRawToken_NULLABLE_TYPE
     | AlexRawToken_ARRAY_TYPE
     | AlexRawToken_ARRAY_RANK_SPECIFIER
     | AlexRawToken_OMITTED_ARRAY_SIZE_EXPRESSION
     | AlexRawToken_METHOD_DECLARATION
     | AlexRawToken_LOCAL_FUNCTION_STATEMENT
     | AlexRawToken_DELEGATE_DECLARATION
     | AlexRawToken_EXPLICIT_INTERFACE_SPECIFIER
     | AlexRawToken_PARAMETER_LIST
     | AlexRawToken_BRACKETED_PARAMETER_LIST
     | AlexRawToken_BLOCK
     | AlexRawToken_IF_STATEMENT
     | AlexRawToken_EQUALS_EXPRESSION
     | AlexRawToken_IS_EXPRESSION
     | AlexRawToken_AS_EXPRESSION
     | AlexRawToken_GREATER_THAN_EXPRESSION
     | AlexRawToken_GREATER_THAN_OR_EQUAL_EXPRESSION
     | AlexRawToken_LESS_THAN_EXPRESSION
     | AlexRawToken_LESS_THAN_OR_EQUAL_EXPRESSION
     | AlexRawToken_NOT_EQUALS_EXPRESSION
     | AlexRawToken_LOGICAL_AND_EXPRESSION
     | AlexRawToken_LOGICAL_OR_EXPRESSION
     | AlexRawToken_BITWISE_OR_EXPRESSION
     | AlexRawToken_COALESCE_EXPRESSION
     | AlexRawToken_ADD_EXPRESSION
     | AlexRawToken_MULTIPLY_EXPRESSION
     | AlexRawToken_SUBTRACT_EXPRESSION
     | AlexRawToken_DIVIDE_EXPRESSION
     | AlexRawToken_COALESCE_ASSIGNMENT_EXPRESSION
     | AlexRawToken_ADD_ASSIGNMENT_EXPRESSION
     | AlexRawToken_CONDITIONAL_ACCESS_EXPRESSION
     | AlexRawToken_CONDITIONAL_EXPRESSION
     | AlexRawToken_RANGE_EXPRESSION
     | AlexRawToken_INDEX_EXPRESSION
     | AlexRawToken_NULL_LITERAL_EXPRESSION
     | AlexRawToken_DEFAULT_LITERAL_EXPRESSION
     | AlexRawToken_WHILE_STATEMENT
     | AlexRawToken_FOR_STATEMENT
     | AlexRawToken_TRY_STATEMENT
     | AlexRawToken_USING_STATEMENT
     | AlexRawToken_LOCK_STATEMENT
     | AlexRawToken_FOR_EACH_STATEMENT
     | AlexRawToken_FOR_EACH_VARIABLE_STATEMENT
     | AlexRawToken_INVOCATION_EXPRESSION
     | AlexRawToken_MEMBER_BINDING_EXPRESSION
     | AlexRawToken_SIMPLE_MEMBER_ACCESS_EXPRESSION
     | AlexRawToken_EXPRESSION_STATEMENT
     | AlexRawToken_SIMPLE_ASSIGNMENT_EXPRESSION
     | AlexRawToken_IS_PATTERN_EXPRESSION
     | AlexRawToken_DECLARATION_EXPRESSION
     | AlexRawToken_DECLARATION_PATTERN
     | AlexRawToken_NOT_PATTERN
     | AlexRawToken_RECURSIVE_PATTERN
     | AlexRawToken_OR_PATTERN
     | AlexRawToken_AND_PATTERN
     | AlexRawToken_CONSTANT_PATTERN
     | AlexRawToken_PROPERTY_PATTERN_CLAUSE
     | AlexRawToken_SUBPATTERN
     | AlexRawToken_RELATIONAL_PATTERN
     | AlexRawToken_SINGLE_VARIABLE_DESIGNATION
     | AlexRawToken_PARENTHESIZED_VARIABLE_DESIGNATION
     | AlexRawToken_DISCARD_DESIGNATION
     | AlexRawToken_ELSE_CLAUSE
     | AlexRawToken_CATCH_CLAUSE
     | AlexRawToken_CATCH_DECLARATION
     | AlexRawToken_CATCH_FILTER_CLAUSE
     | AlexRawToken_FINALLY_CLAUSE
     | AlexRawToken_WHEN_CLAUSE
     | AlexRawToken_BREAK_STATEMENT
     | AlexRawToken_CONTINUE_STATEMENT
     | AlexRawToken_RETURN_STATEMENT
     | AlexRawToken_YIELD_BREAK_STATEMENT
     | AlexRawToken_YIELD_RETURN_STATEMENT
     | AlexRawToken_CONSTRUCTOR_DECLARATION
     | AlexRawToken_PARAMETER
     | AlexRawToken_BASE_CONSTRUCTOR_INITIALIZER
     | AlexRawToken_THIS_CONSTRUCTOR_INITIALIZER
     | AlexRawToken_THIS_EXPRESSION
     | AlexRawToken_BASE_EXPRESSION
     | AlexRawToken_PROPERTY_DECLARATION
     | AlexRawToken_INDEXER_DECLARATION
     | AlexRawToken_ACCESSOR_LIST
     | AlexRawToken_GET_ACCESSOR_DECLARATION
     | AlexRawToken_SET_ACCESSOR_DECLARATION
     | AlexRawToken_INIT_ACCESSOR_DECLARATION
     | AlexRawToken_PREDEFINED_TYPE
     | AlexRawToken_FALSE_LITERAL_EXPRESSION
     | AlexRawToken_TRUE_LITERAL_EXPRESSION
     | AlexRawToken_NUMERIC_LITERAL_EXPRESSION
     | AlexRawToken_CHARACTER_LITERAL_EXPRESSION
     | AlexRawToken_STRING_LITERAL_EXPRESSION
     | AlexRawToken_INTERPOLATED_STRING_EXPRESSION
     | AlexRawToken_INTERPOLATED_STRING_TEXT
     | AlexRawToken_INTERPOLATION
     | AlexRawToken_INTERPOLATION_FORMAT_CLAUSE
     | AlexRawToken_SWITCH_STATEMENT
     | AlexRawToken_SWITCH_SECTION
     | AlexRawToken_CASE_SWITCH_LABEL
     | AlexRawToken_CASE_PATTERN_SWITCH_LABEL
     | AlexRawToken_DEFAULT_SWITCH_LABEL
     | AlexRawToken_THROW_STATEMENT
     | AlexRawToken_THROW_EXPRESSION
     | AlexRawToken_LOCAL_DECLARATION_STATEMENT
     | AlexRawToken_LOGICAL_NOT_EXPRESSION
     | AlexRawToken_POST_INCREMENT_EXPRESSION
     | AlexRawToken_POST_DECREMENT_EXPRESSION
     | AlexRawToken_PARENTHESIZED_EXPRESSION
     | AlexRawToken_CAST_EXPRESSION
     | AlexRawToken_SIMPLE_LAMBDA_EXPRESSION
     | AlexRawToken_PARENTHESIZED_LAMBDA_EXPRESSION
     | AlexRawToken_ARROW_EXPRESSION_CLAUSE
     | AlexRawToken_AWAIT_EXPRESSION
     | AlexRawToken_TYPE_OF_EXPRESSION
     | AlexRawToken_TUPLE_EXPRESSION
     | AlexRawToken_TUPLE_TYPE
     | AlexRawToken_TUPLE_ELEMENT
     | AlexRawToken_SUPPRESS_NULLABLE_WARNING_EXPRESSION
     | AlexRawToken_UNARY_MINUS_EXPRESSION

     -- ***************
     -- *             *
     -- * parentheses *
     -- *             *
     -- ***************

     | AlexRawToken_LPAREN
     | AlexRawToken_RPAREN
     | AlexRawToken_LBRACK
     | AlexRawToken_RBRACK
     | AlexRawToken_LBRACE
     | AlexRawToken_RBRACE

     -- ***************
     -- *             *
     -- * punctuation *
     -- *             *
     -- ***************

     | AlexRawToken_COLON
     | AlexRawToken_COMMA
 
     -- *******
     -- *     *
     -- * EOF *
     -- *     *
     -- *******
     | TokenEOF -- ^ [EOF](https://en.wikipedia.org/wiki/End-of-file)
 
     deriving ( Show )

-- ***********
-- * alexEOF *
-- ***********
alexEOF :: Alex AlexTokenTag
alexEOF = do
    ((AlexPn _ l c),_,_,_) <- alexGetInput
    alexUserState <- alexGetUserState
    return $
        AlexTokenTag
        {
            tokenRaw = TokenEOF,
            tokenLoc = Location {
                lineStart = fromIntegral l,
                lineEnd = fromIntegral l,
                colStart = fromIntegral c,
                colEnd = fromIntegral c,
                filename = (filepath alexUserState)
            }
        }

-- *******
-- *     *
-- * lex *
-- *     *
-- *******
lex :: (String -> AlexRawToken) -> AlexInput -> Int -> Alex AlexTokenTag
lex f ((AlexPn _ l c),_,_,str) i = do
    alexUserState <- alexGetUserState
    return $
        AlexTokenTag
        {
            tokenRaw = (f (take i str)),
            tokenLoc = Location {
                lineStart = fromIntegral l,
                lineEnd = fromIntegral l,
                colStart = fromIntegral c,
                colEnd = fromIntegral (c+i),
                filename = (filepath alexUserState)
            }
        }

-- *********************************************
-- * lex' for tokens WITHOUT associated values *
-- *********************************************
lex' :: AlexRawToken -> AlexInput -> Int -> Alex AlexTokenTag
lex' = lex . const

-- ****************
-- * lexicalError *
-- ****************
-- alex's own error reports the column after the first char of the unknown
-- word, a parse error on a ( new ) token there reports its start : fail at
-- the start, so adding a token never moves a failure one column back
lexicalError :: AlexInput -> Int -> Alex AlexTokenTag
lexicalError ((AlexPn _ l c),_,_,_) _ = do
    alexUserState <- alexGetUserState
    alexError' (Location (filepath alexUserState) (fromIntegral l) (fromIntegral l) (fromIntegral c) (fromIntegral c))

-- *************
-- * unquote   *
-- *************
-- strip the surrounding double quotes @ID matches ( "foo" -> foo )
unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

-- **************
-- * alexError' *
-- **************
alexError' :: Location -> Alex a
-- the bare location : Main.postFailed reads it back ( readMaybe )
alexError' location = alexError (show location)

-- ************
-- *          *
-- * location *
-- *          *
-- ************
location :: AlexTokenTag -> Location
location = tokenLoc

-- ***************
-- *             *
-- * getFilename *
-- *             *
-- ***************
getFilename :: AlexTokenTag -> String
getFilename = Location.filename . location

-- ***************
-- *             *
-- * tokIntValue *
-- *             *
-- ***************
tokIntValue :: AlexTokenTag -> Int
tokIntValue t = case (tokenRaw t) of { AlexRawToken_INT i -> i; _ -> 0; }

-- ***************
-- *             *
-- * tokIntValue *
-- *             *
-- ***************
tokIDValue :: AlexTokenTag -> String
tokIDValue t = case (tokenRaw t) of { AlexRawToken_ID s -> s; _ -> "" }

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> Common.AdditionalRepoInfo -> String -> Either String a
runAlex' a fp additionalInfo input = runAlex input (setFilePath fp >> alexSetUserState (AlexUserState fp additionalInfo) >> a)
}

