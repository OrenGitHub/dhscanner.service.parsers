{
-- | 
-- * The lexer takes an input source file, and returns a stream of
-- [tokens](https://en.wikipedia.org/wiki/Lexical_analysis#Token).
--
-- * The token data type ( `AlexTokenTag` ) contains the following content:
--
--     * source filename and exact location
--     * token lexical kind
--     * associated metadata (like `AlexRawToken_ID`
--     which holds the underlying identifier name)
-- 
-- * The /single/ client of the lexer code is the `parser` module,
--
--     * it consumes the tokens' stream and gradually builds the [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
--     * when doing so, the parser only inspects the /lexical kind/ of the token
--     * it /completely ignores/ any other infromation in the token
--

{-# OPTIONS -w  #-}

module TsLexer

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

-- ***********************
-- *                     *
-- * regular expressions *
-- *                     *
-- ***********************

-- ***************
-- *             *
-- * parentheses *
-- *             *
-- ***************
@LPAREN = \(
@RPAREN = \)
@LBRACK = \[
@RBRACK = \]

-- ******************
-- *                *
-- * location stuff *
-- *                *
-- ******************
@COLON = ":"
@MINUS = \-
@COMMA = ","

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@KW_Unknown = Unknown
@KW_EndOfFileToken = EndOfFileToken
@KW_SingleLineCommentTrivia = SingleLineCommentTrivia
@KW_MultiLineCommentTrivia = MultiLineCommentTrivia
@KW_NewLineTrivia = NewLineTrivia
@KW_WhitespaceTrivia = WhitespaceTrivia
@KW_ShebangTrivia = ShebangTrivia
@KW_ConflictMarkerTrivia = ConflictMarkerTrivia
@KW_NonTextFileMarkerTrivia = NonTextFileMarkerTrivia
@KW_NumericLiteral = NumericLiteral
@KW_BigIntLiteral = BigIntLiteral
@KW_StringLiteral = StringLiteral
@KW_JsxText = JsxText
@KW_JsxTextAllWhiteSpaces = JsxTextAllWhiteSpaces
@KW_RegularExpressionLiteral = RegularExpressionLiteral
@KW_NoSubstitutionTemplateLiteral = NoSubstitutionTemplateLiteral
@KW_TemplateHead = TemplateHead
@KW_TemplateMiddle = TemplateMiddle
@KW_TemplateTail = TemplateTail
@KW_OpenBraceToken = OpenBraceToken
@KW_CloseBraceToken = CloseBraceToken
@KW_OpenParenToken = OpenParenToken
@KW_CloseParenToken = CloseParenToken
@KW_OpenBracketToken = OpenBracketToken
@KW_CloseBracketToken = CloseBracketToken
@KW_DotToken = DotToken
@KW_DotDotDotToken = DotDotDotToken
@KW_SemicolonToken = SemicolonToken
@KW_CommaToken = CommaToken
@KW_QuestionDotToken = QuestionDotToken
@KW_LessThanToken = LessThanToken
@KW_LessThanSlashToken = LessThanSlashToken
@KW_GreaterThanToken = GreaterThanToken
@KW_LessThanEqualsToken = LessThanEqualsToken
@KW_GreaterThanEqualsToken = GreaterThanEqualsToken
@KW_EqualsEqualsToken = EqualsEqualsToken
@KW_ExclamationEqualsToken = ExclamationEqualsToken
@KW_EqualsEqualsEqualsToken = EqualsEqualsEqualsToken
@KW_ExclamationEqualsEqualsToken = ExclamationEqualsEqualsToken
@KW_EqualsGreaterThanToken = EqualsGreaterThanToken
@KW_PlusToken = PlusToken
@KW_MinusToken = MinusToken
@KW_AsteriskToken = AsteriskToken
@KW_AsteriskAsteriskToken = AsteriskAsteriskToken
@KW_SlashToken = SlashToken
@KW_PercentToken = PercentToken
@KW_PlusPlusToken = PlusPlusToken
@KW_MinusMinusToken = MinusMinusToken
@KW_LessThanLessThanToken = LessThanLessThanToken
@KW_GreaterThanGreaterThanToken = GreaterThanGreaterThanToken
@KW_GreaterThanGreaterThanGreaterThanToken = GreaterThanGreaterThanGreaterThanToken
@KW_AmpersandToken = AmpersandToken
@KW_BarToken = BarToken
@KW_CaretToken = CaretToken
@KW_ExclamationToken = ExclamationToken
@KW_TildeToken = TildeToken
@KW_AmpersandAmpersandToken = AmpersandAmpersandToken
@KW_BarBarToken = BarBarToken
@KW_QuestionToken = QuestionToken
@KW_ColonToken = ColonToken
@KW_AtToken = AtToken
@KW_QuestionQuestionToken = QuestionQuestionToken
@KW_BacktickToken = BacktickToken
@KW_HashToken = HashToken
@KW_EqualsToken = EqualsToken
@KW_PlusEqualsToken = PlusEqualsToken
@KW_MinusEqualsToken = MinusEqualsToken
@KW_AsteriskEqualsToken = AsteriskEqualsToken
@KW_AsteriskAsteriskEqualsToken = AsteriskAsteriskEqualsToken
@KW_SlashEqualsToken = SlashEqualsToken
@KW_PercentEqualsToken = PercentEqualsToken
@KW_LessThanLessThanEqualsToken = LessThanLessThanEqualsToken
@KW_GreaterThanGreaterThanEqualsToken = GreaterThanGreaterThanEqualsToken
@KW_GreaterThanGreaterThanGreaterThanEqualsToken = GreaterThanGreaterThanGreaterThanEqualsToken
@KW_AmpersandEqualsToken = AmpersandEqualsToken
@KW_BarEqualsToken = BarEqualsToken
@KW_BarBarEqualsToken = BarBarEqualsToken
@KW_AmpersandAmpersandEqualsToken = AmpersandAmpersandEqualsToken
@KW_QuestionQuestionEqualsToken = QuestionQuestionEqualsToken
@KW_CaretEqualsToken = CaretEqualsToken
@KW_Identifier = Identifier
@KW_PrivateIdentifier = PrivateIdentifier
@KW_BreakKeyword = BreakKeyword
@KW_CaseKeyword = CaseKeyword
@KW_CatchKeyword = CatchKeyword
@KW_ClassKeyword = ClassKeyword
@KW_ConstKeyword = ConstKeyword
@KW_ContinueKeyword = ContinueKeyword
@KW_DebuggerKeyword = DebuggerKeyword
@KW_DefaultKeyword = DefaultKeyword
@KW_DeleteKeyword = DeleteKeyword
@KW_DoKeyword = DoKeyword
@KW_ElseKeyword = ElseKeyword
@KW_EnumKeyword = EnumKeyword
@KW_ExportKeyword = ExportKeyword
@KW_ExtendsKeyword = ExtendsKeyword
@KW_FalseKeyword = FalseKeyword
@KW_FinallyKeyword = FinallyKeyword
@KW_ForKeyword = ForKeyword
@KW_FunctionKeyword = FunctionKeyword
@KW_IfKeyword = IfKeyword
@KW_ImportKeyword = ImportKeyword
@KW_InKeyword = InKeyword
@KW_InstanceOfKeyword = InstanceOfKeyword
@KW_NewKeyword = NewKeyword
@KW_NullKeyword = NullKeyword
@KW_ReturnKeyword = ReturnKeyword
@KW_SuperKeyword = SuperKeyword
@KW_SwitchKeyword = SwitchKeyword
@KW_ThisKeyword = ThisKeyword
@KW_ThrowKeyword = ThrowKeyword
@KW_TrueKeyword = TrueKeyword
@KW_TryKeyword = TryKeyword
@KW_TypeOfKeyword = TypeOfKeyword
@KW_VarKeyword = VarKeyword
@KW_VoidKeyword = VoidKeyword
@KW_WhileKeyword = WhileKeyword
@KW_WithKeyword = WithKeyword
@KW_ImplementsKeyword = ImplementsKeyword
@KW_InterfaceKeyword = InterfaceKeyword
@KW_LetKeyword = LetKeyword
@KW_PackageKeyword = PackageKeyword
@KW_PrivateKeyword = PrivateKeyword
@KW_ProtectedKeyword = ProtectedKeyword
@KW_PublicKeyword = PublicKeyword
@KW_StaticKeyword = StaticKeyword
@KW_YieldKeyword = YieldKeyword
@KW_AbstractKeyword = AbstractKeyword
@KW_AccessorKeyword = AccessorKeyword
@KW_AsKeyword = AsKeyword
@KW_AssertsKeyword = AssertsKeyword
@KW_AssertKeyword = AssertKeyword
@KW_AnyKeyword = AnyKeyword
@KW_AsyncKeyword = AsyncKeyword
@KW_AwaitKeyword = AwaitKeyword
@KW_BooleanKeyword = BooleanKeyword
@KW_ConstructorKeyword = ConstructorKeyword
@KW_DeclareKeyword = DeclareKeyword
@KW_GetKeyword = GetKeyword
@KW_InferKeyword = InferKeyword
@KW_IntrinsicKeyword = IntrinsicKeyword
@KW_IsKeyword = IsKeyword
@KW_KeyOfKeyword = KeyOfKeyword
@KW_ModuleKeyword = ModuleKeyword
@KW_NamespaceKeyword = NamespaceKeyword
@KW_NeverKeyword = NeverKeyword
@KW_OutKeyword = OutKeyword
@KW_ReadonlyKeyword = ReadonlyKeyword
@KW_RequireKeyword = RequireKeyword
@KW_NumberKeyword = NumberKeyword
@KW_ObjectKeyword = ObjectKeyword
@KW_SatisfiesKeyword = SatisfiesKeyword
@KW_SetKeyword = SetKeyword
@KW_StringKeyword = StringKeyword
@KW_SymbolKeyword = SymbolKeyword
@KW_TypeKeyword = TypeKeyword
@KW_UndefinedKeyword = UndefinedKeyword
@KW_UniqueKeyword = UniqueKeyword
@KW_UnknownKeyword = UnknownKeyword
@KW_UsingKeyword = UsingKeyword
@KW_FromKeyword = FromKeyword
@KW_GlobalKeyword = GlobalKeyword
@KW_BigIntKeyword = BigIntKeyword
@KW_OverrideKeyword = OverrideKeyword
@KW_OfKeyword = OfKeyword
-- Alias for OfKeyword. TypeScript's SyntaxKind enum assigns the same
-- numeric value to `OfKeyword` and `LastContextualKeyword` (OfKeyword is
-- the last entry in the contextual-keyword range), so the reverse-lookup
-- `SyntaxKind[kind]` in frontts can emit either string depending on the
-- enum iteration order baked into the TypeScript version in use. Both
-- strings are kept as DISTINCT lexer tokens (mirrors how
-- `FirstTemplateToken` is lexed separately from `NoSubstitutionTemplateLiteral`
-- for the same reason) and the grammar collapses them in `ofKeyword`.
@KW_LastContextualKeyword = LastContextualKeyword
@KW_QualifiedName = QualifiedName
@KW_ComputedPropertyName = ComputedPropertyName
@KW_TypeParameter = TypeParameter
@KW_Parameter = Parameter
@KW_Decorator = Decorator
@KW_PropertySignature = PropertySignature
@KW_PropertyDeclaration = PropertyDeclaration
@KW_MethodSignature = MethodSignature
@KW_MethodDeclaration = MethodDeclaration
@KW_ClassStaticBlockDeclaration = ClassStaticBlockDeclaration
@KW_Constructor = Constructor
@KW_GetAccessor = GetAccessor
@KW_SetAccessor = SetAccessor
@KW_CallSignature = CallSignature
@KW_ConstructSignature = ConstructSignature
@KW_IndexSignature = IndexSignature
@KW_TypePredicate = TypePredicate
@KW_TypeReference = TypeReference
@KW_FunctionType = FunctionType
@KW_ConstructorType = ConstructorType
@KW_TypeQuery = TypeQuery
@KW_TypeLiteral = TypeLiteral
@KW_ArrayType = ArrayType
@KW_TupleType = TupleType
@KW_OptionalType = OptionalType
@KW_RestType = RestType
@KW_UnionType = UnionType
@KW_IntersectionType = IntersectionType
@KW_ConditionalType = ConditionalType
@KW_InferType = InferType
@KW_ParenthesizedType = ParenthesizedType
@KW_ThisType = ThisType
@KW_TypeOperator = TypeOperator
@KW_IndexedAccessType = IndexedAccessType
@KW_MappedType = MappedType
@KW_LiteralType = LiteralType
@KW_NamedTupleMember = NamedTupleMember
@KW_TemplateLiteralType = TemplateLiteralType
@KW_TemplateLiteralTypeSpan = TemplateLiteralTypeSpan
@KW_ImportType = ImportType
@KW_ObjectBindingPattern = ObjectBindingPattern
@KW_ArrayBindingPattern = ArrayBindingPattern
@KW_BindingElement = BindingElement
@KW_ArrayLiteralExpression = ArrayLiteralExpression
@KW_ObjectLiteralExpression = ObjectLiteralExpression
@KW_PropertyAccessExpression = PropertyAccessExpression
@KW_ElementAccessExpression = ElementAccessExpression
@KW_CallExpression = CallExpression
@KW_NewExpression = NewExpression
@KW_TaggedTemplateExpression = TaggedTemplateExpression
@KW_TypeAssertionExpression = TypeAssertionExpression
@KW_ParenthesizedExpression = ParenthesizedExpression
@KW_FunctionExpression = FunctionExpression
@KW_ArrowFunction = ArrowFunction
@KW_DeleteExpression = DeleteExpression
@KW_TypeOfExpression = TypeOfExpression
@KW_VoidExpression = VoidExpression
@KW_AwaitExpression = AwaitExpression
@KW_PrefixUnaryExpression = PrefixUnaryExpression
@KW_PostfixUnaryExpression = PostfixUnaryExpression
@KW_BinaryExpression = BinaryExpression
@KW_ConditionalExpression = ConditionalExpression
@KW_TemplateExpression = TemplateExpression
@KW_YieldExpression = YieldExpression
@KW_SpreadElement = SpreadElement
@KW_ClassExpression = ClassExpression
@KW_OmittedExpression = OmittedExpression
@KW_ExpressionWithTypeArguments = ExpressionWithTypeArguments
@KW_AsExpression = AsExpression
@KW_NonNullExpression = NonNullExpression
@KW_MetaProperty = MetaProperty
@KW_SyntheticExpression = SyntheticExpression
@KW_SatisfiesExpression = SatisfiesExpression
@KW_TemplateSpan = TemplateSpan
@KW_SemicolonClassElement = SemicolonClassElement
@KW_Block = Block
@KW_EmptyStatement = EmptyStatement
@KW_VariableStatement = VariableStatement
@KW_ExpressionStatement = ExpressionStatement
@KW_IfStatement = IfStatement
@KW_DoStatement = DoStatement
@KW_WhileStatement = WhileStatement
@KW_ForStatement = ForStatement
@KW_ForInStatement = ForInStatement
@KW_ForOfStatement = ForOfStatement
@KW_ContinueStatement = ContinueStatement
@KW_BreakStatement = BreakStatement
@KW_ReturnStatement = ReturnStatement
@KW_WithStatement = WithStatement
@KW_SwitchStatement = SwitchStatement
@KW_LabeledStatement = LabeledStatement
@KW_ThrowStatement = ThrowStatement
@KW_TryStatement = TryStatement
@KW_DebuggerStatement = DebuggerStatement
@KW_VariableDeclaration = VariableDeclaration
@KW_VariableDeclarationList = VariableDeclarationList
@KW_FunctionDeclaration = FunctionDeclaration
@KW_ClassDeclaration = ClassDeclaration
@KW_InterfaceDeclaration = InterfaceDeclaration
@KW_TypeAliasDeclaration = TypeAliasDeclaration
@KW_EnumDeclaration = EnumDeclaration
@KW_ModuleDeclaration = ModuleDeclaration
@KW_ModuleBlock = ModuleBlock
@KW_CaseBlock = CaseBlock
@KW_NamespaceExportDeclaration = NamespaceExportDeclaration
@KW_ImportEqualsDeclaration = ImportEqualsDeclaration
@KW_ImportDeclaration = ImportDeclaration
@KW_ImportClause = ImportClause
@KW_NamespaceImport = NamespaceImport
@KW_NamedImports = NamedImports
@KW_ImportSpecifier = ImportSpecifier
@KW_ExportAssignment = ExportAssignment
@KW_ExportDeclaration = ExportDeclaration
@KW_NamedExports = NamedExports
@KW_NamespaceExport = NamespaceExport
@KW_ExportSpecifier = ExportSpecifier
@KW_MissingDeclaration = MissingDeclaration
@KW_ExternalModuleReference = ExternalModuleReference
@KW_JsxElement = JsxElement
@KW_JsxSelfClosingElement = JsxSelfClosingElement
@KW_JsxOpeningElement = JsxOpeningElement
@KW_JsxClosingElement = JsxClosingElement
@KW_JsxFragment = JsxFragment
@KW_JsxOpeningFragment = JsxOpeningFragment
@KW_JsxClosingFragment = JsxClosingFragment
@KW_JsxAttribute = JsxAttribute
@KW_JsxAttributes = JsxAttributes
@KW_JsxSpreadAttribute = JsxSpreadAttribute
@KW_JsxExpression = JsxExpression
@KW_JsxNamespacedName = JsxNamespacedName
@KW_CaseClause = CaseClause
@KW_DefaultClause = DefaultClause
@KW_HeritageClause = HeritageClause
@KW_CatchClause = CatchClause
@KW_ImportAttributes = ImportAttributes
@KW_ImportAttribute = ImportAttribute
@KW_AssertClause = AssertClause
@KW_AssertEntry = AssertEntry
@KW_ImportTypeAssertionContainer = ImportTypeAssertionContainer
@KW_PropertyAssignment = PropertyAssignment
@KW_ShorthandPropertyAssignment = ShorthandPropertyAssignment
@KW_SpreadAssignment = SpreadAssignment
@KW_EnumMember = EnumMember
@KW_SourceFile = SourceFile
@KW_Bundle = Bundle
@KW_JSDocTypeExpression = JSDocTypeExpression
@KW_JSDocNameReference = JSDocNameReference
@KW_JSDocMemberName = JSDocMemberName
@KW_JSDocAllType = JSDocAllType
@KW_JSDocUnknownType = JSDocUnknownType
@KW_JSDocNullableType = JSDocNullableType
@KW_JSDocNonNullableType = JSDocNonNullableType
@KW_JSDocOptionalType = JSDocOptionalType
@KW_JSDocFunctionType = JSDocFunctionType
@KW_JSDocVariadicType = JSDocVariadicType
@KW_JSDocNamepathType = JSDocNamepathType
@KW_JSDoc = JSDoc
@KW_JSDocComment = JSDocComment
@KW_JSDocText = JSDocText
@KW_JSDocTypeLiteral = JSDocTypeLiteral
@KW_JSDocSignature = JSDocSignature
@KW_JSDocLink = JSDocLink
@KW_JSDocLinkCode = JSDocLinkCode
@KW_JSDocLinkPlain = JSDocLinkPlain
@KW_JSDocTag = JSDocTag
@KW_JSDocAugmentsTag = JSDocAugmentsTag
@KW_JSDocImplementsTag = JSDocImplementsTag
@KW_JSDocAuthorTag = JSDocAuthorTag
@KW_JSDocDeprecatedTag = JSDocDeprecatedTag
@KW_JSDocClassTag = JSDocClassTag
@KW_JSDocPublicTag = JSDocPublicTag
@KW_JSDocPrivateTag = JSDocPrivateTag
@KW_JSDocProtectedTag = JSDocProtectedTag
@KW_JSDocReadonlyTag = JSDocReadonlyTag
@KW_JSDocOverrideTag = JSDocOverrideTag
@KW_JSDocCallbackTag = JSDocCallbackTag
@KW_JSDocOverloadTag = JSDocOverloadTag
@KW_JSDocEnumTag = JSDocEnumTag
@KW_JSDocParameterTag = JSDocParameterTag
@KW_JSDocReturnTag = JSDocReturnTag
@KW_JSDocThisTag = JSDocThisTag
@KW_JSDocTypeTag = JSDocTypeTag
@KW_JSDocTemplateTag = JSDocTemplateTag
@KW_JSDocTypedefTag = JSDocTypedefTag
@KW_JSDocSeeTag = JSDocSeeTag
@KW_JSDocPropertyTag = JSDocPropertyTag
@KW_JSDocThrowsTag = JSDocThrowsTag
@KW_JSDocSatisfiesTag = JSDocSatisfiesTag
@KW_JSDocImportTag = JSDocImportTag
@KW_SyntaxList = SyntaxList
@KW_NotEmittedStatement = NotEmittedStatement
@KW_PartiallyEmittedExpression = PartiallyEmittedExpression
@KW_CommaListExpression = CommaListExpression
@KW_SyntheticReferenceExpression = SyntheticReferenceExpression
@KW_Count = Count
@KW_FirstAssignment = FirstAssignment
@KW_LastAssignment = LastAssignment
@KW_FirstCompoundAssignment = FirstCompoundAssignment
@KW_LastCompoundAssignment = LastCompoundAssignment
@KW_FirstReservedWord = FirstReservedWord
@KW_LastReservedWord = LastReservedWord
@KW_FirstKeyword = FirstKeyword
@KW_LastKeyword = LastKeyword
@KW_FirstFutureReservedWord = FirstFutureReservedWord
@KW_LastFutureReservedWord = LastFutureReservedWord
@KW_FirstTypeNode = FirstTypeNode
@KW_LastTypeNode = LastTypeNode
@KW_FirstPunctuation = FirstPunctuation
@KW_LastPunctuation = LastPunctuation
@KW_FirstToken = FirstToken
@KW_LastToken = LastToken
@KW_FirstTriviaToken = FirstTriviaToken
@KW_LastTriviaToken = LastTriviaToken
@KW_FirstLiteralToken = FirstLiteralToken
@KW_LastLiteralToken = LastLiteralToken
@KW_FirstTemplateToken = FirstTemplateToken
@KW_LastTemplateToken = LastTemplateToken
@KW_FirstBinaryOperator = FirstBinaryOperator
@KW_LastBinaryOperator = LastBinaryOperator
@KW_FirstStatement = FirstStatement
@KW_LastStatement = LastStatement
@KW_FirstNode = FirstNode
@KW_FirstJSDocNode = FirstJSDocNode
@KW_LastJSDocNode = LastJSDocNode
@KW_FirstJSDocTagNode = FirstJSDocTagNode
@KW_LastJSDocTagNode = LastJSDocTagNode

-- ************
-- *          *
-- * integers *
-- *          *
-- ************
@DIGIT = 0-9
@INT   = @DIGIT+

-- ***************
-- *             *
-- * identifiers *
-- *             *
-- ***************
-- `$` is a valid JS / TS identifier character (both as the first char and
-- inside the identifier -- see the TC39 spec, IdentifierStart /
-- IdentifierPart). Widely used in real-world APIs: Prisma's reserved
-- methods `$queryRaw` / `$queryRawUnsafe` / `$executeRaw` /
-- `$executeRawUnsafe` / `$transaction`, jQuery's `$`, nullable-propagation
-- synthetic names (`$ref`), etc. Without `$` here, frontts-emitted
-- `Identifier(...($queryRaw))` tokens hit a lexical error, which turns
-- into a `(1,1,1,1)` sentinel downstream (because `postFailed` can only
-- parse structured `Location`-shaped error messages, so the lexical-error
-- message string collapses to a defaultLoc). Adding `$` is a strict
-- superset: no previously-tokenizable input lexes differently now, and a
-- new class of identifiers now tokenizes.
@LETTER = [A-Za-z_\$]
@LETTER_OR_DIGIT = @LETTER | @DIGIT
@ID = (@LETTER)(@LETTER_OR_DIGIT*)
@QUOTE = [\'\"]
@NOQUOTE = [^\'\"]*
@STR = (@QUOTE)(@NOQUOTE)(@QUOTE)

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

@LPAREN    { lex' AlexRawToken_LPAREN }
@RPAREN    { lex' AlexRawToken_RPAREN }
@LBRACK    { lex' AlexRawToken_LBRACK }
@RBRACK    { lex' AlexRawToken_RBRACK }

-- ************
-- *          *
-- * location *
-- *          *
-- ************

@COLON     { lex' AlexRawToken_COLON }
@COMMA     { lex' AlexRawToken_COMMA }
@MINUS     { lex' AlexRawToken_MINUS }

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@KW_Unknown { lex' AlexRawToken_Unknown }
@KW_EndOfFileToken { lex' AlexRawToken_EndOfFileToken }
@KW_SingleLineCommentTrivia { lex' AlexRawToken_SingleLineCommentTrivia }
@KW_MultiLineCommentTrivia { lex' AlexRawToken_MultiLineCommentTrivia }
@KW_NewLineTrivia { lex' AlexRawToken_NewLineTrivia }
@KW_WhitespaceTrivia { lex' AlexRawToken_WhitespaceTrivia }
@KW_ShebangTrivia { lex' AlexRawToken_ShebangTrivia }
@KW_ConflictMarkerTrivia { lex' AlexRawToken_ConflictMarkerTrivia }
@KW_NonTextFileMarkerTrivia { lex' AlexRawToken_NonTextFileMarkerTrivia }
@KW_NumericLiteral { lex' AlexRawToken_NumericLiteral }
@KW_BigIntLiteral { lex' AlexRawToken_BigIntLiteral }
@KW_StringLiteral { lex' AlexRawToken_StringLiteral }
@KW_JsxText { lex' AlexRawToken_JsxText }
@KW_JsxTextAllWhiteSpaces { lex' AlexRawToken_JsxTextAllWhiteSpaces }
@KW_RegularExpressionLiteral { lex' AlexRawToken_RegularExpressionLiteral }
@KW_NoSubstitutionTemplateLiteral { lex' AlexRawToken_NoSubstitutionTemplateLiteral }
@KW_TemplateHead { lex' AlexRawToken_TemplateHead }
@KW_TemplateMiddle { lex' AlexRawToken_TemplateMiddle }
@KW_TemplateTail { lex' AlexRawToken_TemplateTail }
@KW_OpenBraceToken { lex' AlexRawToken_OpenBraceToken }
@KW_CloseBraceToken { lex' AlexRawToken_CloseBraceToken }
@KW_OpenParenToken { lex' AlexRawToken_OpenParenToken }
@KW_CloseParenToken { lex' AlexRawToken_CloseParenToken }
@KW_OpenBracketToken { lex' AlexRawToken_OpenBracketToken }
@KW_CloseBracketToken { lex' AlexRawToken_CloseBracketToken }
@KW_DotToken { lex' AlexRawToken_DotToken }
@KW_DotDotDotToken { lex' AlexRawToken_DotDotDotToken }
@KW_SemicolonToken { lex' AlexRawToken_SemicolonToken }
@KW_CommaToken { lex' AlexRawToken_CommaToken }
@KW_QuestionDotToken { lex' AlexRawToken_QuestionDotToken }
@KW_LessThanToken { lex' AlexRawToken_LessThanToken }
@KW_LessThanSlashToken { lex' AlexRawToken_LessThanSlashToken }
@KW_GreaterThanToken { lex' AlexRawToken_GreaterThanToken }
@KW_LessThanEqualsToken { lex' AlexRawToken_LessThanEqualsToken }
@KW_GreaterThanEqualsToken { lex' AlexRawToken_GreaterThanEqualsToken }
@KW_EqualsEqualsToken { lex' AlexRawToken_EqualsEqualsToken }
@KW_ExclamationEqualsToken { lex' AlexRawToken_ExclamationEqualsToken }
@KW_EqualsEqualsEqualsToken { lex' AlexRawToken_EqualsEqualsEqualsToken }
@KW_ExclamationEqualsEqualsToken { lex' AlexRawToken_ExclamationEqualsEqualsToken }
@KW_EqualsGreaterThanToken { lex' AlexRawToken_EqualsGreaterThanToken }
@KW_PlusToken { lex' AlexRawToken_PlusToken }
@KW_MinusToken { lex' AlexRawToken_MinusToken }
@KW_AsteriskToken { lex' AlexRawToken_AsteriskToken }
@KW_AsteriskAsteriskToken { lex' AlexRawToken_AsteriskAsteriskToken }
@KW_SlashToken { lex' AlexRawToken_SlashToken }
@KW_PercentToken { lex' AlexRawToken_PercentToken }
@KW_PlusPlusToken { lex' AlexRawToken_PlusPlusToken }
@KW_MinusMinusToken { lex' AlexRawToken_MinusMinusToken }
@KW_LessThanLessThanToken { lex' AlexRawToken_LessThanLessThanToken }
@KW_GreaterThanGreaterThanToken { lex' AlexRawToken_GreaterThanGreaterThanToken }
@KW_GreaterThanGreaterThanGreaterThanToken { lex' AlexRawToken_GreaterThanGreaterThanGreaterThanToken }
@KW_AmpersandToken { lex' AlexRawToken_AmpersandToken }
@KW_BarToken { lex' AlexRawToken_BarToken }
@KW_CaretToken { lex' AlexRawToken_CaretToken }
@KW_ExclamationToken { lex' AlexRawToken_ExclamationToken }
@KW_TildeToken { lex' AlexRawToken_TildeToken }
@KW_AmpersandAmpersandToken { lex' AlexRawToken_AmpersandAmpersandToken }
@KW_BarBarToken { lex' AlexRawToken_BarBarToken }
@KW_QuestionToken { lex' AlexRawToken_QuestionToken }
@KW_ColonToken { lex' AlexRawToken_ColonToken }
@KW_AtToken { lex' AlexRawToken_AtToken }
@KW_QuestionQuestionToken { lex' AlexRawToken_QuestionQuestionToken }
@KW_BacktickToken { lex' AlexRawToken_BacktickToken }
@KW_HashToken { lex' AlexRawToken_HashToken }
@KW_EqualsToken { lex' AlexRawToken_EqualsToken }
@KW_PlusEqualsToken { lex' AlexRawToken_PlusEqualsToken }
@KW_MinusEqualsToken { lex' AlexRawToken_MinusEqualsToken }
@KW_AsteriskEqualsToken { lex' AlexRawToken_AsteriskEqualsToken }
@KW_AsteriskAsteriskEqualsToken { lex' AlexRawToken_AsteriskAsteriskEqualsToken }
@KW_SlashEqualsToken { lex' AlexRawToken_SlashEqualsToken }
@KW_PercentEqualsToken { lex' AlexRawToken_PercentEqualsToken }
@KW_LessThanLessThanEqualsToken { lex' AlexRawToken_LessThanLessThanEqualsToken }
@KW_GreaterThanGreaterThanEqualsToken { lex' AlexRawToken_GreaterThanGreaterThanEqualsToken }
@KW_GreaterThanGreaterThanGreaterThanEqualsToken { lex' AlexRawToken_GreaterThanGreaterThanGreaterThanEqualsToken }
@KW_AmpersandEqualsToken { lex' AlexRawToken_AmpersandEqualsToken }
@KW_BarEqualsToken { lex' AlexRawToken_BarEqualsToken }
@KW_BarBarEqualsToken { lex' AlexRawToken_BarBarEqualsToken }
@KW_AmpersandAmpersandEqualsToken { lex' AlexRawToken_AmpersandAmpersandEqualsToken }
@KW_QuestionQuestionEqualsToken { lex' AlexRawToken_QuestionQuestionEqualsToken }
@KW_CaretEqualsToken { lex' AlexRawToken_CaretEqualsToken }
@KW_Identifier { lex' AlexRawToken_Identifier }
@KW_PrivateIdentifier { lex' AlexRawToken_PrivateIdentifier }
@KW_BreakKeyword { lex' AlexRawToken_BreakKeyword }
@KW_CaseKeyword { lex' AlexRawToken_CaseKeyword }
@KW_CatchKeyword { lex' AlexRawToken_CatchKeyword }
@KW_ClassKeyword { lex' AlexRawToken_ClassKeyword }
@KW_ConstKeyword { lex' AlexRawToken_ConstKeyword }
@KW_ContinueKeyword { lex' AlexRawToken_ContinueKeyword }
@KW_DebuggerKeyword { lex' AlexRawToken_DebuggerKeyword }
@KW_DefaultKeyword { lex' AlexRawToken_DefaultKeyword }
@KW_DeleteKeyword { lex' AlexRawToken_DeleteKeyword }
@KW_DoKeyword { lex' AlexRawToken_DoKeyword }
@KW_ElseKeyword { lex' AlexRawToken_ElseKeyword }
@KW_EnumKeyword { lex' AlexRawToken_EnumKeyword }
@KW_ExportKeyword { lex' AlexRawToken_ExportKeyword }
@KW_ExtendsKeyword { lex' AlexRawToken_ExtendsKeyword }
@KW_FalseKeyword { lex' AlexRawToken_FalseKeyword }
@KW_FinallyKeyword { lex' AlexRawToken_FinallyKeyword }
@KW_ForKeyword { lex' AlexRawToken_ForKeyword }
@KW_FunctionKeyword { lex' AlexRawToken_FunctionKeyword }
@KW_IfKeyword { lex' AlexRawToken_IfKeyword }
@KW_ImportKeyword { lex' AlexRawToken_ImportKeyword }
@KW_InKeyword { lex' AlexRawToken_InKeyword }
@KW_InstanceOfKeyword { lex' AlexRawToken_InstanceOfKeyword }
@KW_NewKeyword { lex' AlexRawToken_NewKeyword }
@KW_NullKeyword { lex' AlexRawToken_NullKeyword }
@KW_ReturnKeyword { lex' AlexRawToken_ReturnKeyword }
@KW_SuperKeyword { lex' AlexRawToken_SuperKeyword }
@KW_SwitchKeyword { lex' AlexRawToken_SwitchKeyword }
@KW_ThisKeyword { lex' AlexRawToken_ThisKeyword }
@KW_ThrowKeyword { lex' AlexRawToken_ThrowKeyword }
@KW_TrueKeyword { lex' AlexRawToken_TrueKeyword }
@KW_TryKeyword { lex' AlexRawToken_TryKeyword }
@KW_TypeOfKeyword { lex' AlexRawToken_TypeOfKeyword }
@KW_VarKeyword { lex' AlexRawToken_VarKeyword }
@KW_VoidKeyword { lex' AlexRawToken_VoidKeyword }
@KW_WhileKeyword { lex' AlexRawToken_WhileKeyword }
@KW_WithKeyword { lex' AlexRawToken_WithKeyword }
@KW_ImplementsKeyword { lex' AlexRawToken_ImplementsKeyword }
@KW_InterfaceKeyword { lex' AlexRawToken_InterfaceKeyword }
@KW_LetKeyword { lex' AlexRawToken_LetKeyword }
@KW_PackageKeyword { lex' AlexRawToken_PackageKeyword }
@KW_PrivateKeyword { lex' AlexRawToken_PrivateKeyword }
@KW_ProtectedKeyword { lex' AlexRawToken_ProtectedKeyword }
@KW_PublicKeyword { lex' AlexRawToken_PublicKeyword }
@KW_StaticKeyword { lex' AlexRawToken_StaticKeyword }
@KW_YieldKeyword { lex' AlexRawToken_YieldKeyword }
@KW_AbstractKeyword { lex' AlexRawToken_AbstractKeyword }
@KW_AccessorKeyword { lex' AlexRawToken_AccessorKeyword }
@KW_AsKeyword { lex' AlexRawToken_AsKeyword }
@KW_AssertsKeyword { lex' AlexRawToken_AssertsKeyword }
@KW_AssertKeyword { lex' AlexRawToken_AssertKeyword }
@KW_AnyKeyword { lex' AlexRawToken_AnyKeyword }
@KW_AsyncKeyword { lex' AlexRawToken_AsyncKeyword }
@KW_AwaitKeyword { lex' AlexRawToken_AwaitKeyword }
@KW_BooleanKeyword { lex' AlexRawToken_BooleanKeyword }
@KW_ConstructorKeyword { lex' AlexRawToken_ConstructorKeyword }
@KW_DeclareKeyword { lex' AlexRawToken_DeclareKeyword }
@KW_GetKeyword { lex' AlexRawToken_GetKeyword }
@KW_InferKeyword { lex' AlexRawToken_InferKeyword }
@KW_IntrinsicKeyword { lex' AlexRawToken_IntrinsicKeyword }
@KW_IsKeyword { lex' AlexRawToken_IsKeyword }
@KW_KeyOfKeyword { lex' AlexRawToken_KeyOfKeyword }
@KW_ModuleKeyword { lex' AlexRawToken_ModuleKeyword }
@KW_NamespaceKeyword { lex' AlexRawToken_NamespaceKeyword }
@KW_NeverKeyword { lex' AlexRawToken_NeverKeyword }
@KW_OutKeyword { lex' AlexRawToken_OutKeyword }
@KW_ReadonlyKeyword { lex' AlexRawToken_ReadonlyKeyword }
@KW_RequireKeyword { lex' AlexRawToken_RequireKeyword }
@KW_NumberKeyword { lex' AlexRawToken_NumberKeyword }
@KW_ObjectKeyword { lex' AlexRawToken_ObjectKeyword }
@KW_SatisfiesKeyword { lex' AlexRawToken_SatisfiesKeyword }
@KW_SetKeyword { lex' AlexRawToken_SetKeyword }
@KW_StringKeyword { lex' AlexRawToken_StringKeyword }
@KW_SymbolKeyword { lex' AlexRawToken_SymbolKeyword }
@KW_TypeKeyword { lex' AlexRawToken_TypeKeyword }
@KW_UndefinedKeyword { lex' AlexRawToken_UndefinedKeyword }
@KW_UniqueKeyword { lex' AlexRawToken_UniqueKeyword }
@KW_UnknownKeyword { lex' AlexRawToken_UnknownKeyword }
@KW_UsingKeyword { lex' AlexRawToken_UsingKeyword }
@KW_FromKeyword { lex' AlexRawToken_FromKeyword }
@KW_GlobalKeyword { lex' AlexRawToken_GlobalKeyword }
@KW_BigIntKeyword { lex' AlexRawToken_BigIntKeyword }
@KW_OverrideKeyword { lex' AlexRawToken_OverrideKeyword }
@KW_OfKeyword { lex' AlexRawToken_OfKeyword }
@KW_LastContextualKeyword { lex' AlexRawToken_LastContextualKeyword }
@KW_QualifiedName { lex' AlexRawToken_QualifiedName }
@KW_ComputedPropertyName { lex' AlexRawToken_ComputedPropertyName }
@KW_TypeParameter { lex' AlexRawToken_TypeParameter }
@KW_Parameter { lex' AlexRawToken_Parameter }
@KW_Decorator { lex' AlexRawToken_Decorator }
@KW_PropertySignature { lex' AlexRawToken_PropertySignature }
@KW_PropertyDeclaration { lex' AlexRawToken_PropertyDeclaration }
@KW_MethodSignature { lex' AlexRawToken_MethodSignature }
@KW_MethodDeclaration { lex' AlexRawToken_MethodDeclaration }
@KW_ClassStaticBlockDeclaration { lex' AlexRawToken_ClassStaticBlockDeclaration }
@KW_Constructor { lex' AlexRawToken_Constructor }
@KW_GetAccessor { lex' AlexRawToken_GetAccessor }
@KW_SetAccessor { lex' AlexRawToken_SetAccessor }
@KW_CallSignature { lex' AlexRawToken_CallSignature }
@KW_ConstructSignature { lex' AlexRawToken_ConstructSignature }
@KW_IndexSignature { lex' AlexRawToken_IndexSignature }
@KW_TypePredicate { lex' AlexRawToken_TypePredicate }
@KW_TypeReference { lex' AlexRawToken_TypeReference }
@KW_FunctionType { lex' AlexRawToken_FunctionType }
@KW_ConstructorType { lex' AlexRawToken_ConstructorType }
@KW_TypeQuery { lex' AlexRawToken_TypeQuery }
@KW_TypeLiteral { lex' AlexRawToken_TypeLiteral }
@KW_ArrayType { lex' AlexRawToken_ArrayType }
@KW_TupleType { lex' AlexRawToken_TupleType }
@KW_OptionalType { lex' AlexRawToken_OptionalType }
@KW_RestType { lex' AlexRawToken_RestType }
@KW_UnionType { lex' AlexRawToken_UnionType }
@KW_IntersectionType { lex' AlexRawToken_IntersectionType }
@KW_ConditionalType { lex' AlexRawToken_ConditionalType }
@KW_InferType { lex' AlexRawToken_InferType }
@KW_ParenthesizedType { lex' AlexRawToken_ParenthesizedType }
@KW_ThisType { lex' AlexRawToken_ThisType }
@KW_TypeOperator { lex' AlexRawToken_TypeOperator }
@KW_IndexedAccessType { lex' AlexRawToken_IndexedAccessType }
@KW_MappedType { lex' AlexRawToken_MappedType }
@KW_LiteralType { lex' AlexRawToken_LiteralType }
@KW_NamedTupleMember { lex' AlexRawToken_NamedTupleMember }
@KW_TemplateLiteralType { lex' AlexRawToken_TemplateLiteralType }
@KW_TemplateLiteralTypeSpan { lex' AlexRawToken_TemplateLiteralTypeSpan }
@KW_ImportType { lex' AlexRawToken_ImportType }
@KW_ObjectBindingPattern { lex' AlexRawToken_ObjectBindingPattern }
@KW_ArrayBindingPattern { lex' AlexRawToken_ArrayBindingPattern }
@KW_BindingElement { lex' AlexRawToken_BindingElement }
@KW_ArrayLiteralExpression { lex' AlexRawToken_ArrayLiteralExpression }
@KW_ObjectLiteralExpression { lex' AlexRawToken_ObjectLiteralExpression }
@KW_PropertyAccessExpression { lex' AlexRawToken_PropertyAccessExpression }
@KW_ElementAccessExpression { lex' AlexRawToken_ElementAccessExpression }
@KW_CallExpression { lex' AlexRawToken_CallExpression }
@KW_NewExpression { lex' AlexRawToken_NewExpression }
@KW_TaggedTemplateExpression { lex' AlexRawToken_TaggedTemplateExpression }
@KW_TypeAssertionExpression { lex' AlexRawToken_TypeAssertionExpression }
@KW_ParenthesizedExpression { lex' AlexRawToken_ParenthesizedExpression }
@KW_FunctionExpression { lex' AlexRawToken_FunctionExpression }
@KW_ArrowFunction { lex' AlexRawToken_ArrowFunction }
@KW_DeleteExpression { lex' AlexRawToken_DeleteExpression }
@KW_TypeOfExpression { lex' AlexRawToken_TypeOfExpression }
@KW_VoidExpression { lex' AlexRawToken_VoidExpression }
@KW_AwaitExpression { lex' AlexRawToken_AwaitExpression }
@KW_PrefixUnaryExpression { lex' AlexRawToken_PrefixUnaryExpression }
@KW_PostfixUnaryExpression { lex' AlexRawToken_PostfixUnaryExpression }
@KW_BinaryExpression { lex' AlexRawToken_BinaryExpression }
@KW_ConditionalExpression { lex' AlexRawToken_ConditionalExpression }
@KW_TemplateExpression { lex' AlexRawToken_TemplateExpression }
@KW_YieldExpression { lex' AlexRawToken_YieldExpression }
@KW_SpreadElement { lex' AlexRawToken_SpreadElement }
@KW_ClassExpression { lex' AlexRawToken_ClassExpression }
@KW_OmittedExpression { lex' AlexRawToken_OmittedExpression }
@KW_ExpressionWithTypeArguments { lex' AlexRawToken_ExpressionWithTypeArguments }
@KW_AsExpression { lex' AlexRawToken_AsExpression }
@KW_NonNullExpression { lex' AlexRawToken_NonNullExpression }
@KW_MetaProperty { lex' AlexRawToken_MetaProperty }
@KW_SyntheticExpression { lex' AlexRawToken_SyntheticExpression }
@KW_SatisfiesExpression { lex' AlexRawToken_SatisfiesExpression }
@KW_TemplateSpan { lex' AlexRawToken_TemplateSpan }
@KW_SemicolonClassElement { lex' AlexRawToken_SemicolonClassElement }
@KW_Block { lex' AlexRawToken_Block }
@KW_EmptyStatement { lex' AlexRawToken_EmptyStatement }
@KW_VariableStatement { lex' AlexRawToken_VariableStatement }
@KW_ExpressionStatement { lex' AlexRawToken_ExpressionStatement }
@KW_IfStatement { lex' AlexRawToken_IfStatement }
@KW_DoStatement { lex' AlexRawToken_DoStatement }
@KW_WhileStatement { lex' AlexRawToken_WhileStatement }
@KW_ForStatement { lex' AlexRawToken_ForStatement }
@KW_ForInStatement { lex' AlexRawToken_ForInStatement }
@KW_ForOfStatement { lex' AlexRawToken_ForOfStatement }
@KW_ContinueStatement { lex' AlexRawToken_ContinueStatement }
@KW_BreakStatement { lex' AlexRawToken_BreakStatement }
@KW_ReturnStatement { lex' AlexRawToken_ReturnStatement }
@KW_WithStatement { lex' AlexRawToken_WithStatement }
@KW_SwitchStatement { lex' AlexRawToken_SwitchStatement }
@KW_LabeledStatement { lex' AlexRawToken_LabeledStatement }
@KW_ThrowStatement { lex' AlexRawToken_ThrowStatement }
@KW_TryStatement { lex' AlexRawToken_TryStatement }
@KW_DebuggerStatement { lex' AlexRawToken_DebuggerStatement }
@KW_VariableDeclaration { lex' AlexRawToken_VariableDeclaration }
@KW_VariableDeclarationList { lex' AlexRawToken_VariableDeclarationList }
@KW_FunctionDeclaration { lex' AlexRawToken_FunctionDeclaration }
@KW_ClassDeclaration { lex' AlexRawToken_ClassDeclaration }
@KW_InterfaceDeclaration { lex' AlexRawToken_InterfaceDeclaration }
@KW_TypeAliasDeclaration { lex' AlexRawToken_TypeAliasDeclaration }
@KW_EnumDeclaration { lex' AlexRawToken_EnumDeclaration }
@KW_ModuleDeclaration { lex' AlexRawToken_ModuleDeclaration }
@KW_ModuleBlock { lex' AlexRawToken_ModuleBlock }
@KW_CaseBlock { lex' AlexRawToken_CaseBlock }
@KW_NamespaceExportDeclaration { lex' AlexRawToken_NamespaceExportDeclaration }
@KW_ImportEqualsDeclaration { lex' AlexRawToken_ImportEqualsDeclaration }
@KW_ImportDeclaration { lex' AlexRawToken_ImportDeclaration }
@KW_ImportClause { lex' AlexRawToken_ImportClause }
@KW_NamespaceImport { lex' AlexRawToken_NamespaceImport }
@KW_NamedImports { lex' AlexRawToken_NamedImports }
@KW_ImportSpecifier { lex' AlexRawToken_ImportSpecifier }
@KW_ExportAssignment { lex' AlexRawToken_ExportAssignment }
@KW_ExportDeclaration { lex' AlexRawToken_ExportDeclaration }
@KW_NamedExports { lex' AlexRawToken_NamedExports }
@KW_NamespaceExport { lex' AlexRawToken_NamespaceExport }
@KW_ExportSpecifier { lex' AlexRawToken_ExportSpecifier }
@KW_MissingDeclaration { lex' AlexRawToken_MissingDeclaration }
@KW_ExternalModuleReference { lex' AlexRawToken_ExternalModuleReference }
@KW_JsxElement { lex' AlexRawToken_JsxElement }
@KW_JsxSelfClosingElement { lex' AlexRawToken_JsxSelfClosingElement }
@KW_JsxOpeningElement { lex' AlexRawToken_JsxOpeningElement }
@KW_JsxClosingElement { lex' AlexRawToken_JsxClosingElement }
@KW_JsxFragment { lex' AlexRawToken_JsxFragment }
@KW_JsxOpeningFragment { lex' AlexRawToken_JsxOpeningFragment }
@KW_JsxClosingFragment { lex' AlexRawToken_JsxClosingFragment }
@KW_JsxAttribute { lex' AlexRawToken_JsxAttribute }
@KW_JsxAttributes { lex' AlexRawToken_JsxAttributes }
@KW_JsxSpreadAttribute { lex' AlexRawToken_JsxSpreadAttribute }
@KW_JsxExpression { lex' AlexRawToken_JsxExpression }
@KW_JsxNamespacedName { lex' AlexRawToken_JsxNamespacedName }
@KW_CaseClause { lex' AlexRawToken_CaseClause }
@KW_DefaultClause { lex' AlexRawToken_DefaultClause }
@KW_HeritageClause { lex' AlexRawToken_HeritageClause }
@KW_CatchClause { lex' AlexRawToken_CatchClause }
@KW_ImportAttributes { lex' AlexRawToken_ImportAttributes }
@KW_ImportAttribute { lex' AlexRawToken_ImportAttribute }
@KW_AssertClause { lex' AlexRawToken_AssertClause }
@KW_AssertEntry { lex' AlexRawToken_AssertEntry }
@KW_ImportTypeAssertionContainer { lex' AlexRawToken_ImportTypeAssertionContainer }
@KW_PropertyAssignment { lex' AlexRawToken_PropertyAssignment }
@KW_ShorthandPropertyAssignment { lex' AlexRawToken_ShorthandPropertyAssignment }
@KW_SpreadAssignment { lex' AlexRawToken_SpreadAssignment }
@KW_EnumMember { lex' AlexRawToken_EnumMember }
@KW_SourceFile { lex' AlexRawToken_SourceFile }
@KW_Bundle { lex' AlexRawToken_Bundle }
@KW_JSDocTypeExpression { lex' AlexRawToken_JSDocTypeExpression }
@KW_JSDocNameReference { lex' AlexRawToken_JSDocNameReference }
@KW_JSDocMemberName { lex' AlexRawToken_JSDocMemberName }
@KW_JSDocAllType { lex' AlexRawToken_JSDocAllType }
@KW_JSDocUnknownType { lex' AlexRawToken_JSDocUnknownType }
@KW_JSDocNullableType { lex' AlexRawToken_JSDocNullableType }
@KW_JSDocNonNullableType { lex' AlexRawToken_JSDocNonNullableType }
@KW_JSDocOptionalType { lex' AlexRawToken_JSDocOptionalType }
@KW_JSDocFunctionType { lex' AlexRawToken_JSDocFunctionType }
@KW_JSDocVariadicType { lex' AlexRawToken_JSDocVariadicType }
@KW_JSDocNamepathType { lex' AlexRawToken_JSDocNamepathType }
@KW_JSDoc { lex' AlexRawToken_JSDoc }
@KW_JSDocComment { lex' AlexRawToken_JSDocComment }
@KW_JSDocText { lex' AlexRawToken_JSDocText }
@KW_JSDocTypeLiteral { lex' AlexRawToken_JSDocTypeLiteral }
@KW_JSDocSignature { lex' AlexRawToken_JSDocSignature }
@KW_JSDocLink { lex' AlexRawToken_JSDocLink }
@KW_JSDocLinkCode { lex' AlexRawToken_JSDocLinkCode }
@KW_JSDocLinkPlain { lex' AlexRawToken_JSDocLinkPlain }
@KW_JSDocTag { lex' AlexRawToken_JSDocTag }
@KW_JSDocAugmentsTag { lex' AlexRawToken_JSDocAugmentsTag }
@KW_JSDocImplementsTag { lex' AlexRawToken_JSDocImplementsTag }
@KW_JSDocAuthorTag { lex' AlexRawToken_JSDocAuthorTag }
@KW_JSDocDeprecatedTag { lex' AlexRawToken_JSDocDeprecatedTag }
@KW_JSDocClassTag { lex' AlexRawToken_JSDocClassTag }
@KW_JSDocPublicTag { lex' AlexRawToken_JSDocPublicTag }
@KW_JSDocPrivateTag { lex' AlexRawToken_JSDocPrivateTag }
@KW_JSDocProtectedTag { lex' AlexRawToken_JSDocProtectedTag }
@KW_JSDocReadonlyTag { lex' AlexRawToken_JSDocReadonlyTag }
@KW_JSDocOverrideTag { lex' AlexRawToken_JSDocOverrideTag }
@KW_JSDocCallbackTag { lex' AlexRawToken_JSDocCallbackTag }
@KW_JSDocOverloadTag { lex' AlexRawToken_JSDocOverloadTag }
@KW_JSDocEnumTag { lex' AlexRawToken_JSDocEnumTag }
@KW_JSDocParameterTag { lex' AlexRawToken_JSDocParameterTag }
@KW_JSDocReturnTag { lex' AlexRawToken_JSDocReturnTag }
@KW_JSDocThisTag { lex' AlexRawToken_JSDocThisTag }
@KW_JSDocTypeTag { lex' AlexRawToken_JSDocTypeTag }
@KW_JSDocTemplateTag { lex' AlexRawToken_JSDocTemplateTag }
@KW_JSDocTypedefTag { lex' AlexRawToken_JSDocTypedefTag }
@KW_JSDocSeeTag { lex' AlexRawToken_JSDocSeeTag }
@KW_JSDocPropertyTag { lex' AlexRawToken_JSDocPropertyTag }
@KW_JSDocThrowsTag { lex' AlexRawToken_JSDocThrowsTag }
@KW_JSDocSatisfiesTag { lex' AlexRawToken_JSDocSatisfiesTag }
@KW_JSDocImportTag { lex' AlexRawToken_JSDocImportTag }
@KW_SyntaxList { lex' AlexRawToken_SyntaxList }
@KW_NotEmittedStatement { lex' AlexRawToken_NotEmittedStatement }
@KW_PartiallyEmittedExpression { lex' AlexRawToken_PartiallyEmittedExpression }
@KW_CommaListExpression { lex' AlexRawToken_CommaListExpression }
@KW_SyntheticReferenceExpression { lex' AlexRawToken_SyntheticReferenceExpression }
@KW_Count { lex' AlexRawToken_Count }
@KW_FirstAssignment { lex' AlexRawToken_FirstAssignment }
@KW_LastAssignment { lex' AlexRawToken_LastAssignment }
@KW_FirstCompoundAssignment { lex' AlexRawToken_FirstCompoundAssignment }
@KW_LastCompoundAssignment { lex' AlexRawToken_LastCompoundAssignment }
@KW_FirstReservedWord { lex' AlexRawToken_FirstReservedWord }
@KW_LastReservedWord { lex' AlexRawToken_LastReservedWord }
@KW_FirstKeyword { lex' AlexRawToken_FirstKeyword }
@KW_LastKeyword { lex' AlexRawToken_LastKeyword }
@KW_FirstFutureReservedWord { lex' AlexRawToken_FirstFutureReservedWord }
@KW_LastFutureReservedWord { lex' AlexRawToken_LastFutureReservedWord }
@KW_FirstTypeNode { lex' AlexRawToken_FirstTypeNode }
@KW_LastTypeNode { lex' AlexRawToken_LastTypeNode }
@KW_FirstPunctuation { lex' AlexRawToken_FirstPunctuation }
@KW_LastPunctuation { lex' AlexRawToken_LastPunctuation }
@KW_FirstToken { lex' AlexRawToken_FirstToken }
@KW_LastToken { lex' AlexRawToken_LastToken }
@KW_FirstTriviaToken { lex' AlexRawToken_FirstTriviaToken }
@KW_LastTriviaToken { lex' AlexRawToken_LastTriviaToken }
@KW_FirstLiteralToken { lex' AlexRawToken_FirstLiteralToken }
@KW_LastLiteralToken { lex' AlexRawToken_LastLiteralToken }
@KW_FirstTemplateToken { lex' AlexRawToken_FirstTemplateToken }
@KW_LastTemplateToken { lex' AlexRawToken_LastTemplateToken }
@KW_FirstBinaryOperator { lex' AlexRawToken_FirstBinaryOperator }
@KW_LastBinaryOperator { lex' AlexRawToken_LastBinaryOperator }
@KW_FirstStatement { lex' AlexRawToken_FirstStatement }
@KW_LastStatement { lex' AlexRawToken_LastStatement }
@KW_FirstNode { lex' AlexRawToken_FirstNode }
@KW_FirstJSDocNode { lex' AlexRawToken_FirstJSDocNode }
@KW_LastJSDocNode { lex' AlexRawToken_LastJSDocNode }
@KW_FirstJSDocTagNode { lex' AlexRawToken_FirstJSDocTagNode }
@KW_LastJSDocTagNode { lex' AlexRawToken_LastJSDocTagNode }

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

@ID  { lex  AlexRawToken_ID                 }
@INT { lex (AlexRawToken_INT . round. read) }
@STR { lex  AlexRawToken_STR                }

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
         tokenLoc :: Location,
         additionalRepoInfo :: Common.AdditionalRepoInfo
     }
     deriving ( Show )

-- *************
-- *           *
-- * Raw Token *
-- *           *
-- *************
data AlexRawToken

     = AlexRawToken_INT Int -- ^ locations and numbers
     | AlexRawToken_ID String -- ^ including constant strings
     | AlexRawToken_STR String -- ^ including constant strings

     -- ***************
     -- *             *
     -- * parentheses *
     -- *             *
     -- ***************

     | AlexRawToken_LPAREN -- ^ Parentheses __(__
     | AlexRawToken_RPAREN -- ^ Parentheses __)__
     | AlexRawToken_LBRACK -- ^ Parentheses __[__
     | AlexRawToken_RBRACK -- ^ Parentheses __]__

     -- ************
     -- *          *
     -- * location *
     -- *          *
     -- ************

     | AlexRawToken_COLON -- ^ For location support
     | AlexRawToken_COMMA -- ^ For location support
     | AlexRawToken_MINUS -- ^ For location support

     -- ************
     -- *          *
     -- * keywords *
     -- *          *
     -- ************
 
     | AlexRawToken_Unknown -- ^ Reserved Keyword
     | AlexRawToken_EndOfFileToken -- ^ Reserved Keyword
     | AlexRawToken_SingleLineCommentTrivia -- ^ Reserved Keyword
     | AlexRawToken_MultiLineCommentTrivia -- ^ Reserved Keyword
     | AlexRawToken_NewLineTrivia -- ^ Reserved Keyword
     | AlexRawToken_WhitespaceTrivia -- ^ Reserved Keyword
     | AlexRawToken_ShebangTrivia -- ^ Reserved Keyword
     | AlexRawToken_ConflictMarkerTrivia -- ^ Reserved Keyword
     | AlexRawToken_NonTextFileMarkerTrivia -- ^ Reserved Keyword
     | AlexRawToken_NumericLiteral -- ^ Reserved Keyword
     | AlexRawToken_BigIntLiteral -- ^ Reserved Keyword
     | AlexRawToken_StringLiteral -- ^ Reserved Keyword
     | AlexRawToken_JsxText -- ^ Reserved Keyword
     | AlexRawToken_JsxTextAllWhiteSpaces -- ^ Reserved Keyword
     | AlexRawToken_RegularExpressionLiteral -- ^ Reserved Keyword
     | AlexRawToken_NoSubstitutionTemplateLiteral -- ^ Reserved Keyword
     | AlexRawToken_TemplateHead -- ^ Reserved Keyword
     | AlexRawToken_TemplateMiddle -- ^ Reserved Keyword
     | AlexRawToken_TemplateTail -- ^ Reserved Keyword
     | AlexRawToken_OpenBraceToken -- ^ Reserved Keyword
     | AlexRawToken_CloseBraceToken -- ^ Reserved Keyword
     | AlexRawToken_OpenParenToken -- ^ Reserved Keyword
     | AlexRawToken_CloseParenToken -- ^ Reserved Keyword
     | AlexRawToken_OpenBracketToken -- ^ Reserved Keyword
     | AlexRawToken_CloseBracketToken -- ^ Reserved Keyword
     | AlexRawToken_DotToken -- ^ Reserved Keyword
     | AlexRawToken_DotDotDotToken -- ^ Reserved Keyword
     | AlexRawToken_SemicolonToken -- ^ Reserved Keyword
     | AlexRawToken_CommaToken -- ^ Reserved Keyword
     | AlexRawToken_QuestionDotToken -- ^ Reserved Keyword
     | AlexRawToken_LessThanToken -- ^ Reserved Keyword
     | AlexRawToken_LessThanSlashToken -- ^ Reserved Keyword
     | AlexRawToken_GreaterThanToken -- ^ Reserved Keyword
     | AlexRawToken_LessThanEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_GreaterThanEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_EqualsEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_ExclamationEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_EqualsEqualsEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_ExclamationEqualsEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_EqualsGreaterThanToken -- ^ Reserved Keyword
     | AlexRawToken_PlusToken -- ^ Reserved Keyword
     | AlexRawToken_MinusToken -- ^ Reserved Keyword
     | AlexRawToken_AsteriskToken -- ^ Reserved Keyword
     | AlexRawToken_AsteriskAsteriskToken -- ^ Reserved Keyword
     | AlexRawToken_SlashToken -- ^ Reserved Keyword
     | AlexRawToken_PercentToken -- ^ Reserved Keyword
     | AlexRawToken_PlusPlusToken -- ^ Reserved Keyword
     | AlexRawToken_MinusMinusToken -- ^ Reserved Keyword
     | AlexRawToken_LessThanLessThanToken -- ^ Reserved Keyword
     | AlexRawToken_GreaterThanGreaterThanToken -- ^ Reserved Keyword
     | AlexRawToken_GreaterThanGreaterThanGreaterThanToken -- ^ Reserved Keyword
     | AlexRawToken_AmpersandToken -- ^ Reserved Keyword
     | AlexRawToken_BarToken -- ^ Reserved Keyword
     | AlexRawToken_CaretToken -- ^ Reserved Keyword
     | AlexRawToken_ExclamationToken -- ^ Reserved Keyword
     | AlexRawToken_TildeToken -- ^ Reserved Keyword
     | AlexRawToken_AmpersandAmpersandToken -- ^ Reserved Keyword
     | AlexRawToken_BarBarToken -- ^ Reserved Keyword
     | AlexRawToken_QuestionToken -- ^ Reserved Keyword
     | AlexRawToken_ColonToken -- ^ Reserved Keyword
     | AlexRawToken_AtToken -- ^ Reserved Keyword
     | AlexRawToken_QuestionQuestionToken -- ^ Reserved Keyword
     | AlexRawToken_BacktickToken -- ^ Reserved Keyword
     | AlexRawToken_HashToken -- ^ Reserved Keyword
     | AlexRawToken_EqualsToken -- ^ Reserved Keyword
     | AlexRawToken_PlusEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_MinusEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_AsteriskEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_AsteriskAsteriskEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_SlashEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_PercentEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_LessThanLessThanEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_GreaterThanGreaterThanEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_GreaterThanGreaterThanGreaterThanEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_AmpersandEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_BarEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_BarBarEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_AmpersandAmpersandEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_QuestionQuestionEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_CaretEqualsToken -- ^ Reserved Keyword
     | AlexRawToken_Identifier -- ^ Reserved Keyword
     | AlexRawToken_PrivateIdentifier -- ^ Reserved Keyword
     | AlexRawToken_BreakKeyword -- ^ Reserved Keyword
     | AlexRawToken_CaseKeyword -- ^ Reserved Keyword
     | AlexRawToken_CatchKeyword -- ^ Reserved Keyword
     | AlexRawToken_ClassKeyword -- ^ Reserved Keyword
     | AlexRawToken_ConstKeyword -- ^ Reserved Keyword
     | AlexRawToken_ContinueKeyword -- ^ Reserved Keyword
     | AlexRawToken_DebuggerKeyword -- ^ Reserved Keyword
     | AlexRawToken_DefaultKeyword -- ^ Reserved Keyword
     | AlexRawToken_DeleteKeyword -- ^ Reserved Keyword
     | AlexRawToken_DoKeyword -- ^ Reserved Keyword
     | AlexRawToken_ElseKeyword -- ^ Reserved Keyword
     | AlexRawToken_EnumKeyword -- ^ Reserved Keyword
     | AlexRawToken_ExportKeyword -- ^ Reserved Keyword
     | AlexRawToken_ExtendsKeyword -- ^ Reserved Keyword
     | AlexRawToken_FalseKeyword -- ^ Reserved Keyword
     | AlexRawToken_FinallyKeyword -- ^ Reserved Keyword
     | AlexRawToken_ForKeyword -- ^ Reserved Keyword
     | AlexRawToken_FunctionKeyword -- ^ Reserved Keyword
     | AlexRawToken_IfKeyword -- ^ Reserved Keyword
     | AlexRawToken_ImportKeyword -- ^ Reserved Keyword
     | AlexRawToken_InKeyword -- ^ Reserved Keyword
     | AlexRawToken_InstanceOfKeyword -- ^ Reserved Keyword
     | AlexRawToken_NewKeyword -- ^ Reserved Keyword
     | AlexRawToken_NullKeyword -- ^ Reserved Keyword
     | AlexRawToken_ReturnKeyword -- ^ Reserved Keyword
     | AlexRawToken_SuperKeyword -- ^ Reserved Keyword
     | AlexRawToken_SwitchKeyword -- ^ Reserved Keyword
     | AlexRawToken_ThisKeyword -- ^ Reserved Keyword
     | AlexRawToken_ThrowKeyword -- ^ Reserved Keyword
     | AlexRawToken_TrueKeyword -- ^ Reserved Keyword
     | AlexRawToken_TryKeyword -- ^ Reserved Keyword
     | AlexRawToken_TypeOfKeyword -- ^ Reserved Keyword
     | AlexRawToken_VarKeyword -- ^ Reserved Keyword
     | AlexRawToken_VoidKeyword -- ^ Reserved Keyword
     | AlexRawToken_WhileKeyword -- ^ Reserved Keyword
     | AlexRawToken_WithKeyword -- ^ Reserved Keyword
     | AlexRawToken_ImplementsKeyword -- ^ Reserved Keyword
     | AlexRawToken_InterfaceKeyword -- ^ Reserved Keyword
     | AlexRawToken_LetKeyword -- ^ Reserved Keyword
     | AlexRawToken_PackageKeyword -- ^ Reserved Keyword
     | AlexRawToken_PrivateKeyword -- ^ Reserved Keyword
     | AlexRawToken_ProtectedKeyword -- ^ Reserved Keyword
     | AlexRawToken_PublicKeyword -- ^ Reserved Keyword
     | AlexRawToken_StaticKeyword -- ^ Reserved Keyword
     | AlexRawToken_YieldKeyword -- ^ Reserved Keyword
     | AlexRawToken_AbstractKeyword -- ^ Reserved Keyword
     | AlexRawToken_AccessorKeyword -- ^ Reserved Keyword
     | AlexRawToken_AsKeyword -- ^ Reserved Keyword
     | AlexRawToken_AssertsKeyword -- ^ Reserved Keyword
     | AlexRawToken_AssertKeyword -- ^ Reserved Keyword
     | AlexRawToken_AnyKeyword -- ^ Reserved Keyword
     | AlexRawToken_AsyncKeyword -- ^ Reserved Keyword
     | AlexRawToken_AwaitKeyword -- ^ Reserved Keyword
     | AlexRawToken_BooleanKeyword -- ^ Reserved Keyword
     | AlexRawToken_ConstructorKeyword -- ^ Reserved Keyword
     | AlexRawToken_DeclareKeyword -- ^ Reserved Keyword
     | AlexRawToken_GetKeyword -- ^ Reserved Keyword
     | AlexRawToken_InferKeyword -- ^ Reserved Keyword
     | AlexRawToken_IntrinsicKeyword -- ^ Reserved Keyword
     | AlexRawToken_IsKeyword -- ^ Reserved Keyword
     | AlexRawToken_KeyOfKeyword -- ^ Reserved Keyword
     | AlexRawToken_ModuleKeyword -- ^ Reserved Keyword
     | AlexRawToken_NamespaceKeyword -- ^ Reserved Keyword
     | AlexRawToken_NeverKeyword -- ^ Reserved Keyword
     | AlexRawToken_OutKeyword -- ^ Reserved Keyword
     | AlexRawToken_ReadonlyKeyword -- ^ Reserved Keyword
     | AlexRawToken_RequireKeyword -- ^ Reserved Keyword
     | AlexRawToken_NumberKeyword -- ^ Reserved Keyword
     | AlexRawToken_ObjectKeyword -- ^ Reserved Keyword
     | AlexRawToken_SatisfiesKeyword -- ^ Reserved Keyword
     | AlexRawToken_SetKeyword -- ^ Reserved Keyword
     | AlexRawToken_StringKeyword -- ^ Reserved Keyword
     | AlexRawToken_SymbolKeyword -- ^ Reserved Keyword
     | AlexRawToken_TypeKeyword -- ^ Reserved Keyword
     | AlexRawToken_UndefinedKeyword -- ^ Reserved Keyword
     | AlexRawToken_UniqueKeyword -- ^ Reserved Keyword
     | AlexRawToken_UnknownKeyword -- ^ Reserved Keyword
     | AlexRawToken_UsingKeyword -- ^ Reserved Keyword
     | AlexRawToken_FromKeyword -- ^ Reserved Keyword
     | AlexRawToken_GlobalKeyword -- ^ Reserved Keyword
     | AlexRawToken_BigIntKeyword -- ^ Reserved Keyword
     | AlexRawToken_OverrideKeyword -- ^ Reserved Keyword
     | AlexRawToken_OfKeyword -- ^ Reserved Keyword
     | AlexRawToken_LastContextualKeyword -- ^ Reserved Keyword (alias for OfKeyword)
     | AlexRawToken_QualifiedName -- ^ Reserved Keyword
     | AlexRawToken_ComputedPropertyName -- ^ Reserved Keyword
     | AlexRawToken_TypeParameter -- ^ Reserved Keyword
     | AlexRawToken_Parameter -- ^ Reserved Keyword
     | AlexRawToken_Decorator -- ^ Reserved Keyword
     | AlexRawToken_PropertySignature -- ^ Reserved Keyword
     | AlexRawToken_PropertyDeclaration -- ^ Reserved Keyword
     | AlexRawToken_MethodSignature -- ^ Reserved Keyword
     | AlexRawToken_MethodDeclaration -- ^ Reserved Keyword
     | AlexRawToken_ClassStaticBlockDeclaration -- ^ Reserved Keyword
     | AlexRawToken_Constructor -- ^ Reserved Keyword
     | AlexRawToken_GetAccessor -- ^ Reserved Keyword
     | AlexRawToken_SetAccessor -- ^ Reserved Keyword
     | AlexRawToken_CallSignature -- ^ Reserved Keyword
     | AlexRawToken_ConstructSignature -- ^ Reserved Keyword
     | AlexRawToken_IndexSignature -- ^ Reserved Keyword
     | AlexRawToken_TypePredicate -- ^ Reserved Keyword
     | AlexRawToken_TypeReference -- ^ Reserved Keyword
     | AlexRawToken_FunctionType -- ^ Reserved Keyword
     | AlexRawToken_ConstructorType -- ^ Reserved Keyword
     | AlexRawToken_TypeQuery -- ^ Reserved Keyword
     | AlexRawToken_TypeLiteral -- ^ Reserved Keyword
     | AlexRawToken_ArrayType -- ^ Reserved Keyword
     | AlexRawToken_TupleType -- ^ Reserved Keyword
     | AlexRawToken_OptionalType -- ^ Reserved Keyword
     | AlexRawToken_RestType -- ^ Reserved Keyword
     | AlexRawToken_UnionType -- ^ Reserved Keyword
     | AlexRawToken_IntersectionType -- ^ Reserved Keyword
     | AlexRawToken_ConditionalType -- ^ Reserved Keyword
     | AlexRawToken_InferType -- ^ Reserved Keyword
     | AlexRawToken_ParenthesizedType -- ^ Reserved Keyword
     | AlexRawToken_ThisType -- ^ Reserved Keyword
     | AlexRawToken_TypeOperator -- ^ Reserved Keyword
     | AlexRawToken_IndexedAccessType -- ^ Reserved Keyword
     | AlexRawToken_MappedType -- ^ Reserved Keyword
     | AlexRawToken_LiteralType -- ^ Reserved Keyword
     | AlexRawToken_NamedTupleMember -- ^ Reserved Keyword
     | AlexRawToken_TemplateLiteralType -- ^ Reserved Keyword
     | AlexRawToken_TemplateLiteralTypeSpan -- ^ Reserved Keyword
     | AlexRawToken_ImportType -- ^ Reserved Keyword
     | AlexRawToken_ObjectBindingPattern -- ^ Reserved Keyword
     | AlexRawToken_ArrayBindingPattern -- ^ Reserved Keyword
     | AlexRawToken_BindingElement -- ^ Reserved Keyword
     | AlexRawToken_ArrayLiteralExpression -- ^ Reserved Keyword
     | AlexRawToken_ObjectLiteralExpression -- ^ Reserved Keyword
     | AlexRawToken_PropertyAccessExpression -- ^ Reserved Keyword
     | AlexRawToken_ElementAccessExpression -- ^ Reserved Keyword
     | AlexRawToken_CallExpression -- ^ Reserved Keyword
     | AlexRawToken_NewExpression -- ^ Reserved Keyword
     | AlexRawToken_TaggedTemplateExpression -- ^ Reserved Keyword
     | AlexRawToken_TypeAssertionExpression -- ^ Reserved Keyword
     | AlexRawToken_ParenthesizedExpression -- ^ Reserved Keyword
     | AlexRawToken_FunctionExpression -- ^ Reserved Keyword
     | AlexRawToken_ArrowFunction -- ^ Reserved Keyword
     | AlexRawToken_DeleteExpression -- ^ Reserved Keyword
     | AlexRawToken_TypeOfExpression -- ^ Reserved Keyword
     | AlexRawToken_VoidExpression -- ^ Reserved Keyword
     | AlexRawToken_AwaitExpression -- ^ Reserved Keyword
     | AlexRawToken_PrefixUnaryExpression -- ^ Reserved Keyword
     | AlexRawToken_PostfixUnaryExpression -- ^ Reserved Keyword
     | AlexRawToken_BinaryExpression -- ^ Reserved Keyword
     | AlexRawToken_ConditionalExpression -- ^ Reserved Keyword
     | AlexRawToken_TemplateExpression -- ^ Reserved Keyword
     | AlexRawToken_YieldExpression -- ^ Reserved Keyword
     | AlexRawToken_SpreadElement -- ^ Reserved Keyword
     | AlexRawToken_ClassExpression -- ^ Reserved Keyword
     | AlexRawToken_OmittedExpression -- ^ Reserved Keyword
     | AlexRawToken_ExpressionWithTypeArguments -- ^ Reserved Keyword
     | AlexRawToken_AsExpression -- ^ Reserved Keyword
     | AlexRawToken_NonNullExpression -- ^ Reserved Keyword
     | AlexRawToken_MetaProperty -- ^ Reserved Keyword
     | AlexRawToken_SyntheticExpression -- ^ Reserved Keyword
     | AlexRawToken_SatisfiesExpression -- ^ Reserved Keyword
     | AlexRawToken_TemplateSpan -- ^ Reserved Keyword
     | AlexRawToken_SemicolonClassElement -- ^ Reserved Keyword
     | AlexRawToken_Block -- ^ Reserved Keyword
     | AlexRawToken_EmptyStatement -- ^ Reserved Keyword
     | AlexRawToken_VariableStatement -- ^ Reserved Keyword
     | AlexRawToken_ExpressionStatement -- ^ Reserved Keyword
     | AlexRawToken_IfStatement -- ^ Reserved Keyword
     | AlexRawToken_DoStatement -- ^ Reserved Keyword
     | AlexRawToken_WhileStatement -- ^ Reserved Keyword
     | AlexRawToken_ForStatement -- ^ Reserved Keyword
     | AlexRawToken_ForInStatement -- ^ Reserved Keyword
     | AlexRawToken_ForOfStatement -- ^ Reserved Keyword
     | AlexRawToken_ContinueStatement -- ^ Reserved Keyword
     | AlexRawToken_BreakStatement -- ^ Reserved Keyword
     | AlexRawToken_ReturnStatement -- ^ Reserved Keyword
     | AlexRawToken_WithStatement -- ^ Reserved Keyword
     | AlexRawToken_SwitchStatement -- ^ Reserved Keyword
     | AlexRawToken_LabeledStatement -- ^ Reserved Keyword
     | AlexRawToken_ThrowStatement -- ^ Reserved Keyword
     | AlexRawToken_TryStatement -- ^ Reserved Keyword
     | AlexRawToken_DebuggerStatement -- ^ Reserved Keyword
     | AlexRawToken_VariableDeclaration -- ^ Reserved Keyword
     | AlexRawToken_VariableDeclarationList -- ^ Reserved Keyword
     | AlexRawToken_FunctionDeclaration -- ^ Reserved Keyword
     | AlexRawToken_ClassDeclaration -- ^ Reserved Keyword
     | AlexRawToken_InterfaceDeclaration -- ^ Reserved Keyword
     | AlexRawToken_TypeAliasDeclaration -- ^ Reserved Keyword
     | AlexRawToken_EnumDeclaration -- ^ Reserved Keyword
     | AlexRawToken_ModuleDeclaration -- ^ Reserved Keyword
     | AlexRawToken_ModuleBlock -- ^ Reserved Keyword
     | AlexRawToken_CaseBlock -- ^ Reserved Keyword
     | AlexRawToken_NamespaceExportDeclaration -- ^ Reserved Keyword
     | AlexRawToken_ImportEqualsDeclaration -- ^ Reserved Keyword
     | AlexRawToken_ImportDeclaration -- ^ Reserved Keyword
     | AlexRawToken_ImportClause -- ^ Reserved Keyword
     | AlexRawToken_NamespaceImport -- ^ Reserved Keyword
     | AlexRawToken_NamedImports -- ^ Reserved Keyword
     | AlexRawToken_ImportSpecifier -- ^ Reserved Keyword
     | AlexRawToken_ExportAssignment -- ^ Reserved Keyword
     | AlexRawToken_ExportDeclaration -- ^ Reserved Keyword
     | AlexRawToken_NamedExports -- ^ Reserved Keyword
     | AlexRawToken_NamespaceExport -- ^ Reserved Keyword
     | AlexRawToken_ExportSpecifier -- ^ Reserved Keyword
     | AlexRawToken_MissingDeclaration -- ^ Reserved Keyword
     | AlexRawToken_ExternalModuleReference -- ^ Reserved Keyword
     | AlexRawToken_JsxElement -- ^ Reserved Keyword
     | AlexRawToken_JsxSelfClosingElement -- ^ Reserved Keyword
     | AlexRawToken_JsxOpeningElement -- ^ Reserved Keyword
     | AlexRawToken_JsxClosingElement -- ^ Reserved Keyword
     | AlexRawToken_JsxFragment -- ^ Reserved Keyword
     | AlexRawToken_JsxOpeningFragment -- ^ Reserved Keyword
     | AlexRawToken_JsxClosingFragment -- ^ Reserved Keyword
     | AlexRawToken_JsxAttribute -- ^ Reserved Keyword
     | AlexRawToken_JsxAttributes -- ^ Reserved Keyword
     | AlexRawToken_JsxSpreadAttribute -- ^ Reserved Keyword
     | AlexRawToken_JsxExpression -- ^ Reserved Keyword
     | AlexRawToken_JsxNamespacedName -- ^ Reserved Keyword
     | AlexRawToken_CaseClause -- ^ Reserved Keyword
     | AlexRawToken_DefaultClause -- ^ Reserved Keyword
     | AlexRawToken_HeritageClause -- ^ Reserved Keyword
     | AlexRawToken_CatchClause -- ^ Reserved Keyword
     | AlexRawToken_ImportAttributes -- ^ Reserved Keyword
     | AlexRawToken_ImportAttribute -- ^ Reserved Keyword
     | AlexRawToken_AssertClause -- ^ Reserved Keyword
     | AlexRawToken_AssertEntry -- ^ Reserved Keyword
     | AlexRawToken_ImportTypeAssertionContainer -- ^ Reserved Keyword
     | AlexRawToken_PropertyAssignment -- ^ Reserved Keyword
     | AlexRawToken_ShorthandPropertyAssignment -- ^ Reserved Keyword
     | AlexRawToken_SpreadAssignment -- ^ Reserved Keyword
     | AlexRawToken_EnumMember -- ^ Reserved Keyword
     | AlexRawToken_SourceFile -- ^ Reserved Keyword
     | AlexRawToken_Bundle -- ^ Reserved Keyword
     | AlexRawToken_JSDocTypeExpression -- ^ Reserved Keyword
     | AlexRawToken_JSDocNameReference -- ^ Reserved Keyword
     | AlexRawToken_JSDocMemberName -- ^ Reserved Keyword
     | AlexRawToken_JSDocAllType -- ^ Reserved Keyword
     | AlexRawToken_JSDocUnknownType -- ^ Reserved Keyword
     | AlexRawToken_JSDocNullableType -- ^ Reserved Keyword
     | AlexRawToken_JSDocNonNullableType -- ^ Reserved Keyword
     | AlexRawToken_JSDocOptionalType -- ^ Reserved Keyword
     | AlexRawToken_JSDocFunctionType -- ^ Reserved Keyword
     | AlexRawToken_JSDocVariadicType -- ^ Reserved Keyword
     | AlexRawToken_JSDocNamepathType -- ^ Reserved Keyword
     | AlexRawToken_JSDoc -- ^ Reserved Keyword
     | AlexRawToken_JSDocComment -- ^ Reserved Keyword
     | AlexRawToken_JSDocText -- ^ Reserved Keyword
     | AlexRawToken_JSDocTypeLiteral -- ^ Reserved Keyword
     | AlexRawToken_JSDocSignature -- ^ Reserved Keyword
     | AlexRawToken_JSDocLink -- ^ Reserved Keyword
     | AlexRawToken_JSDocLinkCode -- ^ Reserved Keyword
     | AlexRawToken_JSDocLinkPlain -- ^ Reserved Keyword
     | AlexRawToken_JSDocTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocAugmentsTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocImplementsTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocAuthorTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocDeprecatedTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocClassTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocPublicTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocPrivateTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocProtectedTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocReadonlyTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocOverrideTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocCallbackTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocOverloadTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocEnumTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocParameterTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocReturnTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocThisTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocTypeTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocTemplateTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocTypedefTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocSeeTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocPropertyTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocThrowsTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocSatisfiesTag -- ^ Reserved Keyword
     | AlexRawToken_JSDocImportTag -- ^ Reserved Keyword
     | AlexRawToken_SyntaxList -- ^ Reserved Keyword
     | AlexRawToken_NotEmittedStatement -- ^ Reserved Keyword
     | AlexRawToken_PartiallyEmittedExpression -- ^ Reserved Keyword
     | AlexRawToken_CommaListExpression -- ^ Reserved Keyword
     | AlexRawToken_SyntheticReferenceExpression -- ^ Reserved Keyword
     | AlexRawToken_Count -- ^ Reserved Keyword
     | AlexRawToken_FirstAssignment -- ^ Reserved Keyword
     | AlexRawToken_LastAssignment -- ^ Reserved Keyword
     | AlexRawToken_FirstCompoundAssignment -- ^ Reserved Keyword
     | AlexRawToken_LastCompoundAssignment -- ^ Reserved Keyword
     | AlexRawToken_FirstReservedWord -- ^ Reserved Keyword
     | AlexRawToken_LastReservedWord -- ^ Reserved Keyword
     | AlexRawToken_FirstKeyword -- ^ Reserved Keyword
     | AlexRawToken_LastKeyword -- ^ Reserved Keyword
     | AlexRawToken_FirstFutureReservedWord -- ^ Reserved Keyword
     | AlexRawToken_LastFutureReservedWord -- ^ Reserved Keyword
     | AlexRawToken_FirstTypeNode -- ^ Reserved Keyword
     | AlexRawToken_LastTypeNode -- ^ Reserved Keyword
     | AlexRawToken_FirstPunctuation -- ^ Reserved Keyword
     | AlexRawToken_LastPunctuation -- ^ Reserved Keyword
     | AlexRawToken_FirstToken -- ^ Reserved Keyword
     | AlexRawToken_LastToken -- ^ Reserved Keyword
     | AlexRawToken_FirstTriviaToken -- ^ Reserved Keyword
     | AlexRawToken_LastTriviaToken -- ^ Reserved Keyword
     | AlexRawToken_FirstLiteralToken -- ^ Reserved Keyword
     | AlexRawToken_LastLiteralToken -- ^ Reserved Keyword
     | AlexRawToken_FirstTemplateToken -- ^ Reserved Keyword
     | AlexRawToken_LastTemplateToken -- ^ Reserved Keyword
     | AlexRawToken_FirstBinaryOperator -- ^ Reserved Keyword
     | AlexRawToken_LastBinaryOperator -- ^ Reserved Keyword
     | AlexRawToken_FirstStatement -- ^ Reserved Keyword
     | AlexRawToken_LastStatement -- ^ Reserved Keyword
     | AlexRawToken_FirstNode -- ^ Reserved Keyword
     | AlexRawToken_FirstJSDocNode -- ^ Reserved Keyword
     | AlexRawToken_LastJSDocNode -- ^ Reserved Keyword
     | AlexRawToken_FirstJSDocTagNode -- ^ Reserved Keyword
     | AlexRawToken_LastJSDocTagNode -- ^ Reserved Keyword

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
            },
            additionalRepoInfo = additional_repo_info alexUserState
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
            },
            additionalRepoInfo = additional_repo_info alexUserState
        }

-- *********************************************
-- * lex' for tokens WITHOUT associated values *
-- *********************************************
lex' :: AlexRawToken -> AlexInput -> Int -> Alex AlexTokenTag
lex' = lex . const

-- **************
-- *            *
-- * alexError' *
-- *            *
-- **************
alexError' :: Location -> Alex a
alexError' location = alexError (show location)

-- ************
-- *          *
-- * filename *
-- *          *
-- ************
getFilename :: AlexTokenTag -> String
getFilename = Location.filename . location

getAdditionalRepoInfo :: AlexTokenTag -> Common.AdditionalRepoInfo
getAdditionalRepoInfo = additionalRepoInfo

-- ************
-- *          *
-- * location *
-- *          *
-- ************
location :: AlexTokenTag -> Location
location = tokenLoc

-- ***************
-- *             *
-- * tokIntValue *
-- *             *
-- ***************
tokIntValue :: AlexTokenTag -> Int
tokIntValue t = case (tokenRaw t) of { AlexRawToken_INT i -> i; _ -> 0; }

-- ***************
-- *             *
-- * tokSTRValue *
-- *             *
-- ***************
tokSTRValue :: AlexTokenTag -> String
tokSTRValue t = case (tokenRaw t) of { AlexRawToken_STR s -> s; _ -> ""; }

-- **************
-- *            *
-- * tokIDValue *
-- *            *
-- **************
tokIDValue :: AlexTokenTag -> String
tokIDValue t = case (tokenRaw t) of { AlexRawToken_ID s -> s; _ -> ""; }

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> Common.AdditionalRepoInfo -> String -> Either String a
runAlex' a fp additionalInfo input = runAlex input (setFilePath fp >> alexSetUserState (AlexUserState fp additionalInfo) >> a)
}

