{
{-# OPTIONS -Werror=missing-fields #-}

module TsParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
-- See the matching NOTE in TsParserActions.hs: `Ast` is imported qualified
-- so that record-field accessors from dhscanner.ast can never accidentally
-- shadow a local binding in an inline action and trip `-Wname-shadowing`
-- (fatal under `-Werror`). Inline actions must spell every Ast reference
-- as `Ast.<thing>`.
import qualified Ast
import TsLexer
import Location
import qualified TsParserActions as Actions
import qualified Token
import qualified Common

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.List ( map, stripPrefix, isPrefixOf )
import Data.Map ( empty, fromList )

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
%lexer { lexwrap } { AlexTokenTag TokenEOF _ _ }

-- ***************************************************
-- * Call this function when an error is encountered *
-- ***************************************************
%error { parseError }

-- ***************************************************************
-- *                                                             *
-- * Zero-tolerance conflict guardrail                            *
-- *                                                             *
-- * Tell Happy we expect EXACTLY 0 shift/reduce conflicts (and  *
-- * 0 reduce/reduce conflicts -- those are always errors). If   *
-- * the actual count differs, Happy aborts with a non-zero exit *
-- * code, which fails `cabal build` (and therefore the docker   *
-- * compose build of the `parsers` service used by              *
-- * agent_loop.py). This guarantees that no grammar edit can    *
-- * silently introduce ambiguity -- a shift/reduce conflict     *
-- * means an existing input may now reduce differently than it  *
-- * did before, which would invalidate the per-iteration        *
-- * strict-progress gate's assumption that previously-passing   *
-- * files still parse the same way.                             *
-- *                                                             *
-- ***************************************************************
%expect 0

%token 

-- ***************
-- *             *
-- * parentheses *
-- *             *
-- ***************

'(' { AlexTokenTag AlexRawToken_LPAREN _ _ }
')' { AlexTokenTag AlexRawToken_RPAREN _ _ }
'[' { AlexTokenTag AlexRawToken_LBRACK _ _ }
']' { AlexTokenTag AlexRawToken_RBRACK _ _ }

-- ************
-- *          *
-- * location *
-- *          *
-- ************

':' { AlexTokenTag AlexRawToken_COLON _ _ }
',' { AlexTokenTag AlexRawToken_COMMA _ _ }
'-' { AlexTokenTag AlexRawToken_MINUS _ _ }

-- reserved keywords start
'Unknown' { AlexTokenTag AlexRawToken_Unknown _ _ }
'EndOfFileToken' { AlexTokenTag AlexRawToken_EndOfFileToken _ _ }
'SingleLineCommentTrivia' { AlexTokenTag AlexRawToken_SingleLineCommentTrivia _ _ }
'MultiLineCommentTrivia' { AlexTokenTag AlexRawToken_MultiLineCommentTrivia _ _ }
'NewLineTrivia' { AlexTokenTag AlexRawToken_NewLineTrivia _ _ }
'WhitespaceTrivia' { AlexTokenTag AlexRawToken_WhitespaceTrivia _ _ }
'ShebangTrivia' { AlexTokenTag AlexRawToken_ShebangTrivia _ _ }
'ConflictMarkerTrivia' { AlexTokenTag AlexRawToken_ConflictMarkerTrivia _ _ }
'NonTextFileMarkerTrivia' { AlexTokenTag AlexRawToken_NonTextFileMarkerTrivia _ _ }
'NumericLiteral' { AlexTokenTag AlexRawToken_NumericLiteral _ _ }
'BigIntLiteral' { AlexTokenTag AlexRawToken_BigIntLiteral _ _ }
'StringLiteral' { AlexTokenTag AlexRawToken_StringLiteral _ _ }
'JsxText' { AlexTokenTag AlexRawToken_JsxText _ _ }
'JsxTextAllWhiteSpaces' { AlexTokenTag AlexRawToken_JsxTextAllWhiteSpaces _ _ }
'RegularExpressionLiteral' { AlexTokenTag AlexRawToken_RegularExpressionLiteral _ _ }
'NoSubstitutionTemplateLiteral' { AlexTokenTag AlexRawToken_NoSubstitutionTemplateLiteral _ _ }
'TemplateHead' { AlexTokenTag AlexRawToken_TemplateHead _ _ }
'TemplateMiddle' { AlexTokenTag AlexRawToken_TemplateMiddle _ _ }
'TemplateTail' { AlexTokenTag AlexRawToken_TemplateTail _ _ }
'OpenBraceToken' { AlexTokenTag AlexRawToken_OpenBraceToken _ _ }
'CloseBraceToken' { AlexTokenTag AlexRawToken_CloseBraceToken _ _ }
'OpenParenToken' { AlexTokenTag AlexRawToken_OpenParenToken _ _ }
'CloseParenToken' { AlexTokenTag AlexRawToken_CloseParenToken _ _ }
'OpenBracketToken' { AlexTokenTag AlexRawToken_OpenBracketToken _ _ }
'CloseBracketToken' { AlexTokenTag AlexRawToken_CloseBracketToken _ _ }
'DotToken' { AlexTokenTag AlexRawToken_DotToken _ _ }
'DotDotDotToken' { AlexTokenTag AlexRawToken_DotDotDotToken _ _ }
'SemicolonToken' { AlexTokenTag AlexRawToken_SemicolonToken _ _ }
'CommaToken' { AlexTokenTag AlexRawToken_CommaToken _ _ }
'QuestionDotToken' { AlexTokenTag AlexRawToken_QuestionDotToken _ _ }
'LessThanToken' { AlexTokenTag AlexRawToken_LessThanToken _ _ }
'LessThanSlashToken' { AlexTokenTag AlexRawToken_LessThanSlashToken _ _ }
'GreaterThanToken' { AlexTokenTag AlexRawToken_GreaterThanToken _ _ }
'LessThanEqualsToken' { AlexTokenTag AlexRawToken_LessThanEqualsToken _ _ }
'GreaterThanEqualsToken' { AlexTokenTag AlexRawToken_GreaterThanEqualsToken _ _ }
'EqualsEqualsToken' { AlexTokenTag AlexRawToken_EqualsEqualsToken _ _ }
'ExclamationEqualsToken' { AlexTokenTag AlexRawToken_ExclamationEqualsToken _ _ }
'EqualsEqualsEqualsToken' { AlexTokenTag AlexRawToken_EqualsEqualsEqualsToken _ _ }
'ExclamationEqualsEqualsToken' { AlexTokenTag AlexRawToken_ExclamationEqualsEqualsToken _ _ }
'EqualsGreaterThanToken' { AlexTokenTag AlexRawToken_EqualsGreaterThanToken _ _ }
'PlusToken' { AlexTokenTag AlexRawToken_PlusToken _ _ }
'MinusToken' { AlexTokenTag AlexRawToken_MinusToken _ _ }
'AsteriskToken' { AlexTokenTag AlexRawToken_AsteriskToken _ _ }
'AsteriskAsteriskToken' { AlexTokenTag AlexRawToken_AsteriskAsteriskToken _ _ }
'SlashToken' { AlexTokenTag AlexRawToken_SlashToken _ _ }
'PercentToken' { AlexTokenTag AlexRawToken_PercentToken _ _ }
'PlusPlusToken' { AlexTokenTag AlexRawToken_PlusPlusToken _ _ }
'MinusMinusToken' { AlexTokenTag AlexRawToken_MinusMinusToken _ _ }
'LessThanLessThanToken' { AlexTokenTag AlexRawToken_LessThanLessThanToken _ _ }
'GreaterThanGreaterThanToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanToken _ _ }
'GreaterThanGreaterThanGreaterThanToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanGreaterThanToken _ _ }
'AmpersandToken' { AlexTokenTag AlexRawToken_AmpersandToken _ _ }
'BarToken' { AlexTokenTag AlexRawToken_BarToken _ _ }
'CaretToken' { AlexTokenTag AlexRawToken_CaretToken _ _ }
'ExclamationToken' { AlexTokenTag AlexRawToken_ExclamationToken _ _ }
'TildeToken' { AlexTokenTag AlexRawToken_TildeToken _ _ }
'AmpersandAmpersandToken' { AlexTokenTag AlexRawToken_AmpersandAmpersandToken _ _ }
'BarBarToken' { AlexTokenTag AlexRawToken_BarBarToken _ _ }
'QuestionToken' { AlexTokenTag AlexRawToken_QuestionToken _ _ }
'ColonToken' { AlexTokenTag AlexRawToken_ColonToken _ _ }
'AtToken' { AlexTokenTag AlexRawToken_AtToken _ _ }
'QuestionQuestionToken' { AlexTokenTag AlexRawToken_QuestionQuestionToken _ _ }
'BacktickToken' { AlexTokenTag AlexRawToken_BacktickToken _ _ }
'HashToken' { AlexTokenTag AlexRawToken_HashToken _ _ }
'EqualsToken' { AlexTokenTag AlexRawToken_EqualsToken _ _ }
'PlusEqualsToken' { AlexTokenTag AlexRawToken_PlusEqualsToken _ _ }
'MinusEqualsToken' { AlexTokenTag AlexRawToken_MinusEqualsToken _ _ }
'AsteriskEqualsToken' { AlexTokenTag AlexRawToken_AsteriskEqualsToken _ _ }
'AsteriskAsteriskEqualsToken' { AlexTokenTag AlexRawToken_AsteriskAsteriskEqualsToken _ _ }
'SlashEqualsToken' { AlexTokenTag AlexRawToken_SlashEqualsToken _ _ }
'PercentEqualsToken' { AlexTokenTag AlexRawToken_PercentEqualsToken _ _ }
'LessThanLessThanEqualsToken' { AlexTokenTag AlexRawToken_LessThanLessThanEqualsToken _ _ }
'GreaterThanGreaterThanEqualsToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanEqualsToken _ _ }
'GreaterThanGreaterThanGreaterThanEqualsToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanGreaterThanEqualsToken _ _ }
'AmpersandEqualsToken' { AlexTokenTag AlexRawToken_AmpersandEqualsToken _ _ }
'BarEqualsToken' { AlexTokenTag AlexRawToken_BarEqualsToken _ _ }
'BarBarEqualsToken' { AlexTokenTag AlexRawToken_BarBarEqualsToken _ _ }
'AmpersandAmpersandEqualsToken' { AlexTokenTag AlexRawToken_AmpersandAmpersandEqualsToken _ _ }
'QuestionQuestionEqualsToken' { AlexTokenTag AlexRawToken_QuestionQuestionEqualsToken _ _ }
'CaretEqualsToken' { AlexTokenTag AlexRawToken_CaretEqualsToken _ _ }
'Identifier' { AlexTokenTag AlexRawToken_Identifier _ _ }
'PrivateIdentifier' { AlexTokenTag AlexRawToken_PrivateIdentifier _ _ }
'BreakKeyword' { AlexTokenTag AlexRawToken_BreakKeyword _ _ }
'CaseKeyword' { AlexTokenTag AlexRawToken_CaseKeyword _ _ }
'CatchKeyword' { AlexTokenTag AlexRawToken_CatchKeyword _ _ }
'ClassKeyword' { AlexTokenTag AlexRawToken_ClassKeyword _ _ }
'ConstKeyword' { AlexTokenTag AlexRawToken_ConstKeyword _ _ }
'ContinueKeyword' { AlexTokenTag AlexRawToken_ContinueKeyword _ _ }
'DebuggerKeyword' { AlexTokenTag AlexRawToken_DebuggerKeyword _ _ }
'DefaultKeyword' { AlexTokenTag AlexRawToken_DefaultKeyword _ _ }
'DeleteKeyword' { AlexTokenTag AlexRawToken_DeleteKeyword _ _ }
'DoKeyword' { AlexTokenTag AlexRawToken_DoKeyword _ _ }
'ElseKeyword' { AlexTokenTag AlexRawToken_ElseKeyword _ _ }
'EnumKeyword' { AlexTokenTag AlexRawToken_EnumKeyword _ _ }
'ExportKeyword' { AlexTokenTag AlexRawToken_ExportKeyword _ _ }
'ExtendsKeyword' { AlexTokenTag AlexRawToken_ExtendsKeyword _ _ }
'FalseKeyword' { AlexTokenTag AlexRawToken_FalseKeyword _ _ }
'FinallyKeyword' { AlexTokenTag AlexRawToken_FinallyKeyword _ _ }
'ForKeyword' { AlexTokenTag AlexRawToken_ForKeyword _ _ }
'FunctionKeyword' { AlexTokenTag AlexRawToken_FunctionKeyword _ _ }
'IfKeyword' { AlexTokenTag AlexRawToken_IfKeyword _ _ }
'ImportKeyword' { AlexTokenTag AlexRawToken_ImportKeyword _ _ }
'InKeyword' { AlexTokenTag AlexRawToken_InKeyword _ _ }
'InstanceOfKeyword' { AlexTokenTag AlexRawToken_InstanceOfKeyword _ _ }
'NewKeyword' { AlexTokenTag AlexRawToken_NewKeyword _ _ }
'NullKeyword' { AlexTokenTag AlexRawToken_NullKeyword _ _ }
'ReturnKeyword' { AlexTokenTag AlexRawToken_ReturnKeyword _ _ }
'SuperKeyword' { AlexTokenTag AlexRawToken_SuperKeyword _ _ }
'SwitchKeyword' { AlexTokenTag AlexRawToken_SwitchKeyword _ _ }
'ThisKeyword' { AlexTokenTag AlexRawToken_ThisKeyword _ _ }
'ThrowKeyword' { AlexTokenTag AlexRawToken_ThrowKeyword _ _ }
'TrueKeyword' { AlexTokenTag AlexRawToken_TrueKeyword _ _ }
'TryKeyword' { AlexTokenTag AlexRawToken_TryKeyword _ _ }
'TypeOfKeyword' { AlexTokenTag AlexRawToken_TypeOfKeyword _ _ }
'VarKeyword' { AlexTokenTag AlexRawToken_VarKeyword _ _ }
'VoidKeyword' { AlexTokenTag AlexRawToken_VoidKeyword _ _ }
'WhileKeyword' { AlexTokenTag AlexRawToken_WhileKeyword _ _ }
'WithKeyword' { AlexTokenTag AlexRawToken_WithKeyword _ _ }
'ImplementsKeyword' { AlexTokenTag AlexRawToken_ImplementsKeyword _ _ }
'InterfaceKeyword' { AlexTokenTag AlexRawToken_InterfaceKeyword _ _ }
'LetKeyword' { AlexTokenTag AlexRawToken_LetKeyword _ _ }
'PackageKeyword' { AlexTokenTag AlexRawToken_PackageKeyword _ _ }
'PrivateKeyword' { AlexTokenTag AlexRawToken_PrivateKeyword _ _ }
'ProtectedKeyword' { AlexTokenTag AlexRawToken_ProtectedKeyword _ _ }
'PublicKeyword' { AlexTokenTag AlexRawToken_PublicKeyword _ _ }
'StaticKeyword' { AlexTokenTag AlexRawToken_StaticKeyword _ _ }
'YieldKeyword' { AlexTokenTag AlexRawToken_YieldKeyword _ _ }
'AbstractKeyword' { AlexTokenTag AlexRawToken_AbstractKeyword _ _ }
'AccessorKeyword' { AlexTokenTag AlexRawToken_AccessorKeyword _ _ }
'AsKeyword' { AlexTokenTag AlexRawToken_AsKeyword _ _ }
'AssertsKeyword' { AlexTokenTag AlexRawToken_AssertsKeyword _ _ }
'AssertKeyword' { AlexTokenTag AlexRawToken_AssertKeyword _ _ }
'AnyKeyword' { AlexTokenTag AlexRawToken_AnyKeyword _ _ }
'AsyncKeyword' { AlexTokenTag AlexRawToken_AsyncKeyword _ _ }
'AwaitKeyword' { AlexTokenTag AlexRawToken_AwaitKeyword _ _ }
'BooleanKeyword' { AlexTokenTag AlexRawToken_BooleanKeyword _ _ }
'ConstructorKeyword' { AlexTokenTag AlexRawToken_ConstructorKeyword _ _ }
'DeclareKeyword' { AlexTokenTag AlexRawToken_DeclareKeyword _ _ }
'GetKeyword' { AlexTokenTag AlexRawToken_GetKeyword _ _ }
'InferKeyword' { AlexTokenTag AlexRawToken_InferKeyword _ _ }
'IntrinsicKeyword' { AlexTokenTag AlexRawToken_IntrinsicKeyword _ _ }
'IsKeyword' { AlexTokenTag AlexRawToken_IsKeyword _ _ }
'KeyOfKeyword' { AlexTokenTag AlexRawToken_KeyOfKeyword _ _ }
'ModuleKeyword' { AlexTokenTag AlexRawToken_ModuleKeyword _ _ }
'NamespaceKeyword' { AlexTokenTag AlexRawToken_NamespaceKeyword _ _ }
'NeverKeyword' { AlexTokenTag AlexRawToken_NeverKeyword _ _ }
'OutKeyword' { AlexTokenTag AlexRawToken_OutKeyword _ _ }
'ReadonlyKeyword' { AlexTokenTag AlexRawToken_ReadonlyKeyword _ _ }
'RequireKeyword' { AlexTokenTag AlexRawToken_RequireKeyword _ _ }
'NumberKeyword' { AlexTokenTag AlexRawToken_NumberKeyword _ _ }
'ObjectKeyword' { AlexTokenTag AlexRawToken_ObjectKeyword _ _ }
'SatisfiesKeyword' { AlexTokenTag AlexRawToken_SatisfiesKeyword _ _ }
'SetKeyword' { AlexTokenTag AlexRawToken_SetKeyword _ _ }
'StringKeyword' { AlexTokenTag AlexRawToken_StringKeyword _ _ }
'SymbolKeyword' { AlexTokenTag AlexRawToken_SymbolKeyword _ _ }
'TypeKeyword' { AlexTokenTag AlexRawToken_TypeKeyword _ _ }
'UndefinedKeyword' { AlexTokenTag AlexRawToken_UndefinedKeyword _ _ }
'UniqueKeyword' { AlexTokenTag AlexRawToken_UniqueKeyword _ _ }
'UnknownKeyword' { AlexTokenTag AlexRawToken_UnknownKeyword _ _ }
'UsingKeyword' { AlexTokenTag AlexRawToken_UsingKeyword _ _ }
'FromKeyword' { AlexTokenTag AlexRawToken_FromKeyword _ _ }
'GlobalKeyword' { AlexTokenTag AlexRawToken_GlobalKeyword _ _ }
'BigIntKeyword' { AlexTokenTag AlexRawToken_BigIntKeyword _ _ }
'OverrideKeyword' { AlexTokenTag AlexRawToken_OverrideKeyword _ _ }
'OfKeyword' { AlexTokenTag AlexRawToken_OfKeyword _ _ }
'QualifiedName' { AlexTokenTag AlexRawToken_QualifiedName _ _ }
'ComputedPropertyName' { AlexTokenTag AlexRawToken_ComputedPropertyName _ _ }
'TypeParameter' { AlexTokenTag AlexRawToken_TypeParameter _ _ }
'Parameter' { AlexTokenTag AlexRawToken_Parameter _ _ }
'Decorator' { AlexTokenTag AlexRawToken_Decorator _ _ }
'PropertySignature' { AlexTokenTag AlexRawToken_PropertySignature _ _ }
'PropertyDeclaration' { AlexTokenTag AlexRawToken_PropertyDeclaration _ _ }
'MethodSignature' { AlexTokenTag AlexRawToken_MethodSignature _ _ }
'MethodDeclaration' { AlexTokenTag AlexRawToken_MethodDeclaration _ _ }
'ClassStaticBlockDeclaration' { AlexTokenTag AlexRawToken_ClassStaticBlockDeclaration _ _ }
'Constructor' { AlexTokenTag AlexRawToken_Constructor _ _ }
'GetAccessor' { AlexTokenTag AlexRawToken_GetAccessor _ _ }
'SetAccessor' { AlexTokenTag AlexRawToken_SetAccessor _ _ }
'CallSignature' { AlexTokenTag AlexRawToken_CallSignature _ _ }
'ConstructSignature' { AlexTokenTag AlexRawToken_ConstructSignature _ _ }
'IndexSignature' { AlexTokenTag AlexRawToken_IndexSignature _ _ }
'TypePredicate' { AlexTokenTag AlexRawToken_TypePredicate _ _ }
'TypeReference' { AlexTokenTag AlexRawToken_TypeReference _ _ }
'FunctionType' { AlexTokenTag AlexRawToken_FunctionType _ _ }
'ConstructorType' { AlexTokenTag AlexRawToken_ConstructorType _ _ }
'TypeQuery' { AlexTokenTag AlexRawToken_TypeQuery _ _ }
'TypeLiteral' { AlexTokenTag AlexRawToken_TypeLiteral _ _ }
'ArrayType' { AlexTokenTag AlexRawToken_ArrayType _ _ }
'TupleType' { AlexTokenTag AlexRawToken_TupleType _ _ }
'OptionalType' { AlexTokenTag AlexRawToken_OptionalType _ _ }
'RestType' { AlexTokenTag AlexRawToken_RestType _ _ }
'UnionType' { AlexTokenTag AlexRawToken_UnionType _ _ }
'IntersectionType' { AlexTokenTag AlexRawToken_IntersectionType _ _ }
'ConditionalType' { AlexTokenTag AlexRawToken_ConditionalType _ _ }
'InferType' { AlexTokenTag AlexRawToken_InferType _ _ }
'ParenthesizedType' { AlexTokenTag AlexRawToken_ParenthesizedType _ _ }
'ThisType' { AlexTokenTag AlexRawToken_ThisType _ _ }
'TypeOperator' { AlexTokenTag AlexRawToken_TypeOperator _ _ }
'IndexedAccessType' { AlexTokenTag AlexRawToken_IndexedAccessType _ _ }
'MappedType' { AlexTokenTag AlexRawToken_MappedType _ _ }
'LiteralType' { AlexTokenTag AlexRawToken_LiteralType _ _ }
'NamedTupleMember' { AlexTokenTag AlexRawToken_NamedTupleMember _ _ }
'TemplateLiteralType' { AlexTokenTag AlexRawToken_TemplateLiteralType _ _ }
'TemplateLiteralTypeSpan' { AlexTokenTag AlexRawToken_TemplateLiteralTypeSpan _ _ }
'ImportType' { AlexTokenTag AlexRawToken_ImportType _ _ }
'ObjectBindingPattern' { AlexTokenTag AlexRawToken_ObjectBindingPattern _ _ }
'ArrayBindingPattern' { AlexTokenTag AlexRawToken_ArrayBindingPattern _ _ }
'BindingElement' { AlexTokenTag AlexRawToken_BindingElement _ _ }
'ArrayLiteralExpression' { AlexTokenTag AlexRawToken_ArrayLiteralExpression _ _ }
'ObjectLiteralExpression' { AlexTokenTag AlexRawToken_ObjectLiteralExpression _ _ }
'PropertyAccessExpression' { AlexTokenTag AlexRawToken_PropertyAccessExpression _ _ }
'ElementAccessExpression' { AlexTokenTag AlexRawToken_ElementAccessExpression _ _ }
'CallExpression' { AlexTokenTag AlexRawToken_CallExpression _ _ }
'NewExpression' { AlexTokenTag AlexRawToken_NewExpression _ _ }
'TaggedTemplateExpression' { AlexTokenTag AlexRawToken_TaggedTemplateExpression _ _ }
'TypeAssertionExpression' { AlexTokenTag AlexRawToken_TypeAssertionExpression _ _ }
'ParenthesizedExpression' { AlexTokenTag AlexRawToken_ParenthesizedExpression _ _ }
'FunctionExpression' { AlexTokenTag AlexRawToken_FunctionExpression _ _ }
'ArrowFunction' { AlexTokenTag AlexRawToken_ArrowFunction _ _ }
'DeleteExpression' { AlexTokenTag AlexRawToken_DeleteExpression _ _ }
'TypeOfExpression' { AlexTokenTag AlexRawToken_TypeOfExpression _ _ }
'VoidExpression' { AlexTokenTag AlexRawToken_VoidExpression _ _ }
'AwaitExpression' { AlexTokenTag AlexRawToken_AwaitExpression _ _ }
'PrefixUnaryExpression' { AlexTokenTag AlexRawToken_PrefixUnaryExpression _ _ }
'PostfixUnaryExpression' { AlexTokenTag AlexRawToken_PostfixUnaryExpression _ _ }
'BinaryExpression' { AlexTokenTag AlexRawToken_BinaryExpression _ _ }
'ConditionalExpression' { AlexTokenTag AlexRawToken_ConditionalExpression _ _ }
'TemplateExpression' { AlexTokenTag AlexRawToken_TemplateExpression _ _ }
'YieldExpression' { AlexTokenTag AlexRawToken_YieldExpression _ _ }
'SpreadElement' { AlexTokenTag AlexRawToken_SpreadElement _ _ }
'ClassExpression' { AlexTokenTag AlexRawToken_ClassExpression _ _ }
'OmittedExpression' { AlexTokenTag AlexRawToken_OmittedExpression _ _ }
'ExpressionWithTypeArguments' { AlexTokenTag AlexRawToken_ExpressionWithTypeArguments _ _ }
'AsExpression' { AlexTokenTag AlexRawToken_AsExpression _ _ }
'NonNullExpression' { AlexTokenTag AlexRawToken_NonNullExpression _ _ }
'MetaProperty' { AlexTokenTag AlexRawToken_MetaProperty _ _ }
'SyntheticExpression' { AlexTokenTag AlexRawToken_SyntheticExpression _ _ }
'SatisfiesExpression' { AlexTokenTag AlexRawToken_SatisfiesExpression _ _ }
'TemplateSpan' { AlexTokenTag AlexRawToken_TemplateSpan _ _ }
'SemicolonClassElement' { AlexTokenTag AlexRawToken_SemicolonClassElement _ _ }
'Block' { AlexTokenTag AlexRawToken_Block _ _ }
'EmptyStatement' { AlexTokenTag AlexRawToken_EmptyStatement _ _ }
'VariableStatement' { AlexTokenTag AlexRawToken_VariableStatement _ _ }
'ExpressionStatement' { AlexTokenTag AlexRawToken_ExpressionStatement _ _ }
'IfStatement' { AlexTokenTag AlexRawToken_IfStatement _ _ }
'DoStatement' { AlexTokenTag AlexRawToken_DoStatement _ _ }
'WhileStatement' { AlexTokenTag AlexRawToken_WhileStatement _ _ }
'ForStatement' { AlexTokenTag AlexRawToken_ForStatement _ _ }
'ForInStatement' { AlexTokenTag AlexRawToken_ForInStatement _ _ }
'ForOfStatement' { AlexTokenTag AlexRawToken_ForOfStatement _ _ }
'ContinueStatement' { AlexTokenTag AlexRawToken_ContinueStatement _ _ }
'BreakStatement' { AlexTokenTag AlexRawToken_BreakStatement _ _ }
'ReturnStatement' { AlexTokenTag AlexRawToken_ReturnStatement _ _ }
'WithStatement' { AlexTokenTag AlexRawToken_WithStatement _ _ }
'SwitchStatement' { AlexTokenTag AlexRawToken_SwitchStatement _ _ }
'LabeledStatement' { AlexTokenTag AlexRawToken_LabeledStatement _ _ }
'ThrowStatement' { AlexTokenTag AlexRawToken_ThrowStatement _ _ }
'TryStatement' { AlexTokenTag AlexRawToken_TryStatement _ _ }
'DebuggerStatement' { AlexTokenTag AlexRawToken_DebuggerStatement _ _ }
'VariableDeclaration' { AlexTokenTag AlexRawToken_VariableDeclaration _ _ }
'VariableDeclarationList' { AlexTokenTag AlexRawToken_VariableDeclarationList _ _ }
'FunctionDeclaration' { AlexTokenTag AlexRawToken_FunctionDeclaration _ _ }
'ClassDeclaration' { AlexTokenTag AlexRawToken_ClassDeclaration _ _ }
'InterfaceDeclaration' { AlexTokenTag AlexRawToken_InterfaceDeclaration _ _ }
'TypeAliasDeclaration' { AlexTokenTag AlexRawToken_TypeAliasDeclaration _ _ }
'EnumDeclaration' { AlexTokenTag AlexRawToken_EnumDeclaration _ _ }
'ModuleDeclaration' { AlexTokenTag AlexRawToken_ModuleDeclaration _ _ }
'ModuleBlock' { AlexTokenTag AlexRawToken_ModuleBlock _ _ }
'CaseBlock' { AlexTokenTag AlexRawToken_CaseBlock _ _ }
'NamespaceExportDeclaration' { AlexTokenTag AlexRawToken_NamespaceExportDeclaration _ _ }
'ImportEqualsDeclaration' { AlexTokenTag AlexRawToken_ImportEqualsDeclaration _ _ }
'ImportDeclaration' { AlexTokenTag AlexRawToken_ImportDeclaration _ _ }
'ImportClause' { AlexTokenTag AlexRawToken_ImportClause _ _ }
'NamespaceImport' { AlexTokenTag AlexRawToken_NamespaceImport _ _ }
'NamedImports' { AlexTokenTag AlexRawToken_NamedImports _ _ }
'ImportSpecifier' { AlexTokenTag AlexRawToken_ImportSpecifier _ _ }
'ExportAssignment' { AlexTokenTag AlexRawToken_ExportAssignment _ _ }
'ExportDeclaration' { AlexTokenTag AlexRawToken_ExportDeclaration _ _ }
'NamedExports' { AlexTokenTag AlexRawToken_NamedExports _ _ }
'NamespaceExport' { AlexTokenTag AlexRawToken_NamespaceExport _ _ }
'ExportSpecifier' { AlexTokenTag AlexRawToken_ExportSpecifier _ _ }
'MissingDeclaration' { AlexTokenTag AlexRawToken_MissingDeclaration _ _ }
'ExternalModuleReference' { AlexTokenTag AlexRawToken_ExternalModuleReference _ _ }
'JsxElement' { AlexTokenTag AlexRawToken_JsxElement _ _ }
'JsxSelfClosingElement' { AlexTokenTag AlexRawToken_JsxSelfClosingElement _ _ }
'JsxOpeningElement' { AlexTokenTag AlexRawToken_JsxOpeningElement _ _ }
'JsxClosingElement' { AlexTokenTag AlexRawToken_JsxClosingElement _ _ }
'JsxFragment' { AlexTokenTag AlexRawToken_JsxFragment _ _ }
'JsxOpeningFragment' { AlexTokenTag AlexRawToken_JsxOpeningFragment _ _ }
'JsxClosingFragment' { AlexTokenTag AlexRawToken_JsxClosingFragment _ _ }
'JsxAttribute' { AlexTokenTag AlexRawToken_JsxAttribute _ _ }
'JsxAttributes' { AlexTokenTag AlexRawToken_JsxAttributes _ _ }
'JsxSpreadAttribute' { AlexTokenTag AlexRawToken_JsxSpreadAttribute _ _ }
'JsxExpression' { AlexTokenTag AlexRawToken_JsxExpression _ _ }
'JsxNamespacedName' { AlexTokenTag AlexRawToken_JsxNamespacedName _ _ }
'CaseClause' { AlexTokenTag AlexRawToken_CaseClause _ _ }
'DefaultClause' { AlexTokenTag AlexRawToken_DefaultClause _ _ }
'HeritageClause' { AlexTokenTag AlexRawToken_HeritageClause _ _ }
'CatchClause' { AlexTokenTag AlexRawToken_CatchClause _ _ }
'ImportAttributes' { AlexTokenTag AlexRawToken_ImportAttributes _ _ }
'ImportAttribute' { AlexTokenTag AlexRawToken_ImportAttribute _ _ }
'AssertClause' { AlexTokenTag AlexRawToken_AssertClause _ _ }
'AssertEntry' { AlexTokenTag AlexRawToken_AssertEntry _ _ }
'ImportTypeAssertionContainer' { AlexTokenTag AlexRawToken_ImportTypeAssertionContainer _ _ }
'PropertyAssignment' { AlexTokenTag AlexRawToken_PropertyAssignment _ _ }
'ShorthandPropertyAssignment' { AlexTokenTag AlexRawToken_ShorthandPropertyAssignment _ _ }
'SpreadAssignment' { AlexTokenTag AlexRawToken_SpreadAssignment _ _ }
'EnumMember' { AlexTokenTag AlexRawToken_EnumMember _ _ }
'SourceFile' { AlexTokenTag AlexRawToken_SourceFile _ _ }
'Bundle' { AlexTokenTag AlexRawToken_Bundle _ _ }
'JSDocTypeExpression' { AlexTokenTag AlexRawToken_JSDocTypeExpression _ _ }
'JSDocNameReference' { AlexTokenTag AlexRawToken_JSDocNameReference _ _ }
'JSDocMemberName' { AlexTokenTag AlexRawToken_JSDocMemberName _ _ }
'JSDocAllType' { AlexTokenTag AlexRawToken_JSDocAllType _ _ }
'JSDocUnknownType' { AlexTokenTag AlexRawToken_JSDocUnknownType _ _ }
'JSDocNullableType' { AlexTokenTag AlexRawToken_JSDocNullableType _ _ }
'JSDocNonNullableType' { AlexTokenTag AlexRawToken_JSDocNonNullableType _ _ }
'JSDocOptionalType' { AlexTokenTag AlexRawToken_JSDocOptionalType _ _ }
'JSDocFunctionType' { AlexTokenTag AlexRawToken_JSDocFunctionType _ _ }
'JSDocVariadicType' { AlexTokenTag AlexRawToken_JSDocVariadicType _ _ }
'JSDocNamepathType' { AlexTokenTag AlexRawToken_JSDocNamepathType _ _ }
'JSDoc' { AlexTokenTag AlexRawToken_JSDoc _ _ }
'JSDocComment' { AlexTokenTag AlexRawToken_JSDocComment _ _ }
'JSDocText' { AlexTokenTag AlexRawToken_JSDocText _ _ }
'JSDocTypeLiteral' { AlexTokenTag AlexRawToken_JSDocTypeLiteral _ _ }
'JSDocSignature' { AlexTokenTag AlexRawToken_JSDocSignature _ _ }
'JSDocLink' { AlexTokenTag AlexRawToken_JSDocLink _ _ }
'JSDocLinkCode' { AlexTokenTag AlexRawToken_JSDocLinkCode _ _ }
'JSDocLinkPlain' { AlexTokenTag AlexRawToken_JSDocLinkPlain _ _ }
'JSDocTag' { AlexTokenTag AlexRawToken_JSDocTag _ _ }
'JSDocAugmentsTag' { AlexTokenTag AlexRawToken_JSDocAugmentsTag _ _ }
'JSDocImplementsTag' { AlexTokenTag AlexRawToken_JSDocImplementsTag _ _ }
'JSDocAuthorTag' { AlexTokenTag AlexRawToken_JSDocAuthorTag _ _ }
'JSDocDeprecatedTag' { AlexTokenTag AlexRawToken_JSDocDeprecatedTag _ _ }
'JSDocClassTag' { AlexTokenTag AlexRawToken_JSDocClassTag _ _ }
'JSDocPublicTag' { AlexTokenTag AlexRawToken_JSDocPublicTag _ _ }
'JSDocPrivateTag' { AlexTokenTag AlexRawToken_JSDocPrivateTag _ _ }
'JSDocProtectedTag' { AlexTokenTag AlexRawToken_JSDocProtectedTag _ _ }
'JSDocReadonlyTag' { AlexTokenTag AlexRawToken_JSDocReadonlyTag _ _ }
'JSDocOverrideTag' { AlexTokenTag AlexRawToken_JSDocOverrideTag _ _ }
'JSDocCallbackTag' { AlexTokenTag AlexRawToken_JSDocCallbackTag _ _ }
'JSDocOverloadTag' { AlexTokenTag AlexRawToken_JSDocOverloadTag _ _ }
'JSDocEnumTag' { AlexTokenTag AlexRawToken_JSDocEnumTag _ _ }
'JSDocParameterTag' { AlexTokenTag AlexRawToken_JSDocParameterTag _ _ }
'JSDocReturnTag' { AlexTokenTag AlexRawToken_JSDocReturnTag _ _ }
'JSDocThisTag' { AlexTokenTag AlexRawToken_JSDocThisTag _ _ }
'JSDocTypeTag' { AlexTokenTag AlexRawToken_JSDocTypeTag _ _ }
'JSDocTemplateTag' { AlexTokenTag AlexRawToken_JSDocTemplateTag _ _ }
'JSDocTypedefTag' { AlexTokenTag AlexRawToken_JSDocTypedefTag _ _ }
'JSDocSeeTag' { AlexTokenTag AlexRawToken_JSDocSeeTag _ _ }
'JSDocPropertyTag' { AlexTokenTag AlexRawToken_JSDocPropertyTag _ _ }
'JSDocThrowsTag' { AlexTokenTag AlexRawToken_JSDocThrowsTag _ _ }
'JSDocSatisfiesTag' { AlexTokenTag AlexRawToken_JSDocSatisfiesTag _ _ }
'JSDocImportTag' { AlexTokenTag AlexRawToken_JSDocImportTag _ _ }
'SyntaxList' { AlexTokenTag AlexRawToken_SyntaxList _ _ }
'NotEmittedStatement' { AlexTokenTag AlexRawToken_NotEmittedStatement _ _ }
'PartiallyEmittedExpression' { AlexTokenTag AlexRawToken_PartiallyEmittedExpression _ _ }
'CommaListExpression' { AlexTokenTag AlexRawToken_CommaListExpression _ _ }
'SyntheticReferenceExpression' { AlexTokenTag AlexRawToken_SyntheticReferenceExpression _ _ }
'Count' { AlexTokenTag AlexRawToken_Count _ _ }
'FirstAssignment' { AlexTokenTag AlexRawToken_FirstAssignment _ _ }
'LastAssignment' { AlexTokenTag AlexRawToken_LastAssignment _ _ }
'FirstCompoundAssignment' { AlexTokenTag AlexRawToken_FirstCompoundAssignment _ _ }
'LastCompoundAssignment' { AlexTokenTag AlexRawToken_LastCompoundAssignment _ _ }
'FirstReservedWord' { AlexTokenTag AlexRawToken_FirstReservedWord _ _ }
'LastReservedWord' { AlexTokenTag AlexRawToken_LastReservedWord _ _ }
'FirstKeyword' { AlexTokenTag AlexRawToken_FirstKeyword _ _ }
'LastKeyword' { AlexTokenTag AlexRawToken_LastKeyword _ _ }
'FirstFutureReservedWord' { AlexTokenTag AlexRawToken_FirstFutureReservedWord _ _ }
'LastFutureReservedWord' { AlexTokenTag AlexRawToken_LastFutureReservedWord _ _ }
'FirstTypeNode' { AlexTokenTag AlexRawToken_FirstTypeNode _ _ }
'LastTypeNode' { AlexTokenTag AlexRawToken_LastTypeNode _ _ }
'FirstPunctuation' { AlexTokenTag AlexRawToken_FirstPunctuation _ _ }
'LastPunctuation' { AlexTokenTag AlexRawToken_LastPunctuation _ _ }
'FirstToken' { AlexTokenTag AlexRawToken_FirstToken _ _ }
'LastToken' { AlexTokenTag AlexRawToken_LastToken _ _ }
'FirstTriviaToken' { AlexTokenTag AlexRawToken_FirstTriviaToken _ _ }
'LastTriviaToken' { AlexTokenTag AlexRawToken_LastTriviaToken _ _ }
'FirstLiteralToken' { AlexTokenTag AlexRawToken_FirstLiteralToken _ _ }
'LastLiteralToken' { AlexTokenTag AlexRawToken_LastLiteralToken _ _ }
'FirstTemplateToken' { AlexTokenTag AlexRawToken_FirstTemplateToken _ _ }
'LastTemplateToken' { AlexTokenTag AlexRawToken_LastTemplateToken _ _ }
'FirstBinaryOperator' { AlexTokenTag AlexRawToken_FirstBinaryOperator _ _ }
'LastBinaryOperator' { AlexTokenTag AlexRawToken_LastBinaryOperator _ _ }
'FirstStatement' { AlexTokenTag AlexRawToken_FirstStatement _ _ }
'LastStatement' { AlexTokenTag AlexRawToken_LastStatement _ _ }
'FirstNode' { AlexTokenTag AlexRawToken_FirstNode _ _ }
'FirstJSDocNode' { AlexTokenTag AlexRawToken_FirstJSDocNode _ _ }
'LastJSDocNode' { AlexTokenTag AlexRawToken_LastJSDocNode _ _ }
'FirstJSDocTagNode' { AlexTokenTag AlexRawToken_FirstJSDocTagNode _ _ }
'LastJSDocTagNode' { AlexTokenTag AlexRawToken_LastJSDocTagNode _ _ }
-- reserved keywords end

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************

INT { AlexTokenTag (AlexRawToken_INT  i) _ _ }
STR { AlexTokenTag (AlexRawToken_STR  s) _ _ }
ID  { AlexTokenTag (AlexRawToken_ID  id) _ _ }

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
listof(a):          a { [$1] } | a listof(a)                                       { $1:$2 }
barlistof(a):       a { [$1] } | a ',' 'BarToken'   loc '(' ')' ',' barlistof(a)   { $1:$8 }
ampersandlistof(a): a { [$1] } | a 'AmpersandToken' loc '(' ')' ampersandlistof(a) { $1:$6 } | a ',' 'AmpersandToken' loc '(' ')' ',' ampersandlistof(a) { $1:$8 }

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
commalistof(a): a { [$1] } | a ',' commalistof(a) { $1:$3 }
possibly_empty_commalistof(a): { [] } | commalistof(a) { $1 }

-- ********************************************************
-- *                                                      *
-- * parametrized list with optional trailing comma       *
-- *                                                      *
-- ********************************************************
possibly_empty_commalistof_with_optional_trailing_comma(a): { [] } | commalistof_with_optional_trailing_comma(a) { $1 }
commalistof_with_optional_trailing_comma(a): a commalistof_with_optional_trailing_comma_rest(a) { $1:$2 }
commalistof_with_optional_trailing_comma_rest(a): ',' a commalistof_with_optional_trailing_comma_rest(a) { $2:$3 } | ',' { [] } | { [] }

-- ******************
-- *                *
-- * optional rules *
-- *                *
-- ******************
optional(a): { Nothing } | a { Just $1 }

-- ****************
-- *              *
-- * choice rules *
-- *              *
-- ****************
choice(a, b): a { Left $1 } | b { Right $1 }

-- direct translation to: Ast.Root
program:
program_1 { $1 } |
program_2 { $1 } |
program_3 { $1 }

program_1: optional(',') commalistof(stmt) { Actions.root $2 }
program_2: 'SourceFile' loc '(' 'SyntaxList' loc '(' commalistof(stmt) ')' ')' { Actions.root $7 }
program_3: 'ModuleDeclaration' loc '(' optional(declareKeyword) identifier moduleBlock ')' { Actions.root $6 }

stmt:
stmtIf { $1 } |
stmtExp { $1 } |
stmtTry { $1 } |
stmtFunc { $1 } |
stmtImport { $1 } |
stmtExport { $1 } |
stmt_property    { $1 } |
stmtClass        { $1 } |
stmtReturn       { $1 } |
stmtThrow        { $1 } |
stmtBreak       { $1 } |
stmtContinue     { $1 } |
stmtWhile       { $1 } |
stmtDo       { $1 } |
stmtForOf        { $1 } |
stmtForIn        { $1 } |
stmtFor        { $1 } |
stmtSwitch     { $1 } |
stmtEnum        { $1 } |
stmtTypeAlias    { $1 } |
indexSignature   { $1 } |
stmtDecvar       { $1 }

-- direct translation to dhscanner Ast.StmtIf
stmtIf:
'IfStatement' loc
'('
    ifKeyword
    openParenToken
    exp
    closeParenToken
    stmtOrBlock
    optional(elsePart)
')'
{
    Actions.stmtIf $2 $6 $8 $9
}

-- helpers related to stmtIf
elsePart: elseKeyword stmtOrBlock { $2 }
stmtOrBlock: stmt { [$1] } | block { $1 }

-- direct translation to dhscanner Ast.StmtExp
stmtExp: 'ExpressionStatement' loc '(' expOrStmtAssign ')' { $4 }

-- helpers related to stmtExp
expOrStmtAssign: exp { Ast.StmtExp $1 } | stmtAssign { $1 }

-- direct translation to dhscanner Ast.StmtTry
stmtTry:
'TryStatement' loc
'('
    tryKeyword
    block
    catchPart
    optional(finallyPart)
')'
{
    Actions.stmtTry $2 $5 $6
}

-- helpers related to stmtTry
catchClauseVariable: openParenToken 'VariableDeclaration' loc '(' identifier optional(type_hint) ')' closeParenToken { Nothing }
catchPart: 'CatchClause' loc '(' catchKeyword optional(catchClauseVariable) block ')' { $6 }

finallyPart:
finallyKeyword block { $2 }

-- instrumented as dhscanner Ast.StmtExp
stmtThrow: 'ThrowStatement' loc '(' throwKeyword exp ')' { Actions.stmtThrow $2 $5 }

-- instrumented as dhscanner Ast.StmtWhile
stmtForOf:
'ForOfStatement' loc
'('
    forKeyword
    openParenToken
    'VariableDeclarationList' loc
    '('
        'VariableDeclaration' loc
        '('
            decvarLhs
        ')'
    ')'
    ofKeyword
    exp
    closeParenToken
    stmtOrBlock
')'
{
    Actions.stmtWhile $2 $16 $18
}

-- instrumented as dhscanner Ast.StmtWhile
stmtForIn:
'ForInStatement' loc
'('
    forKeyword
    openParenToken
    'VariableDeclarationList' loc
    '('
        'VariableDeclaration' loc
        '('
            decvarLhs
        ')'
    ')'
    inKeyword
    exp
    closeParenToken
    stmtOrBlock
')'
{
    Actions.stmtWhile $2 $16 $18
}

-- instrumented as dhscanner Ast.StmtWhile
stmtFor:
'ForStatement' loc
'('
    forKeyword
    openParenToken
    'VariableDeclarationList' loc
    '('
        'VariableDeclaration' loc
        '('
            decvarLhs
            firstAssignment
            exp
        ')'
    ')'
    optional(semicolonToken)
    exp
    optional(semicolonToken)
    exp
    closeParenToken
    stmtOrBlock
')'
{
    Actions.stmtWhile $2 $18 $22
}

-- direct dhscanner subtree creation: Ast.StmtWhile
stmtWhile:
'WhileStatement' loc
'('
    whileKeyword
    openParenToken
    exp
    closeParenToken
    stmtOrBlock
')'
{
    Actions.stmtWhile $2 $6 $8
}

-- instrumented as dhscanner Ast.StmtWhile
stmtDo:
'DoStatement' loc
'('
    doKeyword
    stmtOrBlock
    whileKeyword
    openParenToken
    exp
    closeParenToken
    optional(semicolonToken)
')'
{
    Actions.stmtWhile $2 $8 $5
}

-- instrumented as dhscanner Ast.StmtBlock
stmtSwitch:
'SwitchStatement' loc
'('
    switchKeyword
    openParenToken
    exp
    closeParenToken
    'CaseBlock' loc
    '('
        possibly_empty_commalistof(switchClause)
    ')'
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = concat $11,
        Ast.stmtBlockLocation = $2
    }
}

switchClause:
switchCase { $1 } |
switchDefault { $1 }

switchCase:
'CaseClause' loc
'('
    optional(caseKeyword)
    exp
    optional(colonToken)
    possibly_empty_commalistof(stmtOrBlock)
')'
{
    concat $7
}

switchDefault:
'DefaultClause' loc
'('
    optional(defaultKeyword)
    optional(colonToken)
    possibly_empty_commalistof(stmtOrBlock)
')'
{
    concat $6
}

-- direct translation to dhscanner Ast.StmtClass
stmtClass:
stmtClass_1 { $1 } |
stmtClass_2 { $1 }

stmtClass_1:
'InterfaceDeclaration' loc
'('
    interfaceKeyword
    identifier
    optional(generics)
    optional(extends)
    commalistof(stmt)
')'
{
    Actions.stmtClass $5 $7 []
}

stmtClass_2:
'ClassDeclaration' loc
'('
    classKeyword
    identifier
    optional(generics)
    optional(extends)
    possibly_empty_commalistof(stmtPropertyOrMethod)
')'
{
    Actions.stmtClass $5 $7 $8
}

stmtPropertyOrMethod:
stmtProperty { $1 } |
stmtMethod { $1 } |
stmtRedundantExtraSemicolon { $1 }

stmtProperty:
'PropertyDeclaration' loc
'('
    optional(staticKeyword)
    identifier optional(questionToken) optional(type_hint) optional(default_value) optional(semicolonToken)
')'
{
    Nothing
}

stmtMethod:
stmtMethod_1 { $1 } |
stmtMethod_2 { $1 }

stmtMethod_1:
'MethodDeclaration' loc
'('
    optional(publicKeyword)
    optional(staticKeyword)
    identifier
    openParenToken
    parameters
    closeParenToken
    optional(type_hint)
    optional(block)
')'
{
    Actions.stmtMethod $2 $6 $8 $10 $11
}

stmtMethod_2:
'Constructor' loc
'('
    optional(constructorKeyword)
    openParenToken
    parameters
    closeParenToken
    optional(block)
')'
{
    Actions.stmtConstructor $2 $6 $8
}

stmtRedundantExtraSemicolon: 'SemicolonClassElement' loc '(' ')' { Nothing }

stmtFunc:
'FunctionDeclaration' loc
'('
    functionKeyword
    identifier
    optional(typeParameters)
    openParenToken
    parameters
    closeParenToken
    optional(type_hint)
    optional(block)
')'
{
    Actions.stmtFunc $2 $5 $8 $11
}

-- helpers related to stmtFunc
identifier:
'Identifier' loc
'('
    ID
')'
{
    Token.Named
    {
        Token.content = tokIDValue $4,
        Token.location = $2
    }
}

parameters:
possibly_empty_commalistof(parameterChunk) { concat $1 }

parameterChunk:
parameterChunk1 { $1 } |
parameterChunk2 { $1 } |
parameterChunk3 { $1 } |
parameterChunk4 { $1 } |
parameterChunk5 { $1 } |
parameterChunk6 { $1 } |
parameterChunk7 { $1 } |
parameterChunk8 { $1 } |
parameterChunk9 { $1 }

parameterChunk1:
'Parameter' loc
'('
    identifier
    optional(questionToken)
    optional(type_hint)
    optional(default_value)
')'
{
    Actions.parameterChunk1 $4 $6
}

parameterChunk2:
'Parameter' loc
'('
    objectBindingPattern
')'
{
    []
}

parameterChunk3:
'Parameter' loc
'('
    objectBindingPattern
    colonToken
    'TypeLiteral' loc
    '('
        commalistof(property_signature_as_param)
    ')'
    optional(default_value)
')'
{
    $9
}

-- helpers related to stmtFunc
parameterChunk4:
'Parameter' loc
'('
    arrayBindingPattern
')'
{
    []
}

parameterChunk5:
'Parameter' loc
'('
    objectBindingPattern
    colonToken
    typeReference
')'
{
    []
}

parameterChunk6:
'Parameter' loc
'('
    dotDotDotToken
    identifier
    optional(type_hint)
')'
{
    Actions.parameterChunk1 $5 $6
}

parameterChunk7:
'Parameter' loc
'('
    objectBindingPattern
    colonToken
    intersection_type
')'
{
    []
}

parameterChunk8:
'Parameter' loc
'('
    objectBindingPattern
    colonToken
    union_type
    optional(default_value)
')'
{
    []
}

-- Allow parameter properties with 'public' modifier in constructors
parameterChunk9:
'Parameter' loc
'('
    publicKeyword
    identifier
    optional(questionToken)
    optional(type_hint)
    optional(default_value)
')'
{
    Actions.parameterChunk1 $5 $7
}

property_signature_as_param:
'PropertySignature' loc
'('
    identifier
    optional(questionToken)
    type_hint
')'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $4,
        Ast.paramNominalType = case $6 of { Just t -> Just (Actions.varify t); _ -> Nothing },
        Ast.paramSerialIdx = 156
    }
}

-- helpers related to stmtFunc
type_hint: colonToken type { $2 }

-- helpers related to stmtFunc
type:
expressionWithTypeArguments { $1 } |
indexedAccessType { Nothing } |
union_type { Nothing } |
intersection_type { Nothing } |
parenthesized_type { Nothing } |
type_operator { Nothing } |
array_type { Nothing } |
function_type { $1 } |
typeQuery { Nothing } |
firstNode optional(generics) { $1 } |
firstTypeNode { $1 } |
typeParameter { Nothing } |
mapped_type { Nothing } |
conditional_type { Nothing } |
infer_type { Nothing } |
tuple_type { Nothing } |
internal_type optional(generics) { $1 }

-- helpers related to type
expressionWithTypeArguments: 'ExpressionWithTypeArguments' loc '(' type ')' { $4 }

-- helpers related to type
indexedAccessType:
'IndexedAccessType' loc
'('
    type
    openBracketToken
    internal_type
    closeBracketToken
')'
{
    $4
}

typeQuery:
'TypeQuery' loc
'('
    typeOfKeyword
    identifier
')'
{
    Nothing
}

array_type:
'ArrayType' loc
'('
    type
    openBracketToken
    closeBracketToken
')'
{
    Nothing
}

function_type:
'FunctionType' loc
'('
    openParenToken
    parameters
    closeParenToken
    equalsGreaterThanToken
    type
')'
{
    Nothing
}

-- helpers related to type
-- support for TypeScript tuple types (e.g., [A, ...B[]])
tuple_type:
'TupleType' loc
'('
    openBracketToken
    possibly_empty_commalistof(tuple_element)
    closeBracketToken
')'
{
    Nothing
}

tuple_element:
 type { 0 } |
 rest_type { 0 }

rest_type:
'RestType' loc
'('
    dotDotDotToken
    type
')'
{
    0
}

union_type: 'UnionType' loc '(' barlistof(type) ')' { Nothing } |
'UnionType' loc '(' unionTypeTail ')' { Nothing }
unionTypeTail: 'BarToken' loc '(' ')' ',' type unionTypeTailRest { 0 }
unionTypeTailRest: ',' 'BarToken' loc '(' ')' ',' type unionTypeTailRest { 0 } | { 0 }
intersection_type: 'IntersectionType' loc '(' ampersandlistof(type) ')' { Nothing }
parenthesized_type: 'ParenthesizedType' loc '(' openParenToken type closeParenToken ')' { $5 }
type_operator: 'TypeOperator' loc '(' type ')' { $4 }

conditional_type:
'ConditionalType' loc
'('
    type
    extendsKeyword
    type
    questionToken
    type
    colonToken
    type
')'
{
    Nothing
}

-- helpers related to type
infer_type:
'InferType' loc
'('
    inferKeyword
    typeParameter
')'
{
    Nothing
}

mapped_type:
'MappedType' loc
'('
    openBracketToken
    'TypeParameter' loc
    '('
        identifier
        inKeyword
        type_operator
    ')'
    closeBracketToken
    colonToken
    type
')'
{
    Nothing
}

internal_type:
booleanKeyword { Nothing } |
anyKeyword { Nothing } |
unknownKeyword { Nothing } |
undefinedKeyword { Nothing } |
stringKeyword { Nothing } |
numberKeyword { Nothing } |
objectKeyword { Nothing } |
voidKeyword { Nothing } |
neverKeyword { Nothing } |
identifier { Just $1 } |
typeReference { $1 } |
literalType { Nothing } |
typeLiteral { Nothing }

-- helpers related to type
generics: firstBinaryOperator commalistof(type) greaterThanToken { Nothing }
typeReference: 'TypeReference' loc '(' type ')' { $4 }
firstNode:
'FirstNode' loc '(' identifier dotToken identifier ')' { Nothing } |
'FirstNode' loc '(' firstNode dotToken identifier ')' { Nothing }
firstTypeNode: 'FirstTypeNode' loc '(' identifier isKeyword internal_type ')' { Nothing }

typeParameter:
'TypeParameter' loc
'('
    identifier
    optional(typeParameterExtends)
    optional(firstAssignment)
    optional(type)
')'
{
    Nothing
}

typeParameterExtends:
extendsKeyword type { Nothing }

-- helpers related to arrow function generics
typeParameters:
firstBinaryOperator commalistof(typeParameter) greaterThanToken { Nothing }

typeLiteral: 'TypeLiteral' loc '(' optional(commalistof(type_literal_member)) ')' { Nothing }

type_literal_member:
stmt_property { $1 } |
indexSignature { $1 }

indexSignature:
'IndexSignature' loc
'('
    openBracketToken
    'Parameter' loc
    '('
        identifier
        colonToken
        stringKeyword
    ')'
    closeBracketToken
    colonToken
    type
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = [],
        Ast.stmtBlockLocation = $2
    }
}

literalType:
'LiteralType' loc '(' stringLiteral ')' { Nothing } |
'LiteralType' loc '(' nullKeyword   ')' { Nothing } |
'LiteralType' loc '(' trueKeyword   ')' { Nothing } |
'LiteralType' loc '(' falseKeyword  ')' { Nothing }


throwKeyword:        'ThrowKeyword'        loc '(' ')' { Nothing }
importKeyword:       'ImportKeyword'       loc '(' ')' { Nothing }
declareKeyword:      'DeclareKeyword'      loc '(' ')' { Nothing }
interfaceKeyword:    'InterfaceKeyword'    loc '(' ')' { Nothing }
instanceOfKeyword:   'InstanceOfKeyword'   loc '(' ')' { Nothing }
isKeyword:           'IsKeyword'           loc '(' ')' { Nothing }
nullKeyword:         'NullKeyword'         loc '(' ')' { $2 }
trueKeyword:         'TrueKeyword'         loc '(' ')' { $2 }
falseKeyword:        'FalseKeyword'        loc '(' ')' { $2 }
ifKeyword:           'IfKeyword'           loc '(' ')' { Nothing }
whileKeyword:        'WhileKeyword'        loc '(' ')' { Nothing }
functionKeyword:     'FunctionKeyword'     loc '(' ')' { Nothing }
constructorKeyword:  'ConstructorKeyword'  loc '(' ')' { Nothing }
inKeyword:           'InKeyword'           loc '(' ')' { Nothing }
anyKeyword:          'AnyKeyword'          loc '(' ')' { Nothing }
commaToken:          'CommaToken'          loc '(' ')' { Nothing }
semicolonToken:       'SemicolonToken'      loc '(' ')' { Nothing }
booleanKeyword:      'BooleanKeyword'      loc '(' ')' { Nothing }
newKeyword:          'NewKeyword'          loc '(' ')' { Nothing }
unknownKeyword:      'UnknownKeyword'      loc '(' ')' { Nothing }
neverKeyword:       'NeverKeyword'       loc '(' ')' { Nothing }
inferKeyword:        'InferKeyword'        loc '(' ')' { Nothing }
deleteKeyword:       'DeleteKeyword'       loc '(' ')' { Nothing }
typeOfKeyword:       'TypeOfKeyword'       loc '(' ')' { Nothing }
returnKeyword:       'ReturnKeyword'       loc '(' ')' { Nothing }
slashToken:          'SlashToken'          loc '(' ')' { Nothing }
percentToken:        'PercentToken'        loc '(' ')' { Nothing }
exclamationToken:    'ExclamationToken'    loc '(' ')' { Nothing }
undefinedKeyword:    'UndefinedKeyword'    loc '(' ')' { Nothing }
templateHead:        'TemplateHead'        loc '(' ')' { Nothing }
templateMiddle:      'TemplateMiddle'      loc '(' ')' { Nothing }
lastTemplateToken:   'LastTemplateToken'   loc '(' ')' { Nothing }
dotToken:            'DotToken'            loc '(' ')' { Nothing }
questionDotToken:    'QuestionDotToken'    loc '(' ')' { Nothing }
barBarToken:         'BarBarToken'         loc '(' ')' { Nothing }
stringKeyword:       'StringKeyword'       loc '(' ')' { Nothing }
numberKeyword:       'NumberKeyword'       loc '(' ')' { Nothing }
objectKeyword:       'ObjectKeyword'       loc '(' ')' { Nothing }
voidKeyword:         'VoidKeyword'         loc '(' ')' { Nothing }
awaitKeyword:        'AwaitKeyword'        loc '(' ')' { Nothing }
fromKeyword:         'FromKeyword'         loc '(' ')' { Nothing }
typeKeyword:          'TypeKeyword'          loc '(' ')' { Nothing }
extendsKeyword:      'ExtendsKeyword'      loc '(' ')' { Nothing }
questionToken:       'QuestionToken'       loc '(' ')' { Nothing }
openParenToken:      'OpenParenToken'      loc '(' ')' { Nothing }
openBracketToken:    'OpenBracketToken'    loc '(' ')' { Nothing }
closeBracketToken:   'CloseBracketToken'   loc '(' ')' { Nothing }
closeParenToken:     'CloseParenToken'     loc '(' ')' { Nothing }
asKeyword:           'AsKeyword'           loc '(' ')' { Nothing }
satisfiesKeyword:    'SatisfiesKeyword'    loc '(' ')' { Nothing }
asteriskToken:       'AsteriskToken'       loc '(' ')' { Nothing }
asteriskAsteriskToken: 'AsteriskAsteriskToken' loc '(' ')' { Nothing }
plusToken:           'PlusToken'           loc '(' ')' { Nothing }
colonToken:          'ColonToken'          loc '(' ')' { Nothing }
tryKeyword:          'TryKeyword'          loc '(' ')' { Nothing }
elseKeyword:         'ElseKeyword'         loc '(' ')' { Nothing }
forKeyword:          'ForKeyword'          loc '(' ')' { Nothing }
ofKeyword:           'OfKeyword'           loc '(' ')' { Nothing }
minusToken:          'MinusToken'          loc '(' ')' { Nothing }
catchKeyword:        'CatchKeyword'        loc '(' ')' { Nothing }
finallyKeyword:      'FinallyKeyword'      loc '(' ')' { Nothing }
firstAssignment:     'FirstAssignment'     loc '(' ')' { Nothing }
firstBinaryOperator: 'FirstBinaryOperator' loc '(' ')' { Nothing }
firstCompoundAssignment: 'FirstCompoundAssignment' loc '(' ')' { Nothing }
greaterThanToken:    'GreaterThanToken'    loc '(' ')' { Nothing }
greaterThanEqualsToken: 'GreaterThanEqualsToken' loc '(' ')' { Nothing }
lessThanEqualsToken: 'LessThanEqualsToken' loc '(' ')' { Nothing }
lessThanLessThanToken: 'LessThanLessThanToken' loc '(' ')' { Nothing }
questionQuestionToken: 'QuestionQuestionToken' loc '(' ')' { Nothing }
questionQuestionEqualsToken: 'QuestionQuestionEqualsToken' loc '(' ')' { Nothing }
equalsGreaterThanToken: 'EqualsGreaterThanToken' loc '(' ')' { Nothing }
ampAmpToken:         'AmpersandAmpersandToken' loc '(' ')' { Nothing }
eqEqToken:         'EqualsEqualsToken' loc '(' ')' { Nothing }
eqEqEqToken:         'EqualsEqualsEqualsToken' loc '(' ')' { Nothing }
exclamationEqToken: 'ExclamationEqualsToken' loc '(' ')' { Nothing }
exclamationEqEqToken: 'ExclamationEqualsEqualsToken' loc '(' ')' { Nothing }
dotDotDotToken: 'DotDotDotToken' loc '(' ')' { Nothing }
caseKeyword:        'CaseKeyword'        loc '(' ')' { Nothing }
defaultKeyword:     'DefaultKeyword'     loc '(' ')' { Nothing }
classKeyword:       'ClassKeyword'       loc '(' ')' { Nothing }
continueKeyword:    'ContinueKeyword'    loc '(' ')' { Nothing }
doKeyword:          'DoKeyword'          loc '(' ')' { Nothing }
enumKeyword:        'EnumKeyword'        loc '(' ')' { Nothing }
publicKeyword:      'PublicKeyword'      loc '(' ')' { Nothing }
staticKeyword:      'StaticKeyword'      loc '(' ')' { Nothing }
superKeyword:       'SuperKeyword'       loc '(' ')' { Nothing }
switchKeyword:      'SwitchKeyword'      loc '(' ')' { Nothing }

importee: asteriskToken { Nothing }

importSpecifier:
'ImportSpecifier' loc
'('
    identifier optional(alias)
')'
{
    case $5 of { Just a -> a; _ -> $4 }
}


namedImports:
'NamedImports' loc
'('
    possibly_empty_commalistof(importSpecifier)
')'
{
    $4
}

importClauseStuff_1:
namedImports
{
    $1
}

importClauseStuff_2:
identifier { [$1] }

importClauseStuff_3:
namespaceImport { [] }

importClauseStuff:
importClauseStuff_1 { $1 } |
importClauseStuff_2 { $1 } |
importClauseStuff_3 { $1 }

importClause:
'ImportClause' loc
'('
    listof(importClauseStuff)
')'
{
    concat $4
}

alias: asKeyword identifier { $2 }

namespaceImport:
'NamespaceImport' loc
'('
    importee
    optional(alias)
')'
{
    Nothing
}

stringLiteral:
'StringLiteral' loc '(' STR ')'
{
    Token.ConstStr
    {
        Token.constStrValue = unquote (tokSTRValue $4),
        Token.constStrLocation = $2
    }
}

importAttributes:
'ImportAttributes' loc
'('
    possibly_empty_commalistof(importAttribute)
')'
{
    Nothing
}

importAttribute:
'ImportAttribute' loc
'('
    identifier
    colonToken
    stringLiteral
')'
{
    Nothing
}

assertClause:
'AssertClause' loc
'('
    possibly_empty_commalistof(assertEntry)
')'
{
    Nothing
}

assertEntry:
'AssertEntry' loc
'('
    identifier
    colonToken
    stringLiteral
')'
{
    Nothing
}

stmtImport:
'ImportDeclaration' loc
'('
    stringLiteral
')'
{
    Actions.stmtImport (getAdditionalRepoInfo $1) $2 Nothing $4
}
|
'ImportDeclaration' loc
'('
    importKeyword
    optional(typeKeyword)
    optional(importClause)
    optional(fromKeyword)
    stringLiteral
    optional(choice(importAttributes, assertClause))
')'
{
    Actions.stmtImport (getAdditionalRepoInfo $1) $2 $6 $8
}
|
'ImportDeclaration' loc
'('
    importKeyword
    optional(typeKeyword)
    optional(importClause)
')'
optional(fromKeyword)
stringLiteral
optional(choice(importAttributes, assertClause))
{
    Actions.stmtImport (getAdditionalRepoInfo $1) $2 $6 $9
}

-- instrumented as dhscanner Ast.StmtBlock
stmtExport:
'ExportDeclaration' loc
'('
    'NamedExports' loc
    '('
        commalistof(exportSpecifier)
    ')'
    optional(exportFrom)
')'
{
    Actions.stmtExport $2
}
|
'ExportDeclaration' loc
'('
    asteriskToken
    fromKeyword
    stringLiteral
')'
{
    Actions.stmtExport $2
}

exportSpecifier:
'ExportSpecifier' loc
'('
    identifier
')'
{
    Nothing
}
|
'ExportSpecifier' loc
'('
    identifier
    asKeyword
    identifier
')'
{
    Nothing
}

exportFrom:
fromKeyword stringLiteral { Nothing }

-- The bound local in an object destructuring element.
--   { x }    -> shorthand: property `x`, local `x`         -> returns `x`
--   { x: y } -> rename:    property `x`, local `y`         -> returns `y`
-- In both cases this returns the identifier that actually becomes a local
-- (i.e. the one after the colon when there is a colon, else the only one).
destructBinding:
identifier                       { $1 } |
identifier colonToken identifier { $3 }

bindingElement:
'BindingElement' loc
'('
    identifier
    colonToken
    objectBindingPattern
    optional(default_value)
')'
{
    $6
}
|
'BindingElement' loc
'('
    identifier
    colonToken
    arrayBindingPattern
    optional(default_value)
')'
{
    $6
}
|
'BindingElement' loc
'('
    destructBinding optional(default_value)
')'
{
    [Actions.varify $4]
}
|
'BindingElement' loc
'('
    dotDotDotToken
    identifier
')'
{
    [Actions.varify $5]
}

objectBindingPattern:
'ObjectBindingPattern' loc
'('
    commalistof(bindingElement)
')'
{
    concat $4
}

arrayBindingPattern:
'ArrayBindingPattern' loc
'('
    openBracketToken
    possibly_empty_commalistof(arrayBindingElement)
    closeBracketToken
')'
{
    concat $5
}

-- One slot of an `arrayBindingPattern` (the `[...]` LHS of a destructure).
-- The three sub-rules cover the three shapes a slot can take:
--   _1: bare identifier       -> const [x]    = ...
--   _2: nested object pattern -> const [{x}]  = ...
--   _3: nested array pattern  -> const [[x]]  = ...
-- All three return a list of bound locals (alt _1 is a singleton; _2 / _3
-- can be N, depending on how many names the nested pattern declares).
arrayBindingElement:
arrayBindingElement_1 { $1 } |
arrayBindingElement_2 { $1 } |
arrayBindingElement_3 { $1 } |
arrayBindingElement_4 { $1 } |
arrayBindingElement_omitted { $1 }

arrayBindingElement_1:
'BindingElement' loc
'('
    identifier
')'
{
    [Actions.varify $4]
}

arrayBindingElement_2:
'BindingElement' loc
'('
    objectBindingPattern
')'
{
    $4
}

arrayBindingElement_3:
'BindingElement' loc
'('
    arrayBindingPattern
')'
{
    $4
}

arrayBindingElement_4:
'BindingElement' loc
'('
    dotDotDotToken
    identifier
')'
{
    [Actions.varify $5]
}

arrayBindingElement_omitted:
'OmittedExpression' loc
'('
')'
{
    []
}

decvarLhs:
identifier optional(type_hint) { [Actions.varify $1] } |
objectBindingPattern           { $1 } |
arrayBindingPattern            { $1 }

-- The optional `= exp` tail of a `stmtDecvar`. `firstAssignment` matches the
-- literal `=` token (its value is discarded); only the initializer flows on.
decvarInit: firstAssignment exp { $2 }

variableDeclarationUnit:
'VariableDeclaration' loc
'('
    decvarLhs
    optional(decvarInit)
')'
{
    ($4, $5)
}

stmtDecvar:
'VariableDeclarationList' loc
'('
    commalistof(variableDeclarationUnit)
')'
{
    Actions.stmtDecvarList $2 $4
}

extends:
'HeritageClause' loc
'('
    extendsKeyword
    commalistof(type)
')'
{
    $5
}

-- instrumented as dhscanner Ast.StmtClass (enum modeled as class of data members, no methods)
stmtEnum:
'EnumDeclaration' loc
'('
    enumKeyword
    identifier
    commalistof(enumMember)
')'
{
    Actions.stmtEnum $5 $6
}

-- instrumented as dhscanner Ast.StmtBlock
stmtTypeAlias:
'TypeAliasDeclaration' loc
'('
    typeKeyword
    identifier
    optional(typeParameters)
    firstAssignment
    type
')'
{
    Actions.stmtTypeAlias $2
}

enumMember:
'EnumMember' loc
'('
    identifier
    optional(enumInitializer)
')'
{
    Actions.enumDataMember $4
}

enumInitializer:
firstAssignment stringLiteral { Nothing } |
firstAssignment 'NumericLiteral' loc '(' INT ')' { Nothing }

-- ***************
-- *             *
-- * data member *
-- *             *
-- ***************
stmt_property:
'PropertySignature' loc
'('
    identifier optional(questionToken) optional(type_hint) optional(commaToken)
')'
{
    Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = Token.VarName $4,
        Ast.stmtVardecNominalType = Just (Actions.varify $4),
        Ast.stmtVardecInitValue = Nothing,
        Ast.stmtVardecLocation = $2
    }
}
|
'PropertySignature' loc
'('
    stringLiteral optional(questionToken) optional(type_hint) optional(commaToken)
')'
{
    let name = Token.Named { Token.content = Token.constStrValue $4, Token.location = Token.constStrLocation $4 } in
    Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = Token.VarName name,
        Ast.stmtVardecNominalType = Just (Actions.varify name),
        Ast.stmtVardecInitValue = Nothing,
        Ast.stmtVardecLocation = $2
    }
}

block:
'Block' loc
'('
    possibly_empty_commalistof(stmt)
')'
{
    $4
}

moduleBlock:
'ModuleBlock' loc
'('
    commalistof(stmt)
')'
{
    $4
}

lambdaBody:
block { $1 } |
exp { [ Ast.StmtExp $1 ] }

default_value:
firstAssignment exp
{
    $2
}

stmtReturn:
'ReturnStatement' loc
'('
    returnKeyword
    optional(exp)
')'
{
    Actions.stmtReturn $2 $5
}

-- direct dhscanner subtree creation: Ast.StmtBreak
stmtBreak:
'BreakStatement' loc
'('
    'FirstKeyword' loc '(' ')'
')'
{
    Actions.stmtBreak $2
}

-- direct dhscanner subtree creation: Ast.StmtContinue
stmtContinue:
'ContinueStatement' loc
'('
    continueKeyword
')'
{
    Actions.stmtContinue $2
}

expArrowFunction:
'ArrowFunction' loc
'('
    optional(typeParameters)
    openParenToken
    parameters
    closeParenToken
    optional(type_hint)
    equalsGreaterThanToken
    lambdaBody
')'
{
    Actions.expArrowFunction $2 $6 $10
}

-- direct dhscanner subtree creation: Ast.ExpLambda
expFunctionExpression:
'FunctionExpression' loc
'('
    functionKeyword
    optional(identifier)
    openParenToken
    parameters
    closeParenToken
    optional(type_hint)
    block
')'
{
    Actions.expFunctionExpression $2 $7 $10
}

callArg:
exp { $1 } |
identifier colonToken exp { Actions.expvarify $1 }

expCall:
'CallExpression' loc
'('
    exp
    optional(generics)
    optional(questionDotToken)
    openParenToken
    possibly_empty_commalistof(callArg)
    closeParenToken
')'
{
    Actions.expCall $2 $4 $8
}
|
'CallExpression' loc
'('
    importKeyword
    openParenToken
    possibly_empty_commalistof(exp)
    closeParenToken
')'
{
    Actions.instrumentationCall "import" $2 $6
}
|
'CallExpression' loc
'('
    superKeyword
    openParenToken
    possibly_empty_commalistof(exp)
    closeParenToken
')'
{
    Actions.instrumentationCall "super" $2 $6
}

-- ***********
-- *         *
-- * exp str *
-- *         *
-- ***********
exp_str:
stringLiteral
{
    Ast.ExpStr $ Ast.ExpStrContent $1
}

-- ***************
-- *             *
-- * exp template token *
-- *             *
-- ***************
exp_template_token:
'FirstTemplateToken' loc '(' ')'
{
    Ast.ExpStr $ Ast.ExpStrContent $ Token.ConstStr
    {
        Token.constStrValue = "",
        Token.constStrLocation = $2
    }
}

-- The four equality tokens ( `==` , `===` , `!=` , `!==` ) are handled
-- by dedicated alternatives on `expBinop` below ( they route into
-- `Actions.expBinopEq` / `Actions.expBinopNeq` so the AST preserves
-- `Ast.EQ` / `Ast.NEQ` instead of collapsing to `Ast.PLUS` ). They are
-- intentionally NOT listed here -- keeping them out prevents a shift /
-- reduce ambiguity between the dedicated `expBinop` alternatives and
-- the generic `operator`-driven alternative below.
operator:
inKeyword            { Nothing } |
firstBinaryOperator  { Nothing } |
firstCompoundAssignment  { Nothing } |
instanceOfKeyword    { Nothing } |
barBarToken          { Nothing } |
ampAmpToken          { Nothing } |
asteriskToken        { Nothing } |
asteriskAsteriskToken { Nothing } |
plusToken            { Nothing } |
minusToken           { Nothing } |
slashToken           { Nothing } |
percentToken         { Nothing } |
greaterThanToken     { Nothing } |
greaterThanEqualsToken { Nothing } |
lessThanEqualsToken  { Nothing } |
lessThanLessThanToken { Nothing } |
questionQuestionToken { Nothing } |
questionQuestionEqualsToken { Nothing }

-- Equality-token helpers -- collapse strict-vs-loose to a single non-terminal
-- each ( `==` and `===` -> `eqOp` ; `!=` and `!==` -> `neqOp` ). Return
-- `Nothing` because the polarity itself is carried by which non-terminal
-- matched, not by the token's own value ; the surrounding `expBinop`
-- alternative dispatches to the right smart constructor
-- ( `Actions.expBinopEq` / `Actions.expBinopNeq` ) which bakes the
-- corresponding `Ast.EQ` / `Ast.NEQ` into the AST. These helpers are
-- intentionally used only by `expBinop` -- no other production reaches
-- them.
eqOp:  eqEqToken            { Nothing } | eqEqEqToken           { Nothing }
neqOp: exclamationEqToken   { Nothing } | exclamationEqEqToken  { Nothing }

-- *************
-- *           *
-- * exp binop *
-- *           *
-- *************
expBinop:
'BinaryExpression' loc
'('
    exp
    eqOp
    exp
')'
{
    Actions.expBinopEq $2 $4 $6
}
|
'BinaryExpression' loc
'('
    exp
    neqOp
    exp
')'
{
    Actions.expBinopNeq $2 $4 $6
}
|
'BinaryExpression' loc
'('
    exp
    operator
    exp
')'
{
    Actions.expBinop $2 $4 $6
}
|
'BinaryExpression' loc
'('
    exp
    exp
')'
{
    Actions.expBinop $2 $4 $5
}

-- ***************
-- *             *
-- * stmt assign *
-- *             *
-- ***************
stmtAssign:
'BinaryExpression' loc
'('
    var
    firstAssignment
    exp
')'
{
    Actions.stmtAssign $4 $6
}

varField:
'PropertyAccessExpression' loc
'('
    exp
    choice(dotToken, questionDotToken)
    identifier
')'
{
    Actions.varField $2 $4 $6
}

varSubscript:
'ElementAccessExpression' loc
'('
    exp
    optional(questionDotToken)
    openBracketToken
    exp
    closeBracketToken
')'
{
    Actions.varSubscript $2 $4 $7
}

var_simple:
identifier
{
    Actions.varify $1
}

var:
var_simple   { $1 } |
varField     { $1 } |
varSubscript { $1 }

-- ***********
-- *         *
-- * exp var *
-- *         *
-- ***********
exp_var:
var
{
    Ast.ExpVar $ Ast.ExpVarContent $1
}

-- ***********
-- *         *
-- * exp var *
-- *         *
-- ***********
exp_meta:
'MetaProperty' loc
'('
    'ImportKeyword' loc '(' ')' dotToken identifier
')'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = "meta",
        Token.location = $2
    }
}

template_span:
'TemplateSpan' loc '(' exp templateMiddle    ')' { $4 } |
'TemplateSpan' loc '(' exp lastTemplateToken ')' { $4 }

-- ***********
-- *         *
-- * fstring *
-- *         *
-- ***********
fstring:
'TemplateExpression' loc
'('
    templateHead
    commalistof(template_span)
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "fstring",
            Token.location = $2
        },
        Ast.args = $5,
        Ast.expCallLocation = $2
    }
}

unary_operator:
exclamationToken { Nothing } |
minusToken { Nothing }

-- ************
-- *          *
-- * exp unop *
-- *          *
-- ************
exp_unop:
'PrefixUnaryExpression' loc
'('
    unary_operator
    exp
')'
{
    $5
}

postfix_operator:
'PlusPlusToken' loc '(' ')' { Nothing } |
'MinusMinusToken' loc '(' ')' { Nothing }

-- ***************
-- *             *
-- * exp postunop*
-- *             *
-- ***************
exp_post_unop:
'PostfixUnaryExpression' loc
'('
    exp
    postfix_operator
')'
{
    $4
}

-- *************
-- *           *
-- * exp paren *
-- *           *
-- *************
exp_paren:
'ParenthesizedExpression' loc
'('
    openParenToken
    exp
    closeParenToken
')'
{
    $5
}
|
'ParenthesizedExpression' loc
'('
    openParenToken
    'BinaryExpression' loc
    '('
        var
        firstAssignment
        exp
    ')'
    closeParenToken
')'
{
    $10
}
|
'ParenthesizedExpression' loc
'('
    openParenToken
    'BinaryExpression' loc
    '('
        'ObjectLiteralExpression' loc
        '('
            possibly_empty_commalistof(property_assignment)
        ')'
        firstAssignment
        exp
    ')'
    closeParenToken
')'
{
    $14
}

-- instrumented as dhscanner Ast.ExpCall
expTypeof:
'TypeOfExpression' loc
'('
    typeOfKeyword
    exp
')'
{
    Actions.expTypeof $2 $5
}

exp_void:
'VoidExpression' loc
'('
    voidKeyword
    exp
')'
{
    $5
}

-- instrumented as dhscanner Ast.ExpCall
expTernary:
'ConditionalExpression' loc
'('
    exp
    questionToken
    exp
    colonToken
    exp
')'
{
    Actions.expTernary $2 $4 $6 $8
}

stmt_method:
'MethodDeclaration' loc
'('
    identifier
    openParenToken
    parameters
    closeParenToken
    block
')'
{
    Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = Just (Actions.varify (Token.Named "any" $2)),
        Ast.stmtMethodName = Token.MethodName $4,
        Ast.stmtMethodParams = $6,
        Ast.stmtMethodBody = $8,
        Ast.stmtMethodLocation = $2,
        Ast.hostingClassName = Token.ClassName (Token.Named "host" $2),
        Ast.hostingClassSupers = []
    }
}

shorthandElement:
identifier
{
    Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = Just (Actions.varify $1),
        Ast.stmtMethodName = Token.MethodName $1,
        Ast.stmtMethodParams = [],
        Ast.stmtMethodBody = [],
        Ast.stmtMethodLocation = Token.location $1,
        Ast.hostingClassName = Token.ClassName $1,
        Ast.hostingClassSupers = []
    } 
}

shorthandPropertyAssignment_1:
'ShorthandPropertyAssignment' loc
'('
    shorthandElement
')'
{
    $4
}

shorthandPropertyAssignment_2:
stmt_method { $1 }


shorthandPropertyAssignment:
shorthandPropertyAssignment_1 { $1 } |
shorthandPropertyAssignment_2 { $1 }

-- instrumented as dhscanner Ast.ExpCall
expDelete:
'DeleteExpression' loc
'('
    deleteKeyword
    exp
')'
{
    Actions.expDelete $2 $5
}

-- **********
-- *        *
-- * exp as *
-- *        *
-- **********
exp_as:
'AsExpression' loc
'('
    exp
    asKeyword
    type
')'
{
    $4
}

-- ***************
-- *             *
-- * exp satisfies *
-- *             *
-- ***************
exp_satisfies:
'SatisfiesExpression' loc
'('
    exp
    optional(satisfiesTail)
')'
{
    $4
}

satisfiesTail:
satisfiesKeyword type { Nothing }

-- ***********
-- *         *
-- * exp this *
-- *         *
-- ***********
exp_this:
'ThisKeyword' loc
'('
    ')'
{
    Actions.expvarify (Token.Named "this" $2)
}

expTrue: trueKeyword { Actions.expBool True $1 }
expFalse: falseKeyword { Actions.expBool False $1 }
expBool: expTrue  { $1 } | expFalse { $1 }

expNull: nullKeyword { Actions.expNull $1 }

-- instrumented as dhscanner Ast.ExpCall
expNew:
'NewExpression' loc
'('
    newKeyword
    type
    openParenToken
    optional(commalistof(exp))
    closeParenToken
')'
{
    Actions.expNew $2 $5 $7
}
|
'NewExpression' loc
'('
    newKeyword
    varField
    openParenToken
    optional(commalistof(exp))
    closeParenToken
')'
{
    Actions.expNewCalleeVar $2 $5 $7
}

-- *************
-- *           *
-- * exp regex *
-- *           *
-- *************
exp_regex:
'RegularExpressionLiteral' loc '(' ')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 888,
        Token.constIntLocation = $2
    }
}

exp_int:
'FirstLiteralToken' loc '(' INT ')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt (fromIntegral (tokIntValue $4)) $2
}

exp_jsx:
'JsxElement' loc
'('
    'JsxOpeningElement' loc
    '('
        firstBinaryOperator
        identifier
        greaterThanToken
    ')'
    possibly_empty_commalistof(jsx_maybe_text_or_expr)
    'JsxClosingElement' loc
    '('
        choice(jsxClosingRich, jsxClosingSimple)
    ')'
')'
{
    Actions.jsxChoose $2 $11
}

jsx_maybe_text_or_expr:
'JsxText' loc '(' ')' { Nothing } |
'JsxExpression' loc '(' exp ')' { Just $4 } |
'JsxExpression' loc
'('
    'BinaryExpression' loc
    '('
        var
        firstAssignment
        exp
    ')'
')'
{ Just $9 } |
'JsxExpression' loc '(' 'Identifier' loc '(' ')' ')' { Nothing } |
'JsxExpression' loc '(' ')' { Nothing } |
'JsxSelfClosingElement' loc
'('
    firstBinaryOperator
    identifier
')'
{ Nothing } |
exp_jsx { Just $1 }

jsxClosingRich:
firstBinaryOperator slashToken identifier greaterThanToken { Nothing }

jsxClosingSimple:
identifier { Nothing } |
'Identifier' loc '(' ')' { Nothing }

-- *************
-- *           *
-- * exp await *
-- *           *
-- *************
exp_await:
'AwaitExpression' loc
'('
    awaitKeyword
    exp
')'
{
    $5
}

-- instrumented as dhscanner Ast.ExpCall
property:
property_1 { $1 } |
property_2 { $1 }

-- Object-literal property KEY.
--
-- The frontts native AST captures the source-level distinction between a
-- string-literal-form key ( `{ "status": 401 }` -> `StringLiteral( ... )` )
-- and an identifier-form key ( `{ status: 401 }` -> `Identifier( status )` ).
-- Both are semantically *property names* -- a PropertyName in ECMAScript --
-- not variable reads, so lowering both into the same `Ast.ExpStr` shape
-- preserves the intent of the source :
--
--   { status: 401 }         == { "status": 401 }
--
-- Downstream this makes kbgen emit `kb_const_string( KeyLoc, 'status' )`
-- for BOTH forms ( previously only the quoted form emitted it, because
-- the identifier form was lowered as `Ast.ExpVar` and never reached
-- `Kbgen.ConstStringCtor` in `Factify.hs::getConstStringsFromValue` ).
-- That is what lets structural predicates in `queryengine/utils.pl` --
-- eg `utils_ts_response_json_at_with_status/2`, which pins the outer
-- `Response.json` call and then requires
-- `kb_const_string( KeyLoc, 'status' )` on the init-dict's kv-pair --
-- fire on real-world `Response.json({ status: 401, headers })` sites
-- without needing to loosen the key-side gate.
--
-- Numeric keys ( `{ 42: "answer" }` ) are intentionally NOT handled here
-- yet -- they are rare in TS ; leaf-add a `FirstLiteralToken` branch
-- when a real-world case demands it.
--
-- Computed keys ( `{ [expr]: value }` ) are handled separately by
-- `property_2` via `ComputedPropertyName` and stay on the general `exp`
-- path -- they legitimately ARE expressions and lifting them to
-- `Ast.ExpStr` would erase real dataflow information.
property_key:
stringLiteral
{
    Ast.ExpStr $ Ast.ExpStrContent $1
}
|
'Identifier' loc
'('
    ID
')'
{
    Ast.ExpStr $ Ast.ExpStrContent $ Token.ConstStr
    {
        Token.constStrValue = tokIDValue $4,
        Token.constStrLocation = $2
    }
}

property_1:
'PropertyAssignment' loc
'('
    property_key
    colonToken
    exp
')'
{
    Actions.property $2 $4 $6
}

property_2:
'PropertyAssignment' loc
'('
    'ComputedPropertyName' loc
    '('
        openBracketToken
        exp
        closeBracketToken
    ')'
    colonToken
    exp
')'
{
    Actions.property $2 $8 $12
}

spread_exp:
'SpreadAssignment' loc
'('
    dotDotDotToken
    exp
')'
{
    $5
}


property_assignment:
property { $1 } |
shorthandPropertyAssignment { lambdame' $1 } |
spread_exp { $1 }

-- ************
-- *          *
-- * exp dict *
-- *          *
-- ************
exp_dict:
'ObjectLiteralExpression' loc
'('
    possibly_empty_commalistof(property_assignment)
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "dictify" $2,
        Ast.args = $4,
        Ast.expCallLocation = $2
    }
}

-- *************
-- *           *
-- * exp array *
-- *           *
-- *************

-- Accept assignment expressions as array elements.
expOrAssign:
exp { $1 } |
'BinaryExpression' loc
'('
    var
    firstAssignment
    exp
')'
{
    $6
}

exp_array:
'ArrayLiteralExpression' loc
'('
    openBracketToken
    possibly_empty_commalistof_with_optional_trailing_comma(expOrAssign)
    closeBracketToken
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "arrayify" $2,
        Ast.args = $5,
        Ast.expCallLocation = $2
    }
}

exp_non_null:
'NonNullExpression' loc
'('
    exp
    exclamationToken
')'
{
    $4
}

exp_spread_element:
'SpreadElement' loc
'('
    dotDotDotToken
    exp
')'
{
    $5
}

-- instrumented as dhscanner Ast.ExpCall
exp_tagged_template:
'TaggedTemplateExpression' loc
'('
    varField
    'FirstTemplateToken' loc '(' ')'
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $4,
        Ast.args = [],
        Ast.expCallLocation = $2
    }
}

exp:
exp_str        { $1 } |
exp_template_token { $1 } |
exp_this       { $1 } |
exp_int        { $1 } |
expNew         { $1 } |
exp_dict       { $1 } |
exp_await      { $1 } |
expBool        { $1 } |
expNull        { $1 } |
fstring        { $1 } |
expCall        { $1 } |
exp_tagged_template { $1 } |
exp_meta       { $1 } |
exp_array      { $1 } |
expTernary     { $1 } |
exp_var        { $1 } |
exp_satisfies  { $1 } |
exp_as         { $1 } |
exp_paren      { $1 } |
exp_unop       { $1 } |
exp_post_unop  { $1 } |
expDelete      { $1 } |
expTypeof      { $1 } |
exp_void       { $1 } |
expBinop       { $1 } |
exp_regex      { $1 } |
exp_non_null   { $1 } |
exp_jsx        { $1 } |
exp_spread_element { $1 } |
expFunctionExpression { $1 } |
expArrowFunction { $1 }

loc:
'[' INT ':' INT '-' INT ':' INT ']'
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

unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

lambdame' :: Ast.StmtMethodContent -> Ast.Exp
lambdame' m = let
    p = Token.ParamName (Token.getMethodNameToken (Ast.stmtMethodName m))
    in Ast.ExpLambda $ Ast.ExpLambdaContent {
        Ast.expLambdaParams = [(Ast.Param p Nothing 174)] ++ (Ast.stmtMethodParams m),
        Ast.expLambdaBody = Ast.stmtMethodBody m,
        Ast.expLambdaLocation = Ast.stmtMethodLocation m
    }

lambdame :: [ Ast.StmtMethodContent ] -> [ Ast.Exp ]
lambdame = Data.List.map lambdame'

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
