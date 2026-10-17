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

module JavaLexer

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
@KW_IF = \"IF\"
@KW_PLUS = \"PLUS\"
@KW_BLOCK = \"BLOCK\"
@KW_CLASS = \"CLASS\"
@KW_INTERFACE = \"INTERFACE\"
@KW_ENUM = \"ENUM\"
@KW_NEW_CLASS = \"NEW_CLASS\"
@KW_IMPORT = \"IMPORT\"
@KW_METHOD = \"METHOD\"
@KW_RETURN = \"RETURN\"
@KW_CONSTRUCTOR = \"CONSTRUCTOR\"
@KW_PACKAGE = \"PACKAGE\"
@KW_EQUAL_TO = \"EQUAL_TO\"
@KW_MULTIPLY = \"MULTIPLY\"
@KW_VARIABLE = \"VARIABLE\"
@KW_ANNOTATION = \"ANNOTATION\"
@KW_ASSIGNMENT = \"ASSIGNMENT\"
@KW_IDENTIFIER = \"IDENTIFIER\"
@KW_INT_LITERAL = \"INT_LITERAL\"
@KW_NULL_LITERAL = \"NULL_LITERAL\"
@KW_MEMBER_SELECT = \"MEMBER_SELECT\"
@KW_PRIMITIVE_TYPE = \"PRIMITIVE_TYPE\"
@KW_STRING_LITERAL = \"STRING_LITERAL\"
@KW_BOOLEAN_LITERAL = \"BOOLEAN_LITERAL\"
@KW_COMPILATION_UNIT = \"COMPILATION_UNIT\"
@KW_METHOD_INVOCATION = \"METHOD_INVOCATION\"
@KW_LOGICAL_COMPLEMENT = \"LOGICAL_COMPLEMENT\"
@KW_PARAMETER = \"PARAMETER\"
@KW_PARAMETERIZED_TYPE = \"PARAMETERIZED_TYPE\"
@KW_EXPRESSION_STATEMENT = \"EXPRESSION_STATEMENT\"
@KW_TRY = \"TRY\"
@KW_CATCH = \"CATCH\"
@KW_CONDITIONAL_EXPRESSION = \"CONDITIONAL_EXPRESSION\"
@KW_ANNOTATED_TYPE = \"ANNOTATED_TYPE\"
@KW_TYPE_ANNOTATION = \"TYPE_ANNOTATION\"
@KW_ARRAY_ACCESS = \"ARRAY_ACCESS\"
@KW_ARRAY_TYPE = \"ARRAY_TYPE\"
@KW_ASSERT = \"ASSERT\"
@KW_BREAK = \"BREAK\"
@KW_CASE = \"CASE\"
@KW_CONTINUE = \"CONTINUE\"
@KW_DO_WHILE_LOOP = \"DO_WHILE_LOOP\"
@KW_ENHANCED_FOR_LOOP = \"ENHANCED_FOR_LOOP\"
@KW_MEMBER_REFERENCE = \"MEMBER_REFERENCE\"
@KW_FOR_LOOP = \"FOR_LOOP\"
@KW_INSTANCE_OF = \"INSTANCE_OF\"
@KW_TEMPLATE = \"TEMPLATE\"
@KW_LABELED_STATEMENT = \"LABELED_STATEMENT\"
@KW_MODIFIERS = \"MODIFIERS\"
@KW_NEW_ARRAY = \"NEW_ARRAY\"
@KW_LAMBDA_EXPRESSION = \"LAMBDA_EXPRESSION\"
@KW_PARENTHESIZED = \"PARENTHESIZED\"
@KW_ANY_PATTERN = \"ANY_PATTERN\"
@KW_BINDING_PATTERN = \"BINDING_PATTERN\"
@KW_DEFAULT_CASE_LABEL = \"DEFAULT_CASE_LABEL\"
@KW_CONSTANT_CASE_LABEL = \"CONSTANT_CASE_LABEL\"
@KW_PATTERN_CASE_LABEL = \"PATTERN_CASE_LABEL\"
@KW_DECONSTRUCTION_PATTERN = \"DECONSTRUCTION_PATTERN\"
@KW_EMPTY_STATEMENT = \"EMPTY_STATEMENT\"
@KW_SWITCH = \"SWITCH\"
@KW_SWITCH_EXPRESSION = \"SWITCH_EXPRESSION\"
@KW_SYNCHRONIZED = \"SYNCHRONIZED\"
@KW_THROW = \"THROW\"
@KW_UNION_TYPE = \"UNION_TYPE\"
@KW_INTERSECTION_TYPE = \"INTERSECTION_TYPE\"
@KW_TYPE_CAST = \"TYPE_CAST\"
@KW_TYPE_PARAMETER = \"TYPE_PARAMETER\"
@KW_WHILE_LOOP = \"WHILE_LOOP\"
@KW_POSTFIX_INCREMENT = \"POSTFIX_INCREMENT\"
@KW_POSTFIX_DECREMENT = \"POSTFIX_DECREMENT\"
@KW_PREFIX_INCREMENT = \"PREFIX_INCREMENT\"
@KW_PREFIX_DECREMENT = \"PREFIX_DECREMENT\"
@KW_UNARY_PLUS = \"UNARY_PLUS\"
@KW_UNARY_MINUS = \"UNARY_MINUS\"
@KW_BITWISE_COMPLEMENT = \"BITWISE_COMPLEMENT\"
@KW_DIVIDE = \"DIVIDE\"
@KW_REMAINDER = \"REMAINDER\"
@KW_MINUS = \"MINUS\"
@KW_LEFT_SHIFT = \"LEFT_SHIFT\"
@KW_RIGHT_SHIFT = \"RIGHT_SHIFT\"
@KW_UNSIGNED_RIGHT_SHIFT = \"UNSIGNED_RIGHT_SHIFT\"
@KW_LESS_THAN = \"LESS_THAN\"
@KW_GREATER_THAN = \"GREATER_THAN\"
@KW_LESS_THAN_EQUAL = \"LESS_THAN_EQUAL\"
@KW_GREATER_THAN_EQUAL = \"GREATER_THAN_EQUAL\"
@KW_NOT_EQUAL_TO = \"NOT_EQUAL_TO\"
@KW_AND = \"AND\"
@KW_XOR = \"XOR\"
@KW_OR = \"OR\"
@KW_CONDITIONAL_AND = \"CONDITIONAL_AND\"
@KW_CONDITIONAL_OR = \"CONDITIONAL_OR\"
@KW_MULTIPLY_ASSIGNMENT = \"MULTIPLY_ASSIGNMENT\"
@KW_DIVIDE_ASSIGNMENT = \"DIVIDE_ASSIGNMENT\"
@KW_REMAINDER_ASSIGNMENT = \"REMAINDER_ASSIGNMENT\"
@KW_PLUS_ASSIGNMENT = \"PLUS_ASSIGNMENT\"
@KW_MINUS_ASSIGNMENT = \"MINUS_ASSIGNMENT\"
@KW_LEFT_SHIFT_ASSIGNMENT = \"LEFT_SHIFT_ASSIGNMENT\"
@KW_RIGHT_SHIFT_ASSIGNMENT = \"RIGHT_SHIFT_ASSIGNMENT\"
@KW_UNSIGNED_RIGHT_SHIFT_ASSIGNMENT = \"UNSIGNED_RIGHT_SHIFT_ASSIGNMENT\"
@KW_AND_ASSIGNMENT = \"AND_ASSIGNMENT\"
@KW_XOR_ASSIGNMENT = \"XOR_ASSIGNMENT\"
@KW_OR_ASSIGNMENT = \"OR_ASSIGNMENT\"
@KW_LONG_LITERAL = \"LONG_LITERAL\"
@KW_FLOAT_LITERAL = \"FLOAT_LITERAL\"
@KW_DOUBLE_LITERAL = \"DOUBLE_LITERAL\"
@KW_CHAR_LITERAL = \"CHAR_LITERAL\"
@KW_UNBOUNDED_WILDCARD = \"UNBOUNDED_WILDCARD\"
@KW_EXTENDS_WILDCARD = \"EXTENDS_WILDCARD\"
@KW_SUPER_WILDCARD = \"SUPER_WILDCARD\"
@KW_ERRONEOUS = \"ERRONEOUS\"
@KW_ANNOTATION_TYPE = \"ANNOTATION_TYPE\"
@KW_MODULE = \"MODULE\"
@KW_EXPORTS = \"EXPORTS\"
@KW_OPENS = \"OPENS\"
@KW_PROVIDES = \"PROVIDES\"
@KW_RECORD = \"RECORD\"
@KW_REQUIRES = \"REQUIRES\"
@KW_USES = \"USES\"
@KW_OTHER = \"OTHER\"
@KW_YIELD = \"YIELD\"

-- ****************
-- *              *
-- * JSON keys    *
-- *              *
-- ****************
@KW_KIND = \"kind\"
@KW_LOC = \"loc\"
@KW_CHILDREN = \"children\"
@KW_START_LINE = \"startLine\"
@KW_START_COL = \"startCol\"
@KW_END_LINE = \"endLine\"
@KW_END_COL = \"endCol\"
@KW_TEXT = \"text\"
@KW_ANNOTATIONS = \"annotations\"
@KW_PARAMETERS_KEY = \"parameters\"
@KW_SUPERS = \"supers\"
@KW_TYPE_KEY = \"type\"
@KW_INIT_KEY = \"init\"
@KW_COND_KEY = \"cond\"
@KW_BODY_KEY = \"body\"
@KW_RETURN_TYPE_KEY = \"returnType\"
@KW_LEFT_KEY = \"left\"
@KW_RIGHT_KEY = \"right\"

-- ************
-- *          *
-- * integers *
-- *          *
-- ************
@DIGIT = 0-9
@INT = @DIGIT+

-- *******************
-- *                 *
-- * quoted strings  *
-- *                 *
-- *******************
-- any run of non-" characters wrapped in double quotes;
-- the fixed @KW_* macros are listed earlier in `tokens :-` so
-- they win on equal-length matches (Alex picks the first rule
-- that reaches the longest match).
@ID = \"[^\"]*\"

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

@LBRACK    { lex' AlexRawToken_LBRACK }
@RBRACK    { lex' AlexRawToken_RBRACK }
@LBRACE    { lex' AlexRawToken_LBRACE }
@RBRACE    { lex' AlexRawToken_RBRACE }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

@COLON     { lex' AlexRawToken_COLON  }
@COMMA     { lex' AlexRawToken_COMMA  }

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@KW_IF { lex' AlexRawToken_IF }
@KW_PLUS { lex' AlexRawToken_PLUS }
@KW_BLOCK { lex' AlexRawToken_BLOCK }
@KW_CLASS { lex' AlexRawToken_CLASS }
@KW_INTERFACE { lex' AlexRawToken_INTERFACE }
@KW_ENUM { lex' AlexRawToken_ENUM }
@KW_NEW_CLASS { lex' AlexRawToken_NEW_CLASS }
@KW_IMPORT { lex' AlexRawToken_IMPORT }
@KW_METHOD { lex' AlexRawToken_METHOD }
@KW_RETURN { lex' AlexRawToken_RETURN }
@KW_CONSTRUCTOR { lex' AlexRawToken_CONSTRUCTOR }
@KW_PACKAGE { lex' AlexRawToken_PACKAGE }
@KW_EQUAL_TO { lex' AlexRawToken_EQUAL_TO }
@KW_MULTIPLY { lex' AlexRawToken_MULTIPLY }
@KW_VARIABLE { lex' AlexRawToken_VARIABLE }
@KW_ANNOTATION { lex' AlexRawToken_ANNOTATION }
@KW_ASSIGNMENT { lex' AlexRawToken_ASSIGNMENT }
@KW_IDENTIFIER { lex' AlexRawToken_IDENTIFIER }
@KW_INT_LITERAL { lex' AlexRawToken_INT_LITERAL }
@KW_NULL_LITERAL { lex' AlexRawToken_NULL_LITERAL }
@KW_MEMBER_SELECT { lex' AlexRawToken_MEMBER_SELECT }
@KW_PRIMITIVE_TYPE { lex' AlexRawToken_PRIMITIVE_TYPE }
@KW_STRING_LITERAL { lex' AlexRawToken_STRING_LITERAL }
@KW_BOOLEAN_LITERAL { lex' AlexRawToken_BOOLEAN_LITERAL }
@KW_COMPILATION_UNIT { lex' AlexRawToken_COMPILATION_UNIT }
@KW_METHOD_INVOCATION { lex' AlexRawToken_METHOD_INVOCATION }
@KW_LOGICAL_COMPLEMENT { lex' AlexRawToken_LOGICAL_COMPLEMENT }
@KW_PARAMETER { lex' AlexRawToken_PARAMETER }
@KW_PARAMETERIZED_TYPE { lex' AlexRawToken_PARAMETERIZED_TYPE }
@KW_EXPRESSION_STATEMENT { lex' AlexRawToken_EXPRESSION_STATEMENT }
@KW_TRY { lex' AlexRawToken_TRY }
@KW_CATCH { lex' AlexRawToken_CATCH }
@KW_CONDITIONAL_EXPRESSION { lex' AlexRawToken_CONDITIONAL_EXPRESSION }
@KW_ANNOTATED_TYPE { lex' AlexRawToken_ANNOTATED_TYPE }
@KW_TYPE_ANNOTATION { lex' AlexRawToken_TYPE_ANNOTATION }
@KW_ARRAY_ACCESS { lex' AlexRawToken_ARRAY_ACCESS }
@KW_ARRAY_TYPE { lex' AlexRawToken_ARRAY_TYPE }
@KW_ASSERT { lex' AlexRawToken_ASSERT }
@KW_BREAK { lex' AlexRawToken_BREAK }
@KW_CASE { lex' AlexRawToken_CASE }
@KW_CONTINUE { lex' AlexRawToken_CONTINUE }
@KW_DO_WHILE_LOOP { lex' AlexRawToken_DO_WHILE_LOOP }
@KW_ENHANCED_FOR_LOOP { lex' AlexRawToken_ENHANCED_FOR_LOOP }
@KW_MEMBER_REFERENCE { lex' AlexRawToken_MEMBER_REFERENCE }
@KW_FOR_LOOP { lex' AlexRawToken_FOR_LOOP }
@KW_INSTANCE_OF { lex' AlexRawToken_INSTANCE_OF }
@KW_TEMPLATE { lex' AlexRawToken_TEMPLATE }
@KW_LABELED_STATEMENT { lex' AlexRawToken_LABELED_STATEMENT }
@KW_MODIFIERS { lex' AlexRawToken_MODIFIERS }
@KW_NEW_ARRAY { lex' AlexRawToken_NEW_ARRAY }
@KW_LAMBDA_EXPRESSION { lex' AlexRawToken_LAMBDA_EXPRESSION }
@KW_PARENTHESIZED { lex' AlexRawToken_PARENTHESIZED }
@KW_ANY_PATTERN { lex' AlexRawToken_ANY_PATTERN }
@KW_BINDING_PATTERN { lex' AlexRawToken_BINDING_PATTERN }
@KW_DEFAULT_CASE_LABEL { lex' AlexRawToken_DEFAULT_CASE_LABEL }
@KW_CONSTANT_CASE_LABEL { lex' AlexRawToken_CONSTANT_CASE_LABEL }
@KW_PATTERN_CASE_LABEL { lex' AlexRawToken_PATTERN_CASE_LABEL }
@KW_DECONSTRUCTION_PATTERN { lex' AlexRawToken_DECONSTRUCTION_PATTERN }
@KW_EMPTY_STATEMENT { lex' AlexRawToken_EMPTY_STATEMENT }
@KW_SWITCH { lex' AlexRawToken_SWITCH }
@KW_SWITCH_EXPRESSION { lex' AlexRawToken_SWITCH_EXPRESSION }
@KW_SYNCHRONIZED { lex' AlexRawToken_SYNCHRONIZED }
@KW_THROW { lex' AlexRawToken_THROW }
@KW_UNION_TYPE { lex' AlexRawToken_UNION_TYPE }
@KW_INTERSECTION_TYPE { lex' AlexRawToken_INTERSECTION_TYPE }
@KW_TYPE_CAST { lex' AlexRawToken_TYPE_CAST }
@KW_TYPE_PARAMETER { lex' AlexRawToken_TYPE_PARAMETER }
@KW_WHILE_LOOP { lex' AlexRawToken_WHILE_LOOP }
@KW_POSTFIX_INCREMENT { lex' AlexRawToken_POSTFIX_INCREMENT }
@KW_POSTFIX_DECREMENT { lex' AlexRawToken_POSTFIX_DECREMENT }
@KW_PREFIX_INCREMENT { lex' AlexRawToken_PREFIX_INCREMENT }
@KW_PREFIX_DECREMENT { lex' AlexRawToken_PREFIX_DECREMENT }
@KW_UNARY_PLUS { lex' AlexRawToken_UNARY_PLUS }
@KW_UNARY_MINUS { lex' AlexRawToken_UNARY_MINUS }
@KW_BITWISE_COMPLEMENT { lex' AlexRawToken_BITWISE_COMPLEMENT }
@KW_DIVIDE { lex' AlexRawToken_DIVIDE }
@KW_REMAINDER { lex' AlexRawToken_REMAINDER }
@KW_MINUS { lex' AlexRawToken_MINUS }
@KW_LEFT_SHIFT { lex' AlexRawToken_LEFT_SHIFT }
@KW_RIGHT_SHIFT { lex' AlexRawToken_RIGHT_SHIFT }
@KW_UNSIGNED_RIGHT_SHIFT { lex' AlexRawToken_UNSIGNED_RIGHT_SHIFT }
@KW_LESS_THAN { lex' AlexRawToken_LESS_THAN }
@KW_GREATER_THAN { lex' AlexRawToken_GREATER_THAN }
@KW_LESS_THAN_EQUAL { lex' AlexRawToken_LESS_THAN_EQUAL }
@KW_GREATER_THAN_EQUAL { lex' AlexRawToken_GREATER_THAN_EQUAL }
@KW_NOT_EQUAL_TO { lex' AlexRawToken_NOT_EQUAL_TO }
@KW_AND { lex' AlexRawToken_AND }
@KW_XOR { lex' AlexRawToken_XOR }
@KW_OR { lex' AlexRawToken_OR }
@KW_CONDITIONAL_AND { lex' AlexRawToken_CONDITIONAL_AND }
@KW_CONDITIONAL_OR { lex' AlexRawToken_CONDITIONAL_OR }
@KW_MULTIPLY_ASSIGNMENT { lex' AlexRawToken_MULTIPLY_ASSIGNMENT }
@KW_DIVIDE_ASSIGNMENT { lex' AlexRawToken_DIVIDE_ASSIGNMENT }
@KW_REMAINDER_ASSIGNMENT { lex' AlexRawToken_REMAINDER_ASSIGNMENT }
@KW_PLUS_ASSIGNMENT { lex' AlexRawToken_PLUS_ASSIGNMENT }
@KW_MINUS_ASSIGNMENT { lex' AlexRawToken_MINUS_ASSIGNMENT }
@KW_LEFT_SHIFT_ASSIGNMENT { lex' AlexRawToken_LEFT_SHIFT_ASSIGNMENT }
@KW_RIGHT_SHIFT_ASSIGNMENT { lex' AlexRawToken_RIGHT_SHIFT_ASSIGNMENT }
@KW_UNSIGNED_RIGHT_SHIFT_ASSIGNMENT { lex' AlexRawToken_UNSIGNED_RIGHT_SHIFT_ASSIGNMENT }
@KW_AND_ASSIGNMENT { lex' AlexRawToken_AND_ASSIGNMENT }
@KW_XOR_ASSIGNMENT { lex' AlexRawToken_XOR_ASSIGNMENT }
@KW_OR_ASSIGNMENT { lex' AlexRawToken_OR_ASSIGNMENT }
@KW_LONG_LITERAL { lex' AlexRawToken_LONG_LITERAL }
@KW_FLOAT_LITERAL { lex' AlexRawToken_FLOAT_LITERAL }
@KW_DOUBLE_LITERAL { lex' AlexRawToken_DOUBLE_LITERAL }
@KW_CHAR_LITERAL { lex' AlexRawToken_CHAR_LITERAL }
@KW_UNBOUNDED_WILDCARD { lex' AlexRawToken_UNBOUNDED_WILDCARD }
@KW_EXTENDS_WILDCARD { lex' AlexRawToken_EXTENDS_WILDCARD }
@KW_SUPER_WILDCARD { lex' AlexRawToken_SUPER_WILDCARD }
@KW_ERRONEOUS { lex' AlexRawToken_ERRONEOUS }
@KW_ANNOTATION_TYPE { lex' AlexRawToken_ANNOTATION_TYPE }
@KW_MODULE { lex' AlexRawToken_MODULE }
@KW_EXPORTS { lex' AlexRawToken_EXPORTS }
@KW_OPENS { lex' AlexRawToken_OPENS }
@KW_PROVIDES { lex' AlexRawToken_PROVIDES }
@KW_RECORD { lex' AlexRawToken_RECORD }
@KW_REQUIRES { lex' AlexRawToken_REQUIRES }
@KW_USES { lex' AlexRawToken_USES }
@KW_OTHER { lex' AlexRawToken_OTHER }
@KW_YIELD { lex' AlexRawToken_YIELD }

@KW_KIND { lex' AlexRawToken_KIND }
@KW_LOC { lex' AlexRawToken_LOC }
@KW_CHILDREN { lex' AlexRawToken_CHILDREN }
@KW_START_LINE { lex' AlexRawToken_START_LINE }
@KW_START_COL { lex' AlexRawToken_START_COL }
@KW_END_LINE { lex' AlexRawToken_END_LINE }
@KW_END_COL { lex' AlexRawToken_END_COL }
@KW_TEXT { lex' AlexRawToken_TEXT }
@KW_ANNOTATIONS { lex' AlexRawToken_annotations }
@KW_PARAMETERS_KEY { lex' AlexRawToken_parameters }
@KW_SUPERS { lex' AlexRawToken_supers }
@KW_TYPE_KEY { lex' AlexRawToken_TYPE }
@KW_INIT_KEY { lex' AlexRawToken_INIT }
@KW_COND_KEY { lex' AlexRawToken_COND }
@KW_BODY_KEY { lex' AlexRawToken_BODY }
@KW_RETURN_TYPE_KEY { lex' AlexRawToken_RETURN_TYPE }
@KW_LEFT_KEY { lex' AlexRawToken_LEFT }
@KW_RIGHT_KEY { lex' AlexRawToken_RIGHT }

-- ***************************
-- *                         *
-- * whitespace ? do nothing *
-- *                         *
-- ***************************

@WHITE_SPACE ;

-- ****************************
-- *                          *
-- * integers                 *
-- *                          *
-- ****************************

@INT { lex (AlexRawToken_INT . round . read) }
@ID  { lex (AlexRawToken_ID . unquote) }
.    { lexicalError }

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

     = AlexRawToken_INT Int
     | AlexRawToken_ID String

     | AlexRawToken_LBRACK
     | AlexRawToken_RBRACK
     | AlexRawToken_LBRACE
     | AlexRawToken_RBRACE
     | AlexRawToken_COLON
     | AlexRawToken_COMMA

     | AlexRawToken_KIND
     | AlexRawToken_LOC
     | AlexRawToken_CHILDREN
     | AlexRawToken_TEXT
     | AlexRawToken_START_LINE
     | AlexRawToken_START_COL
     | AlexRawToken_END_LINE
     | AlexRawToken_END_COL
     | AlexRawToken_annotations
     | AlexRawToken_parameters
     | AlexRawToken_supers
     | AlexRawToken_TYPE
     | AlexRawToken_INIT
     | AlexRawToken_COND
     | AlexRawToken_BODY
     | AlexRawToken_RETURN_TYPE
     | AlexRawToken_LEFT
     | AlexRawToken_RIGHT

     | AlexRawToken_COMPILATION_UNIT
     | AlexRawToken_PACKAGE
     | AlexRawToken_IMPORT
     | AlexRawToken_MEMBER_SELECT
     | AlexRawToken_CLASS
     | AlexRawToken_INTERFACE
     | AlexRawToken_ENUM
     | AlexRawToken_NEW_CLASS
     | AlexRawToken_ANNOTATION
     | AlexRawToken_ASSIGNMENT
     | AlexRawToken_STRING_LITERAL
     | AlexRawToken_VARIABLE
     | AlexRawToken_METHOD
     | AlexRawToken_PARAMETER
     | AlexRawToken_PARAMETERIZED_TYPE
     | AlexRawToken_PRIMITIVE_TYPE
     | AlexRawToken_INT_LITERAL
     | AlexRawToken_NULL_LITERAL
     | AlexRawToken_BOOLEAN_LITERAL
     | AlexRawToken_BLOCK
     | AlexRawToken_RETURN
     | AlexRawToken_CONSTRUCTOR
     | AlexRawToken_IF
     | AlexRawToken_METHOD_INVOCATION
     | AlexRawToken_EQUAL_TO
     | AlexRawToken_PLUS
     | AlexRawToken_MULTIPLY
     | AlexRawToken_LOGICAL_COMPLEMENT
     | AlexRawToken_EXPRESSION_STATEMENT
     | AlexRawToken_TRY
     | AlexRawToken_CATCH
     | AlexRawToken_CONDITIONAL_EXPRESSION
     | AlexRawToken_IDENTIFIER
     | AlexRawToken_ANNOTATED_TYPE
     | AlexRawToken_TYPE_ANNOTATION
     | AlexRawToken_ARRAY_ACCESS
     | AlexRawToken_ARRAY_TYPE
     | AlexRawToken_ASSERT
     | AlexRawToken_BREAK
     | AlexRawToken_CASE
     | AlexRawToken_CONTINUE
     | AlexRawToken_DO_WHILE_LOOP
     | AlexRawToken_ENHANCED_FOR_LOOP
     | AlexRawToken_MEMBER_REFERENCE
     | AlexRawToken_FOR_LOOP
     | AlexRawToken_INSTANCE_OF
     | AlexRawToken_TEMPLATE
     | AlexRawToken_LABELED_STATEMENT
     | AlexRawToken_MODIFIERS
     | AlexRawToken_NEW_ARRAY
     | AlexRawToken_LAMBDA_EXPRESSION
     | AlexRawToken_PARENTHESIZED
     | AlexRawToken_ANY_PATTERN
     | AlexRawToken_BINDING_PATTERN
     | AlexRawToken_DEFAULT_CASE_LABEL
     | AlexRawToken_CONSTANT_CASE_LABEL
     | AlexRawToken_PATTERN_CASE_LABEL
     | AlexRawToken_DECONSTRUCTION_PATTERN
     | AlexRawToken_EMPTY_STATEMENT
     | AlexRawToken_SWITCH
     | AlexRawToken_SWITCH_EXPRESSION
     | AlexRawToken_SYNCHRONIZED
     | AlexRawToken_THROW
     | AlexRawToken_UNION_TYPE
     | AlexRawToken_INTERSECTION_TYPE
     | AlexRawToken_TYPE_CAST
     | AlexRawToken_TYPE_PARAMETER
     | AlexRawToken_WHILE_LOOP
     | AlexRawToken_POSTFIX_INCREMENT
     | AlexRawToken_POSTFIX_DECREMENT
     | AlexRawToken_PREFIX_INCREMENT
     | AlexRawToken_PREFIX_DECREMENT
     | AlexRawToken_UNARY_PLUS
     | AlexRawToken_UNARY_MINUS
     | AlexRawToken_BITWISE_COMPLEMENT
     | AlexRawToken_DIVIDE
     | AlexRawToken_REMAINDER
     | AlexRawToken_MINUS
     | AlexRawToken_LEFT_SHIFT
     | AlexRawToken_RIGHT_SHIFT
     | AlexRawToken_UNSIGNED_RIGHT_SHIFT
     | AlexRawToken_LESS_THAN
     | AlexRawToken_GREATER_THAN
     | AlexRawToken_LESS_THAN_EQUAL
     | AlexRawToken_GREATER_THAN_EQUAL
     | AlexRawToken_NOT_EQUAL_TO
     | AlexRawToken_AND
     | AlexRawToken_XOR
     | AlexRawToken_OR
     | AlexRawToken_CONDITIONAL_AND
     | AlexRawToken_CONDITIONAL_OR
     | AlexRawToken_MULTIPLY_ASSIGNMENT
     | AlexRawToken_DIVIDE_ASSIGNMENT
     | AlexRawToken_REMAINDER_ASSIGNMENT
     | AlexRawToken_PLUS_ASSIGNMENT
     | AlexRawToken_MINUS_ASSIGNMENT
     | AlexRawToken_LEFT_SHIFT_ASSIGNMENT
     | AlexRawToken_RIGHT_SHIFT_ASSIGNMENT
     | AlexRawToken_UNSIGNED_RIGHT_SHIFT_ASSIGNMENT
     | AlexRawToken_AND_ASSIGNMENT
     | AlexRawToken_XOR_ASSIGNMENT
     | AlexRawToken_OR_ASSIGNMENT
     | AlexRawToken_LONG_LITERAL
     | AlexRawToken_FLOAT_LITERAL
     | AlexRawToken_DOUBLE_LITERAL
     | AlexRawToken_CHAR_LITERAL
     | AlexRawToken_UNBOUNDED_WILDCARD
     | AlexRawToken_EXTENDS_WILDCARD
     | AlexRawToken_SUPER_WILDCARD
     | AlexRawToken_ERRONEOUS
     | AlexRawToken_ANNOTATION_TYPE
     | AlexRawToken_MODULE
     | AlexRawToken_EXPORTS
     | AlexRawToken_OPENS
     | AlexRawToken_PROVIDES
     | AlexRawToken_RECORD
     | AlexRawToken_REQUIRES
     | AlexRawToken_USES
     | AlexRawToken_OTHER
     | AlexRawToken_YIELD

     | TokenEOF
 
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

lexicalError :: AlexInput -> Int -> Alex AlexTokenTag
lexicalError ((AlexPn _ l c),_,_,str) i = alexEOF

-- **************
-- *            *
-- * alexError' *
-- *            *
-- **************
alexError' :: Location -> Alex a
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

-- **************
-- *            *
-- * tokIDValue *
-- *            *
-- **************
tokIDValue :: AlexTokenTag -> String
tokIDValue t = case (tokenRaw t) of { AlexRawToken_ID s -> s; _ -> ""; }

-- ***********
-- *         *
-- * unquote *
-- *         *
-- ***********
-- Strip the surrounding double quotes that @ID matches so the
-- resulting String carries just the payload ( "foo" -> foo ).
unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> Common.AdditionalRepoInfo -> String -> Either String a
runAlex' a fp additionalInfo input = runAlex input (setFilePath fp >> alexSetUserState (AlexUserState fp additionalInfo) >> a)
}

