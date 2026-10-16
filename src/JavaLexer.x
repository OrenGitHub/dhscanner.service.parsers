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

