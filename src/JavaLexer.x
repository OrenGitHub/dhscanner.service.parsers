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
@HYPHEN = \- 

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
@KW_MODIFIERS = \"MODIFIERS\"
@KW_TYPE_CAST = \"TYPE_CAST\"
@KW_ANNOTATION = \"ANNOTATION\"
@KW_ASSIGNMENT = \"ASSIGNMENT\"
@KW_IDENTIFIER = \"IDENTIFIER\"
@KW_INT_LITERAL = \"INT_LITERAL\"
@KW_NULL_LITERAL = \"NULL_LITERAL\"
@KW_MEMBER_SELECT = \"MEMBER_SELECT\"
@KW_PARENTHESIZED = \"PARENTHESIZED\"
@KW_PRIMITIVE_TYPE = \"PRIMITIVE_TYPE\"
@KW_STRING_LITERAL = \"STRING_LITERAL\"
@KW_BOOLEAN_LITERAL = \"BOOLEAN_LITERAL\"
@KW_COMPILATION_UNIT = \"COMPILATION_UNIT\"
@KW_METHOD_INVOCATION = \"METHOD_INVOCATION\"
@KW_LOGICAL_COMPLEMENT = \"LOGICAL_COMPLEMENT\"
@KW_PARAMETER = \"PARAMETER\"
@KW_PARAMETERIZED_TYPE = \"PARAMETERIZED_TYPE\"
@KW_UNBOUNDED_WILDCARD = \"UNBOUNDED_WILDCARD\"
@KW_EXPRESSION_STATEMENT = \"EXPRESSION_STATEMENT\"

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

@LPAREN    { lex' AlexRawToken_LPAREN }
@RPAREN    { lex' AlexRawToken_RPAREN }
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
@HYPHEN    { lex' AlexRawToken_HYPHEN }

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
@KW_MODIFIERS { lex' AlexRawToken_MODIFIERS }
@KW_TYPE_CAST { lex' AlexRawToken_TYPE_CAST }
@KW_ANNOTATION { lex' AlexRawToken_ANNOTATION }
@KW_ASSIGNMENT { lex' AlexRawToken_ASSIGNMENT }
@KW_IDENTIFIER { lex' AlexRawToken_IDENTIFIER }
@KW_INT_LITERAL { lex' AlexRawToken_INT_LITERAL }
@KW_NULL_LITERAL { lex' AlexRawToken_NULL_LITERAL }
@KW_MEMBER_SELECT { lex' AlexRawToken_MEMBER_SELECT }
@KW_PARENTHESIZED { lex' AlexRawToken_PARENTHESIZED }
@KW_PRIMITIVE_TYPE { lex' AlexRawToken_PRIMITIVE_TYPE }
@KW_STRING_LITERAL { lex' AlexRawToken_STRING_LITERAL }
@KW_BOOLEAN_LITERAL { lex' AlexRawToken_BOOLEAN_LITERAL }
@KW_COMPILATION_UNIT { lex' AlexRawToken_COMPILATION_UNIT }
@KW_METHOD_INVOCATION { lex' AlexRawToken_METHOD_INVOCATION }
@KW_LOGICAL_COMPLEMENT { lex' AlexRawToken_LOGICAL_COMPLEMENT }
@KW_PARAMETER { lex' AlexRawToken_PARAMETER }
@KW_PARAMETERIZED_TYPE { lex' AlexRawToken_PARAMETERIZED_TYPE }
@KW_UNBOUNDED_WILDCARD { lex' AlexRawToken_UNBOUNDED_WILDCARD }
@KW_EXPRESSION_STATEMENT { lex' AlexRawToken_EXPRESSION_STATEMENT }

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

     = AlexRawToken_INT Int         -- ^ locations and numbers
     | AlexRawToken_ID String       -- ^ including constant strings

     -- ***************
     -- *             *
     -- * parentheses *
     -- *             *
     -- ***************

     | AlexRawToken_LPAREN          -- ^ Parentheses __(__
     | AlexRawToken_RPAREN          -- ^ Parentheses __)__
     | AlexRawToken_LBRACK          -- ^ Parentheses __[__
     | AlexRawToken_RBRACK          -- ^ Parentheses __]__
     | AlexRawToken_LBRACE          -- ^ Parentheses __{__
     | AlexRawToken_RBRACE          -- ^ Parentheses __}__
 
     | AlexRawToken_KWID            -- ^ Reserved Keyword
     | AlexRawToken_END             -- ^ Reserved Keyword
     | AlexRawToken_RAW             -- ^ Reserved Keyword
     | AlexRawToken_LOC             -- ^ Reserved Keyword
     | AlexRawToken_ARG             -- ^ Reserved Keyword
     | AlexRawToken_VAR             -- ^ Reserved Keyword
     | AlexRawToken_TEST            -- ^ Reserved Keyword
     | AlexRawToken_NULL            -- ^ Reserved Keyword
     | AlexRawToken_TAIL            -- ^ Reserved Keyword
     | AlexRawToken_KIND            -- ^ Reserved Keyword
     | AlexRawToken_LINE            -- ^ Reserved Keyword
     | AlexRawToken_TRUE            -- ^ Reserved Keyword
     | AlexRawToken_ARGS            -- ^ Reserved Keyword
     | AlexRawToken_NAME            -- ^ Reserved Keyword
     | AlexRawToken_EXPR            -- ^ Reserved Keyword
     | AlexRawToken_MAME            -- ^ Reserved Keyword
     | AlexRawToken_TYPE            -- ^ Reserved Keyword
     | AlexRawToken_LEFT            -- ^ Reserved Keyword
     | AlexRawToken_LOOP            -- ^ Reserved Keyword
     | AlexRawToken_INIT            -- ^ Reserved Keyword
     | AlexRawToken_COND            -- ^ Reserved Keyword
     | AlexRawToken_BODY            -- ^ Reserved Keyword
     | AlexRawToken_START           -- ^ Reserved Keyword
     | AlexRawToken_COOKED          -- ^ Reserved Keyword
     | AlexRawToken_UPDATE          -- ^ Reserved Keyword
     | AlexRawToken_QUASIS          -- ^ Reserved Keyword
     | AlexRawToken_FALSE           -- ^ Reserved Keyword
     | AlexRawToken_EXPRS           -- ^ Reserved Keyword
     | AlexRawToken_VALUE           -- ^ Reserved Keyword
     | AlexRawToken_RIGHT           -- ^ Reserved Keyword
     | AlexRawToken_STMTS           -- ^ Reserved Keyword
     | AlexRawToken_ARRAY           -- ^ Reserved Keyword
     | AlexRawToken_PARAM           -- ^ Reserved Keyword
     | AlexRawToken_OBJECT          -- ^ Reserved Keyword
     | AlexRawToken_PREFIX          -- ^ Reserved Keyword
     | AlexRawToken_PARAMS          -- ^ Reserved Keyword
     | AlexRawToken_COLUMN          -- ^ Reserved Keyword
     | AlexRawToken_LITERAL         -- ^ Reserved Keyword
     | AlexRawToken_PROGRAM         -- ^ Reserved Keyword
     | AlexRawToken_PROPERTY        -- ^ Reserved Keyword
     | AlexRawToken_COMPUTED        -- ^ Reserved Keyword
     | AlexRawToken_OPERATOR        -- ^ Reserved Keyword
     | AlexRawToken_ALTERNATE       -- ^ Reserved Keyword
     | AlexRawToken_CONSEQUENT      -- ^ Reserved Keyword
     | AlexRawToken_ARGUMENT        -- ^ Reserved Keyword
     | AlexRawToken_ARGUMENTS       -- ^ Reserved Keyword
     | AlexRawToken_CALLEE          -- ^ Reserved Keyword
     | AlexRawToken_ASYNC           -- ^ Reserved Keyword
     | AlexRawToken_EXPRESSION      -- ^ Reserved Keyword
     | AlexRawToken_EXPRESSIONS     -- ^ Reserved Keyword
     | AlexRawToken_DECLARATIONS    -- ^ Reserved Keyword
     | AlexRawToken_SRC_TYPE        -- ^ Reserved Keyword
     | AlexRawToken_GENERATOR       -- ^ Reserved Keyword
     | AlexRawToken_STMT_ECHO       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_VAR        -- ^ Reserved Keyword
     | AlexRawToken_STMT_EXPR       -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_INT      -- ^ Reserved Keyword
     | AlexRawToken_IDENTIFIER      -- ^ Reserved Keyword
     | AlexRawToken_RETURN_TYPE     -- ^ Reserved Keyword
     | AlexRawToken_TEMPLATE_LI     -- ^ Reserved Keyword
     | AlexRawToken_TEMPLATE_EL     -- ^ Reserved Keyword
     | AlexRawToken_STMT_FUNCTION   -- ^ Reserved Keyword
     | AlexRawToken_FUNCTION_DEC    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CONST_GET  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_LT   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_PLUS -- ^ Reserved Keyword
     | AlexRawToken_VAR_DECLARATION -- ^ Reserved Keyword
     | AlexRawToken_VAR_DECLARATOR  -- ^ Reserved Keyword
     | AlexRawToken_local
     | AlexRawToken_source
     | AlexRawToken_specifiers
     | AlexRawToken_imported
     | AlexRawToken_ImportDeclaration
     | AlexRawToken_ImportSpecifier
     | AlexRawToken_ImportDefaultSpecifier
     | AlexRawToken_key
     | AlexRawToken_properties
     | AlexRawToken_ObjectExpression
     | AlexRawToken_shorthand
     | AlexRawToken_method
     | AlexRawToken_Property
     | AlexRawToken_AssignmentPattern
     | AlexRawToken_LogicalExpression
     | AlexRawToken_ArrayPattern
     | AlexRawToken_elements
     | AlexRawToken_ObjectPattern
     | AlexRawToken_TryStatement
     | AlexRawToken_block
     | AlexRawToken_handler
     | AlexRawToken_finalizer
     | AlexRawToken_AwaitExpression
     | AlexRawToken_CatchClause
     | AlexRawToken_param
     | AlexRawToken_ArrayExpression
     | AlexRawToken_OP_AND
     | AlexRawToken_UnaryExpression
     | AlexRawToken_bang
     | AlexRawToken_OP_NEQ
     | AlexRawToken_OP_IN
     | AlexRawToken_QUOTED_NULL
     | AlexRawToken_declaration
     | AlexRawToken_ExportDefaultDeclaration
     | AlexRawToken_annotations
     | AlexRawToken_parameters
     | AlexRawToken_supers
     -- last keywords third part

     | AlexRawToken_IF
     | AlexRawToken_PLUS
     | AlexRawToken_BLOCK
     | AlexRawToken_CLASS
     | AlexRawToken_INTERFACE
     | AlexRawToken_ENUM
     | AlexRawToken_NEW_CLASS
     | AlexRawToken_IMPORT
     | AlexRawToken_METHOD
     | AlexRawToken_RETURN
     | AlexRawToken_CONSTRUCTOR
     | AlexRawToken_PARAMETER
     | AlexRawToken_PACKAGE
     | AlexRawToken_EQUAL_TO
     | AlexRawToken_MULTIPLY
     | AlexRawToken_VARIABLE
     | AlexRawToken_MODIFIERS
     | AlexRawToken_TYPE_CAST
     | AlexRawToken_ANNOTATION
     | AlexRawToken_ASSIGNMENT
     | AlexRawToken_INT_LITERAL
     | AlexRawToken_NULL_LITERAL
     | AlexRawToken_MEMBER_SELECT
     | AlexRawToken_PARENTHESIZED
     | AlexRawToken_PRIMITIVE_TYPE
     | AlexRawToken_STRING_LITERAL
     | AlexRawToken_BOOLEAN_LITERAL
     | AlexRawToken_COMPILATION_UNIT
     | AlexRawToken_METHOD_INVOCATION
     | AlexRawToken_LOGICAL_COMPLEMENT
     | AlexRawToken_PARAMETERIZED_TYPE
     | AlexRawToken_UNBOUNDED_WILDCARD
     | AlexRawToken_EXPRESSION_STATEMENT
     | AlexRawToken_CHILDREN
     | AlexRawToken_START_LINE
     | AlexRawToken_START_COL
     | AlexRawToken_END_LINE
     | AlexRawToken_END_COL
     | AlexRawToken_TEXT

     -- *********
     -- *       *
     -- * other *
     -- *       *
     -- *********

     | AlexRawToken_QUOTED_INT      -- ^ Reserved Keyword
     | AlexRawToken_QUOTED_STR      -- ^ Reserved Keyword
     | AlexRawToken_QUOTED_BOOL     -- ^ Reserved Keyword

     -- *************
     -- *           *
     -- * operators *
     -- *           *
     -- *************

     | AlexRawToken_OP_LT           -- ^ Reserved Keyword
     | AlexRawToken_OP_GT           -- ^ Reserved Keyword
     | AlexRawToken_OP_EQ           -- ^ Reserved Keyword
     | AlexRawToken_OP_ASSIGN       -- ^ Reserved Keyword
     | AlexRawToken_OP_TIMES        -- ^ Reserved Keyword
     | AlexRawToken_OP_PLUS         -- ^ Reserved Keyword
     | AlexRawToken_OP_PLUSPLUS     -- ^ Reserved Keyword
     | AlexRawToken_OP_OR           -- ^ Reserved Keyword

     -- ***************
     -- *             *
     -- * expressions *
     -- *             *
     -- ***************

     | AlexRawToken_EXPR_NEW        -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CALL       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_MEMBER     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_UPDATE     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ASSIGN     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_LAMBDA     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_FUNCTION   -- ^ Reserved Keyword

     -- **************
     -- *            *
     -- * statements *
     -- *            *
     -- **************

     | AlexRawToken_STMT_IF         -- ^ Reserved Keyword
     | AlexRawToken_STMT_FOR        -- ^ Reserved Keyword
     | AlexRawToken_STMT_BLOCK      -- ^ Reserved Keyword
     | AlexRawToken_STMT_RETURN     -- ^ Reserved Keyword
     | AlexRawToken_STMT_EXP        -- ^ Reserved Keyword

     -- ***************
     -- *             *
     -- * punctuation *
     -- *             *
     -- ***************

     | AlexRawToken_COLON           -- ^ Punctuation __:__
     | AlexRawToken_COMMA           -- ^ Punctuation __,__
     | AlexRawToken_HYPHEN          -- ^ Punctuation __-__

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

