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

module RbLexer

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
@KW                 = \"kw\"
@KW_ID              = \"id\"
@KW_INT             = \"int\"
@KW_FLOAT           = \"float\"
@KW_OP              = \"op\"
@KW_END             = \"end\"
@KW_RAW             = \"raw\"
@KW_SELF            = \"self\"
@KW_GVAR            = \"gvar\"
@KW_BEGIN           = \"begin\"
@KW_BREAK           = \"break\"
@KW_CASE            = \"case\"
@KW_WHEN            = \"when\"
@KW_KEYWORD         = \"keyword\"
@KW_CALL            = \"call\"
@KW_VCALL           = \"vcall\"
@KW_DEFINED         = \"defined\"
@KW_DICT            = \"bare_assoc_hash\"
@KW_DICT2           = \"hash\"
@KW_LABEL           = \"label\"
@KW_KEYWORDS        = \"keywords\"
@KW_BLOCK           = \"block\"
@KW_BLOCK_VAR       = \"block_var\"
@KW_BLOCK_ARG       = \"blockarg\"
@KW_MODULE          = \"module\"
@KW_BLOCK2          = \"method_add_block\"
@KW_ASSOC           = \"assoc\"
@KW_ASSOC2          = \"assoc_splat\"
@KW_ASSOCS          = \"assocs\"
@KW_PERIOD          = \"period\"
@KW_PARENT          = \"parent\"
@KW_RECEIVER        = \"receiver\"
@KW_STRING1         = \"string_literal\"
@KW_STRING2         = \"tstring_content\"
@KW_STRING3         = \"symbol_literal\"
@KW_STRING4         = \"string_embexpr\"
@KW_HEREDOC         = \"heredoc\"
@KW_SUPER           = \"superclass\"
@KW_SUPER2          = \"super\"
@KW_ZSUPER          = \"zsuper\"
@KW_CLASS           = \"class\"
@KW_SCLASS          = \"sclass\"
@KW_ASSIGN          = \"assign\"
@KW_ASSIGN2         = \"opassign\"
@KW_ASSIGN3         = \"massign\"
@KW_LOC             = \"location\"
@KW_COMMAND         = \"command\"
@KW_COMMAND2        = \"command_call\"
@KW_MESSAGE         = \"message\"
@KW_COMMENT         = \"comment\"
@KW_CONSTANT        = \"constant\"
@KW_CONSTANT2       = \"const_ref\"
@KW_AREF_FIELD      = \"aref_field\"
@KW_CONSTANT4       = \"const_path_ref\"
@KW_CONSTANT5       = \"top_const_ref\"
@KW_CONSTANT3       = \"const\"
@KW_KEY             = \"key\"
@KW_ARG             = "Arg"
@KW_VAR             = \"var_field\"
@KW_FIELD           = \"field\"
@KW_NULL            = null
@KW_TEST            = \"test\"
@KW_LINE            = \"line\"
@KW_TRUE            = \"true\"
@KW_ARGS            = \"args\"
@KW_ARG_STAR        = \"arg_star\"
@KW_REGEX           = \"regexp_literal\"
@KW_NAME            = \"name\"
@KW_EXPR            =  expr
@KW_MAME            =  Name
@KW_TYPE            = \"type\"
@KW_LEFT            = \"left\"
@KW_LEFT2           = \"mlhs\"
@KW_LEFT3           = \"mlhs_paren\"
@KW_MRHS           = \"mrhs\"
@KW_ARG_BLOCK       = \"arg_block\"
@KW_REST            = \"keyword_rest\"
@KW_REST2           = \"kwrest_param\"
@KW_REST3           = \"rest\"
@KW_REST_PARAM      = \"rest_param\"
@KW_DYNA            = \"dyna_symbol\"
@KW_EXCEPTION       = \"exception\"
@KW_EXCEPTIONS      = \"exceptions\"
@KW_RESCUE          = \"rescue\"
@KW_RESCUE2         = \"rescue_clause\"
@KW_RESCUE4         = \"else_clause\"
@KW_RESCUE3         = \"rescue_ex\"
@KW_ENSURE          = \"ensure\"
@KW_ENSURE_CLAUSE   = \"ensure_clause\"
@KW_VARIABLE        = \"variable\"
@KW_BACKREF         = \"backref\"
@KW_NEXT            = \"next\"
@KW_LOOP            = "loop"
@KW_INIT            = \"init\"
@KW_COND            = "cond"
@KW_BODY            = \"body\"
@KW_UPDATE          = \"update\"
@KW_INDEX           = \"index\"
@KW_PARTS           = \"parts\"
@KW_ELEMENTS        = \"elements\"
@KW_OPTIONS         = \"options\"
@KW_RANGE           = \"range\"
@KW_PAREN           = \"paren\"
@KW_FALSE           = \"false\"
@KW_FALSY           = \"falsy\"
@KW_TRUTHY          = \"truthy\"
@KW_START           = \"start\"
@KW_EXPRS           = "exprs"
@KW_VALUE           = \"value\"
@KW_RIGHT           = \"right\"
@KW_STMTS           = \"statements\"
@KW_ARRAY           = \"array\"
@KW_QWORDS           = \"qwords\"
@KW_QSYMBOLS         = \"qsymbols\"
@KW_PARAM           = Param
@KW_PARAMS          = \"params\"
@KW_OBJECT          = \"object\"
@KW_PREFIX          = \"prefix\"
@KW_TARGET          = \"target\"
@KW_COLUMN          = \"column\"
@KW_LITERAL         = \"Literal\"
@KW_PROGRAM         = \"program\"
@KW_COMPUTED        = \"computed\"
@KW_PROPERTY        = \"property\"
@KW_CONTENTS        = \"contents\"
@KW_OPERATOR        = \"operator\"
@KW_COMMENTS        = \"comments\"
@KW_ARGUMENT        = \"argument\"
@KW_ARGUMENTS       = \"arguments\"
@KW_ARGUMENTS2      = \"arg_paren\"
@KW_BODYSTMT        = \"bodystmt\"
@KW_STATEMENT       = \"statement\"
@KW_CALLEE          = \"callee\"
@KW_ASYNC           = \"async\"
@KW_COLLECTION      = \"collection\"
@KW_GENERATOR       = \"generator\"
@KW_SRC_TYPE        = \"sourceType\"
@KW_EXPRESSION      = \"expression\"
@KW_REQUIREDS       = \"requireds\"
@KW_OPTIONALS       = \"optionals\"
@KW_PREDICATE       = \"predicate\"
@KW_ALTERNATE       = \"alternate\"
@KW_CONSEQUENT      = \"consequent\"
@KW_STMT_VOID       = \"void_stmt\"
@KW_EXPR_VAR        = \"var_ref\"
@KW_EXPR_SUBSCRIPT  = \"aref\"
@KW_STMT_EXPR       = "Stmt_Expression"
@KW_SCALAR_INT      = "Scalar_Int"
@KW_IDENTIFIER      = \"ident\"
@KW_YIELD           = \"yield\"
@KW_IVAR            = \"ivar\"
@KW_CVAR            = \"cvar\"
@KW_RETURN_TYPE     = "returnType"
@KW_STMT_FUNCTION   = "Stmt_Function"
@KW_FUNCTION_DEC    = \"def\"

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

@KW_STMT_IF     = \"if\"
@KW_STMT_ELSE   = \"else\"
@KW_STMT_ELSE2  = \"elsif\"
@KW_STMT_IF2    = \"if_op\"
@KW_STMT_FOR    = \"for\"
@KW_STMT_BLOCK  = \"BlockStatement\"
@KW_STMT_RETURN = \"return\"
@KW_STMT_UNLESS = \"unless\"
@KW_STMT_WHILE  = \"while\"
@KW_STMT_EXP    = \"ExpressionStatement\"

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

@KW_EXPR_CALL   = \"CallExpression\"
@KW_EXPR_UNOP   = \"unary\"
@KW_EXPR_BINOP  = \"binary\"
@KW_EXPR_MEMBER = \"MemberExpression\"
@KW_EXPR_UPDATE = \"UpdateExpression\"
@KW_EXPR_ASSIGN = \"AssignmentExpression\"
@string_concat  = \"string_concat\"
@lambda         = \"lambda\"
@lambda_var     = \"lambda_var\"
-- last keywords first part

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

@KW_OP_LT       = \"\<\"
@KW_OP_GEQ      = \"\>=\"
@KW_OP_LEQ      = \"\<=\"
@KW_OP_GT       = \"\>\"
@KW_OP_SHL      = \"\<\<\"
@KW_OP_EQ       = \"==\"
@KW_OP_EQ3      = \"===\"
@KW_OP_NE       = \"!=\"
@KW_OP_EQMATCH  = \"\=\~\"
@KW_OP_PLUSEQ   = \"\+=\"
@KW_OP_MINUSEQ  = \"\-=\"
@KW_OP_NEQ      = \"!\~\"
@KW_OP_BANG     = \"!\"
@KW_OP_OR       = \"\|\|\"
@KW_OP_AND      = \"&&\"
@KW_OP_AND2     = \"and\"
@KW_OP_OR2      = \"\|\|=\"
@KW_OP_AMP_EQ   = \"\&\=\"
@KW_OP_ASSIGN   = \"=\"
@KW_OP_DOT      = \"\.\"
@KW_OP_AMP      = \"\&\"
@KW_OP_AMP_DOT  = \"&\.\"
@KW_OP_PLUS     = \"\+\"
@KW_OP_MINUS    = \"\-\"
@KW_OP_TIMES    = \"\*\"
@KW_OP_DIV      = \"\/\"
@KW_OP_POW      = \"\*\*\"
@KW_OP_PERCENT  = \"\%\"
@KW_OP_DOTDOT   = \"\.\.\"
@KW_OP_DOTDOTDOT = \"\.\.\.\"
@KW_OP_PLUSPLUS = \"\+\+\"
@KW_OP_PIPE     = \"\|\"
@KW_OP_COLON2   = \"::\"

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
@LETTER = [A-Za-z_]
@LETTER_OR_DIGIT = @LETTER | @DIGIT
@QUOTE = \"
@BS    = \\
@ESC   = @BS.
@NON_QUOTE = $printable # [\"\\]
@STRING_CHAR = (@NON_QUOTE)|(@ESC)
@ID = (@QUOTE)(@STRING_CHAR*)(@QUOTE)

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

@KW                 { lex' AlexRawToken_KW              }
@KW_ID              { lex' AlexRawToken_KWID            }
@KW_INT             { lex' AlexRawToken_KWINT           }
@KW_FLOAT           { lex' AlexRawToken_FLOAT           }
@KW_OP              { lex' AlexRawToken_OP              }
@KW_END             { lex' AlexRawToken_END             }
@KW_RAW             { lex' AlexRawToken_RAW             }
@KW_SELF            { lex' AlexRawToken_SELF            }
@KW_GVAR            { lex' AlexRawToken_GVAR            }
@KW_BEGIN           { lex' AlexRawToken_BEGIN           }
@KW_BREAK           { lex' AlexRawToken_BREAK           }
@KW_CASE            { lex' AlexRawToken_CASE            }
@KW_WHEN            { lex' AlexRawToken_WHEN            }
@KW_KEYWORD         { lex' AlexRawToken_KEYWORD         }
@KW_CALL            { lex' AlexRawToken_CALL            }
@KW_VCALL           { lex' AlexRawToken_VCALL           }
@KW_DEFINED         { lex' AlexRawToken_DEFINED         }
@KW_LOC             { lex' AlexRawToken_LOC             }
@KW_DICT            { lex' AlexRawToken_DICT            }
@KW_DICT2           { lex' AlexRawToken_DICT2           }
@KW_LABEL           { lex' AlexRawToken_LABEL           }
@KW_KEYWORDS        { lex' AlexRawToken_KEYWORDS        }
@KW_BLOCK           { lex' AlexRawToken_BLOCK           }
@KW_BLOCK_VAR       { lex' AlexRawToken_BLOCK_VAR       }
@KW_BLOCK_ARG       { lex' AlexRawToken_BLOCK_ARG       }
@KW_MODULE          { lex' AlexRawToken_MODULE          }
@KW_BLOCK2          { lex' AlexRawToken_BLOCK2          }
@KW_ASSOC           { lex' AlexRawToken_ASSOC           }
@KW_ASSOC2          { lex' AlexRawToken_ASSOC2          }
@KW_ASSOCS          { lex' AlexRawToken_ASSOCS          }
@KW_PERIOD          { lex' AlexRawToken_PERIOD          }
@KW_PARENT          { lex' AlexRawToken_PARENT          }
@KW_RECEIVER        { lex' AlexRawToken_RECEIVER        }
@KW_STRING1         { lex' AlexRawToken_STRING1         }
@KW_STRING2         { lex' AlexRawToken_STRING2         }
@KW_STRING3         { lex' AlexRawToken_STRING3         }
@KW_STRING4         { lex' AlexRawToken_STRING4         }
@KW_HEREDOC         { lex' AlexRawToken_HEREDOC         }
@KW_SUPER           { lex' AlexRawToken_SUPER           }
@KW_SUPER2          { lex' AlexRawToken_SUPER2          }
@KW_ZSUPER          { lex' AlexRawToken_ZSUPER          }
@KW_CLASS           { lex' AlexRawToken_CLASS           }
@KW_SCLASS          { lex' AlexRawToken_SCLASS          }
@KW_ASSIGN          { lex' AlexRawToken_ASSIGN          }
@KW_ASSIGN2         { lex' AlexRawToken_ASSIGN2         }
@KW_ASSIGN3         { lex' AlexRawToken_ASSIGN3         }
@KW_COMMAND         { lex' AlexRawToken_COMMAND         }
@KW_COMMAND2        { lex' AlexRawToken_COMMAND2        }
@KW_MESSAGE         { lex' AlexRawToken_MESSAGE         }
@KW_COMMENT         { lex' AlexRawToken_COMMENT         }
@KW_CONSTANT        { lex' AlexRawToken_CONSTANT        }
@KW_CONSTANT2       { lex' AlexRawToken_CONSTANT2       }
@KW_CONSTANT3       { lex' AlexRawToken_CONSTANT3       }
@KW_AREF_FIELD      { lex' AlexRawToken_AREF_FIELD      }
@KW_CONSTANT4       { lex' AlexRawToken_CONSTANT4       }
@KW_CONSTANT5       { lex' AlexRawToken_CONSTANT5       }
@KW_ARG             { lex' AlexRawToken_ARG             }
@KW_KEY             { lex' AlexRawToken_KEY             }
@KW_VAR             { lex' AlexRawToken_VAR             }
@KW_FIELD           { lex' AlexRawToken_FIELD           }
@KW_NULL            { lex' AlexRawToken_NULL            }
@KW_TEST            { lex' AlexRawToken_TEST            }
@KW_LINE            { lex' AlexRawToken_LINE            }
@KW_TRUE            { lex' AlexRawToken_TRUE            }
@KW_ARGS            { lex' AlexRawToken_ARGS            }
@KW_ARG_STAR        { lex' AlexRawToken_ARG_STAR        }
@KW_REGEX           { lex' AlexRawToken_REGEX           }
@KW_NAME            { lex' AlexRawToken_NAME            }
@KW_EXPR            { lex' AlexRawToken_EXPR            }
@KW_MAME            { lex' AlexRawToken_MAME            }
@KW_TYPE            { lex' AlexRawToken_TYPE            }
@KW_LEFT            { lex' AlexRawToken_LEFT            }
@KW_LEFT2           { lex' AlexRawToken_LEFT2           }
@KW_LEFT3           { lex' AlexRawToken_LEFT3           }
@KW_MRHS           { lex' AlexRawToken_MRHS           }
@KW_ARG_BLOCK       { lex' AlexRawToken_ARG_BLOCK       }
@KW_REST            { lex' AlexRawToken_REST            }
@KW_REST2           { lex' AlexRawToken_REST2           }
@KW_REST3           { lex' AlexRawToken_REST3           }
@KW_REST_PARAM      { lex' AlexRawToken_REST_PARAM      }
@KW_DYNA            { lex' AlexRawToken_DYNA            }
@KW_EXCEPTION       { lex' AlexRawToken_EXCEPTION       }
@KW_EXCEPTIONS      { lex' AlexRawToken_EXCEPTIONS      }
@KW_RESCUE          { lex' AlexRawToken_RESCUE          }
@KW_RESCUE2         { lex' AlexRawToken_RESCUE2         }
@KW_RESCUE4         { lex' AlexRawToken_RESCUE4         }
@KW_RESCUE3         { lex' AlexRawToken_RESCUE3         }
@KW_ENSURE          { lex' AlexRawToken_ENSURE          }
@KW_ENSURE_CLAUSE   { lex' AlexRawToken_ENSURE_CLAUSE   }
@KW_VARIABLE        { lex' AlexRawToken_VARIABLE        }
@KW_BACKREF         { lex' AlexRawToken_BACKREF         }
@KW_NEXT            { lex' AlexRawToken_NEXT            }
@KW_LOOP            { lex' AlexRawToken_LOOP            }
@KW_INIT            { lex' AlexRawToken_INIT            }
@KW_COND            { lex' AlexRawToken_COND            }
@KW_BODY            { lex' AlexRawToken_BODY            }
@KW_UPDATE          { lex' AlexRawToken_UPDATE          }
@KW_PARTS           { lex' AlexRawToken_PARTS           }
@KW_ELEMENTS        { lex' AlexRawToken_ELEMENTS        }
@KW_OPTIONS         { lex' AlexRawToken_OPTIONS         }
@KW_RANGE           { lex' AlexRawToken_RANGE           }
@KW_INDEX           { lex' AlexRawToken_INDEX           }
@KW_PAREN           { lex' AlexRawToken_PAREN           }
@KW_FALSE           { lex' AlexRawToken_FALSE           }
@KW_FALSY           { lex' AlexRawToken_FALSY           }
@KW_TRUTHY          { lex' AlexRawToken_TRUTHY          }
@KW_START           { lex' AlexRawToken_START           }
@KW_EXPRS           { lex' AlexRawToken_EXPRS           }
@KW_VALUE           { lex' AlexRawToken_VALUE           }
@KW_RIGHT           { lex' AlexRawToken_RIGHT           }
@KW_STMTS           { lex' AlexRawToken_STMTS           }
@KW_ARRAY           { lex' AlexRawToken_ARRAY           }
@KW_QWORDS           { lex' AlexRawToken_QWORDS           }
@KW_QSYMBOLS         { lex' AlexRawToken_QSYMBOLS         }
@KW_PARAM           { lex' AlexRawToken_PARAM           }
@KW_PARAMS          { lex' AlexRawToken_PARAMS          }
@KW_OBJECT          { lex' AlexRawToken_OBJECT          }
@KW_PREFIX          { lex' AlexRawToken_PREFIX          }
@KW_TARGET          { lex' AlexRawToken_TARGET          }
@KW_COLUMN          { lex' AlexRawToken_COLUMN          }
@KW_LITERAL         { lex' AlexRawToken_LITERAL         }
@KW_PROGRAM         { lex' AlexRawToken_PROGRAM         }
@KW_PROPERTY        { lex' AlexRawToken_PROPERTY        }
@KW_COMPUTED        { lex' AlexRawToken_COMPUTED        }
@KW_CONTENTS        { lex' AlexRawToken_CONTENTS        }
@KW_OPERATOR        { lex' AlexRawToken_OPERATOR        }
@KW_COMMENTS        { lex' AlexRawToken_COMMENTS        }
@KW_PREDICATE       { lex' AlexRawToken_PREDICATE       }
@KW_REQUIREDS       { lex' AlexRawToken_REQUIREDS       }
@KW_OPTIONALS       { lex' AlexRawToken_OPTIONALS       }
@KW_ALTERNATE       { lex' AlexRawToken_ALTERNATE       }
@KW_CONSEQUENT      { lex' AlexRawToken_CONSEQUENT      }
@KW_ARGUMENT        { lex' AlexRawToken_ARGUMENT        }
@KW_BODYSTMT        { lex' AlexRawToken_BODYSTMT        }
@KW_STATEMENT       { lex' AlexRawToken_STATEMENT       }
@KW_ARGUMENTS       { lex' AlexRawToken_ARGUMENTS       }
@KW_ARGUMENTS2      { lex' AlexRawToken_ARGUMENTS2      }
@KW_CALLEE          { lex' AlexRawToken_CALLEE          }
@KW_ASYNC           { lex' AlexRawToken_ASYNC           }
@KW_EXPRESSION      { lex' AlexRawToken_EXPRESSION      }
@KW_SRC_TYPE        { lex' AlexRawToken_SRC_TYPE        }
@KW_COLLECTION      { lex' AlexRawToken_COLLECTION      }
@KW_GENERATOR       { lex' AlexRawToken_GENERATOR       }
@KW_STMT_IF         { lex' AlexRawToken_STMT_IF         }
@KW_STMT_ELSE       { lex' AlexRawToken_STMT_ELSE       }
@KW_STMT_ELSE2      { lex' AlexRawToken_STMT_ELSE2      }
@KW_STMT_IF2        { lex' AlexRawToken_STMT_IF2        }
@KW_STMT_WHILE      { lex' AlexRawToken_STMT_WHILE      }
@KW_STMT_VOID       { lex' AlexRawToken_STMT_VOID       }
@KW_EXPR_VAR        { lex' AlexRawToken_EXPR_VAR        }
@KW_EXPR_SUBSCRIPT  { lex' AlexRawToken_EXPR_SUBSCRIPT  }
@KW_EXPR_CALL       { lex' AlexRawToken_EXPR_CALL       }
@KW_STMT_EXPR       { lex' AlexRawToken_STMT_EXPR       }
@KW_SCALAR_INT      { lex' AlexRawToken_SCALAR_INT      }
@KW_IDENTIFIER      { lex' AlexRawToken_IDENTIFIER      }
@KW_YIELD           { lex' AlexRawToken_YIELD           }
@KW_IVAR            { lex' AlexRawToken_IVAR            }
@KW_CVAR            { lex' AlexRawToken_CVAR            }
@KW_RETURN_TYPE     { lex' AlexRawToken_RETURN_TYPE     }
@KW_FUNCTION_DEC    { lex' AlexRawToken_FUNCTION_DEC    }

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

@KW_EXPR_CALL   { lex' AlexRawToken_EXPR_CALL   }
@KW_EXPR_MEMBER { lex' AlexRawToken_EXPR_MEMBER }
@KW_EXPR_UNOP   { lex' AlexRawToken_EXPR_UNOP   }
@KW_EXPR_BINOP  { lex' AlexRawToken_EXPR_BINOP  }
@KW_EXPR_UPDATE { lex' AlexRawToken_EXPR_UPDATE }
@KW_EXPR_ASSIGN { lex' AlexRawToken_EXPR_ASSIGN }

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

@KW_STMT_FOR      { lex' AlexRawToken_STMT_FOR      }
@KW_STMT_BLOCK    { lex' AlexRawToken_STMT_BLOCK    }
@KW_STMT_RETURN   { lex' AlexRawToken_STMT_RETURN   }
@KW_STMT_UNLESS   { lex' AlexRawToken_STMT_UNLESS   }
@KW_STMT_EXP      { lex' AlexRawToken_STMT_EXP      }
@KW_STMT_FUNCTION { lex' AlexRawToken_STMT_FUNCTION }
@string_concat {lex' AlexRawToken_string_concat}
@lambda {lex' AlexRawToken_lambda}
@lambda_var {lex' AlexRawToken_lambda_var}
-- last keywords second part

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

@KW_OP_LT       { lex' AlexRawToken_OP_LT       }
@KW_OP_GT       { lex' AlexRawToken_OP_GT       }
@KW_OP_GEQ      { lex' AlexRawToken_OP_GEQ      }
@KW_OP_LEQ      { lex' AlexRawToken_OP_LEQ      }
@KW_OP_SHL      { lex' AlexRawToken_OP_SHL      }
@KW_OP_EQ       { lex' AlexRawToken_OP_EQ       }
@KW_OP_EQ3      { lex' AlexRawToken_OP_EQ3      }
@KW_OP_NE       { lex' AlexRawToken_OP_NE       }
@KW_OP_EQMATCH  { lex' AlexRawToken_OP_EQMATCH  }
@KW_OP_PLUSEQ   { lex' AlexRawToken_OP_PLUSEQ   }
@KW_OP_MINUSEQ  { lex' AlexRawToken_OP_MINUSEQ  }
@KW_OP_NEQ      { lex' AlexRawToken_OP_NEQ      }
@KW_OP_BANG     { lex' AlexRawToken_OP_BANG     }
@KW_OP_OR       { lex' AlexRawToken_OP_OR       }
@KW_OP_AND      { lex' AlexRawToken_OP_AND      }
@KW_OP_AND2     { lex' AlexRawToken_OP_AND2     }
@KW_OP_OR2      { lex' AlexRawToken_OP_OR2      }
@KW_OP_AMP_EQ   { lex' AlexRawToken_OP_AMP_EQ   }
@KW_OP_ASSIGN   { lex' AlexRawToken_OP_ASSIGN   }
@KW_OP_DOT      { lex' AlexRawToken_OP_DOT      }
@KW_OP_AMP      { lex' AlexRawToken_OP_AMP      }
@KW_OP_AMP_DOT  { lex' AlexRawToken_OP_AMP_DOT  }
@KW_OP_PLUS     { lex' AlexRawToken_OP_PLUS     }
@KW_OP_MINUS    { lex' AlexRawToken_OP_MINUS    }
@KW_OP_TIMES    { lex' AlexRawToken_OP_TIMES    }
@KW_OP_DIV      { lex' AlexRawToken_OP_DIV      }
@KW_OP_POW      { lex' AlexRawToken_OP_POW      }
@KW_OP_DOTDOT   { lex' AlexRawToken_OP_DOTDOT   }
@KW_OP_DOTDOTDOT { lex' AlexRawToken_OP_DOTDOTDOT }
@KW_OP_PERCENT  { lex' AlexRawToken_OP_PERCENT  }
@KW_OP_PLUSPLUS { lex' AlexRawToken_OP_PLUSPLUS }
@KW_OP_PIPE     { lex' AlexRawToken_OP_PIPE     }
@KW_OP_COLON2   { lex' AlexRawToken_OP_COLON2   }

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

@ID        { lex  AlexRawToken_ID                 }
@INT       { lex (AlexRawToken_INT . round. read) }

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
 
     | AlexRawToken_KW              -- ^ Reserved Keyword
     | AlexRawToken_KWID            -- ^ Reserved Keyword
     | AlexRawToken_KWINT           -- ^ Reserved Keyword
     | AlexRawToken_FLOAT          -- ^ Reserved Keyword
     | AlexRawToken_OP              -- ^ Reserved Keyword
     | AlexRawToken_END             -- ^ Reserved Keyword
     | AlexRawToken_RAW             -- ^ Reserved Keyword
     | AlexRawToken_SELF            -- ^ Reserved Keyword
     | AlexRawToken_GVAR            -- ^ Reserved Keyword
     | AlexRawToken_BEGIN           -- ^ Reserved Keyword
     | AlexRawToken_BREAK           -- ^ Reserved Keyword
     | AlexRawToken_CASE            -- ^ Reserved Keyword
     | AlexRawToken_WHEN            -- ^ Reserved Keyword
     | AlexRawToken_KEYWORD         -- ^ Reserved Keyword
     | AlexRawToken_CALL            -- ^ Reserved Keyword
     | AlexRawToken_VCALL           -- ^ Reserved Keyword
     | AlexRawToken_DEFINED         -- ^ Reserved Keyword
     | AlexRawToken_LOC             -- ^ Reserved Keyword
     | AlexRawToken_CLASS           -- ^ Reserved Keyword
     | AlexRawToken_SCLASS          -- ^ Reserved Keyword
     | AlexRawToken_ASSIGN          -- ^ Reserved Keyword
     | AlexRawToken_ASSIGN2         -- ^ Reserved Keyword
     | AlexRawToken_ASSIGN3         -- ^ Reserved Keyword
     | AlexRawToken_SUPER           -- ^ Reserved Keyword
     | AlexRawToken_SUPER2          -- ^ Reserved Keyword
     | AlexRawToken_ZSUPER          -- ^ Reserved Keyword
     | AlexRawToken_DICT            -- ^ Reserved Keyword
     | AlexRawToken_DICT2           -- ^ Reserved Keyword
     | AlexRawToken_LABEL           -- ^ Reserved Keyword
     | AlexRawToken_KEYWORDS        -- ^ Reserved Keyword
     | AlexRawToken_BLOCK           -- ^ Reserved Keyword
     | AlexRawToken_BLOCK_VAR       -- ^ Reserved Keyword
     | AlexRawToken_BLOCK_ARG       -- ^ Reserved Keyword
     | AlexRawToken_MODULE          -- ^ Reserved Keyword
     | AlexRawToken_BLOCK2          -- ^ Reserved Keyword
     | AlexRawToken_ASSOC           -- ^ Reserved Keyword
     | AlexRawToken_ASSOC2          -- ^ Reserved Keyword
     | AlexRawToken_ASSOCS          -- ^ Reserved Keyword
     | AlexRawToken_PERIOD          -- ^ Reserved Keyword
     | AlexRawToken_PARENT          -- ^ Reserved Keyword
     | AlexRawToken_RECEIVER        -- ^ Reserved Keyword
     | AlexRawToken_STRING1         -- ^ Reserved Keyword
     | AlexRawToken_STRING2         -- ^ Reserved Keyword
     | AlexRawToken_STRING3         -- ^ Reserved Keyword
     | AlexRawToken_STRING4         -- ^ Reserved Keyword
     | AlexRawToken_HEREDOC         -- ^ Reserved Keyword
     | AlexRawToken_COMMAND         -- ^ Reserved Keyword
     | AlexRawToken_COMMAND2        -- ^ Reserved Keyword
     | AlexRawToken_COMMENT         -- ^ Reserved Keyword
     | AlexRawToken_MESSAGE         -- ^ Reserved Keyword
     | AlexRawToken_CONSTANT        -- ^ Reserved Keyword
     | AlexRawToken_CONSTANT2       -- ^ Reserved Keyword
     | AlexRawToken_CONSTANT3       -- ^ Reserved Keyword
     | AlexRawToken_CONSTANT4       -- ^ Reserved Keyword
     | AlexRawToken_CONSTANT5       -- ^ Reserved Keyword
     | AlexRawToken_AREF_FIELD      -- ^ Reserved Keyword
     | AlexRawToken_ARG             -- ^ Reserved Keyword
     | AlexRawToken_KEY             -- ^ Reserved Keyword
     | AlexRawToken_VAR             -- ^ Reserved Keyword
     | AlexRawToken_FIELD           -- ^ Reserved Keyword
     | AlexRawToken_TEST            -- ^ Reserved Keyword
     | AlexRawToken_NULL            -- ^ Reserved Keyword
     | AlexRawToken_LINE            -- ^ Reserved Keyword
     | AlexRawToken_TRUE            -- ^ Reserved Keyword
     | AlexRawToken_ARGS            -- ^ Reserved Keyword
     | AlexRawToken_ARG_STAR        -- ^ Reserved Keyword
     | AlexRawToken_REGEX           -- ^ Reserved Keyword
     | AlexRawToken_NAME            -- ^ Reserved Keyword
     | AlexRawToken_EXPR            -- ^ Reserved Keyword
     | AlexRawToken_MAME            -- ^ Reserved Keyword
     | AlexRawToken_TYPE            -- ^ Reserved Keyword
     | AlexRawToken_LEFT            -- ^ Reserved Keyword
     | AlexRawToken_LEFT2           -- ^ Reserved Keyword
     | AlexRawToken_LEFT3           -- ^ Reserved Keyword
     | AlexRawToken_MRHS           -- ^ Reserved Keyword
     | AlexRawToken_ARG_BLOCK       -- ^ Reserved Keyword
     | AlexRawToken_REST            -- ^ Reserved Keyword
     | AlexRawToken_REST2           -- ^ Reserved Keyword
     | AlexRawToken_REST3           -- ^ Reserved Keyword
     | AlexRawToken_REST_PARAM      -- ^ Reserved Keyword
     | AlexRawToken_DYNA            -- ^ Reserved Keyword
     | AlexRawToken_EXCEPTION       -- ^ Reserved Keyword
     | AlexRawToken_EXCEPTIONS      -- ^ Reserved Keyword
     | AlexRawToken_RESCUE          -- ^ Reserved Keyword
     | AlexRawToken_RESCUE2         -- ^ Reserved Keyword
     | AlexRawToken_RESCUE4         -- ^ Reserved Keyword
     | AlexRawToken_RESCUE3         -- ^ Reserved Keyword
     | AlexRawToken_ENSURE          -- ^ Reserved Keyword
     | AlexRawToken_ENSURE_CLAUSE   -- ^ Reserved Keyword
     | AlexRawToken_VARIABLE        -- ^ Reserved Keyword
     | AlexRawToken_BACKREF         -- ^ Reserved Keyword
     | AlexRawToken_NEXT            -- ^ Reserved Keyword
     | AlexRawToken_LOOP            -- ^ Reserved Keyword
     | AlexRawToken_INIT            -- ^ Reserved Keyword
     | AlexRawToken_COND            -- ^ Reserved Keyword
     | AlexRawToken_BODY            -- ^ Reserved Keyword
     | AlexRawToken_START           -- ^ Reserved Keyword
     | AlexRawToken_UPDATE          -- ^ Reserved Keyword
     | AlexRawToken_PARTS           -- ^ Reserved Keyword
     | AlexRawToken_ELEMENTS      -- ^ Reserved Keyword
     | AlexRawToken_OPTIONS         -- ^ Reserved Keyword
     | AlexRawToken_RANGE           -- ^ Reserved Keyword
     | AlexRawToken_INDEX           -- ^ Reserved Keyword
     | AlexRawToken_PAREN           -- ^ Reserved Keyword
     | AlexRawToken_FALSE           -- ^ Reserved Keyword
     | AlexRawToken_FALSY           -- ^ Reserved Keyword
     | AlexRawToken_TRUTHY          -- ^ Reserved Keyword
     | AlexRawToken_EXPRS           -- ^ Reserved Keyword
     | AlexRawToken_VALUE           -- ^ Reserved Keyword
     | AlexRawToken_RIGHT           -- ^ Reserved Keyword
     | AlexRawToken_STMTS           -- ^ Reserved Keyword
     | AlexRawToken_ARRAY           -- ^ Reserved Keyword
     | AlexRawToken_QWORDS        -- ^ Reserved Keyword
     | AlexRawToken_QSYMBOLS      -- ^ Reserved Keyword
     | AlexRawToken_PARAM           -- ^ Reserved Keyword
     | AlexRawToken_OBJECT          -- ^ Reserved Keyword
     | AlexRawToken_PREFIX          -- ^ Reserved Keyword
     | AlexRawToken_PARAMS          -- ^ Reserved Keyword
     | AlexRawToken_COLUMN          -- ^ Reserved Keyword
     | AlexRawToken_TARGET          -- ^ Reserved Keyword
     | AlexRawToken_LITERAL         -- ^ Reserved Keyword
     | AlexRawToken_PROGRAM         -- ^ Reserved Keyword
     | AlexRawToken_PROPERTY        -- ^ Reserved Keyword
     | AlexRawToken_COMPUTED        -- ^ Reserved Keyword
     | AlexRawToken_OPERATOR        -- ^ Reserved Keyword
     | AlexRawToken_CONTENTS        -- ^ Reserved Keyword
     | AlexRawToken_COMMENTS        -- ^ Reserved Keyword
     | AlexRawToken_ALTERNATE       -- ^ Reserved Keyword
     | AlexRawToken_PREDICATE       -- ^ Reserved Keyword
     | AlexRawToken_REQUIREDS       -- ^ Reserved Keyword
     | AlexRawToken_OPTIONALS       -- ^ Reserved Keyword
     | AlexRawToken_CONSEQUENT      -- ^ Reserved Keyword
     | AlexRawToken_ARGUMENT        -- ^ Reserved Keyword
     | AlexRawToken_BODYSTMT        -- ^ Reserved Keyword
     | AlexRawToken_STATEMENT       -- ^ Reserved Keyword
     | AlexRawToken_ARGUMENTS       -- ^ Reserved Keyword
     | AlexRawToken_ARGUMENTS2      -- ^ Reserved Keyword
     | AlexRawToken_CALLEE          -- ^ Reserved Keyword
     | AlexRawToken_ASYNC           -- ^ Reserved Keyword
     | AlexRawToken_EXPRESSION      -- ^ Reserved Keyword
     | AlexRawToken_SRC_TYPE        -- ^ Reserved Keyword
     | AlexRawToken_COLLECTION      -- ^ Reserved Keyword
     | AlexRawToken_GENERATOR       -- ^ Reserved Keyword
     | AlexRawToken_STMT_VOID       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_VAR        -- ^ Reserved Keyword
     | AlexRawToken_EXPR_SUBSCRIPT  -- ^ Reserved Keyword
     | AlexRawToken_STMT_EXPR       -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_INT      -- ^ Reserved Keyword
     | AlexRawToken_IDENTIFIER      -- ^ Reserved Keyword
     | AlexRawToken_YIELD           -- ^ Reserved Keyword
     | AlexRawToken_IVAR            -- ^ Reserved Keyword
     | AlexRawToken_CVAR            -- ^ Reserved Keyword
     | AlexRawToken_RETURN_TYPE     -- ^ Reserved Keyword
     | AlexRawToken_STMT_FUNCTION   -- ^ Reserved Keyword
     | AlexRawToken_FUNCTION_DEC    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CONST_GET  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_LT   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_PLUS -- ^ Reserved Keyword
     | AlexRawToken_string_concat
     | AlexRawToken_lambda
     | AlexRawToken_lambda_var
     -- last keywords third part

     -- *************
     -- *           *
     -- * operators *
     -- *           *
     -- *************

     | AlexRawToken_OP_LT           -- ^ Reserved Keyword
     | AlexRawToken_OP_GT           -- ^ Reserved Keyword
     | AlexRawToken_OP_GEQ          -- ^ Reserved Keyword
     | AlexRawToken_OP_LEQ          -- ^ Reserved Keyword
     | AlexRawToken_OP_SHL          -- ^ Reserved Keyword
     | AlexRawToken_OP_EQ           -- ^ Reserved Keyword
     | AlexRawToken_OP_EQ3          -- ^ Reserved Keyword
     | AlexRawToken_OP_NE           -- ^ Reserved Keyword
     | AlexRawToken_OP_EQMATCH      -- ^ Reserved Keyword
     | AlexRawToken_OP_PLUSEQ       -- ^ Reserved Keyword
     | AlexRawToken_OP_MINUSEQ      -- ^ Reserved Keyword
     | AlexRawToken_OP_NEQ          -- ^ Reserved Keyword
     | AlexRawToken_OP_BANG         -- ^ Reserved Keyword
     | AlexRawToken_OP_OR           -- ^ Reserved Keyword
     | AlexRawToken_OP_AND          -- ^ Reserved Keyword
     | AlexRawToken_OP_AND2         -- ^ Reserved Keyword
     | AlexRawToken_OP_OR2          -- ^ Reserved Keyword
     | AlexRawToken_OP_AMP_EQ       -- ^ Reserved Keyword
     | AlexRawToken_OP_ASSIGN       -- ^ Reserved Keyword
     | AlexRawToken_OP_DOT          -- ^ Reserved Keyword
     | AlexRawToken_OP_AMP          -- ^ Reserved Keyword
     | AlexRawToken_OP_AMP_DOT      -- ^ Reserved Keyword
     | AlexRawToken_OP_PLUS         -- ^ Reserved Keyword
     | AlexRawToken_OP_MINUS        -- ^ Reserved Keyword
     | AlexRawToken_OP_TIMES        -- ^ Reserved Keyword
     | AlexRawToken_OP_DIV          -- ^ Reserved Keyword
     | AlexRawToken_OP_POW          -- ^ Reserved Keyword
     | AlexRawToken_OP_DOTDOT       -- ^ Reserved Keyword
     | AlexRawToken_OP_DOTDOTDOT    -- ^ Reserved Keyword
     | AlexRawToken_OP_PERCENT      -- ^ Reserved Keyword
     | AlexRawToken_OP_PLUSPLUS     -- ^ Reserved Keyword
     | AlexRawToken_OP_PIPE         -- ^ Reserved Keyword
     | AlexRawToken_OP_COLON2       -- ^ Reserved Keyword

     -- ***************
     -- *             *
     -- * expressions *
     -- *             *
     -- ***************

     | AlexRawToken_EXPR_CALL       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_UNOP       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_MEMBER     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_UPDATE     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ASSIGN     -- ^ Reserved Keyword

     -- **************
     -- *            *
     -- * statements *
     -- *            *
     -- **************

     | AlexRawToken_STMT_IF         -- ^ Reserved Keyword
     | AlexRawToken_STMT_ELSE       -- ^ Reserved Keyword
     | AlexRawToken_STMT_ELSE2      -- ^ Reserved Keyword
     | AlexRawToken_STMT_IF2        -- ^ Reserved Keyword
     | AlexRawToken_STMT_FOR        -- ^ Reserved Keyword
     | AlexRawToken_STMT_BLOCK      -- ^ Reserved Keyword
     | AlexRawToken_STMT_RETURN     -- ^ Reserved Keyword
     | AlexRawToken_STMT_UNLESS     -- ^ Reserved Keyword
     | AlexRawToken_STMT_WHILE      -- ^ Reserved Keyword
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

-- **************
-- * alexError' *
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

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> Common.AdditionalRepoInfo -> String -> Either String a
runAlex' a fp additionalInfo input = runAlex input (setFilePath fp >> alexSetUserState (AlexUserState fp additionalInfo) >> a)
}

