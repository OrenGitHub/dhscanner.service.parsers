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

module PyLexer

where

-- project imports
import Common
import Location
import qualified Ast

-- general imports
import Prelude hiding (lex)
import Control.Monad ( liftM )
import System.FilePath ( takeDirectory, (</>), (<.>) )

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
@KW_ID              = id
@KW_CTX             = ctx
@KW_EQ              = Eq
@KW_GT              = Gt
@KW_GE              = GtE
@KW_LE              = LtE
@KW_LT              = Lt
@KW_IN              = In
@KW_IS              = Is
@KW_ISNOT           = IsNot
@KW_OR              = Or
@KW_OR2             = BitOr
@KW_BITXOR          = BitXor
@KW_AND             = And
@KW_AND2            = BitAnd
@KW_LSHIFT          = LShift
@KW_RSHIFT          = RShift
@KW_NOT             = Not
@KW_NOTEQ           = NotEq
@KW_NOTIN           = NotIn
@KW_ADD             = Add
@KW_POW             = Pow
@KW_FLOOR_DIV       = FloorDiv
@KW_MOD             = Mod
@KW_DIV             = Div
@KW_SUB             = Sub
@KW_USUB            = USub
@KW_INVERT          = Invert
@KW_MULT            = Mult
@KW_ARG             = arg
@KW_VARARG          = vararg
@KW_NULL            = null
@KW_COL             = col_offset
@KW_ECOL            = end_col_offset
@KW_LOAD            = Load
@KW_IS_ASYNC        = is_async
@KW_STORE           = Store
@KW_LINENO          = lineno
@KW_ELINENO         = end_lineno
@KW_TARGET          = target
@KW_TARGETS         = targets
@KW_DEFAULTS        = defaults
@KW_KWARG           = kwarg
@KW_COMPREHENSION   = comprehension
@KW_GENERATORS      = generators
@KW_KW_DEFAULTS     = kw_defaults
@KW_TRUE            = true
@KW_FUNC            = func
@KW_ARGS            = args
@KW_ATTR            = attr
@KW_ATTR2           = Attribute
@KW_STARRED         = Starred
@KW_SUBSCRIPT       = Subscript
@KW_SLICE           = slice
@KW_LOWER           = lower
@KW_STEP            = step
@KW_UPPER           = upper
@KW_EXPR_SLICE      = Slice
@KW_ELLIPSIS        = Ellipsis
@KW_ARGS2           = arguments
@KW_ARGS3           = posonlyargs
@KW_ARGS4           = kwonlyargs
@KW_EXPR            = expr
@KW_CALL            = Call
@KW_NAME2           = Name
@KW_COND            = cond
@KW_BODY            = body
@KW_NONE            = None
@KW_HANDLERS        = handlers
@KW_TYPE            = type
@KW_EXCEPT_HANDLER  = ExceptHandler
@KW_BODY2           = finalbody
@KW_TEST            = test
@KW_LEVEL           = level
@KW_NAME            = name
@KW_ASNAME          = asname
@KW_NAMES           = names
@KW_ALIAS           = alias
@KW_ORELSE          = orelse
@KW_KEYWORD         = keyword
@KW_KEYWORDS        = keywords
@KW_IMPORT          = Import
@KW_FSTRING         = JoinedStr
@KW_IMPORTF         = ImportFrom
@KW_CONVERSION      = conversion
@KW_FORMATTED_VAL   = FormattedValue
@KW_FORMAT_SPEC     = format_spec
@KW_ASSIGN          = Assign
@KW_AWAIT           = Await
@KW_NAMED_EXPR      = NamedExpr
@KW_ASSERT          = Assert
@KW_LAMBDA          = Lambda
@KW_SIMPLE          = simple
@KW_ASSIGN2         = AugAssign
@KW_ASSIGN3         = AnnAssign
@KW_TYPE_ALIAS      = TypeAlias
@KW_TYPE_VAR       = TypeVar
@KW_MATCH          = Match
@KW_MATCH_CASE     = match_case
@KW_MATCH_VALUE    = MatchValue
@KW_MATCH_SINGLETON= MatchSingleton
@KW_MATCH_SEQUENCE = MatchSequence
@KW_MATCH_MAPPING  = MatchMapping
@KW_MATCH_CLASS    = MatchClass
@KW_MATCH_STAR     = MatchStar
@KW_MATCH_AS       = MatchAs
@KW_MATCH_OR       = MatchOr
@KW_SUBJECT        = subject
@KW_CASES          = cases
@KW_PATTERN        = pattern
@KW_PATTERNS       = patterns
@KW_GUARD          = guard
@KW_CLS            = cls
@KW_KWD_ATTRS      = kwd_attrs
@KW_KWD_PATTERNS   = kwd_patterns
@KW_REST           = rest
@KW_ANNOTATION      = annotation
@KW_MODULE          = Module
@KW_MODULE2         = module
@KW_FALSE           = False
@KW_LIST            = List
@KW_SET             = Set
@KW_SET_COMP        = SetComp
@KW_LIST_COMP       = ListComp
@KW_DICT_COMP       = DictComp
@KW_GENERATOR_EXP   = GeneratorExp
@KW_TUPLE           = Tuple
@KW_ELT             = elt
@KW_ELTS            = elts
@KW_TRUE            = True
@KW_ITEMS           = items
@KW_FOR             = For|AsyncFor
@KW_WITH            = With|AsyncWith
@KW_WITH2           = withitem
@KW_CTX_MANAGER     = context_expr
@KW_KEY             = key
@KW_KIND            = kind
@KW_VALUE           = value
@KW_VALUES          = values
@KW_ARRAY           = array
@KW_PARAM           = Param
@KW_COMPARE         = Compare
@KW_COMPARE3        = BoolOp
@KW_COMPARE2        = comparators
@KW_DECORATORS      = decorator_list
@KW_TYPE_PARAMS     = type_params
@KW_TYPE_IGNORES    = type_ignores
@KW_LEFT            = left
@KW_EXC             = exc
@KW_ITER            = iter
@KW_WITH_VARS       = optional_vars
@KW_DICT            = Dict
@KW_KEYS            = keys
@KW_RIGHT           = right
@KW_OPERATOR        = op
@KW_OPERATOR2       = ops
@KW_OPERAND         = operand
@KW_STMT_EXPR       = Expr
@KW_STMT_FUNCTION   = FunctionDef|AsyncFunctionDef
@KW_STMT_CLASS      = ClassDef
@KW_SUPERS          = bases
@KW_EXPR_CONST      = Constant
@KW_EXPR_UNOP       = UnaryOp
@KW_EXPR_BINOP      = BinOp
@msg = msg
-- last keywords first part

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

@KW_STMT_IF       = If
@KW_STMT_IFS      = ifs
@KW_STMT_WHILE    = While
@KW_EXPR_IF       = IfExp
@KW_STMT_RETURN   = Return
@KW_STMT_RETURN2  = returns
@KW_STMT_CONTINUE = Continue
@KW_STMT_BREAK    = Break
@KW_STMT_PASS     = Pass
@KW_STMT_YIELD    = Yield
@KW_STMT_YIELDFROM = YieldFrom
@KW_STMT_DEL      = Del
@KW_STMT_DELETE   = Delete
@KW_STMT_GLOBAL   = Global
@KW_STMT_RAISE    = Raise
@KW_STMT_CAUSE    = cause
@KW_STMT_TRY      = Try

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

@KW_OP_LT       = \"\<\"
@KW_OP_EQ       = \"==\"
@KW_OP_ASSIGN   = "="
@KW_OP_TIMES    = \"\*\"

-- ************
-- *          *
-- * integers *
-- *          *
-- ************
@DIGIT = 0-9
@DOT   = \.
@INT   = @DIGIT+(@DOT(@DIGIT+))?

-- ***************
-- *             *
-- * identifiers *
-- *             *
-- ***************
@LETTER1 = [^\']
@LETTER1_OR_DIGIT = @LETTER1 | @DIGIT
@LETTER2 = [^\"]
@LETTER2_OR_DIGIT = @LETTER2 | @DIGIT
@SQUOTE = \'
@DQUOTE = \"
@BYTES = [b]
@ID1 = (@BYTES?)(@SQUOTE)(@LETTER1_OR_DIGIT*)(@SQUOTE)
@ID2 = (@BYTES?)(@DQUOTE)(@LETTER2_OR_DIGIT+)(@DQUOTE)
@ID = @ID1 | @ID2

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

@KW_ID              { lex' AlexRawToken_KWID            }
@KW_CTX             { lex' AlexRawToken_CTX             }
@KW_OR              { lex' AlexRawToken_OR              }
@KW_OR2             { lex' AlexRawToken_OR2             }
@KW_BITXOR          { lex' AlexRawToken_BITXOR          }
@KW_AND             { lex' AlexRawToken_AND             }
@KW_AND2            { lex' AlexRawToken_AND2            }
@KW_LSHIFT          { lex' AlexRawToken_LSHIFT          }
@KW_RSHIFT          { lex' AlexRawToken_RSHIFT          }
@KW_EQ              { lex' AlexRawToken_EQ              }
@KW_GT              { lex' AlexRawToken_GT              }
@KW_GE              { lex' AlexRawToken_GE              }
@KW_LE              { lex' AlexRawToken_LE              }
@KW_LT              { lex' AlexRawToken_LT              }
@KW_IN              { lex' AlexRawToken_IN              }
@KW_IS              { lex' AlexRawToken_IS              }
@KW_ISNOT           { lex' AlexRawToken_ISNOT           }
@KW_NOT             { lex' AlexRawToken_NOT             }
@KW_NOTEQ           { lex' AlexRawToken_NOTEQ           }
@KW_NOTIN           { lex' AlexRawToken_NOTIN           }
@KW_ADD             { lex' AlexRawToken_ADD             }
@KW_POW             { lex' AlexRawToken_POW             }
@KW_FLOOR_DIV       { lex' AlexRawToken_FLOOR_DIV       }
@KW_MOD             { lex' AlexRawToken_MOD             }
@KW_DIV             { lex' AlexRawToken_DIV             }
@KW_SUB             { lex' AlexRawToken_SUB             }
@KW_USUB            { lex' AlexRawToken_USUB            }
@KW_INVERT          { lex' AlexRawToken_INVERT          }
@KW_MULT            { lex' AlexRawToken_MULT            }
@KW_ARG             { lex' AlexRawToken_ARG             }
@KW_VARARG          { lex' AlexRawToken_VARARG          }
@KW_NULL            { lex' AlexRawToken_NULL            }
@KW_KIND            { lex' AlexRawToken_KIND            }
@KW_TEST            { lex' AlexRawToken_TEST            }
@KW_COL             { lex' AlexRawToken_COL             }
@KW_ECOL            { lex' AlexRawToken_ECOL            }
@KW_LOAD            { lex' AlexRawToken_LOAD            }
@KW_IS_ASYNC        { lex' AlexRawToken_IS_ASYNC        }
@KW_STORE           { lex' AlexRawToken_STORE           }
@KW_LINENO          { lex' AlexRawToken_LINE            }
@KW_TARGET          { lex' AlexRawToken_TARGET          }
@KW_TARGETS         { lex' AlexRawToken_TARGETS         }
@KW_COMPREHENSION   { lex' AlexRawToken_COMPREHENSION   }
@KW_GENERATORS      { lex' AlexRawToken_GENERATORS      }
@KW_DEFAULTS        { lex' AlexRawToken_DEFAULTS        }
@KW_KWARG           { lex' AlexRawToken_KWARG           }
@KW_KW_DEFAULTS     { lex' AlexRawToken_KW_DEFAULTS     }
@KW_ELINENO         { lex' AlexRawToken_ELINE           }
@KW_TRUE            { lex' AlexRawToken_TRUE            }
@KW_FUNC            { lex' AlexRawToken_FUNC            }
@KW_ARGS            { lex' AlexRawToken_ARGS            }
@KW_ATTR            { lex' AlexRawToken_ATTR            }
@KW_ATTR2           { lex' AlexRawToken_ATTR2           }
@KW_STARRED         { lex' AlexRawToken_STARRED         }
@KW_SUBSCRIPT       { lex' AlexRawToken_SUBSCRIPT       }
@KW_SLICE           { lex' AlexRawToken_SLICE           }
@KW_ELLIPSIS        { lex' AlexRawToken_ELLIPSIS        }
@KW_LOWER           { lex' AlexRawToken_LOWER           }
@KW_STEP            { lex' AlexRawToken_STEP            }
@KW_UPPER           { lex' AlexRawToken_UPPER           }
@KW_EXPR_SLICE      { lex' AlexRawToken_EXPR_SLICE      }
@KW_ARGS2           { lex' AlexRawToken_ARGS2           }
@KW_ARGS3           { lex' AlexRawToken_ARGS3           }
@KW_ARGS4           { lex' AlexRawToken_ARGS4           }
@KW_EXPR            { lex' AlexRawToken_EXPR            }
@KW_NAME            { lex' AlexRawToken_NAME            }
@KW_TYPE            { lex' AlexRawToken_TYPE            }
@KW_LEFT            { lex' AlexRawToken_LEFT            }
@KW_EXC             { lex' AlexRawToken_EXC             }
@KW_ITER            { lex' AlexRawToken_ITER            }
@KW_WITH_VARS       { lex' AlexRawToken_WITH_VARS       }
@KW_FOR             { lex' AlexRawToken_FOR             }
@KW_DICT            { lex' AlexRawToken_DICT            }
@KW_KEYS            { lex' AlexRawToken_KEYS            }
@KW_RIGHT           { lex' AlexRawToken_RIGHT           }
@KW_COND            { lex' AlexRawToken_COND            }
@KW_BODY            { lex' AlexRawToken_BODY            }
@KW_NONE            { lex' AlexRawToken_NONE            }
@KW_HANDLERS        { lex' AlexRawToken_HANDLERS        }
@KW_EXCEPT_HANDLER  { lex' AlexRawToken_EXCEPT_HANDLER  }
@KW_BODY2           { lex' AlexRawToken_BODY2           }
@KW_LEVEL           { lex' AlexRawToken_LEVEL           }
@KW_NAME            { lex' AlexRawToken_NAME            }
@KW_CALL            { lex' AlexRawToken_CALL            }
@KW_NAME2           { lex' AlexRawToken_NAME2           }
@KW_ASNAME          { lex' AlexRawToken_ASNAME          }
@KW_NAMES           { lex' AlexRawToken_NAMES           }
@KW_ALIAS           { lex' AlexRawToken_ALIAS           }
@KW_ORELSE          { lex' AlexRawToken_ORELSE          }
@KW_KEYWORD         { lex' AlexRawToken_KEYWORD         }
@KW_KEYWORDS        { lex' AlexRawToken_KEYWORDS        }
@KW_IMPORT          { lex' AlexRawToken_IMPORT          }
@KW_CONVERSION      { lex' AlexRawToken_CONVERSION      }
@KW_FSTRING         { lex' AlexRawToken_FSTRING         }
@KW_IMPORTF         { lex' AlexRawToken_IMPORTF         }
@KW_FORMATTED_VAL   { lex' AlexRawToken_FORMATTED_VAL   }
@KW_FORMAT_SPEC     { lex' AlexRawToken_FORMAT_SPEC     }
@KW_ASSIGN          { lex' AlexRawToken_ASSIGN          }
@KW_AWAIT           { lex' AlexRawToken_AWAIT           }
@KW_NAMED_EXPR      { lex' AlexRawToken_NAMED_EXPR      }
@KW_ASSERT          { lex' AlexRawToken_ASSERT          }
@KW_LAMBDA          { lex' AlexRawToken_LAMBDA          }
@KW_SIMPLE          { lex' AlexRawToken_SIMPLE          }
@KW_ASSIGN2         { lex' AlexRawToken_ASSIGN2         }
@KW_ASSIGN3         { lex' AlexRawToken_ASSIGN3         }
@KW_TYPE_ALIAS      { lex' AlexRawToken_TYPE_ALIAS      }
@KW_TYPE_VAR       { lex' AlexRawToken_TYPE_VAR       }
@KW_MATCH          { lex' AlexRawToken_MATCH           }
@KW_MATCH_CASE     { lex' AlexRawToken_MATCH_CASE      }
@KW_MATCH_VALUE    { lex' AlexRawToken_MATCH_VALUE     }
@KW_MATCH_SINGLETON{ lex' AlexRawToken_MATCH_SINGLETON }
@KW_MATCH_SEQUENCE { lex' AlexRawToken_MATCH_SEQUENCE  }
@KW_MATCH_MAPPING  { lex' AlexRawToken_MATCH_MAPPING   }
@KW_MATCH_CLASS    { lex' AlexRawToken_MATCH_CLASS     }
@KW_MATCH_STAR     { lex' AlexRawToken_MATCH_STAR      }
@KW_MATCH_AS       { lex' AlexRawToken_MATCH_AS        }
@KW_MATCH_OR       { lex' AlexRawToken_MATCH_OR        }
@KW_SUBJECT        { lex' AlexRawToken_SUBJECT         }
@KW_CASES          { lex' AlexRawToken_CASES           }
@KW_PATTERN        { lex' AlexRawToken_PATTERN         }
@KW_PATTERNS       { lex' AlexRawToken_PATTERNS        }
@KW_GUARD          { lex' AlexRawToken_GUARD           }
@KW_CLS            { lex' AlexRawToken_CLS             }
@KW_KWD_ATTRS      { lex' AlexRawToken_KWD_ATTRS       }
@KW_KWD_PATTERNS   { lex' AlexRawToken_KWD_PATTERNS    }
@KW_REST           { lex' AlexRawToken_REST            }
@KW_ANNOTATION      { lex' AlexRawToken_ANNOTATION      }
@KW_MODULE          { lex' AlexRawToken_MODULE          }
@KW_MODULE2         { lex' AlexRawToken_MODULE2         }
@KW_FALSE           { lex' AlexRawToken_FALSE           }
@KW_LIST            { lex' AlexRawToken_LIST            }
@KW_SET             { lex' AlexRawToken_SET             }
@KW_SET_COMP        { lex' AlexRawToken_SET_COMP        }
@KW_LIST_COMP       { lex' AlexRawToken_LIST_COMP       }
@KW_DICT_COMP       { lex' AlexRawToken_DICT_COMP       }
@KW_GENERATOR_EXP   { lex' AlexRawToken_GENERATOR_EXP   }
@KW_TUPLE           { lex' AlexRawToken_TUPLE           }
@KW_ELT             { lex' AlexRawToken_ELT             }
@KW_ELTS            { lex' AlexRawToken_ELTS            }
@KW_ITEMS           { lex' AlexRawToken_ITEMS           }
@KW_WITH            { lex' AlexRawToken_WITH            }
@KW_WITH2           { lex' AlexRawToken_WITH2           }
@KW_CTX_MANAGER     { lex' AlexRawToken_CTX_MANAGER     }
@KW_KEY             { lex' AlexRawToken_KEY             }
@KW_VALUE           { lex' AlexRawToken_VALUE           }
@KW_VALUES          { lex' AlexRawToken_VALUES          }
@KW_ARRAY           { lex' AlexRawToken_ARRAY           }
@KW_PARAM           { lex' AlexRawToken_PARAM           }
@KW_COMPARE         { lex' AlexRawToken_COMPARE         }
@KW_DECORATORS      { lex' AlexRawToken_DECORATORS      }
@KW_TYPE_PARAMS     { lex' AlexRawToken_TYPE_PARAMS     }
@KW_TYPE_IGNORES    { lex' AlexRawToken_TYPE_IGNORES    }
@KW_COMPARE3        { lex' AlexRawToken_COMPARE3        }
@KW_COMPARE2        { lex' AlexRawToken_COMPARE2        }
@KW_OPERATOR        { lex' AlexRawToken_OPERATOR        }
@KW_OPERATOR2       { lex' AlexRawToken_OPERATOR2       }
@KW_OPERAND         { lex' AlexRawToken_OPERAND         }
@KW_STMT_IF         { lex' AlexRawToken_STMT_IF         }
@KW_STMT_IFS        { lex' AlexRawToken_STMT_IFS        }
@KW_STMT_WHILE      { lex' AlexRawToken_STMT_WHILE      }
@KW_EXPR_IF         { lex' AlexRawToken_EXPR_IF         }
@KW_STMT_EXPR       { lex' AlexRawToken_STMT_EXPR       }
@KW_EXPR_CONST      { lex' AlexRawToken_EXPR_CONST      }
@KW_EXPR_UNOP       { lex' AlexRawToken_EXPR_UNOP       }
@KW_EXPR_BINOP      { lex' AlexRawToken_EXPR_BINOP      }
@msg {lex' AlexRawToken_msg}
-- last keywords second part

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

@KW_STMT_RETURN    { lex' AlexRawToken_STMT_RETURN    }
@KW_STMT_RETURN2   { lex' AlexRawToken_STMT_RETURN2   }
@KW_STMT_CONTINUE  { lex' AlexRawToken_STMT_CONTINUE  }
@KW_STMT_BREAK     { lex' AlexRawToken_STMT_BREAK     }
@KW_STMT_PASS      { lex' AlexRawToken_STMT_PASS      }
@KW_STMT_YIELD     { lex' AlexRawToken_STMT_YIELD     }
@KW_STMT_YIELDFROM { lex' AlexRawToken_STMT_YIELDFROM }
@KW_STMT_DELETE    { lex' AlexRawToken_STMT_DELETE    }
@KW_STMT_GLOBAL    { lex' AlexRawToken_STMT_GLOBAL    }
@KW_STMT_DEL       { lex' AlexRawToken_STMT_DEL       }
@KW_STMT_RAISE     { lex' AlexRawToken_STMT_RAISE     }
@KW_STMT_CAUSE     { lex' AlexRawToken_STMT_CAUSE     }
@KW_STMT_TRY       { lex' AlexRawToken_STMT_TRY       }
@KW_STMT_FUNCTION  { lex' AlexRawToken_STMT_FUNCTION  }
@KW_STMT_CLASS     { lex' AlexRawToken_STMT_CLASS     }
@KW_SUPERS         { lex' AlexRawToken_SUPERS         }

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

@KW_OP_LT       { lex' AlexRawToken_OP_LT       }
@KW_OP_EQ       { lex' AlexRawToken_OP_EQ       }
@KW_OP_ASSIGN   { lex' AlexRawToken_OP_ASSIGN   }
@KW_OP_TIMES    { lex' AlexRawToken_OP_TIMES    }

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
.          { lexicalError                         }

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
-- > must also be defined in the user’s program
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

setAdditionalRepoInfo :: Common.AdditionalRepoInfo -> Alex ()
setAdditionalRepoInfo info = do
  state <- alexGetUserState
  alexSetUserState (AlexUserState { filepath = filepath state, additional_repo_info = info })

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
     | AlexRawToken_CTX             -- ^ Reserved Keyword
     | AlexRawToken_OR              -- ^ Reserved Keyword
     | AlexRawToken_OR2             -- ^ Reserved Keyword
     | AlexRawToken_BITXOR          -- ^ Reserved Keyword
     | AlexRawToken_AND             -- ^ Reserved Keyword
     | AlexRawToken_AND2            -- ^ Reserved Keyword
     | AlexRawToken_LSHIFT          -- ^ Reserved Keyword
     | AlexRawToken_RSHIFT          -- ^ Reserved Keyword
     | AlexRawToken_EQ              -- ^ Reserved Keyword
     | AlexRawToken_GT              -- ^ Reserved Keyword
     | AlexRawToken_GE              -- ^ Reserved Keyword
     | AlexRawToken_LE              -- ^ Reserved Keyword
     | AlexRawToken_LT              -- ^ Reserved Keyword
     | AlexRawToken_IN              -- ^ Reserved Keyword
     | AlexRawToken_IS              -- ^ Reserved Keyword
     | AlexRawToken_ISNOT           -- ^ Reserved Keyword
     | AlexRawToken_NOT             -- ^ Reserved Keyword
     | AlexRawToken_NOTEQ           -- ^ Reserved Keyword
     | AlexRawToken_NOTIN           -- ^ Reserved Keyword
     | AlexRawToken_ADD             -- ^ Reserved Keyword
     | AlexRawToken_POW             -- ^ Reserved Keyword
     | AlexRawToken_FLOOR_DIV       -- ^ Reserved Keyword
     | AlexRawToken_MOD             -- ^ Reserved Keyword
     | AlexRawToken_DIV             -- ^ Reserved Keyword
     | AlexRawToken_SUB             -- ^ Reserved Keyword
     | AlexRawToken_USUB            -- ^ Reserved Keyword
     | AlexRawToken_INVERT          -- ^ Reserved Keyword
     | AlexRawToken_MULT            -- ^ Reserved Keyword
     | AlexRawToken_END             -- ^ Reserved Keyword
     | AlexRawToken_RAW             -- ^ Reserved Keyword
     | AlexRawToken_LOC             -- ^ Reserved Keyword
     | AlexRawToken_ARG             -- ^ Reserved Keyword
     | AlexRawToken_VARARG          -- ^ Reserved Keyword
     | AlexRawToken_TEST            -- ^ Reserved Keyword
     | AlexRawToken_NULL            -- ^ Reserved Keyword
     | AlexRawToken_TAIL            -- ^ Reserved Keyword
     | AlexRawToken_COL             -- ^ Reserved Keyword
     | AlexRawToken_ECOL            -- ^ Reserved Keyword
     | AlexRawToken_LINE            -- ^ Reserved Keyword
     | AlexRawToken_LOAD            -- ^ Reserved Keyword
     | AlexRawToken_STORE           -- ^ Reserved Keyword
     | AlexRawToken_TARGET          -- ^ Reserved Keyword
     | AlexRawToken_TARGETS         -- ^ Reserved Keyword
     | AlexRawToken_DEFAULTS        -- ^ Reserved Keyword
     | AlexRawToken_KWARG           -- ^ Reserved Keyword
     | AlexRawToken_COMPREHENSION   -- ^ Reserved Keyword
     | AlexRawToken_GENERATORS      -- ^ Reserved Keyword
     | AlexRawToken_KW_DEFAULTS     -- ^ Reserved Keyword
     | AlexRawToken_ELINE           -- ^ Reserved Keyword
     | AlexRawToken_TRUE            -- ^ Reserved Keyword
     | AlexRawToken_FUNC            -- ^ Reserved Keyword
     | AlexRawToken_ATTR            -- ^ Reserved Keyword
     | AlexRawToken_ATTR2           -- ^ Reserved Keyword
     | AlexRawToken_STARRED         -- ^ Reserved Keyword
     | AlexRawToken_SUBSCRIPT       -- ^ Reserved Keyword
     | AlexRawToken_SLICE           -- ^ Reserved Keyword
     | AlexRawToken_ELLIPSIS        -- ^ Reserved Keyword
     | AlexRawToken_LOWER           -- ^ Reserved Keyword
     | AlexRawToken_STEP            -- ^ Reserved Keyword
     | AlexRawToken_UPPER           -- ^ Reserved Keyword
     | AlexRawToken_EXPR_SLICE      -- ^ Reserved Keyword
     | AlexRawToken_ARGS            -- ^ Reserved Keyword
     | AlexRawToken_ARGS2           -- ^ Reserved Keyword
     | AlexRawToken_ARGS3           -- ^ Reserved Keyword
     | AlexRawToken_ARGS4           -- ^ Reserved Keyword
     | AlexRawToken_EXPR            -- ^ Reserved Keyword
     | AlexRawToken_MAME            -- ^ Reserved Keyword
     | AlexRawToken_TYPE            -- ^ Reserved Keyword
     | AlexRawToken_LEFT            -- ^ Reserved Keyword
     | AlexRawToken_EXC             -- ^ Reserved Keyword
     | AlexRawToken_ITER            -- ^ Reserved Keyword
     | AlexRawToken_WITH_VARS       -- ^ Reserved Keyword
     | AlexRawToken_FOR             -- ^ Reserved Keyword
     | AlexRawToken_DICT            -- ^ Reserved Keyword
     | AlexRawToken_KEYS            -- ^ Reserved Keyword
     | AlexRawToken_RIGHT           -- ^ Reserved Keyword
     | AlexRawToken_LOOP            -- ^ Reserved Keyword
     | AlexRawToken_INIT            -- ^ Reserved Keyword
     | AlexRawToken_COND            -- ^ Reserved Keyword
     | AlexRawToken_BODY            -- ^ Reserved Keyword
     | AlexRawToken_NONE            -- ^ Reserved Keyword
     | AlexRawToken_HANDLERS        -- ^ Reserved Keyword
     | AlexRawToken_EXCEPT_HANDLER  -- ^ Reserved Keyword
     | AlexRawToken_BODY2           -- ^ Reserved Keyword
     | AlexRawToken_LEVEL           -- ^ Reserved Keyword
     | AlexRawToken_NAME            -- ^ Reserved Keyword
     | AlexRawToken_NAME2           -- ^ Reserved Keyword
     | AlexRawToken_ASNAME          -- ^ Reserved Keyword
     | AlexRawToken_NAMES           -- ^ Reserved Keyword
     | AlexRawToken_ALIAS           -- ^ Reserved Keyword
     | AlexRawToken_ORELSE          -- ^ Reserved Keyword
     | AlexRawToken_KEYWORD         -- ^ Reserved Keyword
     | AlexRawToken_KEYWORDS        -- ^ Reserved Keyword
     | AlexRawToken_IMPORT          -- ^ Reserved Keyword
     | AlexRawToken_FSTRING         -- ^ Reserved Keyword
     | AlexRawToken_IMPORTF         -- ^ Reserved Keyword
     | AlexRawToken_CONVERSION      -- ^ Reserved Keyword
     | AlexRawToken_FORMAT_SPEC     -- ^ Reserved Keyword
     | AlexRawToken_FORMATTED_VAL   -- ^ Reserved Keyword
     | AlexRawToken_ASSIGN          -- ^ Reserved Keyword
     | AlexRawToken_AWAIT           -- ^ Reserved Keyword
     | AlexRawToken_NAMED_EXPR      -- ^ Reserved Keyword
     | AlexRawToken_ASSERT          -- ^ Reserved Keyword
     | AlexRawToken_LAMBDA          -- ^ Reserved Keyword
     | AlexRawToken_SIMPLE          -- ^ Reserved Keyword
     | AlexRawToken_ASSIGN2         -- ^ Reserved Keyword
     | AlexRawToken_ASSIGN3         -- ^ Reserved Keyword
     | AlexRawToken_TYPE_ALIAS      -- ^ Reserved Keyword
     | AlexRawToken_TYPE_VAR        -- ^ Reserved Keyword
     | AlexRawToken_MATCH          -- ^ Reserved Keyword
     | AlexRawToken_MATCH_CASE     -- ^ Reserved Keyword
     | AlexRawToken_MATCH_VALUE    -- ^ Reserved Keyword
     | AlexRawToken_MATCH_SINGLETON-- ^ Reserved Keyword
     | AlexRawToken_MATCH_SEQUENCE -- ^ Reserved Keyword
     | AlexRawToken_MATCH_MAPPING  -- ^ Reserved Keyword
     | AlexRawToken_MATCH_CLASS    -- ^ Reserved Keyword
     | AlexRawToken_MATCH_STAR     -- ^ Reserved Keyword
     | AlexRawToken_MATCH_AS       -- ^ Reserved Keyword
     | AlexRawToken_MATCH_OR       -- ^ Reserved Keyword
     | AlexRawToken_SUBJECT        -- ^ Reserved Keyword
     | AlexRawToken_CASES          -- ^ Reserved Keyword
     | AlexRawToken_PATTERN        -- ^ Reserved Keyword
     | AlexRawToken_PATTERNS       -- ^ Reserved Keyword
     | AlexRawToken_GUARD          -- ^ Reserved Keyword
     | AlexRawToken_CLS            -- ^ Reserved Keyword
     | AlexRawToken_KWD_ATTRS      -- ^ Reserved Keyword
     | AlexRawToken_KWD_PATTERNS   -- ^ Reserved Keyword
     | AlexRawToken_REST           -- ^ Reserved Keyword
     | AlexRawToken_ANNOTATION      -- ^ Reserved Keyword
     | AlexRawToken_MODULE          -- ^ Reserved Keyword
     | AlexRawToken_MODULE2         -- ^ Reserved Keyword
     | AlexRawToken_START           -- ^ Reserved Keyword
     | AlexRawToken_COOKED          -- ^ Reserved Keyword
     | AlexRawToken_FALSE           -- ^ Reserved Keyword
     | AlexRawToken_LIST            -- ^ Reserved Keyword
     | AlexRawToken_SET             -- ^ Reserved Keyword
     | AlexRawToken_SET_COMP        -- ^ Reserved Keyword
     | AlexRawToken_LIST_COMP       -- ^ Reserved Keyword
     | AlexRawToken_DICT_COMP       -- ^ Reserved Keyword
     | AlexRawToken_GENERATOR_EXP   -- ^ Reserved Keyword
     | AlexRawToken_TUPLE           -- ^ Reserved Keyword
     | AlexRawToken_ELT             -- ^ Reserved Keyword
     | AlexRawToken_ELTS            -- ^ Reserved Keyword
     | AlexRawToken_ITEMS           -- ^ Reserved Keyword
     | AlexRawToken_WITH            -- ^ Reserved Keyword
     | AlexRawToken_WITH2           -- ^ Reserved Keyword
     | AlexRawToken_CTX_MANAGER     -- ^ Reserved Keyword
     | AlexRawToken_EXPRS           -- ^ Reserved Keyword
     | AlexRawToken_KEY             -- ^ Reserved Keyword
     | AlexRawToken_KIND            -- ^ Reserved Keyword
     | AlexRawToken_VALUE           -- ^ Reserved Keyword
     | AlexRawToken_VALUES          -- ^ Reserved Keyword
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
     | AlexRawToken_DECORATORS      -- ^ Reserved Keyword
     | AlexRawToken_TYPE_PARAMS     -- ^ Reserved Keyword
     | AlexRawToken_TYPE_IGNORES    -- ^ Reserved Keyword
     | AlexRawToken_COMPARE         -- ^ Reserved Keyword
     | AlexRawToken_COMPARE3        -- ^ Reserved Keyword
     | AlexRawToken_COMPARE2        -- ^ Reserved Keyword
     | AlexRawToken_OPERATOR        -- ^ Reserved Keyword
     | AlexRawToken_OPERATOR2       -- ^ Reserved Keyword
     | AlexRawToken_OPERAND         -- ^ Reserved Keyword
     | AlexRawToken_ALTERNATE       -- ^ Reserved Keyword
     | AlexRawToken_CONSEQUENT      -- ^ Reserved Keyword
     | AlexRawToken_ARGUMENT        -- ^ Reserved Keyword
     | AlexRawToken_ARGUMENTS       -- ^ Reserved Keyword
     | AlexRawToken_CALL            -- ^ Reserved Keyword
     | AlexRawToken_CALLEE          -- ^ Reserved Keyword
     | AlexRawToken_ASYNC           -- ^ Reserved Keyword
     | AlexRawToken_IS_ASYNC        -- ^ Reserved Keyword
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
     | AlexRawToken_STMT_CLASS      -- ^ Reserved Keyword
     | AlexRawToken_SUPERS          -- ^ Reserved Keyword
     | AlexRawToken_FUNCTION_DEC    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CONST      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_UNOP       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_PLUS -- ^ Reserved Keyword
     | AlexRawToken_VAR_DECLARATION -- ^ Reserved Keyword
     | AlexRawToken_VAR_DECLARATOR  -- ^ Reserved Keyword
     | AlexRawToken_msg
     -- last keywords third part

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
     | AlexRawToken_OP_EQ           -- ^ Reserved Keyword
     | AlexRawToken_OP_ASSIGN       -- ^ Reserved Keyword
     | AlexRawToken_OP_TIMES        -- ^ Reserved Keyword
     | AlexRawToken_OP_PLUSPLUS     -- ^ Reserved Keyword

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

     -- **************
     -- *            *
     -- * statements *
     -- *            *
     -- **************

     | AlexRawToken_STMT_IF         -- ^ Reserved Keyword
     | AlexRawToken_STMT_IFS        -- ^ Reserved Keyword
     | AlexRawToken_STMT_WHILE      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_IF         -- ^ Reserved Keyword
     | AlexRawToken_STMT_FOR        -- ^ Reserved Keyword
     | AlexRawToken_STMT_BLOCK      -- ^ Reserved Keyword
     | AlexRawToken_STMT_YIELD      -- ^ Reserved Keyword
     | AlexRawToken_STMT_YIELDFROM  -- ^ Reserved Keyword
     | AlexRawToken_STMT_DEL        -- ^ Reserved Keyword
     | AlexRawToken_STMT_DELETE     -- ^ Reserved Keyword
     | AlexRawToken_STMT_GLOBAL     -- ^ Reserved Keyword
     | AlexRawToken_STMT_RAISE      -- ^ Reserved Keyword
     | AlexRawToken_STMT_CAUSE      -- ^ Reserved Keyword
     | AlexRawToken_STMT_RETURN     -- ^ Reserved Keyword
     | AlexRawToken_STMT_RETURN2    -- ^ Reserved Keyword
     | AlexRawToken_STMT_CONTINUE   -- ^ Reserved Keyword
     | AlexRawToken_STMT_BREAK      -- ^ Reserved Keyword
     | AlexRawToken_STMT_PASS       -- ^ Reserved Keyword
     | AlexRawToken_STMT_TRY        -- ^ Reserved Keyword
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

-- ***********
-- *         *
-- * dirname *
-- *         *
-- ***********
dirname :: FilePath -> String -> FilePath
dirname path src = takeDirectory path </> src

-- ***************
-- *             *
-- * addPySuffix *
-- *             *
-- ***************
addPySuffix :: FilePath -> FilePath
addPySuffix f = f <.> "py"

-- *****************************
-- *                           *
-- * resolve 1st party imports *
-- *                           *
-- *****************************
mkFileCandidate :: FilePath -> String -> FilePath
mkFileCandidate path src = addPySuffix (dirname path src)

mkDirCandidate :: FilePath -> String -> FilePath
mkDirCandidate path src = dirname path src

mkDirInitCandidate :: FilePath -> String -> FilePath
mkDirInitCandidate path src = dirname path src </> "__init__.py"

-- *****************************
-- *                           *
-- * resolve 1st party imports *
-- *                           *
-- *****************************
resolveFirstParty :: FilePath -> String -> Common.AdditionalRepoInfo -> Ast.ImportSource
resolveFirstParty path src repoInfo = let
    fileCandidate = mkFileCandidate path src
    dirCandidate = mkDirCandidate path src
    dirInitCandidate = mkDirInitCandidate path src
    filenamesRepo = Common.filenames repoInfo
    in resolveFirstParty' fileCandidate dirCandidate dirInitCandidate filenamesRepo src

-- *****************************
-- *                           *
-- * resolve 1st party imports *
-- *                           *
-- *****************************
resolveFirstParty' :: FilePath -> FilePath -> FilePath -> [ String ] -> String -> Ast.ImportSource
resolveFirstParty' fileCandidate dirCandidate dirInitCandidate files src = let
    importedLocalFileExists = elem fileCandidate files
    importedLocalDirAsPyModuleExists = elem dirInitCandidate files
    in resolveFirstParty'' importedLocalFileExists importedLocalDirAsPyModuleExists fileCandidate dirCandidate src

-- *****************************
-- *                           *
-- * resolve 1st party imports *
-- *                           *
-- *****************************
resolveFirstParty'' :: Bool -> Bool -> FilePath -> FilePath -> String -> Ast.ImportSource
resolveFirstParty'' True _ fileCandidate _ _ = Ast.ImportLocal (Ast.ImportLocalFile fileCandidate)
resolveFirstParty'' False True _ dirCandidate _ = Ast.ImportLocal (Ast.ImportLocalDir dirCandidate)
resolveFirstParty'' False False _ _ src = Ast.ImportThirdParty (Ast.ImportThirdPartyContent src)

-- *****************************
-- *                           *
-- * resolve 3rd party imports *
-- *                           *
-- *****************************
resolveThirdParty :: String -> Ast.ImportSource
resolveThirdParty src = Ast.ImportThirdParty (Ast.ImportThirdPartyContent src)

-- ***********************************
-- *                                 *
-- * resolve 1st / 3rd party imports *
-- *                                 *
-- ***********************************
resolve :: FilePath -> String -> Bool -> Common.AdditionalRepoInfo -> Ast.ImportSource
resolve _ src False repoInfo = resolveAbsolute src repoInfo
resolve path src True repoInfo = resolveFirstParty path src repoInfo

-- | `from weblate.trans.util import x` ( level 0 ) is 1st party when
-- `weblate/trans/util.py` ( or `weblate/trans/util/__init__.py` ) is a
-- scanned file, like python finds it ( runner #131 )
resolveAbsolute :: String -> Common.AdditionalRepoInfo -> Ast.ImportSource
resolveAbsolute src repoInfo = let
    path = map dotToSlash src
    files = Common.filenames repoInfo
    in resolveFirstParty'' (elem (addPySuffix path) files) (elem (path </> "__init__.py") files) (addPySuffix path) path src

dotToSlash :: Char -> Char
dotToSlash '.' = '/'
dotToSlash c = c

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

-- *************************
-- *                       *
-- * getAdditionalRepoInfo *
-- *                       *
-- *************************
getAdditionalRepoInfo :: AlexTokenTag -> Common.AdditionalRepoInfo
getAdditionalRepoInfo = additionalRepoInfo

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> Common.AdditionalRepoInfo -> String -> Either String a
runAlex' a fp additionalInfo input = runAlex input (setFilePath fp >> setAdditionalRepoInfo additionalInfo >> a)
}

