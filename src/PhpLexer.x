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

module PhpLexer

where

import Prelude hiding (lex)
import Control.Monad ( liftM )
import Data.List
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

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************
@COLON  = ":"
@SLASH  = "\"
@HYPHEN = \- 
@OR     = \|

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************
@KW_ARG             = "Arg"
@KW_VAR             = "var"
@KW_ARGS            = "args"
@KW_NAME            = "name"
@KW_USES            = "uses"
@KW_EXPR            = "expr"
@KW_MAME            = "Name"
@KW_NULL            = "null"
@KW_ALIAS           = "alias"
@KW_TYPE            = "type"
@KW_LEFT            = "left"
@KW_LOOP            = "loop"
@KW_INIT            = "init"
@KW_COND            = "cond"
@KW_CONST           = "Const"
@KW_EXPRS           = "exprs"
@KW_VALUE           = "value"
@KW_RIGHT           = "right"
@KW_STMTS           = "stmts"
@KW_ARRAY           = "array"
@KW_PARAM           = "Param"
@KW_STMT_IF         = "Stmt_If"
@KW_STMT_ELSE       = "Stmt_Else"
@KW_STMT_ELIF       = "Stmt_ElseIf"
@KW_STMT_WHILE      = "Stmt_While"
@KW_STMT_DO         = "Stmt_Do"
@KW_STMT_CATCH      = "Stmt_Catch"
@KW_STMT_TRY_CATCH  = "Stmt_TryCatch"
@KW_USE_ITEM        = "UseItem"
@KW_STMT_FOR        = "Stmt_For"
@KW_STMT_NOP        = "Stmt_Nop"
@KW_STMT_NAMESPACE  = "Stmt_Namespace"
@KW_STMT_TRAIT      = "Stmt_Trait"
@KW_STMT_TRAITUSE   = "Stmt_TraitUse"
@KW_STMT_SWITCH     = "Stmt_Switch"
@KW_STMT_CASE       = "Stmt_Case"
@KW_STMT_FOREACH    = "Stmt_Foreach"
@KW_STMT_GLOBAL     = "Stmt_Global"
@KW_STMT_USE        = "Stmt_Use"
@KW_STMT_ECHO       = "Stmt_Echo"
@KW_STMT_UNSET      = "Stmt_Unset"
@KW_EXPR_VAR        = "Expr_Variable"
@KW_EXPR_INSTOF     = "Expr_Instanceof"
@KW_EXPR_NEW        = "Expr_New"
@KW_EXPR_EXIT       = "Expr_Exit"
@KW_EXPR_IMPORT     = "Expr_Include"
@KW_EXPR_TERNARY    = "Expr_Ternary"
@KW_EXPR_THROW      = "Expr_Throw"
@KW_EXPR_LAMBDA     = "Expr_Closure"
@KW_CLOSURE_USE     = "ClosureUse"
@KW_ERROR_SUPPRESS  = "Expr_ErrorSuppress"
@KW_FULLY_QUALIFIED = "Name_FullyQualified"
@KW_EXPR_CAST       = "Expr_Cast_Double"
@KW_EXPR_CAST2      = "Expr_Cast_Int"
@KW_EXPR_CAST3      = "Expr_Cast_String"
@KW_EXPR_CAST4      = "Expr_Cast_Bool"
@KW_EXPR_CAST5      = "Expr_Cast_Object"
@KW_EXPR_ASSIGN     = "Expr_Assign"
@KW_EXPR_ASSIGN2    = "Expr_AssignOp_Plus"
@KW_EXPR_ASSIGN5    = "Expr_AssignOp_Minus"
@KW_EXPR_ASSIGN4    = "Expr_AssignOp_Div"
@KW_EXPR_ASSIGN3    = "Expr_AssignOp_Concat"
@KW_EXPR_ISSET      = "Expr_Isset"
@KW_EXPR_LIST       = "Expr_List"
@KW_EXPR_ARRAY      = "Expr_Array"
@KW_EXPR_EMPTY      = "Expr_Empty"
@KW_EXPR_ARRAY3     = "ArrayItem"
@KW_EXPR_ARRAY2     = "Expr_ArrayDimFetch"
@KW_EXPR_FETCH      = "Expr_ClassConstFetch"
@KW_EXPR_CALL       = "Expr_FuncCall"
@KW_EXPR_SCALL      = "Expr_StaticCall"
@KW_EXPR_MCALL      = "Expr_MethodCall"
@KW_STMT_EXPR       = "Stmt_Expression"
@KW_SCALAR_INT      = "Scalar_Int"
@KW_SCALAR_FLOAT    = "Scalar_Float"
@KW_SCALAR_FSTRING  = "Scalar_InterpolatedString"
@KW_SCALAR_FILE     = "Scalar_MagicConst_File"
@KW_SCALAR_CLASS    = "Scalar_MagicConst_Class"
@KW_SCALAR_DIR      = "Scalar_MagicConst_Dir"
@KW_IDENTIFIER      = "Identifier"
@KW_RETURN_TYPE     = "returnType"
@KW_STMT_RETURN     = "Stmt_Return"
@KW_STMT_CLASS      = "Stmt_Class"
@KW_STMT_CLASS_CONST = "Stmt_ClassConst"
@KW_STMT_INTERFACE  = "Stmt_Interface"
@KW_STMT_CONT       = "Stmt_Continue"
@KW_STMT_BREAK      = "Stmt_Break"
@KW_STMT_STATIC     = "Stmt_Static"
@KW_STATIC_VAR      = "StaticVar"
@KW_STMT_PROPERTY   = "Stmt_Property"
@KW_STMT_CLASSMETH  = "Stmt_ClassMethod"
@KW_STMT_FUNCTION   = "Stmt_Function"
@KW_EXPR_CONST_GET  = "Expr_ConstFetch"
@KW_EXPR_PROP_GET   = "Expr_PropertyFetch"
@KW_EXPR_PROP_GET2  = "Expr_StaticPropertyFetch"
@KW_EXPR_BINOP_LT   = "Expr_BinaryOp_Smaller"
@KW_EXPR_BINOP_LEQ  = "Expr_BinaryOp_SmallerOrEqual"
@KW_EXPR_BINOP_CO   = "Expr_BinaryOp_Coalesce"
@KW_EXPR_BINOP_GT   = "Expr_BinaryOp_Greater"
@KW_EXPR_BINOP_GEQ  = "Expr_BinaryOp_GreaterOrEqual"
@KW_EXPR_BINOP_EQ   = "Expr_BinaryOp_Equal"
@KW_EXPR_BINOP_LOR  = "Expr_BinaryOp_LogicalOr"
@KW_EXPR_BINOP_NEQ  = "Expr_BinaryOp_NotEqual"
@KW_EXPR_BINOP_IS   = "Expr_BinaryOp_Identical"
@KW_EXPR_BINOP_ISNOT = "Expr_BinaryOp_NotIdentical"
@KW_EXPR_UNOP_NOT   = "Expr_BooleanNot"
@KW_EXPR_BINOP_MUL  = "Expr_BinaryOp_Mul"
@KW_EXPR_BINOP_DIV  = "Expr_BinaryOp_Div"
@KW_EXPR_BINOP_PLUS = "Expr_BinaryOp_Plus"
@KW_EXPR_BINOP_MINUS = "Expr_BinaryOp_Minus"
@KW_EXPR_UNOP_MINUS = "Expr_UnaryMinus"
@KW_EXPR_POST_INC   = "Expr_PostInc"
@KW_EXPR_PRINT      = "Expr_Print"
@KW_EXPR_POST_DEC   = "Expr_PostDec"
@KW_EXPR_BINOP_CONCAT = "Expr_BinaryOp_Concat"
@KW_EXPR_BINOP_OR   = "Expr_BinaryOp_BooleanOr"
@KW_EXPR_BINOP_OR2  = "Expr_BinaryOp_BitwiseOr"
@KW_EXPR_BINOP_AND  = "Expr_BinaryOp_BooleanAnd"
@Expr_NullsafePropertyFetch = Expr_NullsafePropertyFetch
@Expr_BinaryOp_LogicalAnd = Expr_BinaryOp_LogicalAnd
@attrGroups = attrGroups
@flags = flags
@extends = extends
@implements = implements
@false = false
@byRef = byRef
@params = params
@default = default
@variadic = variadic
@unpack = unpack
@key = key
@keyVar = keyVar
@valueVar = valueVar
@consts = consts
@static = static
@Expr_PreInc = Expr_PreInc
@Scalar_String = Scalar_String
@traits = traits
@adaptations = adaptations
@props = props
@VarLikeIdentifier = VarLikeIdentifier
@PropertyItem = PropertyItem
@Expr_BinaryOp_Pow = Expr_BinaryOp_Pow
@Expr_ArrowFunction = Expr_ArrowFunction
@parts = parts
@InterpolatedStringPart = InterpolatedStringPart
@true = true
@Expr_BinaryOp_BitwiseAnd = Expr_BinaryOp_BitwiseAnd
@Expr_BinaryOp_ShiftRight = Expr_BinaryOp_ShiftRight
@Expr_BinaryOp_ShiftLeft = Expr_BinaryOp_ShiftLeft
@Expr_AssignOp_BitwiseXor = Expr_AssignOp_BitwiseXor
@Expr_BinaryOp_BitwiseXor = Expr_BinaryOp_BitwiseXor
@Expr_AssignOp_ShiftRight = Expr_AssignOp_ShiftRight
@Expr_AssignOp_BitwiseAnd = Expr_AssignOp_BitwiseAnd
@Expr_AssignOp_BitwiseOr = Expr_AssignOp_BitwiseOr
@NullableType = NullableType
@MatchArm = MatchArm
@conds = conds
@body = body
@arms = arms
@Expr_Match = Expr_Match
@class = class
@Stmt_InlineHTML = Stmt_InlineHTML
@Expr_Cast_Array = Expr_Cast_Array
@Scalar_MagicConst_Function = Scalar_MagicConst_Function
@Stmt_GroupUse = Stmt_GroupUse
@prefix = prefix
@Scalar_MagicConst_Method = Scalar_MagicConst_Method
@Expr_PreDec = Expr_PreDec
@Expr_Clone = Expr_Clone
@Expr_BinaryOp_Spaceship = Expr_BinaryOp_Spaceship
@Expr_BinaryOp_Mod = Expr_BinaryOp_Mod
@Stmt_Block = Stmt_Block
@Expr_AssignOp_Mul = Expr_AssignOp_Mul
@Expr_Eval = Expr_Eval
@items = items
@dim = dim
@Stmt_Declare = Stmt_Declare
@DeclareItem = DeclareItem
@declares = declares
-- last keywords first part

-- ************
-- *          *
-- * integers *
-- *          *
-- ************
@DIGIT = 0-9
@INT   = @DIGIT+
@DOT   = \.
@FLOAT = (@DIGIT+)(@DOT)(@DIGIT+)

-- ***************
-- *             *
-- * identifiers *
-- *             *
-- ***************
@LETTER = [A-Za-z_\\]
@LETTER_OR_DIGIT = @LETTER | @DIGIT
@ID = @LETTER(@LETTER_OR_DIGIT*)

-- ***********
-- *         *
-- * strings *
-- *         *
-- ***********
@QUOTE = \"
--@STR = (@QUOTE)params(@QUOTE)
@STR = (@QUOTE)([^"]*)(@QUOTE)

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

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

@COLON     { lex' AlexRawToken_COLON  }
@SLASH     { lex' AlexRawToken_SLASH  }
@HYPHEN    { lex' AlexRawToken_HYPHEN }
@OR        { lex' AlexRawToken_OR     }

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@KW_ARG             { lex' AlexRawToken_ARG             }
@KW_VAR             { lex' AlexRawToken_VAR             }
@KW_ARGS            { lex' AlexRawToken_ARGS            }
@KW_NAME            { lex' AlexRawToken_NAME            }
@KW_USES            { lex' AlexRawToken_USES            }
@KW_EXPR            { lex' AlexRawToken_EXPR            }
@KW_MAME            { lex' AlexRawToken_MAME            }
@KW_NULL            { lex' AlexRawToken_NULL            }
@KW_ALIAS           { lex' AlexRawToken_ALIAS           }
@KW_TYPE            { lex' AlexRawToken_TYPE            }
@KW_LEFT            { lex' AlexRawToken_LEFT            }
@KW_LOOP            { lex' AlexRawToken_LOOP            }
@KW_INIT            { lex' AlexRawToken_INIT            }
@KW_COND            { lex' AlexRawToken_COND            }
@KW_CONST           { lex' AlexRawToken_CONST           }
@KW_EXPRS           { lex' AlexRawToken_EXPRS           }
@KW_VALUE           { lex' AlexRawToken_VALUE           }
@KW_RIGHT           { lex' AlexRawToken_RIGHT           }
@KW_STMTS           { lex' AlexRawToken_STMTS           }
@KW_ARRAY           { lex' AlexRawToken_ARRAY           }
@KW_PARAM           { lex' AlexRawToken_PARAM           }
@KW_STMT_IF         { lex' AlexRawToken_STMT_IF         }
@KW_STMT_ELSE       { lex' AlexRawToken_STMT_ELSE       }
@KW_STMT_CATCH      { lex' AlexRawToken_STMT_CATCH      }
@KW_STMT_DO         { lex' AlexRawToken_STMT_DO         }
@KW_STMT_WHILE      { lex' AlexRawToken_STMT_WHILE      }
@KW_STMT_ELIF       { lex' AlexRawToken_STMT_ELIF       }
@KW_STMT_TRY_CATCH  { lex' AlexRawToken_STMT_TRY_CATCH  }
@KW_USE_ITEM        { lex' AlexRawToken_USE_ITEM        }
@KW_STMT_FOR        { lex' AlexRawToken_STMT_FOR        }
@KW_STMT_NOP        { lex' AlexRawToken_STMT_NOP        }
@KW_STMT_NAMESPACE  { lex' AlexRawToken_STMT_NAMESPACE  }
@KW_STMT_TRAIT      { lex' AlexRawToken_STMT_TRAIT      }
@KW_STMT_TRAITUSE   { lex' AlexRawToken_STMT_TRAITUSE   }
@KW_STMT_SWITCH     { lex' AlexRawToken_STMT_SWITCH     }
@KW_STMT_CASE       { lex' AlexRawToken_STMT_CASE       }
@KW_STMT_FOREACH    { lex' AlexRawToken_STMT_FOREACH    }
@KW_STMT_GLOBAL     { lex' AlexRawToken_STMT_GLOBAL     }
@KW_STMT_USE        { lex' AlexRawToken_STMT_USE        }
@KW_STMT_ECHO       { lex' AlexRawToken_STMT_ECHO       }
@KW_STMT_UNSET      { lex' AlexRawToken_STMT_UNSET      }
@KW_EXPR_VAR        { lex' AlexRawToken_EXPR_VAR        }
@KW_EXPR_INSTOF     { lex' AlexRawToken_EXPR_INSTOF     }
@KW_EXPR_NEW        { lex' AlexRawToken_EXPR_NEW        }
@KW_EXPR_EXIT       { lex' AlexRawToken_EXPR_EXIT       }
@KW_EXPR_IMPORT     { lex' AlexRawToken_EXPR_IMPORT     }
@KW_EXPR_TERNARY    { lex' AlexRawToken_EXPR_TERNARY    }
@KW_EXPR_THROW      { lex' AlexRawToken_EXPR_THROW      }
@KW_EXPR_LAMBDA     { lex' AlexRawToken_EXPR_LAMBDA     }
@KW_CLOSURE_USE     { lex' AlexRawToken_CLOSURE_USE     }
@KW_ERROR_SUPPRESS  { lex' AlexRawToken_ERROR_SUPPRESS  }
@KW_FULLY_QUALIFIED { lex' AlexRawToken_FULLY_QUALIFIED }
@KW_EXPR_CAST       { lex' AlexRawToken_EXPR_CAST       }
@KW_EXPR_CAST5      { lex' AlexRawToken_EXPR_CAST5      }
@KW_EXPR_CAST4      { lex' AlexRawToken_EXPR_CAST4      }
@KW_EXPR_CAST3      { lex' AlexRawToken_EXPR_CAST3      }
@KW_EXPR_CAST2      { lex' AlexRawToken_EXPR_CAST2      }
@KW_EXPR_ASSIGN     { lex' AlexRawToken_EXPR_ASSIGN     }
@KW_EXPR_ASSIGN5    { lex' AlexRawToken_EXPR_ASSIGN5    }
@KW_EXPR_ASSIGN4    { lex' AlexRawToken_EXPR_ASSIGN4    }
@KW_EXPR_ASSIGN2    { lex' AlexRawToken_EXPR_ASSIGN2    }
@KW_EXPR_ASSIGN3    { lex' AlexRawToken_EXPR_ASSIGN3    }
@KW_EXPR_ISSET      { lex' AlexRawToken_EXPR_ISSET      }
@KW_EXPR_ARRAY      { lex' AlexRawToken_EXPR_ARRAY      }
@KW_EXPR_LIST       { lex' AlexRawToken_EXPR_LIST       }
@KW_EXPR_FETCH      { lex' AlexRawToken_EXPR_FETCH      }
@KW_EXPR_EMPTY      { lex' AlexRawToken_EXPR_EMPTY      }
@KW_EXPR_ARRAY2     { lex' AlexRawToken_EXPR_ARRAY2     }
@KW_EXPR_ARRAY3     { lex' AlexRawToken_EXPR_ARRAY3     }
@KW_EXPR_CALL       { lex' AlexRawToken_EXPR_CALL       }
@KW_EXPR_MCALL      { lex' AlexRawToken_EXPR_MCALL      }
@KW_EXPR_SCALL      { lex' AlexRawToken_EXPR_SCALL      }
@KW_STMT_EXPR       { lex' AlexRawToken_STMT_EXPR       }
@KW_SCALAR_INT      { lex' AlexRawToken_SCALAR_INT      }
@KW_SCALAR_FLOAT    { lex' AlexRawToken_SCALAR_FLOAT    }
@KW_SCALAR_FSTRING  { lex' AlexRawToken_SCALAR_FSTRING  }
@KW_SCALAR_FILE     { lex' AlexRawToken_SCALAR_FILE     }
@KW_SCALAR_CLASS    { lex' AlexRawToken_SCALAR_CLASS    }
@KW_SCALAR_DIR      { lex' AlexRawToken_SCALAR_DIR      }
@KW_IDENTIFIER      { lex' AlexRawToken_IDENTIFIER      }
@KW_RETURN_TYPE     { lex' AlexRawToken_RETURN_TYPE     }
@KW_STMT_RETURN     { lex' AlexRawToken_STMT_RETURN     }
@KW_STMT_CLASS      { lex' AlexRawToken_STMT_CLASS      }
@KW_STMT_CLASS_CONST { lex' AlexRawToken_STMT_CLASS_CONST }
@KW_STMT_INTERFACE  { lex' AlexRawToken_STMT_INTERFACE  }
@KW_STMT_CONT       { lex' AlexRawToken_STMT_CONT       }
@KW_STMT_BREAK      { lex' AlexRawToken_STMT_BREAK      }
@KW_STMT_STATIC     { lex' AlexRawToken_STMT_STATIC     }
@KW_STATIC_VAR      { lex' AlexRawToken_STATIC_VAR      }
@KW_STMT_PROPERTY   { lex' AlexRawToken_STMT_PROPERTY   }
@KW_STMT_CLASSMETH  { lex' AlexRawToken_STMT_CLASSMETH  }
@KW_STMT_FUNCTION   { lex' AlexRawToken_STMT_FUNCTION   }
@KW_EXPR_CONST_GET  { lex' AlexRawToken_EXPR_CONST_GET  }
@KW_EXPR_PROP_GET   { lex' AlexRawToken_EXPR_PROP_GET   }
@KW_EXPR_PROP_GET2  { lex' AlexRawToken_EXPR_PROP_GET2  }
@KW_EXPR_BINOP_LT   { lex' AlexRawToken_EXPR_BINOP_LT   }
@KW_EXPR_BINOP_LEQ  { lex' AlexRawToken_EXPR_BINOP_LEQ  }
@KW_EXPR_BINOP_CO   { lex' AlexRawToken_EXPR_BINOP_CO   }
@KW_EXPR_BINOP_GT   { lex' AlexRawToken_EXPR_BINOP_GT   }
@KW_EXPR_BINOP_GEQ  { lex' AlexRawToken_EXPR_BINOP_GEQ  }
@KW_EXPR_BINOP_EQ   { lex' AlexRawToken_EXPR_BINOP_EQ   }
@KW_EXPR_BINOP_NEQ  { lex' AlexRawToken_EXPR_BINOP_NEQ  }
@KW_EXPR_BINOP_IS   { lex' AlexRawToken_EXPR_BINOP_IS   }
@KW_EXPR_BINOP_ISNOT { lex' AlexRawToken_EXPR_BINOP_ISNOT }
@KW_EXPR_BINOP_MUL  { lex' AlexRawToken_EXPR_BINOP_MUL  }
@KW_EXPR_BINOP_DIV  { lex' AlexRawToken_EXPR_BINOP_DIV  }
@KW_EXPR_BINOP_PLUS { lex' AlexRawToken_EXPR_BINOP_PLUS }
@KW_EXPR_BINOP_MINUS { lex' AlexRawToken_EXPR_BINOP_MINUS }
@KW_EXPR_UNOP_MINUS { lex' AlexRawToken_EXPR_UNOP_MINUS }
@KW_EXPR_POST_INC   { lex' AlexRawToken_EXPR_POST_INC   }
@KW_EXPR_PRINT      { lex' AlexRawToken_EXPR_PRINT      }
@KW_EXPR_POST_DEC   { lex' AlexRawToken_EXPR_POST_DEC   }
@KW_EXPR_BINOP_CONCAT { lex' AlexRawToken_EXPR_BINOP_CONCAT }
@KW_EXPR_BINOP_OR   { lex' AlexRawToken_EXPR_BINOP_OR   }
@KW_EXPR_BINOP_OR2  { lex' AlexRawToken_EXPR_BINOP_OR2  }
@KW_EXPR_BINOP_LOR  { lex' AlexRawToken_EXPR_BINOP_LOR  }
@KW_EXPR_BINOP_AND  { lex' AlexRawToken_EXPR_BINOP_AND  }
@KW_EXPR_UNOP_NOT   { lex' AlexRawToken_EXPR_UNOP_NOT   }
@Expr_NullsafePropertyFetch {lex' AlexRawToken_Expr_NullsafePropertyFetch}
@Expr_BinaryOp_LogicalAnd {lex' AlexRawToken_Expr_BinaryOp_LogicalAnd}
@attrGroups {lex' AlexRawToken_attrGroups}
@flags {lex' AlexRawToken_flags}
@extends {lex' AlexRawToken_extends}
@implements {lex' AlexRawToken_implements}
@false {lex' AlexRawToken_false}
@byRef {lex' AlexRawToken_byRef}
@params {lex' AlexRawToken_params}
@default {lex' AlexRawToken_default}
@variadic {lex' AlexRawToken_variadic}
@unpack {lex' AlexRawToken_unpack}
@key {lex' AlexRawToken_key}
@keyVar {lex' AlexRawToken_keyVar}
@valueVar {lex' AlexRawToken_valueVar}
@consts {lex' AlexRawToken_consts}
@static {lex' AlexRawToken_static}
@Expr_PreInc {lex' AlexRawToken_Expr_PreInc}
@Scalar_String {lex' AlexRawToken_Scalar_String}
@traits {lex' AlexRawToken_traits}
@adaptations {lex' AlexRawToken_adaptations}
@props {lex' AlexRawToken_props}
@VarLikeIdentifier {lex' AlexRawToken_VarLikeIdentifier}
@PropertyItem {lex' AlexRawToken_PropertyItem}
@Expr_BinaryOp_Pow {lex' AlexRawToken_Expr_BinaryOp_Pow}
@Expr_ArrowFunction {lex' AlexRawToken_Expr_ArrowFunction}
@parts {lex' AlexRawToken_parts}
@InterpolatedStringPart {lex' AlexRawToken_InterpolatedStringPart}
@true {lex' AlexRawToken_true}
@Expr_BinaryOp_BitwiseAnd {lex' AlexRawToken_Expr_BinaryOp_BitwiseAnd}
@Expr_BinaryOp_ShiftRight {lex' AlexRawToken_Expr_BinaryOp_ShiftRight}
@Expr_BinaryOp_ShiftLeft {lex' AlexRawToken_Expr_BinaryOp_ShiftLeft}
@Expr_AssignOp_BitwiseXor {lex' AlexRawToken_Expr_AssignOp_BitwiseXor}
@Expr_BinaryOp_BitwiseXor {lex' AlexRawToken_Expr_BinaryOp_BitwiseXor}
@Expr_AssignOp_ShiftRight {lex' AlexRawToken_Expr_AssignOp_ShiftRight}
@Expr_AssignOp_BitwiseAnd {lex' AlexRawToken_Expr_AssignOp_BitwiseAnd}
@Expr_AssignOp_BitwiseOr {lex' AlexRawToken_Expr_AssignOp_BitwiseOr}
@NullableType {lex' AlexRawToken_NullableType}
@MatchArm {lex' AlexRawToken_MatchArm}
@conds {lex' AlexRawToken_conds}
@body {lex' AlexRawToken_body}
@arms {lex' AlexRawToken_arms}
@Expr_Match {lex' AlexRawToken_Expr_Match}
@class {lex' AlexRawToken_class}
@Stmt_InlineHTML {lex' AlexRawToken_Stmt_InlineHTML}
@Expr_Cast_Array {lex' AlexRawToken_Expr_Cast_Array}
@Scalar_MagicConst_Function {lex' AlexRawToken_Scalar_MagicConst_Function}
@Stmt_GroupUse {lex' AlexRawToken_Stmt_GroupUse}
@prefix {lex' AlexRawToken_prefix}
@Scalar_MagicConst_Method {lex' AlexRawToken_Scalar_MagicConst_Method}
@Expr_PreDec {lex' AlexRawToken_Expr_PreDec}
@Expr_Clone {lex' AlexRawToken_Expr_Clone}
@Expr_BinaryOp_Spaceship {lex' AlexRawToken_Expr_BinaryOp_Spaceship}
@Expr_BinaryOp_Mod {lex' AlexRawToken_Expr_BinaryOp_Mod}
@Stmt_Block {lex' AlexRawToken_Stmt_Block}
@Expr_AssignOp_Mul {lex' AlexRawToken_Expr_AssignOp_Mul}
@Expr_Eval {lex' AlexRawToken_Expr_Eval}
@items {lex' AlexRawToken_items}
@dim {lex' AlexRawToken_dim}
@Stmt_Declare {lex' AlexRawToken_Stmt_Declare}
@DeclareItem {lex' AlexRawToken_DeclareItem}
@declares {lex' AlexRawToken_declares}
-- last keywords second part

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

@ID        { lex  AlexRawToken_ID                    }
@INT       { lex (AlexRawToken_INT . round . read)   }
@FLOAT     { lex (AlexRawToken_FLOAT . round . read) }
@STR       { lex AlexRawToken_STR                    }
.          { lexicalError                            }

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
     | AlexRawToken_ID String       -- ^ including consant strings
     | AlexRawToken_FLOAT Int       -- ^ including consant strings
     | AlexRawToken_STR String      -- ^ constant strings
      
     | AlexRawToken_LPAREN          -- ^ Parentheses __(__
     | AlexRawToken_RPAREN          -- ^ Parentheses __)__
     | AlexRawToken_LBRACK          -- ^ Parentheses __[__
     | AlexRawToken_RBRACK          -- ^ Parentheses __]__
 
     | AlexRawToken_ARG             -- ^ Reserved Keyword
     | AlexRawToken_VAR             -- ^ Reserved Keyword
     | AlexRawToken_ARGS            -- ^ Reserved Keyword
     | AlexRawToken_NAME            -- ^ Reserved Keyword
     | AlexRawToken_USES            -- ^ Reserved Keyword
     | AlexRawToken_EXPR            -- ^ Reserved Keyword
     | AlexRawToken_MAME            -- ^ Reserved Keyword
     | AlexRawToken_NULL            -- ^ Reserved Keyword
     | AlexRawToken_ALIAS           -- ^ Reserved Keyword
     | AlexRawToken_TYPE            -- ^ Reserved Keyword
     | AlexRawToken_LEFT            -- ^ Reserved Keyword
     | AlexRawToken_LOOP            -- ^ Reserved Keyword
     | AlexRawToken_INIT            -- ^ Reserved Keyword
     | AlexRawToken_COND            -- ^ Reserved Keyword
     | AlexRawToken_CONST           -- ^ Reserved Keyword
     | AlexRawToken_EXPRS           -- ^ Reserved Keyword
     | AlexRawToken_VALUE           -- ^ Reserved Keyword
     | AlexRawToken_RIGHT           -- ^ Reserved Keyword
     | AlexRawToken_STMTS           -- ^ Reserved Keyword
     | AlexRawToken_ARRAY           -- ^ Reserved Keyword
     | AlexRawToken_PARAM           -- ^ Reserved Keyword
     | AlexRawToken_STMT_IF         -- ^ Reserved Keyword
     | AlexRawToken_STMT_CATCH      -- ^ Reserved Keyword
     | AlexRawToken_STMT_ELSE       -- ^ Reserved Keyword
     | AlexRawToken_STMT_DO         -- ^ Reserved Keyword
     | AlexRawToken_STMT_WHILE      -- ^ Reserved Keyword
     | AlexRawToken_STMT_ELIF       -- ^ Reserved Keyword
     | AlexRawToken_STMT_TRY_CATCH  -- ^ Reserved Keyword
     | AlexRawToken_USE_ITEM        -- ^ Reserved Keyword
     | AlexRawToken_STMT_FOR        -- ^ Reserved Keyword
     | AlexRawToken_STMT_NOP        -- ^ Reserved Keyword
     | AlexRawToken_STMT_NAMESPACE  -- ^ Reserved Keyword
     | AlexRawToken_STMT_TRAIT      -- ^ Reserved Keyword
     | AlexRawToken_STMT_TRAITUSE   -- ^ Reserved Keyword
     | AlexRawToken_STMT_SWITCH     -- ^ Reserved Keyword
     | AlexRawToken_STMT_CASE       -- ^ Reserved Keyword
     | AlexRawToken_STMT_FOREACH    -- ^ Reserved Keyword
     | AlexRawToken_STMT_GLOBAL     -- ^ Reserved Keyword
     | AlexRawToken_STMT_USE        -- ^ Reserved Keyword
     | AlexRawToken_STMT_ECHO       -- ^ Reserved Keyword
     | AlexRawToken_STMT_UNSET      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_VAR        -- ^ Reserved Keyword
     | AlexRawToken_EXPR_INSTOF     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_NEW        -- ^ Reserved Keyword
     | AlexRawToken_EXPR_EXIT       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_IMPORT     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_TERNARY    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_THROW      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_LAMBDA     -- ^ Reserved Keyword
     | AlexRawToken_CLOSURE_USE     -- ^ Reserved Keyword
     | AlexRawToken_ERROR_SUPPRESS  -- ^ Reserved Keyword
     | AlexRawToken_FULLY_QUALIFIED -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CAST       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CAST5      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CAST4      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CAST3      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CAST2      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ASSIGN     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ASSIGN5    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ASSIGN4    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ASSIGN3    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ASSIGN2    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ISSET      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_LIST       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ARRAY      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_FETCH      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_EMPTY      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ARRAY2     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ARRAY3     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CALL       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_MCALL      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_SCALL      -- ^ Reserved Keyword
     | AlexRawToken_STMT_EXPR       -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_INT      -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_FLOAT    -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_FSTRING  -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_FILE     -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_CLASS    -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_DIR      -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_STR      -- ^ Reserved Keyword
     | AlexRawToken_IDENTIFIER      -- ^ Reserved Keyword
     | AlexRawToken_STMT_RETURN     -- ^ Reserved Keyword
     | AlexRawToken_RETURN_TYPE     -- ^ Reserved Keyword
     | AlexRawToken_STMT_CLASS      -- ^ Reserved Keyword
     | AlexRawToken_STMT_CLASS_CONST -- ^ Reserved Keyword
     | AlexRawToken_STMT_INTERFACE  -- ^ Reserved Keyword
     | AlexRawToken_STMT_CONT       -- ^ Reserved Keyword
     | AlexRawToken_STMT_BREAK      -- ^ Reserved Keyword
     | AlexRawToken_STMT_STATIC     -- ^ Reserved Keyword
     | AlexRawToken_STATIC_VAR      -- ^ Reserved Keyword
     | AlexRawToken_STMT_PROPERTY   -- ^ Reserved Keyword
     | AlexRawToken_STMT_CLASSMETH  -- ^ Reserved Keyword
     | AlexRawToken_STMT_FUNCTION   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CONST_GET  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_PROP_GET   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_PROP_GET2  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_LT   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_LEQ  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_GT   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_GEQ  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_CO   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_EQ   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_NEQ  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_IS   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_ISNOT -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_MUL   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_DIV   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_PLUS  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_MINUS -- ^ Reserved Keyword
     | AlexRawToken_EXPR_UNOP_MINUS  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_POST_INC    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_PRINT       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_POST_DEC    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_CONCAT -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_OR   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_OR2  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_LOR  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_AND  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_UNOP_NOT   -- ^ Reserved Keyword
     | AlexRawToken_Expr_NullsafePropertyFetch
     | AlexRawToken_Expr_BinaryOp_LogicalAnd
     | AlexRawToken_attrGroups
     | AlexRawToken_flags
     | AlexRawToken_extends
     | AlexRawToken_implements
     | AlexRawToken_false
     | AlexRawToken_byRef
     | AlexRawToken_params
     | AlexRawToken_default
     | AlexRawToken_variadic
     | AlexRawToken_unpack
     | AlexRawToken_key
     | AlexRawToken_keyVar
     | AlexRawToken_valueVar
     | AlexRawToken_consts
     | AlexRawToken_static
     | AlexRawToken_Expr_PreInc
     | AlexRawToken_Scalar_String
     | AlexRawToken_traits
     | AlexRawToken_adaptations
     | AlexRawToken_props
     | AlexRawToken_VarLikeIdentifier
     | AlexRawToken_PropertyItem
     | AlexRawToken_Expr_BinaryOp_Pow
     | AlexRawToken_Expr_ArrowFunction
     | AlexRawToken_parts
     | AlexRawToken_InterpolatedStringPart
     | AlexRawToken_true
     | AlexRawToken_Expr_BinaryOp_BitwiseAnd
     | AlexRawToken_Expr_BinaryOp_ShiftRight
     | AlexRawToken_Expr_BinaryOp_ShiftLeft
     | AlexRawToken_Expr_AssignOp_BitwiseXor
     | AlexRawToken_Expr_BinaryOp_BitwiseXor
     | AlexRawToken_Expr_AssignOp_ShiftRight
     | AlexRawToken_Expr_AssignOp_BitwiseAnd
     | AlexRawToken_Expr_AssignOp_BitwiseOr
     | AlexRawToken_NullableType
     | AlexRawToken_MatchArm
     | AlexRawToken_conds
     | AlexRawToken_body
     | AlexRawToken_arms
     | AlexRawToken_Expr_Match
     | AlexRawToken_class
     | AlexRawToken_Stmt_InlineHTML
     | AlexRawToken_Expr_Cast_Array
     | AlexRawToken_Scalar_MagicConst_Function
     | AlexRawToken_Stmt_GroupUse
     | AlexRawToken_prefix
     | AlexRawToken_Scalar_MagicConst_Method
     | AlexRawToken_Expr_PreDec
     | AlexRawToken_Expr_Clone
     | AlexRawToken_Expr_BinaryOp_Spaceship
     | AlexRawToken_Expr_BinaryOp_Mod
     | AlexRawToken_Stmt_Block
     | AlexRawToken_Expr_AssignOp_Mul
     | AlexRawToken_Expr_Eval
     | AlexRawToken_items
     | AlexRawToken_dim
     | AlexRawToken_Stmt_Declare
     | AlexRawToken_DeclareItem
     | AlexRawToken_declares
     -- last keywords third part

     | AlexRawToken_COLON           -- ^ Punctuation __:__
     | AlexRawToken_SLASH           -- ^ Punctuation __:__
     | AlexRawToken_HYPHEN          -- ^ Punctuation __-__
     | AlexRawToken_OR              -- ^ Punctuation __-__

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
-- * filename *
-- *          *
-- ************
getFilename :: AlexTokenTag -> String
getFilename = Location.filename . location

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

normalizeChar :: Char -> String
normalizeChar '\\' = "\\\\"
normalizeChar c = [c]

normalize :: String -> String
normalize = concatMap normalizeChar

-- **************
-- *            *
-- * tokIDValue *
-- *            *
-- **************
tokIDValue :: AlexTokenTag -> String
tokIDValue t = case (tokenRaw t) of { AlexRawToken_ID s -> (normalize s); _ -> ""; }

-- **************
-- *            *
-- * findString *
-- *            *
-- **************
findString :: String -> String -> Maybe Int
findString needle haystack = Data.List.findIndex (Data.List.isPrefixOf needle) (Data.List.tails haystack)

unquote :: String -> String
unquote = filter (/= '"')

-- ***************
-- *             *
-- * tokStrValue *
-- *             *
-- ***************
tokStrValue :: AlexTokenTag -> String
tokStrValue t = case (tokenRaw t) of { (AlexRawToken_STR s) -> (unquote s); _ -> "" }

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> Common.AdditionalRepoInfo -> String -> Either String a
runAlex' a fp additionalInfo input = runAlex input (setFilePath fp >> alexSetUserState (AlexUserState fp additionalInfo) >> a)
}
