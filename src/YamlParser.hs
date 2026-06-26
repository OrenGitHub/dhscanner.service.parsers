{-# LANGUAGE OverloadedStrings #-}

-- |
-- YAML -> dhscanner.ast lowering.
--
-- Unlike the other parsers in this directory, YAML is not parsed with
-- Alex+Happy from a JSON dump of some native AST. Instead we lean on the
-- pure-Haskell HsYAML library (@Data.YAML@) directly: it gives us a
-- position-annotated 'Y.Node' tree, which we then walk into the same
-- 'Ast.Root' shape every other parser produces.
--
-- The lowering convention mirrors the @<dhscanner-instrumentation>[<tag>]@
-- callee-name convention used throughout @TsParserActions.hs@. There are
-- two tags:
--
--   * @yaml-key@   - marks a single \"K: V\" mapping entry. The callee is a
--                    call @<dhscanner-instrumentation>[yaml-key](K)@ whose
--                    single argument is the bare key literal. The outer
--                    call's @args@ is V lowered (see 'lowerAsArgs').
--
--   * @yaml-value@ - marks a scalar leaf. The callee is bare
--                    @<dhscanner-instrumentation>[yaml-value]@ and the
--                    single argument is the typed Ast literal (Str/Int/...).
--
-- Worked example (the service-definition snippet from the task):
--
-- @
-- services:
--   phpbb.ucp.controller.delete_cookies:
--     class: phpbb\\ucp\\controller\\delete_cookies
--     arguments:
--       - '\@config'
--       - '\@dispatcher'
-- @
--
-- lowers to (sketch, omitting boilerplate)
--
-- @
-- Call(
--   callee = Call(yaml-key, [Str "services"]),
--   args   = [Call(
--     callee = Call(yaml-key, [Str "phpbb.ucp.controller.delete_cookies"]),
--     args   = [
--       Call(callee = Call(yaml-key, [Str "class"]),
--            args   = [Call(yaml-value, [Str "phpbb\\ucp\\controller\\delete_cookies"])]),
--       Call(callee = Call(yaml-key, [Str "arguments"]),
--            args   = [ Call(yaml-value, [Str "\@config"])
--                     , Call(yaml-value, [Str "\@dispatcher"]) ])
--     ])])
-- @
module YamlParser ( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
-- See the NOTE in TsParserActions.hs: `Ast` is imported qualified so the
-- many record-field accessors from dhscanner.ast (callee, args, filename,
-- ...) can never silently shadow a local binding and trip `-Wname-shadowing`
-- under `-Werror`. `Location` is imported unqualified because its field
-- names (filename, lineStart, ...) appear inside record-construction
-- syntax only; the `filename` field is spelled `Location.filename` at the
-- single site where it overlaps with `Ast.filename`.
import qualified Ast
import Location
import qualified Token
import qualified Common

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import qualified Data.YAML as Y
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- ****************
-- *              *
-- * parseProgram *
-- *              *
-- ****************
parseProgram
    :: Common.SourceCodeFilePath
    -> Common.SourceCodeContent
    -> Common.AdditionalRepoInfo
    -> Either String Ast.Root
parseProgram (Common.SourceCodeFilePath fp) (Common.SourceCodeContent content) _repoInfo =
    case Y.decodeNode (toLazyUtf8 content) of
        Left (pos, msg) -> Left (show (posToLocation fp pos) ++ ": " ++ msg)
        Right docs      -> Right Ast.Root
            { Ast.filename = fp
            , Ast.stmts    = concatMap (docToStmts fp) docs
            }

toLazyUtf8 :: String -> BSL.ByteString
toLazyUtf8 = BSL.fromStrict . TE.encodeUtf8 . T.pack

-- *********************
-- *                   *
-- * position -> loc   *
-- *                   *
-- *********************
-- HsYAML's `posLine` is 1-based, `posColumn` is 0-based. dhscanner's
-- Location is 1-based on both axes (matches the SARIF convention noted in
-- Location.hs), so we shift the column by 1. A YAML node spans an unknown
-- number of source lines/columns, but for the framework the "point of
-- interest" is the start position, which is what HsYAML hands us; we
-- collapse start==end here and let downstream analyses live with that.
posToLocation :: FilePath -> Y.Pos -> Location
posToLocation fp p =
    let ln  = max 1 (fromIntegral (Y.posLine p))
        col = max 1 (fromIntegral (Y.posColumn p + 1))
    in Location
        { Location.filename = fp
        , lineStart = ln
        , lineEnd   = ln
        , colStart  = col
        , colEnd    = col
        }

nodeLocation :: FilePath -> Y.Node Y.Pos -> Location
nodeLocation fp n = case n of
    Y.Scalar   pos _     -> posToLocation fp pos
    Y.Mapping  pos _ _   -> posToLocation fp pos
    Y.Sequence pos _ _   -> posToLocation fp pos
    Y.Anchor   pos _ _   -> posToLocation fp pos

-- *********************
-- *                   *
-- * doc -> stmt list  *
-- *                   *
-- *********************
-- A YAML document whose root is a mapping turns into one StmtExp per
-- top-level (key, value) pair. Any other root shape (sequence, scalar)
-- turns into a single StmtExp whose body is the whole document lowered as
-- a single Exp -- there's no enclosing kv-call to "spread" it into.
docToStmts :: FilePath -> Y.Doc (Y.Node Y.Pos) -> [Ast.Stmt]
docToStmts fp d = nodeToStmts fp (Y.docRoot d)

nodeToStmts :: FilePath -> Y.Node Y.Pos -> [Ast.Stmt]
nodeToStmts fp n = case n of
    Y.Mapping _ _ m    -> map (\(k, v) -> Ast.StmtExp (yamlKvCall fp k v)) (Map.toList m)
    Y.Anchor _ _ inner -> nodeToStmts fp inner
    _                  -> [Ast.StmtExp (lowerAsExp fp n)]

-- *********************
-- *                   *
-- * instrumentation   *
-- *                   *
-- *********************
-- Local copy of TsParserActions.instrumentationCall. Kept here so this
-- module stays independent of the TS-specific actions module, but the
-- callee-name format MUST stay byte-identical -- downstream analyses grep
-- for the literal "<dhscanner-instrumentation>[<tag>]" string.
instrumentationCall :: String -> Location -> [Ast.Exp] -> Ast.Exp
instrumentationCall tag loc callArgs = Ast.ExpCall $ Ast.ExpCallContent
    { Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $
                       Token.VarName $ Token.Named
                           { Token.content  = "<dhscanner-instrumentation>[" ++ tag ++ "]"
                           , Token.location = loc
                           }
    , Ast.args = callArgs
    , Ast.expCallLocation = loc
    }

-- *********************
-- *                   *
-- * single kv pair    *
-- *                   *
-- *********************
-- Build the standard "K: V" call. The callee is itself a call --
-- @<dhscanner-instrumentation>[yaml-key](kLiteral)@ -- and the args are
-- the value side, spread according to its shape (see 'lowerAsArgs').
yamlKvCall :: FilePath -> Y.Node Y.Pos -> Y.Node Y.Pos -> Ast.Exp
yamlKvCall fp k v =
    let kLoc       = nodeLocation fp k
        keyLiteral = keyAsLiteral fp k
        keyCallee  = instrumentationCall "yaml-key" kLoc [keyLiteral]
    in Ast.ExpCall $ Ast.ExpCallContent
        { Ast.callee          = keyCallee
        , Ast.args            = lowerAsArgs fp v
        , Ast.expCallLocation = kLoc
        }

-- The key position is meant to read as the literal text of the key. Real
-- YAML files in the wild use scalar (almost always string) keys; we keep a
-- structural fallback for the unusual cases (mapping/sequence keys) so we
-- never drop information silently.
keyAsLiteral :: FilePath -> Y.Node Y.Pos -> Ast.Exp
keyAsLiteral fp k = case k of
    Y.Scalar pos s     -> scalarLit (posToLocation fp pos) s
    Y.Anchor _ _ inner -> keyAsLiteral fp inner
    _                  -> lowerAsExp fp k

-- *********************
-- *                   *
-- * scalar -> Ast lit *
-- *                   *
-- *********************
-- HsYAML decodes scalars into a typed sum -- we lower each variant into
-- the dhscanner.ast literal that matches its type. Floats and SUnknown
-- (failed implicit-typing) become strings: dhscanner.ast has no float
-- literal, and SUnknown's only honest carrier is the raw text.
scalarLit :: Location -> Y.Scalar -> Ast.Exp
scalarLit loc s = case s of
    Y.SNull        -> Ast.ExpNull (Ast.ExpNullContent (Token.ConstNull loc))
    Y.SBool b      -> Ast.ExpBool (Ast.ExpBoolContent (Token.ConstBool b loc))
    Y.SInt n       -> Ast.ExpInt  (Ast.ExpIntContent  (Token.ConstInt (fromInteger n) loc))
    Y.SFloat f     -> mkStr loc (show f)
    Y.SStr t       -> mkStr loc (T.unpack t)
    Y.SUnknown _ t -> mkStr loc (T.unpack t)

mkStr :: Location -> String -> Ast.Exp
mkStr loc s = Ast.ExpStr $ Ast.ExpStrContent $ Token.ConstStr
    { Token.constStrValue    = s
    , Token.constStrLocation = loc
    }

-- *********************
-- *                   *
-- * node -> single Exp*
-- *                   *
-- *********************
-- Lower a yaml node to a single 'Ast.Exp'. Used when a single expression
-- is required (e.g. an element of a sequence, or a non-scalar mapping key).
--
--   * scalar           -> @yaml-value@ instrumented call
--   * sequence         -> @yaml-seq@ instrumented call wrapping the lowered
--                         elements (so list shape is preserved when a seq
--                         appears in an Exp position rather than as a value
--                         to be spread into args)
--   * mapping (1 pair) -> the single kv-call directly (no extra wrapper --
--                         indistinguishable from how 'yamlKvCall' would
--                         appear at a parent's args list)
--   * mapping (n pairs)-> @yaml-mapping@ instrumented call whose args are
--                         the n kv-calls. We need a wrapper here because a
--                         multi-pair mapping cannot be expressed as a
--                         single bare exp without one.
--   * anchor           -> recurse on the target
lowerAsExp :: FilePath -> Y.Node Y.Pos -> Ast.Exp
lowerAsExp fp n = case n of
    Y.Scalar pos s ->
        let loc = posToLocation fp pos
        in instrumentationCall "yaml-value" loc [scalarLit loc s]
    Y.Sequence pos _ xs ->
        let loc = posToLocation fp pos
        in instrumentationCall "yaml-seq" loc (map (lowerAsExp fp) xs)
    Y.Mapping pos _ m ->
        let loc = posToLocation fp pos
            pairs = Map.toList m
        in case pairs of
            [(k, v)] -> yamlKvCall fp k v
            _        -> instrumentationCall "yaml-mapping" loc
                            (map (uncurry (yamlKvCall fp)) pairs)
    Y.Anchor _ _ inner -> lowerAsExp fp inner

-- *********************
-- *                   *
-- * node -> args list *
-- *                   *
-- *********************
-- Lower a yaml node as the @args@ of the surrounding @yaml-key@ call.
-- This is the shape requested by the task's example, where a mapping or
-- sequence value is "spread" into the parent kv-call's args rather than
-- wrapped in an extra marker call.
--
--   * mapping  -> one kv-call per entry
--   * sequence -> one lowered exp per element
--   * scalar   -> singleton [yaml-value(...)]
--   * anchor   -> recurse on the target
lowerAsArgs :: FilePath -> Y.Node Y.Pos -> [Ast.Exp]
lowerAsArgs fp n = case n of
    Y.Mapping _ _ m    -> map (uncurry (yamlKvCall fp)) (Map.toList m)
    Y.Sequence _ _ xs  -> map (lowerAsExp fp) xs
    Y.Scalar pos s     ->
        let loc = posToLocation fp pos
        in [instrumentationCall "yaml-value" loc [scalarLit loc s]]
    Y.Anchor _ _ inner -> lowerAsArgs fp inner
