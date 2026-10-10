-- |
-- Smart constructors for 'JavaParser'. Deliberately tiny today: the
-- only rule the grammar has (@root@) builds an empty 'Ast.Root'. As
-- new rules land (one per iteration, same contract as
-- @TsParser.y@ / @TsParserActions.hs@ -- see
-- 'dhscanner.core/dhscanner.service.parsers/AGENTS.md'), their smart
-- constructors move in here and the grammar actions stay as one-line
-- @Actions.foo $1 $2 ...@ calls.
module JavaParserActions where
-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
-- NOTE: identical qualified-Ast posture to TsParserActions.hs; see the
-- long explanation at the top of that file. Short version: an
-- unqualified `import Ast` + `-Wname-shadowing` + `-Werror` makes a
-- helper named `callee` / `args` / `filename` / ... impossible, so we
-- qualify.
import qualified Ast

-- ********
-- *      *
-- * root *
-- *      *
-- ********
-- direct dhscanner subtree creation: Ast.Root
--
-- The parser hands us the file path extracted from the opening-brace
-- token (see 'JavaParser.node'). The @stmts@ list is empty for now
-- because this is the end-to-end-wiring skeleton; coverage iterations
-- will fill it in production by production.
root :: FilePath -> Ast.Root
root fp = Ast.Root
    { Ast.filename = fp
    , Ast.stmts    = []
    }
