module PyParserActions where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
-- `Ast` is imported qualified on purpose, like in `TsParserActions.hs` :
-- with `-Wall -Werror`, an unqualified import leaks every record-field
-- accessor of dhscanner.ast into scope, and a local binding that shares
-- one of those names fails the build on `-Wname-shadowing`.
import qualified Ast
import qualified Token
import Location

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import qualified Data.Map
import Data.Maybe ( mapMaybe, maybeToList )

-- **********************
-- *                    *
-- * instrumentation    *
-- * (shared lowering   *
-- *  for native nodes  *
-- *  that have no 1:1  *
-- *  dhscanner shape)  *
-- *                    *
-- **********************
instrumentationName :: String -> String
instrumentationName tag = "<dhscanner-instrumentation>[" ++ tag ++ "]"

instrumentationCall :: String -> Location -> [Ast.Exp] -> Ast.Exp
instrumentationCall tag loc callArgs = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named (instrumentationName tag) loc,
    Ast.args = callArgs,
    Ast.expCallLocation = loc
}

-- ***************
-- *             *
-- * stmt import *
-- *             *
-- ***************
-- `import a.b.c [as m], ...` -> `Ast.StmtBlock` of one `Ast.StmtImport` per name.
--
-- Python binds only the root name : `import weblate.trans.views.about`
-- binds `weblate`, and the code reaches the module through the dotted
-- access `weblate.trans.views.about.X`, which codegen appends field by
-- field ( -> 'weblate.trans.views.about.X' ). Binding the whole dotted
-- string left the root unresolved ( 'Unknown.trans.views.about.X' ).
-- With an alias, `import a.b.c as m` binds `m` to the full `a.b.c`.
-- 1st vs 3rd party is decided downstream, by name ( runner #126, G1 ).
stmtImport :: Location -> [(String, Maybe String)] -> Ast.Stmt
stmtImport loc names = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = map (stmtImportName loc) names,
    Ast.stmtBlockLocation = loc
}

stmtImportName :: Location -> (String, Maybe String) -> Ast.Stmt
stmtImportName loc (src, alias) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = Ast.ImportThirdParty (Ast.ImportThirdPartyContent (maybe (rootName src) (const src) alias)),
    Ast.stmtImportSpecific = Nothing,
    Ast.stmtImportAlias = fmap Ast.ImportAlias alias,
    Ast.stmtImportLocation = loc
}

-- `weblate.trans.views.about` -> `weblate`
rootName :: String -> String
rootName = takeWhile (/= '.')

-- **************
-- *            *
-- * stmt class *
-- *            *
-- **************
-- `ClassDef( name, bases, body, ... )` -> `Ast.StmtClass`.
--
-- Every class gets one more method, `<dhscanner-instrumentation>[default-method]`,
-- with no params and an empty body, located at the class itself. A class
-- with no methods ( eg `class TermsView(LegalView): page = "terms"` ) would
-- otherwise leave no trace in the kb : codegen / kbgen emit the class def
-- and its supers through its methods ( runner #126, G2 ).
stmtClass :: Location -> String -> [ Ast.Var ] -> [ Ast.Stmt ] -> Ast.Stmt
stmtClass loc name bases body = Ast.StmtClass $ Ast.StmtClassContent {
    Ast.stmtClassName = className,
    Ast.stmtClassSupers = superify bases,
    Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
    Ast.stmtClassMethods = Ast.Methods $ Data.Map.fromList $ defaultMethod className bases loc : methodify className bases body
} where className = Token.ClassName (Token.Named name loc)

defaultMethod :: Token.ClassName -> [ Ast.Var ] -> Location -> (Token.MethodName, Ast.StmtMethodContent)
defaultMethod c bases loc = let m = Token.MethodName (Token.Named (instrumentationName "default-method") loc) in (m, Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Nothing,
    Ast.stmtMethodName = m,
    Ast.stmtMethodParams = [],
    Ast.stmtMethodBody = [],
    Ast.stmtMethodAnnotations = [],
    Ast.stmtMethodLocation = loc,
    Ast.hostingClassName = c,
    Ast.hostingClassSupers = superify bases,
    Ast.hostingClassAnnotations = []
})

methodify :: Token.ClassName -> [ Ast.Var ] -> [ Ast.Stmt ] -> [(Token.MethodName, Ast.StmtMethodContent)]
methodify c bases = mapMaybe (methodify' c bases)

methodify' :: Token.ClassName -> [ Ast.Var ] -> Ast.Stmt -> Maybe (Token.MethodName, Ast.StmtMethodContent)
methodify' c bases (Ast.StmtFunc f) = Just (methodify'' c bases f)
methodify' _ _ _ = Nothing

methodify'' :: Token.ClassName -> [ Ast.Var ] -> Ast.StmtFuncContent -> (Token.MethodName, Ast.StmtMethodContent)
methodify'' c bases f = let m = Token.MethodName $ Token.getFuncNameToken (Ast.stmtFuncName f) in (m, Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Ast.stmtFuncReturnType f,
    Ast.stmtMethodName = m,
    Ast.stmtMethodParams = Ast.stmtFuncParams f,
    Ast.stmtMethodBody = Ast.stmtFuncBody f,
    Ast.stmtMethodAnnotations = Ast.stmtFuncAnnotations f,
    Ast.stmtMethodLocation = Ast.stmtFuncLocation f,
    Ast.hostingClassName = c,
    Ast.hostingClassSupers = superify bases,
    Ast.hostingClassAnnotations = []
})

superify' :: Ast.Var -> Token.SuperName
superify' (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v))) = Token.SuperName v
superify' (Ast.VarField (Ast.VarFieldContent _ (Token.FieldName v) _)) = Token.SuperName v
superify' (Ast.VarSubscript (Ast.VarSubscriptContent _ _ l)) = Token.SuperName (Token.Named "subscript" l)

superify :: [ Ast.Var ] -> [ Token.SuperName ]
superify = map superify'

-- **************
-- *            *
-- * stmt match *
-- *            *
-- **************
-- `match subject: case p1 if g1: b1 ... case _: bn` -> `Ast.StmtBlock` of
--
--   <dhscanner-instrumentation>[match-subject] = subject
--   if test(p1) + g1: bindings(p1) ; b1
--   elif ...
--   else: bindings(pn) ; bn
--
-- The subject is evaluated once, like python, into a synthetic variable that
-- every test and binding reads. A case with an irrefutable pattern ( `case _`,
-- `case x` ) and no guard becomes the else part, and ends the chain.
-- Pattern tests ( `and` / `or` lower to `Ast.PLUS`, like `BoolOp` ) :
--
--   MatchValue v / MatchSingleton v  -> subject == v
--   MatchClass C(...)                -> <dhscanner-instrumentation>[match-class](subject, C)
--   MatchSequence [...]              -> <dhscanner-instrumentation>[match-sequence](subject)
--   MatchMapping {...}               -> <dhscanner-instrumentation>[match-mapping](subject)
--   MatchAs / MatchStar              -> no test
--
-- each `and`-ed with the tests of its sub-patterns. Bindings are assignments
-- from the subject : `case C(a=x)` -> `x = subject.a`, `case [x, *r]` ->
-- `x = subject[0]` and `r = subject`, `case {"k": v}` -> `v = subject["k"]`.
-- Positional class sub-patterns ( `case Point(x, y)` ) bind to the subject
-- itself : `__match_args__` isn't known here, and the subject keeps the dataflow.
data Pattern
   = PatternValue Location Ast.Exp
   | PatternSequence Location [ Pattern ]
   | PatternMapping Location [ (Ast.Exp, Pattern) ] (Maybe String)
   | PatternClass Location Ast.Exp [ Pattern ] [ (String, Pattern) ]
   | PatternStar Location (Maybe String)
   | PatternAs Location (Maybe Pattern) (Maybe String)
   | PatternOr Location [ Pattern ]

data MatchCase = MatchCase Pattern (Maybe Ast.Exp) [ Ast.Stmt ]

stmtMatch :: Location -> Ast.Exp -> [ MatchCase ] -> Ast.Stmt
stmtMatch loc subject cases = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = assign subjectVar subject : matchCases (expVar subjectVar) cases,
    Ast.stmtBlockLocation = loc
} where subjectVar = varSimple (instrumentationName "match-subject") loc

matchCases :: Ast.Exp -> [ MatchCase ] -> [ Ast.Stmt ]
matchCases _ [] = []
matchCases s (MatchCase p Nothing body : rest) = case patternTest s p of
    Nothing -> patternBindings s p ++ body
    Just cond -> [ matchIf cond (patternBindings s p ++ body) (matchCases s rest) (patternLocation p) ]
matchCases s (MatchCase p (Just guard) body : rest) = [ matchIf cond body (matchCases s rest) loc ]
    where
        loc = patternLocation p
        cond = foldr (\a b -> binop Ast.PLUS a b loc) guard (maybeToList (patternTest s p) ++ mapMaybe (expAssign loc) (patternBindings s p))

-- python binds the names of a pattern before it evaluates the guard, so
-- with a guard the bindings move into the condition, as `Ast.ExpAssign`s
-- ( like walrus ), between the pattern test and the guard
expAssign :: Location -> Ast.Stmt -> Maybe Ast.Exp
expAssign loc (Ast.StmtAssign (Ast.StmtAssignContent v e)) = Just $ Ast.ExpAssign $ Ast.ExpAssignContent {
    Ast.expAssignLhs = v,
    Ast.expAssignRhs = e,
    Ast.expAssignLocation = loc
}
expAssign _ _ = Nothing

matchIf :: Ast.Exp -> [ Ast.Stmt ] -> [ Ast.Stmt ] -> Location -> Ast.Stmt
matchIf cond body elseBody loc = Ast.StmtIf $ Ast.StmtIfContent {
    Ast.stmtIfCond = cond,
    Ast.stmtIfBody = body,
    Ast.stmtElseBody = elseBody,
    Ast.stmtIfLocation = loc
}

patternLocation :: Pattern -> Location
patternLocation (PatternValue loc _) = loc
patternLocation (PatternSequence loc _) = loc
patternLocation (PatternMapping loc _ _) = loc
patternLocation (PatternClass loc _ _ _) = loc
patternLocation (PatternStar loc _) = loc
patternLocation (PatternAs loc _ _) = loc
patternLocation (PatternOr loc _) = loc

-- `Nothing` = the pattern always matches
patternTest :: Ast.Exp -> Pattern -> Maybe Ast.Exp
patternTest s (PatternValue loc v) = Just (binop Ast.EQ s v loc)
patternTest s (PatternSequence loc ps) = Just $ andAll loc (instrumentationCall "match-sequence" loc [s]) (mapMaybe (uncurry patternTest) (sequenceElems loc s ps))
patternTest s (PatternMapping loc kvs _) = Just $ andAll loc (instrumentationCall "match-mapping" loc [s]) (mapMaybe (\(k, p) -> patternTest (subscript s k loc) p) kvs)
patternTest s (PatternClass loc cls ps kws) = Just $ andAll loc (instrumentationCall "match-class" loc [s, cls]) (mapMaybe (patternTest s) ps ++ mapMaybe (\(k, p) -> patternTest (field s k loc) p) kws)
patternTest _ (PatternStar _ _) = Nothing
patternTest _ (PatternAs _ Nothing _) = Nothing
patternTest s (PatternAs _ (Just p) _) = patternTest s p
patternTest s (PatternOr loc ps) = case mapM (patternTest s) ps of
    Just (t:ts) -> Just (foldl (\a b -> binop Ast.PLUS a b loc) t ts)
    _ -> Nothing

patternBindings :: Ast.Exp -> Pattern -> [ Ast.Stmt ]
patternBindings _ (PatternValue _ _) = []
patternBindings s (PatternSequence loc ps) = concatMap (uncurry patternBindings) (sequenceElems loc s ps)
patternBindings s (PatternMapping loc kvs rest) = concatMap (\(k, p) -> patternBindings (subscript s k loc) p) kvs ++ bindName s loc rest
patternBindings s (PatternClass loc _ ps kws) = concatMap (patternBindings s) ps ++ concatMap (\(k, p) -> patternBindings (field s k loc) p) kws
patternBindings s (PatternStar loc name) = bindName s loc name
patternBindings s (PatternAs loc p name) = maybe [] (patternBindings s) p ++ bindName s loc name
patternBindings s (PatternOr _ ps) = concatMap (patternBindings s) ps

-- the sub-subject of each sequence element : `subject[i]` before the star,
-- `subject[i - n]` after it ( counted from the end ), the subject itself
-- for the star
sequenceElems :: Location -> Ast.Exp -> [ Pattern ] -> [ (Ast.Exp, Pattern) ]
sequenceElems loc s ps = zipWith elem' [0..] ps
    where
        n = length ps
        star = length (takeWhile (not . isStar) ps)
        elem' i p
            | isStar p = (s, p)
            | i < star = (subscript s (expInt i loc) loc, p)
            | otherwise = (subscript s (expInt (i - n) loc) loc, p)

isStar :: Pattern -> Bool
isStar (PatternStar _ _) = True
isStar _ = False

bindName :: Ast.Exp -> Location -> Maybe String -> [ Ast.Stmt ]
bindName s loc = maybe [] (\name -> [ assign (varSimple name loc) s ])

andAll :: Location -> Ast.Exp -> [ Ast.Exp ] -> Ast.Exp
andAll loc = foldl (\a b -> binop Ast.PLUS a b loc)

binop :: Ast.Operator -> Ast.Exp -> Ast.Exp -> Location -> Ast.Exp
binop op a b loc = Ast.ExpBinop $ Ast.ExpBinopContent {
    Ast.expBinopLeft = a,
    Ast.expBinopRight = b,
    Ast.expBinopOperator = op,
    Ast.expBinopLocation = loc
}

assign :: Ast.Var -> Ast.Exp -> Ast.Stmt
assign v e = Ast.StmtAssign $ Ast.StmtAssignContent {
    Ast.stmtAssignLhs = v,
    Ast.stmtAssignRhs = e
}

varSimple :: String -> Location -> Ast.Var
varSimple name loc = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named name loc

expVar :: Ast.Var -> Ast.Exp
expVar = Ast.ExpVar . Ast.ExpVarContent

subscript :: Ast.Exp -> Ast.Exp -> Location -> Ast.Exp
subscript s idx loc = expVar $ Ast.VarSubscript $ Ast.VarSubscriptContent {
    Ast.varSubscriptLhs = s,
    Ast.varSubscriptIdx = idx,
    Ast.varSubscriptLocation = loc
}

field :: Ast.Exp -> String -> Location -> Ast.Exp
field s name loc = expVar $ Ast.VarField $ Ast.VarFieldContent {
    Ast.varFieldLhs = s,
    Ast.varFieldName = Token.FieldName (Token.Named name loc),
    Ast.varFieldLocation = loc
}

expInt :: Int -> Location -> Ast.Exp
expInt i loc = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt i loc

-- like `exp_none` in PyParser.y
expNone :: Location -> Ast.Exp
expNone = expInt 0

expBool :: Bool -> Location -> Ast.Exp
expBool b loc = Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool b loc
