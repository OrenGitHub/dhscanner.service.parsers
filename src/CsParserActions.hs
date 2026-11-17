module CsParserActions where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
-- `Ast` is imported qualified on purpose, like in `RbParserActions.hs` :
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
import Data.Maybe ( fromMaybe, maybeToList )
import Text.Read ( readMaybe )

-- ******************************************************************
-- * The smart constructors behind `CsParser.y` : every production  *
-- * whose action builds dhscanner AST calls exactly one of these.  *
-- * The native AST is Roslyn's ( dhscanner.service.fronts/cs ) :   *
-- * `value` is a declaration's name, or a leaf node's text.        *
-- ******************************************************************

-- ************
-- *          *
-- * location *
-- *          *
-- ************
-- 1-based lines and columns, as the front reports them
location :: FilePath -> Int -> Int -> Int -> Int -> Location
location fp l1 c1 l2 c2 = Location {
    Location.filename = fp,
    lineStart = fromIntegral l1,
    colStart = fromIntegral c1,
    lineEnd = fromIntegral l2,
    colEnd = fromIntegral c2
}

-- ********
-- *      *
-- * root *
-- *      *
-- ********
-- namespaces ( block or file scoped ) are flattened : their members are
-- top level statements of the file, like java's package members
root :: Location -> [ Ast.Stmt ] -> Ast.Root
root loc stmts = Ast.Root {
    Ast.filename = Location.filename loc,
    Ast.stmts = concatMap unnamespace stmts
}

unnamespace :: Ast.Stmt -> [ Ast.Stmt ]
unnamespace (Ast.StmtBlock (Ast.StmtBlockContent stmts _)) = concatMap unnamespace stmts
unnamespace stmt = [ stmt ]

-- *****************
-- *               *
-- * stmtNamespace *
-- *               *
-- *****************
-- the namespace name itself is dropped
stmtNamespace :: Location -> Ast.Var -> Maybe [ Ast.Stmt ] -> Ast.Stmt
stmtNamespace loc _name members = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = fromMaybe [] members,
    Ast.stmtBlockLocation = loc
}

-- *********
-- *       *
-- * names *
-- *       *
-- *********
named :: Location -> String -> Token.Named
named loc name = Token.Named { Token.content = name, Token.location = loc }

simpleVar :: Location -> String -> Ast.Var
simpleVar loc name = varSimple (named loc name)

varSimple :: Token.Named -> Ast.Var
varSimple = Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

fieldVar :: Location -> Ast.Exp -> Token.Named -> Ast.Var
fieldVar loc lhs name = Ast.VarField $ Ast.VarFieldContent {
    Ast.varFieldLhs = lhs,
    Ast.varFieldName = Token.FieldName name,
    Ast.varFieldLocation = loc
}

-- | `a.B<C>` where RHS is a GenericName: use the base name (drop type args)
fieldVarFromVar :: Location -> Ast.Exp -> Ast.Var -> Ast.Var
fieldVarFromVar loc lhs v = fieldVar loc lhs (named loc (var2name v))

-- | `A.B` in a name position ( usings, namespaces, types )
qualifiedName :: Location -> Ast.Var -> Token.Named -> Ast.Var
qualifiedName loc lhs name = fieldVar loc (expVar lhs) name

-- | `a[i]` ( the first index only )
subscriptVar :: Location -> Ast.Exp -> [ Ast.Exp ] -> Ast.Var
subscriptVar loc lhs (idx:_) = Ast.VarSubscript (Ast.VarSubscriptContent lhs idx loc)
subscriptVar loc lhs [] = Ast.VarSubscript (Ast.VarSubscriptContent lhs (expNull loc) loc)

var2name :: Ast.Var -> String
var2name (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named n _)))) = n
var2name (Ast.VarField (Ast.VarFieldContent lhs (Token.FieldName (Token.Named n _)) _)) = exp2name lhs ++ "." ++ n
var2name (Ast.VarSubscript (Ast.VarSubscriptContent lhs _ _)) = exp2name lhs

exp2name :: Ast.Exp -> String
exp2name (Ast.ExpVar (Ast.ExpVarContent v)) = var2name v
exp2name _ = ""

-- *********
-- *       *
-- * types *
-- *       *
-- *********
-- | `List<T>` is the type `List` ( type arguments dropped )
genericName :: Location -> String -> [ Ast.Var ] -> Ast.Var
genericName loc name _typeArgs = simpleVar loc name

-- | `string`, `int`, `bool` ..
predefinedType :: Location -> String -> Ast.Var
predefinedType = simpleVar

-- | Tuple type like (T1 a, T2 b): mapped to a simple nominal type "tuple"
-- Elements are intentionally ignored for a minimal, conservative mapping.
tupleType :: Location -> [ Ast.Var ] -> Ast.Var
tupleType loc _elements = simpleVar loc "tuple"

-- **********
-- *        *
-- * usings *
-- *        *
-- **********
-- | `using A.B.C;`
stmtUsing :: Location -> Ast.Var -> Ast.Stmt
stmtUsing loc v = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = Ast.ImportThirdParty (Ast.ImportThirdPartyContent (var2name v)),
    Ast.stmtImportSpecific = Nothing,
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = loc
}

-- | `using X = A.B.C;`
stmtUsingAlias :: Location -> Token.Named -> Ast.Var -> Ast.Stmt
stmtUsingAlias loc alias v = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = Ast.ImportThirdParty (Ast.ImportThirdPartyContent (var2name v)),
    Ast.stmtImportSpecific = Nothing,
    Ast.stmtImportAlias = Just (Ast.ImportAlias (Token.content alias)),
    Ast.stmtImportLocation = loc
}

-- *************
-- *           *
-- * stmtClass *
-- *           *
-- *************
-- a class ( also a record / interface ) from its children, in any order :
--   * its attributes annotate every method ( hostingClassAnnotations ),
--   * a primary constructor `class C(T x)` is a ctor named C, and its
--     parameters are also data members ( in scope in every member ),
--   * members : fields ( vardecs, also inside a multi declarator block ),
--     properties ( vardecs ) and methods / ctors. Other statements are dropped.
stmtClass :: Location -> String -> [ ClassPart ] -> Ast.Stmt
stmtClass loc name parts = stmtClass' className supers annotations (primary ++ concatMap unblock members)
    where
        className = Token.ClassName (named loc name)
        supers = map superify (concat [ s | ClassSupers s <- parts ])
        annotations = concat [ a | ClassAttributes a <- parts ]
        members = [ m | ClassMember m <- parts ]
        primary = concat [ primaryConstructor loc name ps | ClassParams ps <- parts ]

-- | one child of a class declaration
data ClassPart
   = ClassAttributes [ Ast.Exp ]
   | ClassParams [ Ast.Param ]
   | ClassSupers [ Ast.Var ]
   | ClassMember Ast.Stmt
   | ClassTypeParams

classAttributes :: [ Ast.Exp ] -> ClassPart
classAttributes = ClassAttributes

classParams :: [ Ast.Param ] -> ClassPart
classParams = ClassParams

classSupers :: [ Ast.Var ] -> ClassPart
classSupers = ClassSupers

classMember :: Ast.Stmt -> ClassPart
classMember = ClassMember

-- | `class C<T>` : the type parameters are dropped
classTypeParams :: () -> ClassPart
classTypeParams _ = ClassTypeParams

-- | `class C(T x)` : the ctor C(x) and the data member x
primaryConstructor :: Location -> String -> [ Ast.Param ] -> [ Ast.Stmt ]
primaryConstructor loc name ps = stmtCtor loc name ps (stmtBlock loc []) : map paramMember ps

paramMember :: Ast.Param -> Ast.Stmt
paramMember p = Ast.StmtVardec $ Ast.StmtVardecContent {
    Ast.stmtVardecName = Token.VarName (Token.getParamNameToken (Ast.paramName p)),
    Ast.stmtVardecNominalType = Ast.paramNominalType p,
    Ast.stmtVardecInitValue = Nothing,
    Ast.stmtVardecLocation = Token.location (Token.getParamNameToken (Ast.paramName p))
}

-- | `: Base(args)` of a primary constructor : the super ( args dropped, TODO )
primaryConstructorBase :: Ast.Var -> [ Ast.Exp ] -> Ast.Var
primaryConstructorBase base _baseArgs = base

stmtClass' :: Token.ClassName -> [ Token.SuperName ] -> [ Ast.Exp ] -> [ Ast.Stmt ] -> Ast.Stmt
stmtClass' className supers annotations members = Ast.StmtClass $ Ast.StmtClassContent {
    Ast.stmtClassName = className,
    Ast.stmtClassSupers = supers,
    Ast.stmtClassDataMembers = Ast.DataMembers $ Data.Map.fromList [ (Ast.dataMemberName d, d) | Ast.StmtVardec v <- members, let d = dataMember v ],
    Ast.stmtClassMethods = Ast.Methods $ Data.Map.fromList [ (Ast.stmtMethodName m, m { Ast.hostingClassName = className, Ast.hostingClassSupers = supers, Ast.hostingClassAnnotations = annotations }) | Ast.StmtMethod m <- members ]
}

dataMember :: Ast.StmtVardecContent -> Ast.DataMember
dataMember d = Ast.DataMember {
    Ast.dataMemberName = Token.MemberName (Token.getVarNameToken (Ast.stmtVardecName d)),
    Ast.dataMemberNominalType = Ast.stmtVardecNominalType d,
    Ast.dataMemberInitValue = Ast.stmtVardecInitValue d,
    Ast.dataMemberAnnotations = []
}

superify :: Ast.Var -> Token.SuperName
superify v@(Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named _ l)))) = Token.SuperName (Token.Named (var2name v) l)
superify v@(Ast.VarField (Ast.VarFieldContent _ _ l)) = Token.SuperName (Token.Named (var2name v) l)
superify v@(Ast.VarSubscript (Ast.VarSubscriptContent _ _ l)) = Token.SuperName (Token.Named (var2name v) l)

-- ************
-- *          *
-- * vardecs  *
-- *          *
-- ************
-- | one declarator of `T a = 1, b;` ( the type is filled by `stmtVardec` )
declarator :: Location -> String -> Maybe Ast.Exp -> Ast.StmtVardecContent
declarator loc name initValue = Ast.StmtVardecContent {
    Ast.stmtVardecName = Token.VarName (named loc name),
    Ast.stmtVardecNominalType = Nothing,
    Ast.stmtVardecInitValue = initValue,
    Ast.stmtVardecLocation = loc
}

-- | `T a = 1, b;` : one vardec per declarator ( a block when there are several )
stmtVardec :: Location -> Ast.Var -> [ Ast.StmtVardecContent ] -> Ast.Stmt
stmtVardec _ typ [d] = Ast.StmtVardec d { Ast.stmtVardecNominalType = Just typ }
stmtVardec loc typ ds = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = [ Ast.StmtVardec d { Ast.stmtVardecNominalType = Just typ } | d <- ds ],
    Ast.stmtBlockLocation = loc
}

-- | a property is a data member ( accessors dropped )
stmtProperty :: Location -> String -> Ast.Var -> Ast.Stmt
stmtProperty loc name typ = Ast.StmtVardec $ Ast.StmtVardecContent {
    Ast.stmtVardecName = Token.VarName (named loc name),
    Ast.stmtVardecNominalType = Just typ,
    Ast.stmtVardecInitValue = Nothing,
    Ast.stmtVardecLocation = loc
}

-- ***********
-- *         *
-- * methods *
-- *         *
-- ***********
parameter :: Location -> String -> Annotated -> Ast.Param
parameter loc name typ = Ast.Param {
    Ast.paramName = Token.ParamName (named loc name),
    Ast.paramNominalType = Just (annotatedVar typ),
    Ast.paramSerialIdx = 0,
    Ast.paramAnnotations = annotations typ
}

-- | lambda parameter without an explicit type
parameterNameOnly :: Location -> String -> Ast.Param
parameterNameOnly loc name = Ast.Param {
    Ast.paramName = Token.ParamName (named loc name),
    Ast.paramNominalType = Nothing,
    Ast.paramSerialIdx = 0,
    Ast.paramAnnotations = []
}

-- | number the parameters in declaration order ( zero-based )
params :: [ Ast.Param ] -> [ Ast.Param ]
params = zipWith serialIdx [0..]

serialIdx :: Word -> Ast.Param -> Ast.Param
serialIdx i p = p { Ast.paramSerialIdx = i }

-- | the hosting class is filled by `stmtClass`
-- | the method's attributes come in front of its return type
stmtMethod :: Location -> String -> [ Ast.Param ] -> Annotated -> Ast.Stmt -> Ast.Stmt
stmtMethod loc name ps returnType body = Ast.StmtMethod $ Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Just (annotatedVar returnType),
    Ast.stmtMethodName = Token.MethodName (named loc name),
    Ast.stmtMethodParams = ps,
    Ast.stmtMethodBody = unblock body,
    Ast.stmtMethodAnnotations = annotations returnType,
    Ast.stmtMethodLocation = loc,
    Ast.hostingClassName = Token.ClassName (named loc ""),
    Ast.hostingClassSupers = [],
    Ast.hostingClassAnnotations = []
}

-- | method declaration without a body (e.g., in interfaces)
stmtMethodNoBody :: Location -> String -> [ Ast.Param ] -> Annotated -> Ast.Stmt
stmtMethodNoBody loc name ps returnType = Ast.StmtMethod $ Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Just (annotatedVar returnType),
    Ast.stmtMethodName = Token.MethodName (named loc name),
    Ast.stmtMethodParams = ps,
    Ast.stmtMethodBody = [],
    Ast.stmtMethodAnnotations = annotations returnType,
    Ast.stmtMethodLocation = loc,
    Ast.hostingClassName = Token.ClassName (named loc ""),
    Ast.hostingClassSupers = [],
    Ast.hostingClassAnnotations = []
}

-- | a ctor is a method named after its class, its `: this(..)` call first
stmtCtor :: Location -> String -> [ Ast.Param ] -> Ast.Stmt -> Ast.Stmt
stmtCtor loc name ps body = stmtCtor' loc name ps Nothing body

-- | a ctor with a `: this(..)` initializer
stmtCtorInit :: Location -> String -> [ Ast.Param ] -> Ast.Stmt -> Ast.Stmt -> Ast.Stmt
stmtCtorInit loc name ps initializer body = stmtCtor' loc name ps (Just initializer) body

stmtCtor' :: Location -> String -> [ Ast.Param ] -> Maybe Ast.Stmt -> Ast.Stmt -> Ast.Stmt
stmtCtor' loc name ps initializer body = Ast.StmtMethod $ Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Nothing,
    Ast.stmtMethodName = Token.MethodName (named loc name),
    Ast.stmtMethodParams = ps,
    Ast.stmtMethodBody = maybeToList initializer ++ unblock body,
    Ast.stmtMethodAnnotations = [],
    Ast.stmtMethodLocation = loc,
    Ast.hostingClassName = Token.ClassName (named loc ""),
    Ast.hostingClassSupers = [],
    Ast.hostingClassAnnotations = []
}

-- | `: this(args)`
thisInitializer :: Location -> [ Ast.Exp ] -> Ast.Stmt
thisInitializer loc callArgs = Ast.StmtExp (call loc (expvarify (named loc "this")) callArgs)

baseInitializer :: Location -> [ Ast.Exp ] -> Ast.Stmt
baseInitializer loc callArgs = Ast.StmtExp (call loc (expvarify (named loc "base")) callArgs)

-- **************
-- *            *
-- * attributes *
-- *            *
-- **************
-- | `[Name(args)]` is the call `Name(args)`, like a java annotation
attribute :: Location -> Ast.Var -> [ Ast.Exp ] -> Ast.Exp
attribute loc name attrArgs = call loc (expVar name) attrArgs

-- | `[Name]`
attributeNoArgs :: Location -> Ast.Var -> Ast.Exp
attributeNoArgs loc name = attribute loc name []

-- | `Name = value` ( attribute arguments )
kwArg :: Location -> Token.Named -> Ast.Exp -> Ast.Exp
kwArg loc name value = Ast.ExpKwArg $ Ast.ExpKwArgContent {
    Ast.expKwArgName = Token.content name,
    Ast.expKwArgValue = value,
    Ast.expKwArgLocation = loc
}

-- | anonymous object shorthand: new { name } -> kwarg name = name
kwArgSelf :: Location -> Token.Named -> Ast.Exp
kwArgSelf loc name = kwArg loc name (expvarify name)

-- | anonymous object shorthand from a member access expression: new { a.b } -> kwarg b = a.b
kwArgSelfFromVar :: Location -> Ast.Var -> Ast.Exp
kwArgSelfFromVar loc v = kwArg loc (named loc (rightmostVarName v)) (expVar v)

rightmostVarName :: Ast.Var -> String
rightmostVarName (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named n _)))) = n
rightmostVarName (Ast.VarField (Ast.VarFieldContent _ (Token.FieldName (Token.Named n _)) _)) = n
rightmostVarName v = var2name v

-- | a type and the attribute lists in front of it ( a method's return
-- type carries the method's attributes, a parameter's type the parameter's )
data Annotated = Annotated { annotations :: [ Ast.Exp ], annotatedVar :: Ast.Var }

unannotated :: Ast.Var -> Annotated
unannotated = Annotated []

annotated :: [ [ Ast.Exp ] ] -> Ast.Var -> Annotated
annotated attributeLists = Annotated (concat attributeLists)

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************
expVar :: Ast.Var -> Ast.Exp
expVar = Ast.ExpVar . Ast.ExpVarContent

expvarify :: Token.Named -> Ast.Exp
expvarify = expVar . varSimple

expStr :: Location -> String -> Ast.Exp
expStr loc value = Ast.ExpStr $ Ast.ExpStrContent (Token.ConstStr value loc)

-- | non decimal / suffixed literals ( 0x1F, 1.5f, 10L ) are 0
expInt :: Location -> String -> Ast.Exp
expInt loc value = Ast.ExpInt $ Ast.ExpIntContent (Token.ConstInt (fromMaybe 0 (readMaybe (filter (/= '_') value))) loc)

expNull :: Location -> Ast.Exp
expNull loc = Ast.ExpNull $ Ast.ExpNullContent (Token.ConstNull loc)

expTrue :: Location -> Ast.Exp
expTrue loc = Ast.ExpBool $ Ast.ExpBoolContent (Token.ConstBool True loc)

expFalse :: Location -> Ast.Exp
expFalse loc = Ast.ExpBool $ Ast.ExpBoolContent (Token.ConstBool False loc)

expThis :: Location -> Ast.Exp
expThis loc = expvarify (named loc "this")

-- | the `base` reference expression
expBase :: Location -> Ast.Exp
expBase loc = expvarify (named loc "base")

expBinop :: Location -> Ast.Exp -> Ast.Exp -> Ast.Operator -> Ast.Exp
expBinop loc lhs rhs op = Ast.ExpBinop $ Ast.ExpBinopContent {
    Ast.expBinopLeft = lhs,
    Ast.expBinopRight = rhs,
    Ast.expBinopOperator = op,
    Ast.expBinopLocation = loc
}

-- the binary operators the native kinds map to
opEq :: Ast.Operator
opEq = Ast.EQ

opNeq :: Ast.Operator
opNeq = Ast.NEQ

opPlus :: Ast.Operator
opPlus = Ast.PLUS

opMinus :: Ast.Operator
opMinus = Ast.MINUS

opTimes :: Ast.Operator
opTimes = Ast.TIMES

opDivide :: Ast.Operator
opDivide = Ast.DIVIDE

unaryMinus :: Location -> Ast.Exp -> Ast.Exp
unaryMinus loc e = expBinop loc (expInt loc "0") e opMinus

-- | `f(args)` ; `this.m(..)` is the bare call `m(..)` ( like java )
expCall :: Location -> Ast.Exp -> [ Ast.Exp ] -> Ast.Exp
expCall loc callee callArgs = call loc (thisless callee) callArgs

call :: Location -> Ast.Exp -> [ Ast.Exp ] -> Ast.Exp
call loc callee callArgs = Ast.ExpCall $ Ast.ExpCallContent {
    Ast.callee = callee,
    Ast.args = callArgs,
    Ast.expCallLocation = loc
}

thisless :: Ast.Exp -> Ast.Exp
thisless (Ast.ExpVar (Ast.ExpVarContent (Ast.VarField (Ast.VarFieldContent lhs (Token.FieldName m) _))))
    | isThis lhs = expvarify m
thisless callee = callee

isThis :: Ast.Exp -> Bool
isThis (Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named "this" _)))))) = True
isThis _ = False

-- | `new T(args)` is the call `T(args)` ( like the other grammars )
expNew :: Location -> Ast.Var -> [ Ast.Exp ] -> Ast.Exp
expNew loc typ callArgs = call loc (expVar typ) callArgs

-- | `new T()` / `new T`
expNewNoArgs :: Location -> Ast.Var -> Ast.Exp
expNewNoArgs loc typ = expNew loc typ []

-- | `new T { A = x }` : the initializer is the last argument
expNewInit :: Location -> Ast.Var -> Ast.Exp -> Ast.Exp
expNewInit loc typ initializer = expNew loc typ [ initializer ]

-- | `new T(args) { A = x }`
expNewArgsInit :: Location -> Ast.Var -> [ Ast.Exp ] -> Ast.Exp -> Ast.Exp
expNewArgsInit loc typ ctorArgs initializer = expNew loc typ (ctorArgs ++ [ initializer ])

-- | implicit object creation with initializer: new() { ... }
implicitNewArgsInit :: Location -> [ Ast.Exp ] -> Ast.Exp -> Ast.Exp
implicitNewArgsInit loc ctorArgs initializer = instrumentationCall "new_implicit" loc (ctorArgs ++ [ initializer ])

-- | instrumented typeof: typeof(T)
typeof :: Location -> Ast.Var -> Ast.Exp
typeof loc typ = instrumentationCall "typeof" loc [ expVar typ ]

-- | `x => body` : direct dhscanner subtree creation, Ast.ExpLambda
simpleLambda :: Location -> Ast.Param -> Ast.Stmt -> Ast.Exp
simpleLambda loc p = parenthesizedLambda loc [ p ]

-- | `(a, b) => body` : direct dhscanner subtree creation, Ast.ExpLambda
-- an expression body ( `x => f(x)` ) is the lambda's return value
parenthesizedLambda :: Location -> [ Ast.Param ] -> Ast.Stmt -> Ast.Exp
parenthesizedLambda loc ps body = Ast.ExpLambda $ Ast.ExpLambdaContent {
    Ast.expLambdaParams = ps,
    Ast.expLambdaBody = lambdaBody loc body,
    Ast.expLambdaLocation = loc
}

lambdaBody :: Location -> Ast.Stmt -> [ Ast.Stmt ]
lambdaBody loc (Ast.StmtExp e) = [ stmtReturn loc e ]
lambdaBody _ body = unblock body


-- | `{ A = x, B = y }` : one Ast.ExpKwArg per member
dictify :: Location -> [ Ast.Exp ] -> Ast.Exp
dictify = instrumentationCall "dictify"

-- | collection expression: [a, b, c]
collection :: Location -> [ Ast.Exp ] -> Ast.Exp
collection = instrumentationCall "collection"

-- **************
-- *            *
-- * statements *
-- *            *
-- **************
stmtExp :: Ast.Exp -> Ast.Stmt
stmtExp = Ast.StmtExp

stmtBlock :: Location -> [ Ast.Stmt ] -> Ast.Stmt
stmtBlock loc stmts = Ast.StmtBlock $ Ast.StmtBlockContent {
    Ast.stmtBlockContent = stmts,
    Ast.stmtBlockLocation = loc
}

unblock :: Ast.Stmt -> [ Ast.Stmt ]
unblock (Ast.StmtBlock (Ast.StmtBlockContent stmts _)) = stmts
unblock stmt = [ stmt ]

stmtAssign :: Ast.Var -> Ast.Exp -> Ast.Stmt
stmtAssign lhs rhs = Ast.StmtAssign $ Ast.StmtAssignContent {
    Ast.stmtAssignLhs = lhs,
    Ast.stmtAssignRhs = rhs
}

-- | `x += y` lowered to `x = x + y`
addAssign :: Location -> Ast.Var -> Ast.Exp -> Ast.Stmt
addAssign loc lhs rhs = stmtAssign lhs (expBinop loc (expVar lhs) rhs opPlus)

stmtIf :: Location -> Ast.Exp -> Ast.Stmt -> Maybe [ Ast.Stmt ] -> Ast.Stmt
stmtIf loc cond body elseBody = Ast.StmtIf $ Ast.StmtIfContent {
    Ast.stmtIfCond = cond,
    Ast.stmtIfBody = unblock body,
    Ast.stmtElseBody = concatMap unblock (fromMaybe [] elseBody),
    Ast.stmtIfLocation = loc
}

stmtWhile :: Location -> Ast.Exp -> Ast.Stmt -> Ast.Stmt
stmtWhile loc cond body = Ast.StmtWhile $ Ast.StmtWhileContent {
    Ast.stmtWhileCond = cond,
    Ast.stmtWhileBody = unblock body,
    Ast.stmtWhileLocation = loc
}

stmtFor :: Location -> [ Ast.Exp ] -> Ast.Stmt -> Ast.Stmt
stmtFor loc condAndIncs body =
  stmtBlock loc (stmtExp (instrumentationCall "for" loc condAndIncs) : unblock body)

-- | for-loop with a variable declaration initializer: place the declaration
-- before the instrumentation call and then the loop body
stmtForFromVardec :: Location -> Ast.Stmt -> [ Ast.Exp ] -> Ast.Stmt -> Ast.Stmt
stmtForFromVardec loc decl condAndIncs body =
  stmtBlock loc (unblock decl ++ (stmtExp (instrumentationCall "for" loc condAndIncs) : unblock body))

-- | `a?.B.C` / `a?.M(x)` : the member binding ( `.B`, `.M` ) parsed as a plain
-- name, so put the receiver `a` back under its leftmost name : a.B.C / a.M(x)
-- ( the null check itself is dropped )
conditionalAccess :: Ast.Exp -> Ast.Exp -> Ast.Exp
conditionalAccess = rebase

rebase :: Ast.Exp -> Ast.Exp -> Ast.Exp
rebase recv (Ast.ExpVar (Ast.ExpVarContent v)) = Ast.ExpVar (Ast.ExpVarContent (rebaseVar recv v))
rebase recv (Ast.ExpCall c) = Ast.ExpCall c { Ast.callee = rebase recv (Ast.callee c) }
rebase _ e = e

rebaseVar :: Ast.Exp -> Ast.Var -> Ast.Var
rebaseVar recv (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName n))) = fieldVar (Token.location n) recv n
rebaseVar recv (Ast.VarField f) = Ast.VarField f { Ast.varFieldLhs = rebase recv (Ast.varFieldLhs f) }
rebaseVar recv (Ast.VarSubscript s) = Ast.VarSubscript s { Ast.varSubscriptLhs = rebase recv (Ast.varSubscriptLhs s) }

-- | instrumented foreach: a block with a leading instrumentation call and the body
stmtForEach :: Location -> String -> Annotated -> Ast.Exp -> Ast.Stmt -> Ast.Stmt
stmtForEach loc name _typ iterable body =
  stmtBlock loc (stmtExp (instrumentationCall "foreach" loc [ expvarify (named loc name), iterable ]) : unblock body)

stmtForEachVar :: Location -> Ast.Var -> Ast.Exp -> Ast.Stmt -> Ast.Stmt
stmtForEachVar loc v iterable body =
  stmtBlock loc (stmtExp (instrumentationCall "foreach" loc [ expVar v, iterable ]) : unblock body)

-- | instrumented using: a block with a leading instrumentation call and the body
stmtUsingStatement :: Location -> Ast.Exp -> Ast.Stmt -> Ast.Stmt
stmtUsingStatement loc resource body =
  stmtBlock loc (stmtExp (instrumentationCall "using" loc [ resource ]) : unblock body)

-- | using with a variable declaration as resource: instrument, then declaration, then body
stmtUsingStatementFromVardec :: Location -> Ast.Stmt -> Ast.Stmt -> Ast.Stmt
stmtUsingStatementFromVardec loc decl body =
  stmtBlock loc (stmtExp (instrumentationCall "using" loc []) : unblock decl ++ unblock body)

stmtLock :: Location -> Ast.Exp -> Ast.Stmt -> Ast.Stmt
stmtLock loc obj body =
  stmtBlock loc (stmtExp (instrumentationCall "lock" loc [ obj ]) : unblock body)

stmtBreak :: Location -> Ast.Stmt
stmtBreak loc = Ast.StmtBreak (Ast.StmtBreakContent loc)

-- | instrumented continue statement
stmtContinue :: Location -> Ast.Stmt
stmtContinue loc = stmtExp (instrumentationCall "continue" loc [])

stmtReturn :: Location -> Ast.Exp -> Ast.Stmt
stmtReturn loc value = Ast.StmtReturn $ Ast.StmtReturnContent {
    Ast.stmtReturnValue = Just value,
    Ast.stmtReturnLocation = loc
}

stmtReturnVoid :: Location -> Ast.Stmt
stmtReturnVoid loc = Ast.StmtReturn $ Ast.StmtReturnContent {
    Ast.stmtReturnValue = Nothing,
    Ast.stmtReturnLocation = loc
}

stmtYieldReturn :: Location -> Ast.Exp -> Ast.Stmt
stmtYieldReturn loc value = Ast.StmtExp (instrumentationCall "yield" loc [value])

stmtYieldBreak :: Location -> Ast.Stmt
stmtYieldBreak loc = stmtExp (instrumentationCall "yield_break" loc [])

-- | instrumented like java : the thrown value stays reachable
stmtThrow :: Location -> Ast.Exp -> Ast.Stmt
stmtThrow loc value = Ast.StmtExp (instrumentationCall "throw" loc [value])

-- | rethrow ("throw;" with no expression)
stmtRethrow :: Location -> Ast.Stmt
stmtRethrow loc = Ast.StmtExp (instrumentationCall "throw" loc [])

-- | `switch (e) { case a: s .. default: s .. }` is a block of the scrutinee,
-- the case labels and the section bodies ( each section's labels first )
stmtSwitch :: Location -> Ast.Exp -> [ [ Ast.Stmt ] ] -> Ast.Stmt
stmtSwitch loc scrutinee sections = stmtBlock loc (Ast.StmtExp scrutinee : concat sections)

-- | `case e: s1, s2, ...`
caseSection :: Ast.Exp -> [ Ast.Stmt ] -> [ Ast.Stmt ]
caseSection label bodies = Ast.StmtExp label : concatMap unblock bodies

-- | multiple case labels sharing the same body: `case a: case b: s1, s2, ...`
caseSectionMulti :: [ Ast.Exp ] -> [ Ast.Stmt ] -> [ Ast.Stmt ]
caseSectionMulti labels bodies = map stmtExp labels ++ concatMap unblock bodies

-- | `default: s ..`
defaultSection :: [ Ast.Stmt ] -> [ Ast.Stmt ]
defaultSection = concatMap unblock

-- | instrumented enum declaration: tag [enum] with enum name and member names
stmtEnum :: Location -> String -> [String] -> Ast.Stmt
stmtEnum loc name members = stmtExp (instrumentationCall "enum" loc (expStr loc name : map (expStr loc) members))

-- ***********************
-- *                     *
-- * instrumentationCall *
-- *                     *
-- ***********************
-- the standard instrumented shape : a call to `<dhscanner-instrumentation>[<tag>]`
-- | instrumented coalesce assignment: x ??= y
-- | `x ??= y` : direct dhscanner subtree creation, Ast.ExpAssign x = y
-- ( the null check is dropped : y may flow into x )
coalesceAssign :: Location -> Ast.Var -> Ast.Exp -> Ast.Exp
coalesceAssign loc lhs rhs = Ast.ExpAssign $ Ast.ExpAssignContent {
    Ast.expAssignLhs = lhs,
    Ast.expAssignRhs = rhs,
    Ast.expAssignLocation = loc
}

instrumentationCall :: String -> Location -> [ Ast.Exp ] -> Ast.Exp
instrumentationCall tag loc callArgs = call loc (expvarify (named loc ("<dhscanner-instrumentation>[" ++ tag ++ "]"))) callArgs
