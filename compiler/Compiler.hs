module Compiler where

-- Standard imports
import Data.Char
import Data.List
import Monad.Fresh
import Control.Monad
import qualified Data.Map as M
import qualified Monad.WriterFresh as WF

-- Local imports
import Syntax
import Descend
import Module
import Bytecode

-- Transformation passes to core
-- =============================

-- List [e0, e1, ...] -> Cons e0 (Cons e1 ...)
desugarList :: [Decl] -> [Decl]
desugarList = onExp list
  where
    list (List []) = Atom "[]"
    list (List (x:xs)) = Cons (list x) (list (List xs))
    list other = descend list other

-- Replace Id constructors with more specific Atom/Var/Fun constructors
removeId :: [Decl] -> [Decl]
removeId ds = onExp rem ds
  where
    funs = foldr ins M.empty [(f, length args) | FunDecl f args g rhs <- ds]
    ins (f, n) m =
      case M.lookup f m of
        Nothing -> M.insert f n m
        Just n2 -> if n == n2 then m else error $
                     "Function " ++ f ++ " declared with different arities"

    rem (Id id)
      | isUpper (head id) = Var id
      | otherwise =
          case M.lookup id funs of
            Nothing -> Atom id
            Just n -> Fun id n
    rem other = descend rem other

-- Replace all if expressions with cond expressions
removeIf :: [Decl] -> [Decl]
removeIf ds = onExp rem ds
  where
    rem (If alts) = toCond alts
    rem other = descend rem other

    toCond [] = Apply (Fun "$ifFail" 0) []
    toCond [(cond, rhs0), (Atom "true", rhs1)] =
      Cond (rem cond) (map rem rhs0) (map rem rhs1)
    toCond ((cond, rhs):alts) = 
      Cond (rem cond) (map rem rhs) [toCond alts]

-- Extract free variables from an expression
free :: Exp -> [Id]
free (Var v) = [v]
free (Case e alts) = free e `union` foldr union []
  [ (maybe [] free g `union` freeSeq es) \\ free p
  | (p, g, es) <- alts ]
free (Lambda eqns) = foldr union []
  [ (maybe [] free g `union` freeSeq es) \\
      foldr union [] (map free ps)
  | (ps, g, es) <- eqns ]
free (Bind p e) = free e
free (Cond e s0 s1) = free e `union` freeSeq s0 `union` freeSeq s1
free e = extract free e

-- Extract free variables from an expression sequence
freeSeq :: [Exp] -> [Id]
freeSeq [] = []
freeSeq (Bind p e : es) = free e `union` (freeSeq es \\ free p)
freeSeq (e : es) = free e `union` freeSeq es

-- Replace lambda expressions with applications of top-level functions
lambdaLift :: [Decl] -> [Decl]
lambdaLift ds = ds' ++ new
  where
    (_, new, ds') = WF.runWF (mapM liftDecl ds) "\\" 0

    liftDecl :: Decl -> WF.WriterFresh Decl Decl
    liftDecl (FunDecl f ps g rhs) = do
      g' <- case g of
              Nothing -> return g
              Just e -> Just <$> bottomupM lift e
      rhs' <- mapM (bottomupM lift) rhs
      return  (FunDecl f ps g' rhs')
    liftDecl other = return other

    lift :: Exp -> WF.WriterFresh Decl Exp
    lift (Lambda eqns) = do
        f <- WF.fresh
        let vs = free (Lambda eqns)
        WF.writeMany [ClosureDecl f vs ps g body | (ps, g, body) <- eqns]
        let n = length vs + getArity eqns
        return (Apply (Fun f n) (map Var vs))
    lift e = return e

    getArity :: [([Exp], Guard, [Exp])] -> Int
    getArity eqns
      | all (== head ns) ns = head ns
      | otherwise = error "Lambda equations have different arities"
      where ns = [length ps | (ps, g, body) <- eqns]

-- Replace unapplied functions with lambdas
insertLambdas :: [Decl] -> [Decl]
insertLambdas = onExp ins
  where
    ins (Apply f es) = Apply f (map ins es)
    ins (Fun f n) = Lambda [(vs, Nothing, [Apply (Fun f n) vs])]
      where vs = [Var ("V" ++ show i) | i <- [1..n]]
    ins other = descend ins other

-- Replace "++" with "prelude:append"
desugarAppend :: [Decl] -> [Decl]
desugarAppend = onExp app
  where
    app (Fun "++" 2) = Fun "prelude:append" 2
    app other = descend app other

-- Desugar list comprehensions
desugarListComp :: [Decl] -> [Decl]
desugarListComp = onExp listComp
  where
    listComp (ListComp e stmts) = comp stmts
      where
        comp [] = Cons (listComp e) (Atom "[]")
        comp (ListCompBind p gen : rest) =
            Apply (Fun "prelude:concatMap" 2) [ok, listComp gen]
          where
            ok = Lambda [ ([p], Nothing, [comp rest])
                        , ([Var "Other"], Nothing, [Atom "[]"]) ]
        comp (ListCompGuard g : rest) =
          If [(listComp g, [comp rest]), (Atom "true", [Atom "[]"])]
    listComp other = descend listComp other

-- Desugar list enumerations
desugarListEnum :: [Decl] -> [Decl]
desugarListEnum = onExp enum
  where
    enum (ListEnum from to) =
      Apply (Fun "prelude:enumFromTo" 2) [enum from, enum to]
    enum other = descend enum other

-- Desugar boolean operators
desugarBool :: [Decl] -> [Decl]
desugarBool = onExp bool
  where
    bool (Apply (Fun "and" 2) [x, y]) =
      If [(bool x, [bool y]), (Atom "true", [Atom "false"])]
    bool (Apply (Fun "or" 2) [x, y]) =
      If [(bool x, [Atom "true"]), (Atom "true", [bool y])]
    bool other = descend bool other

-- Return core E-lite program
core :: String -> [Decl] -> [Decl]
core modName =
    lambdaLift
  . insertLambdas
  . removeUnused modName
  . removeIf
  . desugarListComp
  . removeId
  . desugarList
  . desugarListEnum
  . desugarAppend
  . desugarBool

-- Stack environment
-- =================

-- An environment keeps track of which variables are
-- in scope, and their posistions on the stack.
-- It also tracks scope boundaries, hence the nested list.
type Env = [[Id]]

-- Create new scope on stack
newScope :: Env -> Env
newScope env = []:env

-- Push variables onto stack
push :: Env -> [Id] -> Env
push (s:ss) ids = (ids ++ s):ss

-- Determine stack offset of given variable
get :: Env -> Id -> Int
get env id =
  case elemIndex id (concat env) of
    Nothing -> error ("Unbound variable " ++ id)
    Just i -> i

-- Replace variable on stack
replace :: Env -> Id -> Id -> Env
replace [] v w = error ("replace: unbound variable " ++ v)
replace (s:ss) v w =
  case rep s of
    Nothing -> s : replace ss v w
    Just s' -> s' : ss
  where
    rep [] = Nothing
    rep (x:xs)
      | x == v = Just (w : xs)
      | otherwise = (x:) <$> rep xs
    
-- Determine number of elements to pop when returning from current scope
scopeSize :: Env -> Int
scopeSize (s:ss) = length s

-- Determine stack size
stackSize :: Env -> Int
stackSize = length . concat

-- Anonymous variable
anon :: Id
anon = ""

-- Compilation
-- ===========

compile :: Id -> [Decl] -> [Instr]
compile modName decls =
    snd $ runFresh prog "@" 0
  where
    -- Pre-processed program
    decls' = core modName decls

    -- Compile an expression
    exp :: Env -> Exp -> Fresh [Instr]
    -- Atoms, integers, variables
    exp env (Atom a) =
      return [PUSH (ATOM a)]
    exp env (Int i) =
      return [PUSH (INT (fromInteger i))]
    exp env (Fun f n) =
     return [PUSH (FUN (InstrLabel f) n)]
    exp env (Var v) =
     return [COPY (get env v)]
    -- Lists and tuples
    exp env (Cons e0 e1) = do
      is <- expList env [e0, e1]
      return (is ++ [STORE (Just 2) PtrCons])
    exp env (Tuple es) = do
      is <- expList env es
      return (is ++ [STORE (Just (length es)) PtrTuple])
    -- Application of primitive function
    exp env (Apply (Fun "+" n) [e0, Int i]) =
      prim env (PrimAddImm (fromInteger i)) [e0]
    exp env (Apply (Fun "+" n) [Int i, e0]) =
      prim env (PrimAddImm (fromInteger i)) [e0]
    exp env (Apply (Fun "-" n) [e0, Int i]) =
      prim env (PrimSubImm (fromInteger i)) [e0]
    exp env (Apply (Fun "+" n) [e0, e1]) = prim env PrimAdd [e0, e1]
    exp env (Apply (Fun "-" n) [e0, e1]) = prim env PrimSub [e0, e1]
    exp env (Apply (Fun "==" n) [e0, e1]) = prim env PrimEq [e0, e1]
    exp env (Apply (Fun "/=" n) [e0, e1]) = prim env PrimNotEq [e0, e1]
    exp env (Apply (Fun "<" n) [e0, e1]) = prim env PrimLess [e0, e1]
    exp env (Apply (Fun "<=" n) [e0, e1]) = prim env PrimLessEq [e0, e1]
    exp env (Apply (Fun ">" n) [e0, e1]) = prim env PrimLess [e1, e0]
    exp env (Apply (Fun ">=" n) [e0, e1]) = prim env PrimLessEq [e1, e0]
    -- Saturated application of known function
    exp env (Apply (Fun f n) es)
      | n == length es = do
          is <- expList env es
          return (is ++ [CALL (InstrLabel f) (length es)])
    -- Under-saturated application of known function
    exp env (Apply (Fun f n) es)
      | length es < n = do
          is <- expList env (Fun f n : es)
          return (is ++ [STORE (Just (1 + length es)) PtrApp])
    -- Over-saturated application of known function
    -- Or application of unknown function
    exp env (Apply f es) = do
      is <- expList env (f:es)
      ret <- fresh
      return ([PUSH_RET (InstrLabel ret)] ++ is ++
                [JUMP (InstrLabel "$apply"), LABEL ret])
    -- Conditional expression
    exp env (Cond c e0 e1) = do
      elseLabel <- fresh
      endLabel <- fresh
      is0 <- branchNot env c 0 (InstrLabel elseLabel)
      let env' = push (newScope env) [anon]
      is1 <- seq env' e0 (Just endLabel)
      is2 <- seq env' e1 (Just endLabel)
      return $ is0
            ++ is1
            ++ [ LABEL elseLabel ]
            ++ is2
            ++ [ LABEL endLabel ]
    -- Case expression
    exp env (Case e alts) = do
        endLabel <- fresh
        is <- exp env e
        iss <- mapM (caseAlt endLabel) alts
        return (is ++ concat iss ++
                  [JUMP (InstrLabel "$case_fail"), LABEL endLabel])
      where
        -- Compile case alternative, where subject is on top of stack
        caseAlt endLabel (p, g, body) = do
           subjId <- case p of { Var v -> return v; other -> fresh }
           fail <- fresh
           let failLabel = InstrLabel fail
           let env0 = newScope (push env [subjId])
           (is0, env1) <- match env0 subjId p failLabel
           (is1, env2) <- guard env1 g failLabel
           let env3 = push (newScope env) (head env2 ++ [subjId])
           is2 <- seq env3 body (Just endLabel)
           return (is0 ++ is1 ++ is2 ++ [LABEL fail])

    -- Compile pattern guard
    guard :: Env -> Guard -> InstrPtr -> Fresh ([Instr], Env)
    guard env g failLabel =
      case g of
        Nothing -> return ([], env)
        Just cond -> do
          is <- branchNot env cond (1 + scopeSize env) failLabel
          return (is, push env [anon])

    -- Evalute a list of expressions (each result is pushed onto the stack)
    expList :: Env -> [Exp] -> Fresh [Instr]
    expList env es = do
      let n = length es
      let vs = replicate n anon
      iss <- zipWithM (\e ws -> exp (push env ws) e) (reverse es) (inits vs)
      return (concat iss)

    -- Primitive application
    prim :: Env -> Prim -> [Exp] -> Fresh [Instr]
    prim env p es = do
      is <- expList env es
      return (is ++ [PRIM p])

    -- Branch if expression evaluates to false
    -- (Leaves result of expression on stack)
    branchNot :: Env -> Exp -> Int -> InstrPtr -> Fresh [Instr]
    branchNot env (Apply (Fun "==" 2) [e, Int i]) pop label = do
      is <- exp env e
      return (is ++ [BRANCH (Neg, IsInt (fromInteger i)) pop label])
    branchNot env (Apply (Fun "==" 2) [e, Atom a]) pop label = do
      is <- exp env e
      return (is ++ [BRANCH (Neg, IsAtom a) pop label])
    branchNot env e pop label = do
      is <- exp env e
      return (is ++ [BRANCH (Neg, IsAtom "true") pop label])

    -- Copy given variable to top of stack
    copy :: Env -> Id -> ([Instr], Env)
    copy env v = if i == 0 then ([], env) else ([COPY i], push env [v])
      where i = get env v

    -- Compile pattern matching
    -- Match variable v against given pattern p
    -- Return updated environment containing new variable bindings
    -- On failure, restore stack, and jump to given label
    match :: Env -> Id -> Exp -> InstrPtr -> Fresh ([Instr], Env)
    match env v (Var w) fail =
      return ([], replace env v w)
    match env v (Atom a) fail = do
      let (is0, env0) = copy env v
      let is1 = [BRANCH (Neg, IsAtom a) (scopeSize env0) fail]
      return (is0 ++ is1, env0)
    match env v (Int i) fail = do
      let (is0, env0) = copy env v
      let is1 = [BRANCH (Neg, IsInt (fromInteger i)) (scopeSize env0) fail]
      return (is0 ++ is1, env0)
    match env v (Fun f n) fail =
      error ("Pattern contains function identifier " ++ f)
    match env v (Cons p0 p1) fail = do
      let (is0, env0) = copy env v
      let is1 = [ BRANCH (Neg, IsCons) (scopeSize env0) fail
                , LOAD (Just 2) ]
      v0 <- fresh
      v1 <- fresh
      (is2, env1) <- match (push env0 [v0, v1]) v0 p0 fail
      (is3, env2) <- match env1 v1 p1 fail
      return (is0 ++ is1 ++ is2 ++ is3, env2)
    match env v (Tuple ps) fail = do
      let n = length ps
      let (is0, env0) = copy env v
      let is1 = [ BRANCH (Neg, IsTuple n) (scopeSize env0) fail
                , LOAD (Just n) ]
      ws <- replicateM n fresh
      foldM (\(is, env) (p, w) -> do
                  (instrs, env') <- match env w p fail
                  return (is ++ instrs, env')
            ) (is0 ++ is1, push env0 ws) (zip ps ws)

    -- Compile a sequence of expressions
    seq :: Env -> [Exp] -> Maybe String -> Fresh [Instr]
    -- Return from function
    seq env [] Nothing =
      return [RETURN (stackSize env)]
    -- Return from case alternative
    seq env [] (Just label) =
      return [SLIDE_JUMP (scopeSize env) 1 (InstrLabel label)]
    -- Pattern bindings
    seq env (Bind p e : rest) k = do
      v <- fresh
      is0 <- exp env e
      (is1, env1) <- match (push env [v]) v p (InstrLabel "$bind_fail")
      is2 <- seq env1 rest k
      return (is0 ++ is1 ++ is2)
    -- Tail call of primitive function
    seq env [Apply (Fun f n) es] Nothing
      | isPrim f = 
          if length es /= n
          then error ("Call of primitive " ++ f ++ " with incorrect arity")
          else do
            is <- exp env (Apply (Fun f n) es)
            return (is ++ [RETURN (1 + stackSize env)])
    -- Tail call of known function, with correct number of args
    seq env [Apply (Fun f n) es] Nothing
      | n == length es = do
          is <- expList env es
          return $ is
                ++ [SLIDE_JUMP (stackSize env) n (InstrLabel f)]
    -- Tail call of known function, undersaturated
    seq env [Apply (Fun f n) es] Nothing
      | n > length es = do
          is <- exp env (Apply (Fun f n) es)
          return (is ++ [RETURN (1 + stackSize env)])
    -- Other tail call
    seq env [Apply e es] Nothing = do
      is <- expList env (e:es)
      return $ is 
            ++ [SLIDE (stackSize env) (length (e:es))]
            ++ [JUMP (InstrLabel "$apply")]
    -- Conditional expression (tail context)
    seq env [Cond c e0 e1] k = do
      elseLabel <- fresh
      is0 <- branchNot env c 0 (InstrLabel elseLabel)
      let env' = push env [anon]
      is1 <- seq env' e0 k
      is2 <- seq env' e1 k
      return $ is0
            ++ is1
            ++ [ LABEL elseLabel ]
            ++ is2
    -- Case expression (tail context)
    seq env [Case e alts] k = do
        is <- exp env e
        iss <- mapM caseAlt alts
        return (is ++ concat iss ++ [JUMP (InstrLabel "$case_fail")])
      where
        -- Compile case alternative, where subject is on top of stack
        caseAlt (p, g, body) = do
           subjId <- case p of { Var v -> return v; other -> fresh }
           fail <- fresh
           let failLabel = InstrLabel fail
           let env0 = newScope (push env [subjId])
           (is0, env1) <- match env0 subjId p failLabel
           (is1, env2) <- guard env1 g failLabel
           let env3 = push env (head env2 ++ [subjId])
           is2 <- seq env3 body k
           return (is0 ++ is1 ++ is2 ++ [LABEL fail])
    -- Other
    seq env (e : rest) k = do
      v <- fresh
      is0 <- exp env e
      is1 <- seq (push env [v]) rest k
      return (is0 ++ is1)

    -- Compile a function defined by a list of equations
    fun :: (Id, [([Exp], Guard, [Exp])]) -> Fresh [Instr]
    fun (f, eqns) = do
        is <- concat <$> mapM one eqns
        return ([LABEL f] ++ is ++
                  [JUMP (InstrLabel "$eqn_fail") | not exhaustive])
      where
        -- Compile one equation
        one :: ([Exp], Guard, [Exp]) -> Fresh [Instr]
        one (ps, g, rhs) = do
            fail <- fresh
            let failLabel = InstrLabel fail
            (vs, bs) <- flatten ps
            is <- matcher [[], vs] bs failLabel
            return (is ++ [LABEL fail])
          where
            matcher env [] fail = 
              case g of
                Nothing -> seq env rhs Nothing
                Just cond -> do
                  (is0, env0) <- guard env g fail
                  is1 <- seq env0 rhs Nothing
                  return (is0 ++ is1)
            matcher env ((v, p):rest) fail = do
              (is0, env0) <- match env v p fail
              is1 <- matcher env0 rest fail
              return (is0 ++ is1)

        -- Introduce new variables for pattern arguments
        flatten :: [Exp] -> Fresh ([Id], [(Id, Exp)])
        flatten ps = do
            (ps', bs) <- unzip <$> mapM flat ps
            return (ps', concat bs)
          where
            flat (Var v) = return (v, [])
            flat p = do
              v <- fresh
              return (v, [(v, p)])

        -- Is pattern matching exhasutive (conservative guess)
        exhaustive = or [all isVar args | (args, _, _) <- eqns]
        isVar (Var v) = True
        isVar other = False

    -- Mapping from function name to list of equations
    eqnMap :: M.Map Id [([Exp], Guard, [Exp])]
    eqnMap = M.fromListWith (flip (++)) $
      [ (f, [(args, g, rhs)])
      | FunDecl f args g rhs <- decls' ] ++
      [ (f, [(map Var vs ++ ps, g, rhs)])
      | ClosureDecl f vs ps g rhs <- decls' ]

    -- Compile a program
    prog :: Fresh [Instr]
    prog = do 
      is <- concat <$> mapM fun (M.toList eqnMap)
      return $ [CALL (InstrLabel (modName ++ ":start")) 0]
            ++ [HALT]
            ++ builtinApply
            ++ is
            ++ [LABEL "$bind_fail"]
            ++ [LABEL "$case_fail"]
            ++ [LABEL "$eqn_fail"]
            ++ [LABEL "$apply_fail"]

    -- To implement curried function application
    builtinApply :: [Instr]
    builtinApply =
      [ LABEL "$apply"
      ,   CAN_APPLY
      ,   BRANCH (Neg, IsApplyPtr) 0 (InstrLabel "$apply_done")
      ,   LOAD Nothing
      ,   JUMP (InstrLabel "$apply")
      , LABEL "$apply_done"
      ,   BRANCH (Neg, IsApplyDone) 0 (InstrLabel "$apply_exact")
      ,   RETURN 1
      , LABEL "$apply_exact"
      ,   BRANCH (Neg, IsApplyExact) 0 (InstrLabel "$apply_too_many")
      ,   IJUMP
      , LABEL "$apply_too_many"
      ,   BRANCH (Neg, IsApplyOver) 0 (InstrLabel "$apply_too_few")
      ,   ICALL
      ,   JUMP (InstrLabel "$apply")
      , LABEL "$apply_too_few"
      ,   BRANCH (Neg, IsApplyUnder) 0 (InstrLabel "$apply_fail")
      ,   STORE Nothing PtrApp
      ,   RETURN 1
      ]
