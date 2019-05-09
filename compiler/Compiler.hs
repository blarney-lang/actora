module Compiler where

import Syntax
import Monads
import Descend
import Bytecode
import Data.Char
import Prelude as P
import Data.Map as M
import Control.Monad
import Data.List as L

-- Preprocessing
-- =============

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
    funs = P.foldr ins M.empty [(f, P.length args) | (f, args, g, rhs) <- ds]
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
      Cond (rem cond) (P.map rem rhs0) (P.map rem rhs1)
    toCond ((cond, rhs):alts) = 
      Cond (rem cond) (P.map rem rhs) [toCond alts]

-- Stack environment
-- =================

-- An environment keeps track of which variables are
-- in scope, and their posistions on the stack.
-- It also tracks scope boundaries, hence the nested list.
type Env = [[Id]]

-- Create new scope on stack
newScope :: Env -> Env
newScope env = []:env

-- Push variable onto stack
push :: Env -> [Id] -> Env
push (s:ss) ids = (ids ++ s):ss

-- Determine stack offset of given variable
get :: Env -> Id -> Int
get env id =
  case L.elemIndex id (concat env) of
    Nothing -> error ("Unbound variable " ++ id)
    Just i -> i

-- Replace variable on stack
replace :: Env -> Id -> Id -> Env
reaplce [] v w = error ("replace: unbound variable " ++ v)
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

compile :: [Decl] -> [Instr]
compile decls =
    snd $ runFresh prog "@" 0
  where
    -- Pre-processing
    decls' = removeIf $ removeId $ desugarList decls

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
    -- Saturated application of known function
    exp env (Apply (Fun f n) es)
      | n <= length es = do
          is <- expList env es
          ret <- fresh
          return (is ++ [CALL (InstrLabel f) (length es)])
    -- Partial application of known function
    exp env (Apply (Fun f n) es)
      | length es < n = do
          is <- expList env (Fun f n : es)
          return (is ++ [STORE (Just (1 + length es)) PtrApp])
    -- Application of unknown function
    exp env (Apply f es) = do
      is <- expList env (f:es)
      ret <- fresh
      return ([PUSH_RET (InstrLabel ret)] ++ is ++
                [JUMP (InstrLabel "$apply"), LABEL ret])
    -- Conditional expression
    exp env (Cond c e0 e1) = do
      elseLabel <- fresh
      endLabel <- fresh
      is0 <- exp env c
      let env' = push (newScope env) [anon]
      is1 <- seq env' e0 (Just endLabel)
      is2 <- seq env' e1 (Just endLabel)
      return $ is0
            ++ [ BRANCH (IsNotAtom "true") 0 (InstrLabel elseLabel) ]
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
           subjId <- fresh
           fail <- fresh
           let failLabel = InstrLabel fail
           let env0 = newScope (push env [subjId])
           (is0, env1) <- match env0 subjId p failLabel
           is1 <- guard env1 g failLabel
           let env2 = push (newScope env) (head env1 ++ [subjId])
           is2 <- seq env2 body (Just endLabel)
           return (is0 ++ is1 ++ is2 ++ [LABEL fail])

    -- Compile pattern guard
    guard :: Env -> Guard -> InstrPtr -> Fresh [Instr]
    guard env g failLabel =
      case g of
        Nothing -> return []
        Just cond -> do
          is <- exp env cond
          return $ is
                ++ [BRANCH (IsNotAtom "true") (scopeSize env) failLabel]

    -- Evalute a list of expressions (each result is pushed onto the stack)
    expList :: Env -> [Exp] -> Fresh [Instr]
    expList env es = do
      let n = length es
      let vs = replicate n anon
      iss <- zipWithM (\e ws -> exp (push env ws) e) (reverse es) (inits vs)
      return (concat iss)

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
      let is1 = [BRANCH (IsNotAtom a) (scopeSize env0) fail]
      return (is0 ++ is1, env0)
    match env v (Int i) fail = do
      let (is0, env0) = copy env v
      let is1 = [BRANCH (IsNotInt (fromInteger i)) (scopeSize env0) fail]
      return (is0 ++ is1, env0)
    match env v (Fun f n) fail =
      error ("Pattern contains function identifier " ++ f)
    match env v (Cons p0 p1) fail = do
      let (is0, env0) = copy env v
      let is1 = [ BRANCH IsNotCons (scopeSize env0) fail, LOAD (Just 2) ]
      v0 <- fresh
      v1 <- fresh
      (is2, env1) <- match (push env0 [v0, v1]) v0 p0 fail
      (is3, env2) <- match env1 v1 p1 fail
      return (is0 ++ is1 ++ is2 ++ is3, env2)
    match env v (Tuple ps) fail = do
      let n = length ps
      let (is0, env0) = copy env v
      let is1 = [ BRANCH IsNotTuple (scopeSize env0) fail
                , LOAD (Just n)
                , BRANCH IsLoadFailure (scopeSize env0) fail ]
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
      return [SLIDE_JUMP (scopeSize env) (InstrLabel label)]
    -- Pattern bindings
    seq env (Bind p e : rest) k = do
      v <- fresh
      is0 <- exp env e
      (is1, env1) <- match (push env [v]) v p (InstrLabel "$bind_fail")
      is2 <- seq env1 rest k
      return (is0 ++ is1 ++ is2)
    -- Tail call (TODO: reclaim heap space)
    seq env [Apply (Fun f n) es] Nothing
      | n == length es = do
          is <- expList env es
          return $ is
                ++ [STORE (Just n) PtrTuple]
                ++ [SLIDE (1 + stackSize env)]
                ++ [LOAD Nothing]
                ++ [JUMP (InstrLabel f)]
    -- Conditional expression (tail context)
    seq env [Cond c e0 e1] k = do
      elseLabel <- fresh
      is0 <- exp env c
      let env' = push env [anon]
      is1 <- seq env' e0 k
      is2 <- seq env' e1 k
      return $ is0
            ++ [ BRANCH (IsNotAtom "true") 0 (InstrLabel elseLabel) ]
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
           subjId <- fresh
           fail <- fresh
           let failLabel = InstrLabel fail
           let env0 = newScope (push env [subjId])
           (is0, env1) <- match env0 subjId p failLabel
           is1 <- guard env1 g failLabel
           let env2 = push env (head env1 ++ [subjId])
           is2 <- seq env2 body k
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
                  is0 <- guard env g fail
                  is1 <- seq env rhs Nothing
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
    eqnMap = M.fromListWith (flip (++)) [ (f, [(args, g, rhs)])
                                        | (f, args, g, rhs) <- decls' ]

    -- Compile a program
    prog :: Fresh [Instr]
    prog = do 
      is <- concat <$> mapM fun (toList eqnMap)
      return $ [CALL (InstrLabel "start") 0]
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
      ,   BRANCH IsNotApplyPtr 0 (InstrLabel "$apply_done")
      ,   LOAD Nothing
      ,   JUMP (InstrLabel "$apply")
      , LABEL "$apply_done"
      ,   BRANCH IsNotApplyDone 0 (InstrLabel "$apply_ok")
      ,   RETURN 1
      , LABEL "$apply_ok"
      ,   BRANCH IsNotApplyOk 0 (InstrLabel "$apply_too_few")
      ,   ICALL
      ,   JUMP (InstrLabel "$apply")
      , LABEL "$apply_too_few"
      ,   BRANCH IsNotApplyUnder 0 (InstrLabel "$apply_fail")
      ,   STORE Nothing PtrApp
      ,   RETURN 1
      ]
