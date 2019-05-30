-- Stack machine semantics

module Semantics where

import Bytecode
import Data.Maybe
import Prelude as P
import Data.Map as M
import Data.List as L
import Data.Array as A
import Data.Sequence as S

-- Machine state
-- =============

-- Instruction memory
type InstrMem = Array InstrAddr Instr

-- Heap
type Heap = Seq [Atom]

-- Stack
type Stack = [Atom]

-- Return stack
type ReturnStack = [(StackOffset, InstrAddr)]

-- Flag register
data Flags =
  Flags {
    -- Number of items loaded did not match number requested
    flagLoadFail   :: Bool
    -- Trying to apply a pointer
  , flagApplyPtr   :: Bool
    -- Application has reached normal form
  , flagApplyDone  :: Bool
    -- Application has correct number of arguments or oversaturated
  , flagApplyOk    :: Bool
    -- Application is undersaturated
  , flagApplyUnder :: Bool
    -- Application has terminated
  , flagHalt       :: Bool
  }
  deriving Show

-- Program counter
type PC = Int

-- Machine state
type State = (PC, InstrMem, Heap, Stack, ReturnStack, Flags)

-- Small-step semantics
-- ====================

step :: State -> State
step (pc, i, h, s, r, fs) =
  case i A.! pc of
    -- Push onto stack
    PUSH a -> (pc+1, i, h, a:s, r, fs)
    -- Push onto return stack
    PUSH_RET (InstrAddr a) -> (pc+1, i, h, s, (L.length s, a):r, fs)
    -- Function call
    CALL (InstrAddr a) n -> (a, i, h, s, (L.length s - n, pc+1):r, fs)
    -- Indirect function call
    ICALL ->
      let FUN (InstrAddr a) n : rest = s in
        (a, i, h, rest, (L.length s - n - 1, pc+1):r, fs)
    -- Push from stack
    COPY n -> (pc+1, i, h, (s!!n):s, r, fs)
    -- Direct unconditional jump
    JUMP (InstrAddr a) -> (a, i, h, s, r, fs)
    -- Slide top stack elements, and jump to destination
    SLIDE_JUMP pop n (InstrAddr a) -> 
      (a, i, h, L.take n s ++ L.drop (n+pop) s, r, fs)
    -- Return
    RETURN pop ->
      let (sp, a):rest = r in
        (a, i, h, head s : L.drop pop s, rest, fs)
    -- Load construction from the heap onto the stack
    LOAD pop -> (pc+1, i, h, s', r, fs)
      where
        PTR k n p : rest = s
        Just as = h S.!? p
        s' = if pop then as ++ rest else as ++ s
    -- Store construction to the heap
    STORE n k -> (pc+1, i, h |> L.take len s, PTR k len p:L.drop len s, r, fs)
      where
        len = case n of
                Nothing -> L.length s - fst (head r)
                Just m -> m
        p = S.length h
    -- Conditional branch
    BRANCH (polarity, op) pop (InstrAddr a) -> (pc', i, h, s', r, fs)
      where
        top = s!!0
        pc' = if cond' then a else pc+1
        s' = if cond' then L.drop pop s else s
        cond' = if polarity == Neg then not cond else cond
        cond =
          case op of
            IsAtom str -> top == ATOM str
            IsInt i -> top == INT i
            IsCons -> not $ L.null [() | PTR PtrCons _ _ <- [top]]
            IsTuple n -> not $ L.null [() | PTR PtrTuple m _ <- [top], n == m]
            IsApplyPtr -> flagApplyPtr fs
            IsApplyDone -> flagApplyDone fs
            IsApplyOk -> flagApplyOk fs
            IsApplyUnder -> flagApplyUnder fs
    -- Query application on the stack
    CAN_APPLY -> (pc+1, i, h, s, r, fs')
      where
        top:_ = s
        (slen, _):_ = r
        len = L.length s - slen
        isPtrApp = case top of {PTR PtrApp _ _ -> True; other -> False}
        isFun0 = case top of {FUN _ n -> n == 0; other -> False}
        fs' = fs {
          flagApplyPtr = isPtrApp
        , flagApplyDone = not isPtrApp && not isFun0 && len == 1
        , flagApplyOk = case top of {FUN f n -> len > n; other -> False}
        , flagApplyUnder = case top of {FUN f n -> len <= n; other -> False}
        }
    -- Primitive
    PRIM prim -> (pc+1, i, h, res : L.drop n s, r, fs)
      where
        ~(INT x) = s !! 0
        ~(INT y) = s !! 1
        equal (INT x) (INT y) = x == y
        equal (ATOM x) (ATOM y) = x == y
        equal x y = error "Equality on non-primitive type"
        (n, res) =
          case prim of
            PrimAdd -> (2, INT (x+y))
            PrimSub -> (2, INT (x-y))
            PrimAddImm imm -> (1, INT (x+imm))
            PrimSubImm imm -> (1, INT (x-imm))
            PrimEq -> (2, if (s!!0) `equal` (s!!1)
                          then ATOM "true" else ATOM "false")
            PrimNotEq -> (2, if (s!!0) `equal` (s!!1)
                             then ATOM "false" else ATOM "true")
            PrimLess -> (2, if x < y then ATOM "true" else ATOM "false")
            PrimLessEq -> (2, if x <= y then ATOM "true" else ATOM "false")
    -- Halt
    HALT -> (pc, i, h, s, r, fs { flagHalt = True })

-- Run the program, and return result rendered as a string
run :: [Instr] -> String
run instrs = exec initial
  where
    instrs' = link instrs
    numInstrs = P.length instrs'

    -- Instruction memory
    instrMem = listArray (0, numInstrs) instrs'

    -- Initial state of flags register
    flags = 
      Flags {
        flagLoadFail   = False
      , flagApplyPtr   = False
      , flagApplyDone  = False
      , flagApplyOk    = False
      , flagApplyUnder = False
      , flagHalt       = False
      }

    -- Initial state of abstract machine
    initial = (0, instrMem, S.empty, [], [], flags)

    exec (pc, i, h, s, r, fs)
      | flagHalt fs = render h (head s)
    exec state = exec (step state)

    render h (INT i) = show i
    render h (ATOM s) = s
    render h (FUN f n) = "FUN"
    render h (PTR k n p) =
      case k of
        PtrApp -> render h (head atoms) ++ "(" ++ concat (L.intersperse ", "
                    (L.map (render h) (tail atoms))) ++ ")"
        PtrCons -> "[" ++ render h (atoms !! 0) ++ "|"
                       ++ render h (atoms !! 1) ++ "]"
        PtrTuple -> "{" ++ concat (L.intersperse ", "
                             (L.map (render h) atoms)) ++ "}"
      where Just atoms = h S.!? p
