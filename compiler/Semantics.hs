-- Stack machine semantics

module Semantics where

import StackIR 
import Data.Maybe
import Prelude as P
import Data.Map as M
import Data.List as L
import Data.Bits as B
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
type ReturnStack = [InstrAddr]

-- Flag register
data Flags =
  Flags {
    -- Application has terminated
    flagHalt :: Maybe ErrorCode
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
    -- Set upper bits
    SETU val -> (pc+1, i, h, top:tail s, r, fs)
      where
        top = case head s of
                INT x -> INT ((val `shiftL` 16) .|. (x .&. 0xffff))
                ATOM x -> ATOM x
                FUN (InstrAddr x) ->
                  FUN (InstrAddr ((val `shiftL` 16) .|. (x .&. 0xffff)))
    -- Function call
    CALL (InstrAddr a) -> (a, i, h, s, (pc+1):r, fs)
    -- Indirect function call
    ICALL ->
      let FUN (InstrAddr a) : rest = s in
        (a, i, h, rest, (pc+1):r, fs)
    -- Indirect jump
    IJUMP ->
      case s of
        FUN (InstrAddr a) : rest -> (a, i, h, rest, r, fs)
        other -> error "EJumpAddr"
    -- Push from stack
    COPY n -> (pc+1, i, h, (s!!n):s, r, fs)
    -- Direct unconditional jump
    JUMP (InstrAddr a) -> (a, i, h, s, r, fs)
    -- Slide top stack elements
    SLIDE pop n -> 
      (pc+1, i, h, L.take n s ++ L.drop (n+pop) s, r, fs)
    -- Slide top stack elements, and jump to destination
    SLIDE_JUMP pop n (InstrAddr a) -> 
      (a, i, h, L.take n s ++ L.drop (n+pop) s, r, fs)
    -- Return
    RETURN pop ->
      let a:rest = r in
        (a, i, h, head s : L.drop (pop+1) s, rest, fs)
    -- Load construction from the heap onto the stack
    LOAD pop -> (pc+1, i, h, s', r, fs)
      where
        PTR k m p : rest = s
        Just as = h S.!? p
        s' = if pop then as ++ tail s else as ++ s
    -- Store construction to the heap
    STORE n k -> (pc+1, i, h |> L.take n s, PTR k n p:L.drop n s, r, fs)
      where
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
            IsApp n -> not $ L.null [() | PTR (PtrApp m) _ _ <- [top], n == m]
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
            PrimGreaterEq -> (2, if x >= y then ATOM "true" else ATOM "false")
    -- Halt
    HALT err -> (pc, i, h, s, r, fs { flagHalt = Just err })

-- Run the program, and return result rendered as a string
run :: [Instr] -> String
run instrs = exec initial
  where
    instrs' = fst (link instrs)
    numInstrs = P.length instrs'

    -- Instruction memory
    instrMem = listArray (0, numInstrs) instrs'

    -- Initial state of flags register
    flags = 
      Flags {
        flagHalt = Nothing
      }

    -- Initial state of abstract machine
    initial = (0, instrMem, S.empty, [], [], flags)

    exec state@(pc, i, h, s, r, fs) =
      case flagHalt fs of
        Nothing -> exec (step state)
        Just "ENone" -> render h (head s)
        Just code -> error code

    render h (INT i) = show i
    render h (ATOM s) = s
    render h (FUN f) = "FUN"
    render h (PTR k n p) =
      case k of
        PtrApp n -> render h (head atoms) ++ "/" ++ show n ++
                      "(" ++ concat (L.intersperse ", "
                        (L.map (render h) (tail atoms))) ++ ")"
        PtrCons -> "[" ++ render h (atoms !! 0) ++ "|"
                       ++ render h (atoms !! 1) ++ "]"
        PtrTuple -> "{" ++ concat (L.intersperse ", "
                             (L.map (render h) atoms)) ++ "}"
      where Just atoms = h S.!? p
