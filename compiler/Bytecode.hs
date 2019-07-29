module Bytecode where

import Data.Map as M
import Data.Set as S
import Data.List as L

-- Some meaningful type names, for readability
type Arity = Int
type StackOffset = Int
type InstrAddr = Int
type NumAtoms = Int
type PopAmount = Int

-- An instruction pointer before linking is a label,
-- and after linking is an address
data InstrPtr =
    InstrLabel String
  | InstrAddr InstrAddr
  deriving (Eq, Ord, Show)

-- Atoms are words residing on the stack and heap
data Atom =
    FUN InstrPtr
  | INT Int
  | ATOM String
  | PTR PtrKind NumAtoms Int
  deriving (Eq, Ord, Show)

-- Pointers can point to partial applications, tuples, and cons cells
data PtrKind = PtrApp Arity | PtrTuple | PtrCons
  deriving (Eq, Ord, Show)

-- Primitive operators
data Prim =
    PrimAdd
  | PrimSub
  | PrimAddImm Int
  | PrimSubImm Int
  | PrimEq
  | PrimNotEq
  | PrimLess
  | PrimLessEq
  deriving Show

-- Instruction set
data Instr =
    LABEL String
  | PUSH Atom
  | CALL InstrPtr
  | ICALL
  | COPY StackOffset
  | JUMP InstrPtr
  | IJUMP
  | SLIDE PopAmount NumAtoms
  | SLIDE_JUMP PopAmount NumAtoms InstrPtr
  | RETURN PopAmount
  | LOAD Bool
  | STORE NumAtoms PtrKind
  | BRANCH BranchCond PopAmount InstrPtr
  | PRIM Prim
  | HALT
  deriving Show

-- Branch conditions
type BranchCond = (Polarity, BCond)
data Polarity = Pos | Neg deriving (Eq, Ord, Show)

data BCond =
    IsAtom String
  | IsInt Int
  | IsCons
  | IsTuple NumAtoms
  | IsApp Arity
  deriving (Eq, Ord, Show)

-- Replace labels with addresses
link :: [Instr] -> [Instr]
link instrs = L.map replace (dropLabels instrs)
  where
    -- Compute mapping from labels to addresses
    compute i [] = []
    compute i (LABEL s:rest) = (s, InstrAddr i) : compute i rest
    compute i (instr:rest) = compute (i+1) rest

    -- Mapping from labels to addresses
    toAddr = M.fromList (compute 0 instrs)

    -- Determine address for given label
    resolve s =
      case M.lookup s toAddr of
        Nothing -> error ("link: unknown label " ++ s)
        Just addr -> addr

    -- Drop all labels
    dropLabels [] = []
    dropLabels (LABEL s:rest) = dropLabels rest
    dropLabels (i:is) = i : dropLabels is

    -- Replace labels with addresses
    replace (PUSH (FUN (InstrLabel s))) = PUSH (FUN (resolve s))
    replace (CALL (InstrLabel s)) = CALL (resolve s)
    replace (SLIDE_JUMP n m (InstrLabel s)) = SLIDE_JUMP n m (resolve s)
    replace (JUMP (InstrLabel s)) = JUMP (resolve s)
    replace (BRANCH c n (InstrLabel s)) = BRANCH c n (resolve s)
    replace other = other

-- Determine all atoms used
atoms :: [Instr] -> [String]
atoms is = S.toList (S.unions (L.map get is) `S.union` reserved)
  where
    reserved = S.fromList ["true", "false", "[]"]
    get (PUSH (ATOM a)) = S.singleton a
    get (BRANCH (_, IsAtom a) _ _) = S.singleton a
    get other = S.empty
