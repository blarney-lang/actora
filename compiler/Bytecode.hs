module Bytecode where

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
    FUN InstrPtr Arity
  | INT Int
  | ATOM String
  | PTR PtrKind Int
  deriving (Eq, Ord, Show)

-- Pointers can point to partial applications, tuples, and cons cells
data PtrKind = PtrApp | PtrTuple | PtrCons
  deriving (Eq, Ord, Show)

-- Primitive operators
data Prim = PrimAdd | PrimSub | PrimEq | PrimLess | PrimLessEq
  deriving Show

-- Instruction set
data Instr =
    LABEL String
  | PUSH Atom
  | PUSH_RET InstrPtr
  | COPY StackOffset
  | JUMP InstrPtr
  | SLIDE PopAmount InstrPtr
  | RETURN PopAmount
  | EVAL
  | LOAD NumAtoms
  | STORE NumAtoms PtrKind
  | BRANCH BranchCond PopAmount InstrPtr
  | PRIM Prim
  deriving Show

-- Branch conditions
data BranchCond =
    IsNotAtom String
  | IsNotInt Int
  | IsNotCons
  | IsNotTuple
  | IsLoadFailure
  deriving (Eq, Ord, Show)
