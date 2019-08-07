module Decode where

-- Bit-level representation of instructions and data.
-- See doc/ISA.md for more details.

import Blarney

-- Stack/Heap cells
-- ================

-- Stack/Heap cell type
data Cell = Cell { tag :: Tag, content :: Bit 32 }
  deriving (Generic, Bits, FShow)

-- Tag type
type Tag = Bit 3

-- Tag values
funTag, intTag, atomTag, consTag, tupleTag, closureTag :: Tag
funTag     = 0b000
intTag     = 0b001
atomTag    = 0b010
consTag    = 0b100
tupleTag   = 0b101
closureTag = 0b110

-- Cons, tuple and closure cells all contain heap pointers
isPtr :: Cell -> Bit 1
isPtr cell = index @2 (cell.tag)

-- If it's a pointer, then bits [31:26] of the cell contents
-- hold the length of the object pointed-to.  This field
-- might be enlarged in future to support arrays.
getObjectLen :: Cell -> Bit 6
getObjectLen cell = range @31 @26 (cell.content)

-- If it's a pointer to a closure, bits [25:20] of the cell
-- contents hold the arity of the closure, i.e. how many
-- arguments still need to be applied.
getClosureArity :: Cell -> Bit 6
getClosureArity cell = range @25 @20 (cell.content)

-- Instructions
-- ============

-- 26-bit instructions
type Instr = Bit 26

-- The top 10 bits contains the opcode
opcode :: Instr -> Bit 10
opcode = range @25 @16

-- Many instructions contain a 16-bit operand
operand :: Instr -> Bit 16
operand = range @15 0

-- Is it a Push instruction?
isPush :: Instr -> Bit 1
isPush i = index @25 i .&. range @5 @2 (i.opcode) .==. 0b0000

-- Determine value to push
getPushVal :: Instr -> Cell
getPushVal i =
  Cell {
    tag = range @1 @0 i
    content = signExtend (sign # i.operand)
  }
  where
    sign = index @0 i ? (index @15 i, 0)

-- Is it a Slide or Return instruction?
isSlide :: Instr -> Bit 1
isSlide = index @25 i .&. range @5 @1 (i.opcode) .==. 0b00010

-- Assuming isSlide, is it a Return?
isReturn :: Instr -> Bit 1
isReturn = index @16

-- Determine distance of Slide
getSlideDist :: Instr -> Bit 10
getSlideDist = range @15 @6

-- Determine length of Slide
getSlideLen :: Instr -> Bit 10
getSlideLen = range @5 @0

-- Is it a Copy instruction?
isCopy :: Instr -> Bit 1
isCopy i = index @25 i .&. range @5 @0 (i.opcode) .==. 0b000110

-- Is it a Jump, IJump, Call, or ICall instruction?
isControl :: Instr -> Bit 1
isControl i = index @25 i .&. range @5 @2 (i.opcode) .==. 0b0010

-- Assuming isControl, is it an Jump or IJump?
isJump :: Instr -> Bit 1
isJump = index @17

-- Assuming isControl, is it an IJump or ICall?
isIndirect :: Instr -> Bit 1
isIndirect = index @16

-- Is it a Load instruction?
isLoad :: Instr -> Bit 1
isLoad i = index @25 i .&. range @5 @0 (i.opcode) .==. 0b001101

-- Is it a Store instruction?
isStore :: Instr -> Bit 1
isStore i = index @25 i .&. range @5 @0 (i.opcode) .==. 0b001110

-- Is it a Halt instuction?
isHalt :: Instr -> Bit 1
isHalt i = index @25 i .&. range @5 @0 (i.opcode) .==. 0b001111

-- Is it a primitive function?
isPrim :: Instr -> Bit 1
isPrim = index @25 i .&. index @5 (i.opcode)

-- Assuming isPrim, Is it an arithmetic instruction?
isArith :: Instr -> Bit 1
isArith = inv (index @4 (i.opcode))

-- Assuming isPrim, is it a comparison?
isComparison :: Instr -> Bit 1
isComparison i = index @4 (i.opcode)

-- Assuming isPrim, is second operand an immediate?
isImm :: Instr -> Bit 1
isImm i = index @0 (i.opcode)

-- Assuming isArith, is it an Add?
isAdd :: Instr -> Bit 1
isAdd i = range @3 @1 (i.opcode) .==. 0b000

-- Assuming isArith, is it a Sub?
isSub :: Instr -> Bit 1
isSub i = range @3 @1 (i.opcode) .==. 0b001

-- Assuming isArith, is it a SetUpper?
isSetUpper :: Instr -> Bit 1
isSetUpper i = range @3 @1 (i.opcode) .==. 0b010

-- Assuming isComparison, is it an Eq or NotEq?
isEq :: Instr -> Bit 1
isEq i = index @3 @2 (i.opcode) .==. 0b00

-- Assuming isEq, is it a NotEq?
isNotEq :: Instr -> Bit 1
isNotEq i = index @1 (i.opcode)

-- Assuming isComparison, is it a Less?
isLess :: Instr -> Bit 1
isLess i = range @3 @1 (i.opcode) .==. 0b010

-- Assuming isComparison, is it a LessEq?
isLessEq :: Instr -> Bit 1
isLessEq i = range @3 @1 (i.opcode) .==. 0b011

-- Is it a BranchPop instruction?
isBranchPop :: Instr -> Bit 1
isBranchPop = inv (index @25 i)

-- Format of BranchPop instruction
data BranchPop =
  BranchPop {
    branchOp     :: Bit 1
  , branchNeg    :: Bit 1
  , branchTag    :: Tag
  , branchArg    :: Bit 6
  , branchPop    :: Bit 5
  , branchOffset :: Bit 10
  }
  deriving (Generic, Bits, FShow)
