module Core where

-- Blarney imports
import Blarney
import Blarney.RAM
import Blarney.Queue
import Blarney.Stream

-- Local imports
import Stack
import Types
import Decode

-- Circuit-generator parameters
#include <Config.h>

-- Core implementation
-- ===================

makeCore :: Stream (Bit 8) -> Module (Stream (Bit 8))
makeCore debugIn = do
  -- Instruction memory
  instrMem :: RAM InstrPtr Instr <- makeRAMInit "instrs.hex"

  -- Value stack
  stk :: Stack LogStackSize Cell <- makeStack

  -- Return stack
  rstk :: Stack LogRetStackSize InstrPtr <- makeStack

  -- Heap storing pairs of cells
  (heap :: RAM HeapPtr (Cell, Cell), heap2) <- makeTrueDualRAM

  -- Debug output queue
  debugOut :: Queue (Bit 8) <- makeShiftQueue 1

  -- Program counter
  pc :: Reg InstrPtr <- makeReg ones

  -- Pointer to next instruction to fetch
  -- (Defaults to pc.val + 1)
  nextPC :: Wire InstrPtr <- makeWire (pc.val + 1)

  -- Trigger execute stage
  execute :: Reg (Bit 1) <- makeDReg false

  -- Instruction register
  instrReg :: Reg Instr <- makeReg dontCare

  -- Pointer to end of heap
  hp :: Reg (Bit LogHeapSize) <- makeReg 0

  -- Stall pipeline
  stall :: Wire (Bit 1) <- makeWire false
  let stallReg = reg false (stall.val)

  -- Temporary state for Slide/Return instruction
  retAddr :: Reg InstrPtr <- makeReg dontCare
  slideCount <- makeReg dontCare

  -- Temporary registers for Slide instruction
  -- (Storing copies of the top two stack elements)
  slide1 :: Reg Cell <- makeReg dontCare
  slide2 :: Reg Cell <- makeReg dontCare

  -- Temporary state for Load instruction
  loadOdd :: Reg (Bit 1) <- makeReg dontCare
  loadPtr :: Reg HeapPtr <- makeReg dontCare
  loadCount :: Reg (Bit 6) <- makeReg 0

  -- Temporary state for Store instruction
  storeLen :: Reg (Bit 6) <- makeReg dontCare
  doStore :: Wire (Bit 1) <- makeWire false

  -- Temporary state for BranchPop instruction
  takeBranch :: Reg (Bit 1) <- makeReg dontCare

  -- Stage 1: Fetch
  -- ==============

  -- Fetch next instruction
  always do
    load instrMem (nextPC.val)

    -- When not stalling, trigger execute stage and update PC
    when (stall.val.inv) do
      execute <== true
      pc <== nextPC.val

  let instr = instrMem.out

  -- Stage 2: Execute
  -- ================

  -- First (any maybe only) cycle of instruction execution
  always do
    when (execute.val) do
      -- Save instruction, in case it takes multiple cycles
      instrReg <== instr

      -- Push instruction
      when (instr.isPush) do
        push1 stk (instr.getPushVal)

      -- Copy instruction
      when (instr.isCopy) do
        copy stk (instr.operand.truncate)

      -- Jump/IJump/Call/ICall instruction
      when (instr.isControl) do
        when (instr.isJump.inv) do
          push1 rstk (pc.val + 1)
        nextPC <== instr.isIndirect ?
          (stk.top1.content.truncate, instr.operand.truncate)

      -- Slide/Return instruction
      when (instr.isSlide) do
        slide1 <== stk.top1
        slide2 <== stk.top2
        pop stk (instr.getSlideDist.zeroExtend)
        slideCount <== instr.getSlideLen
        retAddr <== rstk.top1
        stall <== true

      -- Load instruction
      when (instr.isLoad) do
        load heap (stk.top1.getObjectPtr)
        loadPtr <== stk.top1.getObjectPtr - 1
        loadOdd <== index @0 (stk.top1.getObjectLen)
        loadCount <== (0 :: Bit 1) # (stk.top1.getObjectLen.truncateLSB)
        stall <== true
        when (instr.isLoadPop) do
          pop stk 1

      -- Store instruction
      when (instr.isStore) do
        storeLen <== instr.getStoreLen
        stall <== true

      -- BranchPop instruction
      when (instr.isBranchPop) do
        let i :: BranchPop = unpack instr
        let tagOk = stk.top1.tag .==. i.branchTag
        let valOk =
              select [
                0b000 --> true
              , 0b001 --> stk.top1.content .==. i.branchArg.signExtend
              , 0b010 --> stk.top1.content .==. i.branchArg.zeroExtend
              , 0b100 --> true
              , 0b101 --> stk.top1.getObjectLen .==. i.branchArg
              , 0b110 --> stk.top1.getClosureArity .==. i.branchArg
              ]
        let cond = (tagOk .&. valOk) .^. i.branchNeg
        takeBranch <== cond
        stall <== true

      -- Primitive instruction
      when (instr.isPrim) do
        pop stk 1

        -- Arithmetic instruction
        when (instr.isArith) do
          let op1 = stk.top1.content
          let op2 = instr.isImm ? (stk.top2.content, instr.operand.signExtend)
          let result =
                select [
                  (instr.isAdd .|. instr.isSub) -->
                    (instr.isAdd ? (op1, op1.inv)) + op2 +
                      zeroExtend (instr.isAdd)
                , instr.isSetUpper -->
                    (instr.operand # range @15 @0 op1)
                ]
          push1 stk ((stk.top1) { content = result })
        
      -- Halt instruction
      when (instr.isHalt) do
        if debugOut.notFull
          then enq debugOut (instr.operand.truncate)
          else stall <== true

  -- Non-first cycle of instruction execution
  always do
    when stallReg do
      -- Slide/Return instruction
      when (instrReg.val.isSlide) do
        if slideCount.val .<=. 2
          then do
            push1 stk (slide1.val)
            when (index @1 (slideCount.val)) do
              push2 stk (slide2.val)
            when (instrReg.val.isReturn) do
              pop rstk 1
              nextPC <== retAddr.val
          else do
            copy stk (instrReg.val.getSlideLen.signExtend)
            slideCount <== slideCount.val - 1
            stall <== true

      -- Load instruction
      when (instrReg.val.isLoad) do
        let (cell1, cell2) = heap.out
        -- Push first cell
        push1 stk cell1
        -- Push second cell, if it exists
        when (loadOdd.val.inv) do
          push2 stk cell2
        -- Stall if load is incomplete
        when (loadCount.val .!=. 0) do
          stall <== true
        -- Load next cell pair
        load heap (loadPtr.val)
        loadOdd <== false
        loadPtr <== loadPtr.val - 1
        loadCount <== loadCount.val - 1

      -- Store instruction
      when (instrReg.val.isStore) do
        storeLen <== storeLen.val - 2
        -- Write top two elements to heap
        store heap (hp.val.truncate) (stk.top1, stk.top2)
        hp <== hp.val + 1
        -- Pop top element(s) from stack
        let popAmount = storeLen.val .==. 1 ? (1, 2)
        pop stk popAmount
        -- Push the pointer on the last cycle
        if storeLen.val .<=. 2
        then do
          let ptr = makeStorePtr (instrReg.val) (hp.val.truncate)
          push1 stk ptr
        else do
          stall <== true

      -- BranchPop instruction
      when (instrReg.val.isBranchPop) do
        when (takeBranch.val) do
          let i :: BranchPop = unpack (instrReg.val)
          pop stk (i.branchPop.zeroExtend)
          nextPC <== i.branchOffset.zeroExtend

  return (debugOut.toStream)

-- Simulation version
-- ==================

makeCoreSim :: Module ()
makeCoreSim = do
  debugOut :: Stream (Bit 8) <- makeCore nullStream

  -- Consume output, display, and finish simulation
  always do
    when (debugOut.canPeek) do
      debugOut.consume
      display "Exit code: %d" (debugOut.peek)
      finish

-- Code generation
-- ===============

genCore :: IO ()
genCore = do
  writeVerilogTop
    makeCoreSim
    "CoreSim"
    "CoreSim-Verilog/"
  writeVerilogModule
    makeCore
    "Core"
    "Core-Verilog/"
