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
  heap :: RAM HeapPtr (Cell, Cell) <- makeRAM

  -- Debug output queue
  debugOut :: Queue (Bit 8) <- makeShiftQueue 1

  -- Program counter of instruction in stage 2
  pc2 :: Reg InstrPtr <- makeReg ones

  -- Program counter of instruciton in stage 3
  pc :: Reg InstrPtr <- makeReg dontCare

  -- Stall pipeline
  stall :: Wire (Bit 1) <- makeWire false
  let stallReg = reg false (stall.val)

  -- Unconditional jump destination
  jump :: Wire InstrPtr <- makeWire dontCare

  -- Pointer to next instruction to fetch
  nextPC :: Wire InstrPtr <-
    makeWire (jump.active ? (jump.val, pc2.val + (stall.val ? (0, 1))))

  -- Trigger decode stage
  decode :: Reg (Bit 1) <- makeReg false

  -- Trigger execute stage
  execute :: Reg (Bit 1) <- makeDReg false

  -- Instruction register
  instrReg :: Reg Instr <- makeReg dontCare

  -- Pointer to end of heap
  hp :: Reg (Bit LogHeapSize) <- makeReg 0

  -- Temporary state for Slide/Return instruction
  retAddr :: Reg InstrPtr <- makeReg dontCare
  slideCount <- makeReg dontCare
  slideOffset :: Reg StackPtr <- makeReg dontCare

  -- Temporary registers for Slide instruction
  -- (Storing copies of the top two stack elements)
  slide1 :: Reg Cell <- makeReg dontCare
  slide2 :: Reg Cell <- makeReg dontCare

  -- Temporary state for Load instruction
  loadPtr :: Reg HeapPtr <- makeReg dontCare
  loadCount :: Reg (Bit 6) <- makeReg dontCare
  loadOdd :: Reg (Bit 1) <- makeReg dontCare

  -- Temporary state for Store instruction
  storeLen :: Reg (Bit 6) <- makeReg dontCare
  doStore :: Wire (Bit 1) <- makeWire false

  -- Temporary state for BranchPop instruction
  takeBranch :: Reg (Bit 1) <- makeReg dontCare
  doPop :: Reg (Bit 1) <- makeReg dontCare

  -- Stage 1: Fetch
  -- ==============

  -- Flush condition
  let flush = nextPC.active

  -- Fetch next instruction
  always do
    load instrMem (nextPC.val)

    -- When not stalling, enable decode stage and update PC
    when (stall.val.inv) do
      decode <== true
      pc2 <== nextPC.val

    -- When flushing, disable decode stage
    -- Note: must not stall and flush at the same time
    when flush do
      decode <== false

  let instr2 = instrMem.out

  -- Stage 2: Decode
  -- ===============

  always do
    -- When decode stage is enabled
    when (decode.val) do
      -- When not stalling and not flushing, trigger execute stage
      when (stall.val.inv .&. flush.inv) do
        pc <== pc2.val
        execute <== true
        -- Avoid pipeline bubble on unconditional, direct jumps
        when (instr2.isControl .&. instr2.isIndirect.inv) do
          jump <== instr2.operand.truncate

  let instr = instr2.buffer

  -- Stage 3: Execute
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
        when (instr.isIndirect) do
          nextPC <== stk.top1.content.truncate;
          pop stk 1

      -- Slide/Return instruction
      when (instr.isSlide) do
        slide1 <== stk.top1
        slide2 <== stk.top2
        -- Registered sum
        let popAmount = (instr2.getSlideDist +
                         instr2.getSlideLen.zeroExtend).zeroExtend.old
        pop stk popAmount
        slideCount <== instr.getSlideLen
        slideOffset <== instr.getSlideDist.signExtend.inv
        retAddr <== rstk.top1
        stall <== true

      -- Load instruction
      when (instr.isLoad) do
        load heap (stk.top1.getObjectPtr)
        loadPtr <== stk.top1.getObjectPtr - 1
        loadCount <== stk.top1.getObjectLen
        loadOdd <== index @0 (stk.top1.getObjectLen)
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
        let t = stk.top1.tag
        let tagOk = t .==. i.branchTag
        let valOk =
              select [
                t .==. 0b000 --> true
              , t .==. 0b001 --> stk.top1.content .==. i.branchArg.signExtend
              , t .==. 0b010 --> stk.top1.content .==. i.branchArg.zeroExtend
              , t .==. 0b100 --> true
              , t .==. 0b101 --> stk.top1.getObjectLen .==. i.branchArg
              , t .==. 0b110 --> stk.top1.getClosureArity .==. i.branchArg
              ]
        let cond = (tagOk .&. valOk) .^. i.branchNeg
        takeBranch <== cond
        doPop <== i.branchPop .!=. 0
        stall <== true

      -- Primitive instruction
      when (instr.isPrim) do
        let op1 = stk.top1.content
        let op2 = stk.top2.content
        -- Top element will be replaced
        pop stk 2
        -- Add/sub result
        let add1 :: Bit 33 = op1.signExtend
        let add2 :: Bit 33 = op2.signExtend
        -- Registered control bit
        let doAdd = buffer (instr2.isArith .&.
                      instr2.isAddOrSub .&. instr2.isAdd)
        let addSub = add1 + (doAdd ? (add2, add2.inv))
                   + (doAdd ? (0, 1))
        let setUpper = instr.operand # range @15 @0 op1
        -- Comparison result
        let eq = addSub .==. 0
        let lt = index @32 addSub
        -- Overall result
        let result =
              if instr.isArith then 
                instr.isAddOrSub ? (addSub.truncate, setUpper)
              else
                zeroExtend $ select [
                  instr.isEq --> eq .^. (instr.isNegCmp)
                , instr.isLess --> lt .^. (instr.isNegCmp)
                ]
        push1 stk $
          Cell {
            tag = instr.isComparison ? (atomTag, stk.top1.tag)
          , content = result
          }
        
      -- Halt instruction
      when (instr.isHalt) do
        if debugOut.notFull
          then do
            display "Result = %0d" (stk.top1.content)
            display "Stack Size = " (stk.size)
            enq debugOut (instr.operand.truncate)
          else stall <== true

  -- Non-first cycle of instruction execution
  always do
    when stallReg do
      -- Slide/Return instruction
      when (instrReg.val.isSlide) do
        if slideCount.val .<=. 2
          then do
            when (range @1 @0 (slideCount.val) .!=. 0) do
              push1 stk (slide1.val)
            when (index @1 (slideCount.val)) do
              push2 stk (slide2.val)
            when (instrReg.val.isReturn) do
              pop rstk 1
              nextPC <== retAddr.val
          else do
            copy stk (slideOffset.val)
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
        loadOdd <== false
        -- Stall if load is incomplete
        when (loadCount.val .>. 2) do
          stall <== true
        -- Load next cell pair
        load heap (loadPtr.val)
        loadPtr <== loadPtr.val - 1
        loadCount <== loadCount.val - 2

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
          when (doPop.val) do
            pop stk (i.branchPop.zeroExtend)
          nextPC <== pc.val + i.branchOffset.zeroExtend

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
