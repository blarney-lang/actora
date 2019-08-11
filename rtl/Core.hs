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

  -- Program counter
  pc :: Reg InstrPtr <- makeReg ones

  -- Pointer to next instruction to fetch
  -- (Defaults to pc.val + 1)
  nextPC :: Wire InstrPtr <- makeWire (pc.val + 1)

  -- Trigger execute stage
  execute :: Reg (Bit 1) <- makeDReg false

  -- Instruction register
  instrReg :: Reg Instr <- makeReg dontCare

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
