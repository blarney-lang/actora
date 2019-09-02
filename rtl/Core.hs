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

  -- Heap storing pairs of cells
  heap :: RAM HeapPtr (Cell, Cell) <- makeRAM

  -- Scratchpad for copying collector
  scratchpad :: RAM ScratchpadPtr (Cell, Cell) <- makeRAM

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

  -- Condition flag
  condFlag :: Reg (Bit 1) <- makeReg false

  -- Temporary state for Slide/Return instruction
  slideCount <- makeReg dontCare
  slideOffset :: Reg StackPtr <- makeReg dontCare

  -- Temporary registers for Slide instruction
  -- (Storing copies of the top two stack elements)
  slide1 :: Reg Cell <- makeReg dontCare
  slide2 :: Reg Cell <- makeReg dontCare

  -- Temporary state for Load instruction
  loadPtr :: Reg HeapPtr <- makeReg dontCare
  loadCount :: Reg StackPtr <- makeReg dontCare
  loadOdd :: Reg (Bit 1) <- makeReg dontCare

  -- Temporary state for Store instruction
  storeLen :: Reg StackPtr <- makeReg dontCare

  -- Temporary state for BranchPop instruction
  takeBranch :: Reg (Bit 1) <- makeReg dontCare
  doPop :: Reg (Bit 1) <- makeReg dontCare

  -- Temporary state for multicycle primitive
  opReg1 :: Reg Cell <- makeReg dontCare
  opReg2 :: Reg Cell <- makeReg dontCare

  -- Garbage collector state used in CPU pipeline
  gcStart :: Reg (Bit 1) <- makeDReg false
  gcActive :: Reg (Bit 1) <- makeReg false
  gcSavedStoreLen :: Reg StackPtr <- makeReg dontCare
  gcSaveStack :: Reg (Bit 1) <- makeReg false
  gcRestoreStack :: Reg (Bit 1) <- makeReg false
  gcFrontPtr :: Reg ScratchpadPtr <- makeReg dontCare

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

      -- IJump instruction
      when (instr.isControl) do
        when (instr.isIndirect) do
          nextPC <== stk.top1.content.truncate
          pop stk 1

      -- CJumpPop instruction
      when (instr.isCJumpPop) do
        when (condFlag.val) do
          nextPC <== instr.operand.truncate
          let doPop :: Bit 1 = buffer (instr2.getCJumpPop .!=. 0)
          when doPop do
            pop stk (instr.getCJumpPop.zeroExtend)

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
        stall <== true

      -- Load instruction
      when (instr.isLoad) do
        load heap (stk.top1.getObjectPtr)
        loadPtr <== stk.top1.getObjectPtr - 1
        loadCount <== stk.top1.getObjectLen.zeroExtend
        loadOdd <== index @0 (stk.top1.getObjectLen)
        stall <== true
        when (instr.isLoadPop) do
          pop stk 1

      -- Store instruction
      when (instr.isStore) do
        when (hp.val .>=. GCThreshold) do
          gcStart <== true
          gcActive <== true
          gcSavedStoreLen <== instr.getStoreLen.zeroExtend
        stall <== true
        storeLen <== instr.getStoreLen.zeroExtend

      -- Match instruction
      when (instr.isMatch) do
        let t = stk.top1.tag
        let tagOk = t .==. instr.getMatchCond
        let valOk =
              select [
                t .==. 0b000 --> true
              , t .==. 0b001 --> stk.top1.content .==. instr.operand.signExtend
              , t .==. 0b010 --> stk.top1.content .==. instr.operand.zeroExtend
              , t .==. 0b100 --> true
              , t .==. 0b101 --> stk.top1.getObjectLen .==.
                                   instr.operand.truncate
              , t .==. 0b110 --> stk.top1.getClosureArity .==.
                                   instr.operand.truncate
              ]
        let cond = (tagOk .&. valOk) .^. instr.isMatchNeg
        condFlag <== cond

      -- Primitive instruction
      when (instr.isPrim) do
        display "Prim"
        let op1 = stk.top1.content
        let op2 = stk.top2.content
        -- Top element will be replaced
        let popAmount =
             buffer ((instr2.isArith .&. instr2.isSetUpper) ? (1, 2))
        pop stk popAmount
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
 
      -- Multicycle primitive instruction
      when (instr.isMultiPrim) do
        opReg1 <== stk.top1
        opReg2 <== stk.top2
        stall <== true

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
      -- Slide instruction
      when (instrReg.val.isSlide) do
        if slideCount.val .<=. 2
          then do
            when (range @1 @0 (slideCount.val) .!=. 0) do
              push1 stk (slide1.val)
            when (index @1 (slideCount.val)) do
              push2 stk (slide2.val)
          else do
            copy stk (slideOffset.val)
            slideCount <== slideCount.val - 1
            stall <== true
        -- Return instruction
        when (instrReg.val.isReturn) do
          nextPC <== stk.top1.content.truncate
          pop stk 1

      -- Load instruction
      when (instrReg.val.isLoad .|. gcRestoreStack.val) do
        let (cell1, cell2) = heap.out
        -- Push first cell
        push1 stk cell1
        -- Push second cell, if it exists
        when (loadOdd.val.inv) do
          push2 stk cell2
        loadOdd <== false
        -- Stall if load is incomplete
        if loadCount.val .>. 2
          then stall <== true
          else gcRestoreStack <== false
        -- Load next cell pair
        load heap (loadPtr.val)
        loadPtr <== loadPtr.val - 1
        loadCount <== loadCount.val - 2

      -- Store instruction (this code also active when gcSaveStack is true)
      let isStoreReady = instrReg.val.isStore .&. gcActive.val.inv
      when (isStoreReady .|. gcSaveStack.val) do
        storeLen <== storeLen.val - 2
        -- Write top two elements to heap
        let stkTop2 =
              Cell {
                tag = storeLen.val .==. 1 ? (intTag, stk.top2.tag),
                content = stk.top2.content
              }
        if gcSaveStack.val
          then do
            store scratchpad (gcFrontPtr.val) (stk.top1, stkTop2)
            gcFrontPtr <== gcFrontPtr.val + 1
          else do
            store heap (hp.val.truncate) (stk.top1, stkTop2)
            hp <== hp.val + 1
        -- Push the pointer on the last cycle
        if storeLen.val .<=. 2
          then do
            if gcSaveStack.val
              then gcSaveStack <== false
              else do
                let ptr = makeStorePtr (instrReg.val) (hp.val.truncate)
                push1 stk ptr
          else
            when (gcSaveStack.val.inv) do
              stall <== true
        -- Pop top element(s) from stack
        let popAmount = storeLen.val .==. 1 ? (1, 2)
        pop stk popAmount

      -- Multicycle primitive instruction
      when (instrReg.val.isMultiPrim) do
        let i = instrReg.val
        let op1 = opReg1.val.content
        let op2 = opReg2.val.content
        -- Result of bitwise operation
        let bitwiseResult =
              select [
                i.isAnd --> op1 .&. op2,
                i.isOr  --> op1 .|. op2,
                i.isXor --> op1 .^. op2
              ]
        -- Shift amount
        let amount :: Bit 5 = op2.truncate
        -- Result of left shift
        let leftResult = op1 .<<. amount
        -- Right-shift bit extension
        let rext = instrReg.val.isArithShift ? (index @31 op1, 0);
        -- Result of right shift
        let rightResult = (rext # op1) .>>>. amount
        -- Result of shift
        let shiftResult = i.isLeftShift ? (leftResult, rightResult.truncate)
        -- Overall result
        let result = i.isBitwise ? (bitwiseResult, shiftResult)
        pop stk 2
        push1 stk (Cell { tag = intTag, content = result })

  -- Garbage collector
  -- =================

  -- Size of the stack before/after GC
  gcStackSize :: Reg StackPtr <- makeReg dontCare

  -- Pointer to stack when stored on heap
  gcStackPtr :: Reg HeapPtr <- makeReg dontCare

  -- GC copy mode (copier inactive if 0, otherwise active)
  gcCopyMode :: Reg (Bit 3) <- makeReg 0

  -- The cells currently being collected
  gcCell :: Reg Cell <- makeReg dontCare
  gcCell2 :: Reg Cell <- makeReg dontCare

  -- Back pointers for to-space
  gcBackPtr :: Reg ScratchpadPtr <- makeReg dontCare

  -- GC helper: Copy object to scratchpad
  ---------------------------------------

  -- Cell pair being copied to scratchpad
  gcPair :: Reg (Cell, Cell) <- makeReg dontCare

  -- Parameters to the copier
  gcCopyLen :: Reg (Bit 6) <- makeReg dontCare
  gcCopyToAddr :: Reg ScratchpadPtr <- makeReg dontCare
  gcCopyFromAddr :: Reg HeapPtr <- makeReg dontCare

  always do
    -- 1. If cell is a pointer then proceed. Otherwise, no need to copy.
    when (gcCopyMode.val .==. 1) do
      load heap (gcCell.val.content.truncate)
      if (gcCell.val.isPtr)
        then gcCopyMode <== 2
        else gcCopyMode <== 0
      -- Assuming it's a pointer, compute new address/length for object
      let offset = dropBitsLSB @1 (gcCell.val.getObjectLen - 1)
      gcCopyLen <== offset.zeroExtend
      gcCopyToAddr <== gcFrontPtr.val + offset.zeroExtend

    -- 2. If object not already collected then collect it
    when (gcCopyMode.val .==. 2) do
      let (cell1, cell2) = heap.out
      if cell1.tag .==. gcTag.fromInteger
        then do
          -- Already collected
          gcCell <== cell1 { tag = gcCell.val.tag }
          gcCopyMode <== 0
        else do
          when (gcCell.val.getObjectLen .==. 0) do
            display "Invariant broken: zero-length object"
          -- Determine object length in number of cell pairs
          let len = dropBitsLSB @1 (gcCell.val.getObjectLen + 1)
          -- Determine new pointer
          let ptr = gcCopyToAddr.val
          -- Update the gcCell to represent the new pointer
          let newCell = modifyPtr (gcCell.val) (ptr.zeroExtend)
          gcCell <== newCell
          gcFrontPtr <== gcFrontPtr.val + len.zeroExtend
          -- Store GC indirection
          let gcInd = newCell { tag = gcTag.fromInteger }
          store heap (gcCell.val.content.truncate) (gcInd, dontCare)
          -- Copy object to scratchpad
          gcPair <== heap.out
          gcCopyFromAddr <== gcCell.val.content.truncate - 1
          gcCopyMode <== 3
  
    -- 3. Copy object to scratchpad
    when (gcCopyMode.val .==. 3) do
      -- Store to scratchpad
      store scratchpad (gcCopyToAddr.val) (gcPair.val)
      -- Load next pair
      load heap (gcCopyFromAddr.val)
      -- Check if we're done
      gcCopyToAddr <== gcCopyToAddr.val - 1
      gcCopyFromAddr <== gcCopyFromAddr.val - 1
      gcCopyLen <== gcCopyLen.val - 1
      gcCopyMode <== gcCopyLen.val .==. 0 ? (0, 4)
  
    -- 4. Latch heap output
    when (gcCopyMode.val .==. 4) do
      gcPair <== heap.out
      gcCopyMode <== 3
  
  -- Copying collector
  --------------------

  let collector =
        Seq [
          -- Step 1: write stack to heap
          Action do
            gcFrontPtr <== 0
            gcBackPtr <== 0
            storeLen <== stk.size
            gcStackSize <== stk.size
            gcStackPtr <== zeroExtend (dropBitsLSB @1 (stk.size - 1))
            gcSaveStack <== true,
          Wait (gcSaveStack.val .==. false),

          -- Step 2: copying collector
          While (gcBackPtr.val .!=. gcFrontPtr.val) (
            Seq [
              Action do load scratchpad (gcBackPtr.val),
              Action do
                gcCell <== scratchpad.out.fst
                gcCell2 <== scratchpad.out.snd
                gcCopyMode <== 1,
              Wait (gcCopyMode.val .==. 0),
              Action do
                gcCell2 <== gcCell.val
                gcCell <== gcCell2.val
                gcCopyMode <== 1,
              Wait (gcCopyMode.val .==. 0),
              Action do
                store scratchpad (gcBackPtr.val) (gcCell2.val, gcCell.val)
                gcBackPtr <== gcBackPtr.val + 1
            ]
          ),

          -- Step 3: copy scratchpad back to heap
          Action do
            hp <== gcBackPtr.val.zeroExtend
            gcBackPtr <== 0,
          While (gcBackPtr.val .!=. gcFrontPtr.val) (
            Seq [
              Background (
                Do [
                  load scratchpad (gcBackPtr.val),
                  return (),
                  store heap (gcBackPtr.val.old.old.zeroExtend)
                             (scratchpad.out.old)
                ]
              ),
              Action do gcBackPtr <== gcBackPtr.val + 1
            ]
          ),
          Tick, Tick,

          -- Step 4: Restore stack from heap
          Action do
            load heap (gcStackPtr.val)
            gcRestoreStack <== true
            loadCount <== gcStackSize.val
            loadOdd <== index @0 (gcStackSize.val)
            loadPtr <== gcStackPtr.val - 1,
          Wait (gcRestoreStack.val .==. false),

          -- Finished
          Tick
        ]

  -- Compile GC recipe
  gcFinish <- run (gcStart.val) collector

  -- Wait for GC to complete
  always do
    when gcFinish do
      storeLen <== gcSavedStoreLen.val
      gcActive <== false

    -- Stall pipeline during GC
    when (gcActive.val) do
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
