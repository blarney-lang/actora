module DRAM where

-- System parameters
#include <Config.h>

-- Local imports
import Counter

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream

---------------------------
-- Internal DRAM interfaces
---------------------------

type DRAMReqId = Bit DRAMReqIdBits
type DRAMAddr  = Bit LogBeatsPerDRAM
type DRAMBeat  = Bit DRAMBeatWidth
type DRAMBurst = Bit DRAMBurstWidth

data DRAMReq = 
  DRAMReq {
    dramReqId      :: DRAMReqId
  , dramReqIsStore :: Bit 1
  , dramReqAddr    :: DRAMAddr
  , dramReqData    :: DRAMBeat
  , dramReqBurst   :: DRAMBurst
  } deriving (Generic, FShow, Bits, Interface)

data DRAMResp =
  DRAMResp {
    dramRespId   :: DRAMReqId
  , dramRespData :: DRAMBeat
  } deriving (Generic, FShow, Bits, Interface)

-- Used internally
data DRAMInFlightReq =
  DRAMInFlightReq {
    dramInFlightId :: DRAMReqId
  , dramInFlightBurst :: DRAMBurst
  } deriving (Generic, Bits, Interface)

------------------------------------
-- External DRAM interfaces (Avalon)
------------------------------------

data AvalonDRAMIns =
  AvalonDRAMIns {
    dram_readdata      :: DRAMBeat
  , dram_readdatavalid :: Bit 1
  , dram_waitrequest   :: Bit 1
  } deriving (Generic, Bits, Interface)

data AvalonDRAMOuts =
  AvalonDRAMOuts {
    dram_read       :: Bit 1
  , dram_write      :: Bit 1
  , dram_writedata  :: DRAMBeat
  , dram_address    :: DRAMAddr
  , dram_burstcount :: DRAMBurst  
  } deriving (Generic, Bits, Interface)

-----------------------------
-- Implementation (synthesis)
-----------------------------

#ifndef SimMode

makeDRAM :: (Stream DRAMReq, AvalonDRAMIns) ->
              Module (Stream DRAMResp, AvalonDRAMOuts)
makeDRAM (reqs, avlIns) =
  do -- Queue of in-flight requests
     inFlight :: Queue DRAMInFlightReq <- makeSizedQueue DRAMLogMaxInFlight

     -- Response queue
     resps :: Queue DRAMBeat <- makeSizedQueue DRAMLogMaxInFlight

     -- In-flight request counter
     inFlightCount :: Count (DRAMLogMaxInFlight+1) <-
       makeCount (fromInteger (2^DRAMLogMaxInFlight))

     -- Registers
     address    :: Reg DRAMAddr  <- makeReg dontCare
     writeData  :: Reg DRAMBeat  <- makeReg dontCare
     doRead     :: Reg (Bit 1)   <- makeReg 0
     doWrite    :: Reg (Bit 1)   <- makeReg 0
     burstReg   :: Reg DRAMBurst <- makeReg 0
     burstCount :: Reg DRAMBurst <- makeReg 1

     -- Wires
     putLoad     :: Wire (Bit 1) <- makeWire 0
     putStore    :: Wire (Bit 1) <- makeWire 0

     -- Wait request
     let waitRequest = avlIns.dram_waitrequest

     -- Maximum burst size
     let maxBurst = fromInteger (2 ^ (DRAMBurstWidth-1))

     -- Put requests to external DRAM
     always do
       when (putLoad.val) do
         doRead <== 1
         doWrite <== 0
       when (putStore.val) do
         doRead <== 0
         doWrite <== 1
       when (putLoad.val.inv .&. putStore.val.inv .&. waitRequest.inv) do
         doRead <== 0
         doWrite <== 0

     -- Process DRAM requests
     always do
       when (reqs.canPeek .&. waitRequest.inv) do
         let req :: DRAMReq = reqs.peek
         when (inFlightCount.available .>=. maxBurst) do
           reqs.consume
           address <== req.dramReqAddr
           burstReg <== req.dramReqBurst
           writeData <== req.dramReqData
           if req.dramReqIsStore then putStore <== 1 else putLoad <== 1
           when (req.dramReqIsStore.inv) do
             enq inFlight 
               DRAMInFlightReq {
                 dramInFlightId = req.dramReqId
               , dramInFlightBurst = req.dramReqBurst
               }
             inFlightCount `incrBy` zeroExtend (req.dramReqBurst)

     -- Process DRAM responses
     always do
       when (avlIns.dram_readdatavalid) do
         enq resps (avlIns.dram_readdata)

     -- Prepare the response stream
     let respsOut =
           Source {
             canPeek = inFlight.canDeq .&. resps.canDeq
           , peek =
               DRAMResp {
                 dramRespId = inFlight.first.dramInFlightId
               , dramRespData = resps.first
               }
           , consume = do
               resps.deq
               inFlightCount `decrBy` 1
               if burstCount.val .==. inFlight.first.dramInFlightBurst
                 then do
                   inFlight.deq
                   burstCount <== 1
                 else do
                   burstCount <== burstCount.val + 1
           }

     -- Prepare Avalon output signals
     let avlOuts =
           AvalonDRAMOuts {
             dram_read       = doRead.val
           , dram_write      = doWrite.val
           , dram_writedata  = writeData.val
           , dram_address    = address.val
           , dram_burstcount = zeroExtend (burstReg.val)
           }

     return (respsOut, avlOuts)

#endif

------------------------------
-- Implementation (simulation)
------------------------------

#ifdef SimMode

makeDRAM :: (Stream DRAMReq, AvalonDRAMIns) ->
              Module (Stream DRAMResp, AvalonDRAMOuts)
makeDRAM (reqs, _) = do
  -- Simulate DRAM using a RegFile (Verilog array)
  -- (Simulation therefore limited to small DRAMs)
  ram :: RegFile DRAMAddr DRAMBeat <- makeRegFile

  -- Response queue (use a ShiftQueue to simulate latency)
  resps :: Queue DRAMResp <- makeShiftQueue DRAMSimLatency

  -- Burst count
  burstCount :: Reg DRAMBurst <- makeReg 1

  always do
    when (reqs.canPeek .&. resps.notFull) do
      let req = reqs.peek
      -- Perform request
      if req.dramReqIsStore
        then do
          update ram (req.dramReqAddr) (req.dramReqData)
        else do
          enq resps
            DRAMResp {
              dramRespId = req.dramReqId
            , dramRespData = ram ! (req.dramReqAddr)
            }
      -- Consume request when burst is finished
      if (req.dramReqBurst .==. burstCount.val)
        then do
          burstCount <== 1
          reqs.consume
        else do
          burstCount <== burstCount.val + 1

  -- Prepare Avalon output signals
  let avlOuts = error "Unused in simulation: AvalonDRAMOuts"

  return (resps.toStream, avlOuts)

#endif
