{-# LANGUAGE BinaryLiterals #-}
module Bytecode where

-- Standard imports
import Data.Bits
import qualified Data.Map as M

-- Local imports
import StackIR

-- Bytecode format
data Bytecode =
  Bytecode {
    -- Sequence of encoded instructions
    bytecodeInstrs :: [Integer]
    -- Function names and addresses
  , bytecodeLabels :: [(String, InstrAddr)]
    -- Atom names and values
  , bytecodeAtoms  :: [String]
  }

-- Render bytecode in printable format
instance Show Bytecode where
  show code = unlines $
       [numAtoms ++ " " ++ numLabels ++ " " ++ numInstrs]
    ++ bytecodeAtoms code
    ++ [label ++ " " ++ show addr | (label, addr) <- bytecodeLabels code]
    ++ map show (bytecodeInstrs code)
    where
       numAtoms  = show $ length $ bytecodeAtoms code
       numLabels = show $ length $ bytecodeLabels code
       numInstrs = show $ length $ bytecodeInstrs code

-- Data type for sized bit vectors
data BV = BV { width :: Int, value :: Integer }

-- Construct a signed bit vector
signed :: Integral v => Int -> v -> Maybe BV
signed w v
  | i >= lo && i <= hi = Just (BV { width = w, value = i })
  | otherwise = Nothing
  where
    i = toInteger v
    lo = -2 ^ (w-1) + 1
    hi = 2 ^ (w-1)

-- Construct an unsigned bit vector
unsigned :: Integral v => Int -> v -> Maybe BV
unsigned w v
  | i >= 0 && i <= hi = Just (BV { width = w, value = i })
  | otherwise = Nothing
  where
    i = toInteger v
    hi = (2^w) - 1

-- Bit vector concatenation, with error handling
(<.>) :: Maybe BV -> Maybe BV -> Maybe BV
Nothing <.> b1      = Nothing
b0      <.> Nothing = Nothing
Just b0 <.> Just b1 =
  Just $ BV {
    width = width b0 + width b1
  , value = (value b0 `shiftL` width b1) .|. value b1
  }

-- Encode stack IR as bytecode
encode :: [Instr] -> Bytecode
encode instrs =
  Bytecode {
    bytecodeInstrs = zipWith encodeOne [0..] instrs'
  , bytecodeLabels = filter funLabel (M.toList labels)
  , bytecodeAtoms  = atoms instrs
  }
  where
    (instrs', labels) = link instrs

    -- Mapping from atom names to values
    atomMap :: M.Map String Int
    atomMap = M.fromList (zip (atoms instrs) [0..])

    -- Drop labels that are not function labels
    funLabel :: (String, InstrAddr) -> Bool
    funLabel ('@':s, _) = False
    funLabel ('$':s, _) = False
    funLabel (s, _) = True

    -- Encode a single instruction
    encodeOne :: InstrAddr -> Instr -> Integer
    encodeOne myAddr instr =
      case encOne myAddr instr of
        Just (BV w v) | w == 26 -> v
        otherwise ->
          error ("Failed to encode instruction " ++ show instr)

    -- Encode a single instruction, but look out for errors
    encOne :: InstrAddr -> Instr -> Maybe BV
    encOne myAddr instr =
      case instr of
        PUSH (INT i) ->
          unsigned 10 0b1000000000 <.> signed 16 i
        PUSH (ATOM str) ->
          unsigned 10 0b1000000001 <.> unsigned 16 (atomMap M.! str)
        PUSH (FUN (InstrAddr addr)) ->
          unsigned 10 0b1000000010 <.> unsigned 16 addr
        SETU val ->
          unsigned 10 0b1000000011 <.> unsigned 16 val
        SLIDE dist n ->
          unsigned 10 0b1000000100 <.>
          unsigned 10 dist <.>
          unsigned 6 n
        COPY n ->
          unsigned 10 0b1000000101 <.> unsigned 16 n
        COPY2 n m ->
          unsigned 10 0b1000000110 <.> unsigned 8 n <.> unsigned 8 m
        CALL (InstrAddr addr) -> 
          unsigned 10 0b1000001000 <.> unsigned 16 addr
        ICALL ->
          unsigned 10 0b1000001001 <.> unsigned 16 0
        JUMP (InstrAddr addr) ->
          unsigned 10 0b1000001010 <.> unsigned 16 addr
        IJUMP ->
          unsigned 10 0b1000001011 <.> unsigned 16 0
        RETURN pop ->
          unsigned 10 0b1000001100 <.> unsigned 16 pop
        LOAD pop ->
          unsigned 10 0b1000001101 <.>
          unsigned 1 (if pop then 1 else 0) <.>
          unsigned 15 0
        STORE n kind ->
            unsigned 10 0b1000001110 <.>
            unsigned 2 k <.>
            unsigned 6 a <.>
            unsigned 6 n <.>
            unsigned 2 0
          where
            k = case kind of
                  PtrCons  -> 0b00
                  PtrTuple -> 0b01
                  PtrApp n -> 0b10
            a = case kind of
                  PtrCons  -> 0
                  PtrTuple -> 0
                  PtrApp n -> n
        PRIM PrimAdd ->
          unsigned 10 0b1000010000 <.> unsigned 16 0
        PRIM (PrimAddImm imm) ->
          unsigned 10 0b1000010001 <.> signed 16 imm
        PRIM PrimSub ->
          unsigned 10 0b1000010010 <.> unsigned 16 0
        PRIM (PrimSubImm imm) ->
          unsigned 10 0b1000010011 <.> signed 16 imm
        PRIM PrimEq ->
          unsigned 10 0b1000010100 <.> unsigned 16 0
        PRIM PrimNotEq ->
          unsigned 10 0b1000010101 <.> unsigned 16 0
        PRIM PrimLess ->
          unsigned 10 0b1000010110 <.> unsigned 16 0
        PRIM PrimLessEq ->
          unsigned 10 0b1000010111 <.> unsigned 16 0
        HALT err ->
            unsigned 10 0b1000100000 <.> unsigned 16 (errorCode err)
        BRANCH cond pop (InstrAddr addr) ->
          unsigned 1 0 <.>
          unsigned 1 neg <.>
          cond' <.>
          unsigned 5 pop <.>
          unsigned 10 (addr - myAddr)
          where
            neg = case fst cond of { Pos -> 0; Neg -> 1}
            cond' = case snd cond of
                      IsAtom str -> 
                        unsigned 3 0b000 <.>
                        unsigned 6 (atomMap M.! str)
                      IsInt val ->
                        unsigned 3 0b001 <.> signed 6 val
                      IsCons ->
                        unsigned 3 0b010 <.> unsigned 6 2
                      IsTuple len ->
                        unsigned 3 0b011 <.> unsigned 6 len
                      IsApp n ->
                        unsigned 3 0b100 <.> unsigned 6 n
        other -> error ("Unknown instruction " ++ show other)

-- Encode error string
errorCode :: String -> Int
errorCode "ENone"           = 0
errorCode "EStackOverflow"  = 1
errorCode "EHeapOverflow"   = 2
errorCode "EArith"          = 3
errorCode "ELoadAddr"       = 4
errorCode "EJumpAddr"       = 5
errorCode "EStackIndex"     = 6
errorCode "EUnknown"        = 7
errorCode "EInstrIndex"     = 8
errorCode "EUnknownInstr"   = 9
errorCode "EStackUnderflow" = 10
errorCode "EBindFail"       = 16
errorCode "ECaseFail"       = 17
errorCode "EEqnFail"        = 18
errorCode "EApplyFail"      = 19
errorCode other             = errorCode "EUnknown"
