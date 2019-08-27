module Types where

-- Circuit-generator parameters
#include <Config.h>

-- Imports
import Blarney

-- Pointers
-- ========

-- Instruction pointer
type InstrPtr = Bit LogInstrMemSize

-- Stack pointer
type StackPtr = Bit LogStackSize

-- Heap pointer
type HeapPtr = Bit LogHeapSizeMinusOne

-- Scratchpad pointer
type ScratchpadPtr = Bit LogScratchpadSizeMinusOne
