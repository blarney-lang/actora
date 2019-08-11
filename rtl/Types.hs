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

-- Return stack pointer
type RetStackPtr = Bit LogRetStackSize

-- Heap pointer
type HeapPtr = Bit LogHeapSizeMinusOne
