module Util where

import Blarney

-- Add and check for overflow
checkedAdd :: Bit n -> Bit n -> (Bit 1, Bit n)
checkedAdd a b = split result
  where
    z :: Bit 1 = 0
    result = (z # a) .+. (z # b)

-- Subtract and check for underflow
checkedSub :: Bit n -> Bit n -> (Bit 1, Bit n)
checkedSub a b = split result
  where
    z :: Bit 1 = 0
    result = (z # a) .-. (z # b)
