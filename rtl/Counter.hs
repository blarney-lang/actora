module Counter where

import Blarney

data Count n =
  Count {
    incrBy    :: Bit n -> Action ()
  , decrBy    :: Bit n -> Action ()
  , full      :: Bit 1
  , available :: Bit n
  , count     :: Bit n
  }

makeCount :: KnownNat n => Bit n -> Module (Count n)
makeCount maxVal =
  do incrWire :: Wire (Bit n) <- makeWire 0
     decrWire :: Wire (Bit n) <- makeWire 0
     countReg :: Reg (Bit n) <- makeReg 0
     fullReg :: Reg (Bit 1) <- makeReg 0

     always do
       let newCount :: Bit n = (countReg.val + incrWire.val) - decrWire.val
       countReg <== newCount
       fullReg <== newCount .==. maxVal

     return $ Count {
       incrBy = (incrWire <==)
     , decrBy = (decrWire <==)
     , full = fullReg.val
     , count = countReg.val
     , available = maxVal - countReg.val
     }
