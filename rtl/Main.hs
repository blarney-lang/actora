import Core
import DRAM
import Stack
import Blarney

main :: IO ()
main = do
  Stack.genTestBench
  Core.genCore
