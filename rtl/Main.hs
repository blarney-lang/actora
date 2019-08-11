import Blarney
import Stack
import Core

main :: IO ()
main = do
  Stack.genTestBench
  Core.genCore
