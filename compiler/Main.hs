import Syntax
import Parser
import Compiler
import Bytecode
import Semantics
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      prog <- parseFile filename
      putStrLn (run (compile prog))
    other -> putStrLn "Usage: elite <FILE>"
