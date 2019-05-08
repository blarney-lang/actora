import Syntax
import Parser
import Compiler
import Bytecode
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      prog <- parseFile filename
      mapM_ print (compile prog)
    other -> putStrLn "Usage: elite <FILE>"
