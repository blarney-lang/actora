-- Standard imports
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad

-- Local imports
import Syntax
import Parser
import Compiler
import Bytecode
import Semantics
import Backend.C

-- Command-line flags
data Flag =
    Run
  | CompileToC String
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['r'] [] (NoArg Run)
      "Run using small-step semantics"
  , Option ['c'] [] (ReqArg CompileToC "DIR")
      "Compile to C"
  ]

getOptions :: [String] -> IO ([Flag], [String])
getOptions argv = 
  case getOpt Permute options argv of
    (o, n, []) | not (null o) -> return (o, n)
    (_, _, errs) -> do
      putStr (usageInfo header options)
      exitFailure
  where
    header = "Usage: elite [OPTION...] FILE"

main :: IO ()
main = do
  args <- getArgs
  (flags, files) <- getOptions args
  case files of
    [file] -> do
      prog <- parseFile file
      when (Run `elem` flags) $ do
        putStrLn (run (compile prog))
        exitSuccess
      case [dir | CompileToC dir <- flags] of
        [] -> return ()
        [dir] -> do
          gen $
            CGenOpts {
                sourceProg = prog
              , targetDir  = dir
              , genMode    = CGen_32
            }
          exitSuccess
        dir:dirs -> do
          putStrLn "Expected on target directory for compilation"
          exitFailure
    other -> return ()
  putStrLn "Expected single source file as argument"
  exitFailure
