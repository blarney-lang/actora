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
import CBackend

-- Command-line flags
data Flag =
    Run
  | CompileToC String
  | CompileToBareC16
  | CompileToBareC32
  | CompileToC32
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['r'] [] (NoArg Run)
      "Run using small-step semantics"
  , Option ['c'] [] (ReqArg CompileToC "DIR")
      "Compile to C"
  , Option [] ["c32"] (NoArg CompileToC32)
      "C backend: 32-bit standard C (default)"
  , Option [] ["b16"] (NoArg CompileToBareC16)
      "C backend: 16-bit baremetal"
  , Option [] ["b32"] (NoArg CompileToBareC32)
      "C backend: 32-bit baremetal"
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
      -- Evaluate using compiler + small-step semantics
      when (Run `elem` flags) $ do
        putStrLn (run (compile prog))
        exitSuccess
      -- Generate C code
      case [dir | CompileToC dir <- flags] of
        [] -> return ()
        [dir] -> do
          let mode = 
                if CompileToBareC16 `elem` flags then BareGen_16
                else if CompileToBareC32 `elem` flags then BareGen_32
                else CGen_32
          genC $
            CGenOpts {
                sourceProg = prog
              , targetDir  = dir
              , genMode    = mode
            }
          exitSuccess
        dir:dirs -> do
          putStrLn "Expected on target directory for compilation"
          exitFailure
    other -> return ()
  putStrLn "Expected single source file as argument"
  exitFailure
