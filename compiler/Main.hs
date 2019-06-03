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
import Backend.StandardC as StdC
import Backend.BaremetalC as BareC

-- Command-line flags
data Flag =
    Run
  | CompileToStdC String
  | CompileToBareC String
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['r'] [] (NoArg Run)
      "Run using small-step semantics"
  , Option ['c'] [] (ReqArg CompileToStdC "DIR")
      "Compile to standard C"
  , Option ['b'] [] (ReqArg CompileToBareC "DIR")
      "Compile to baremetal C"
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
      case [dir | CompileToStdC dir <- flags] of
        [] -> return ()
        [dir] -> do
          genC $
            CGenOpts {
                StdC.sourceProg = prog
              , StdC.targetDir  = dir
              , StdC.genMode    = CGen_32
            }
          exitSuccess
        dir:dirs -> do
          putStrLn "Expected on target directory for compilation"
          exitFailure
      -- Generate baremetal C code
      case [dir | CompileToBareC dir <- flags] of
        [] -> return ()
        [dir] -> do
          genBareC $
            BareCGenOpts {
                BareC.sourceProg = prog
              , BareC.targetDir  = dir
            }
          exitSuccess
        dir:dirs -> do
          putStrLn "Expected on target directory for compilation"
          exitFailure
    other -> return ()
  putStrLn "Expected single source file as argument"
  exitFailure
