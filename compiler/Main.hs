-- Standard imports
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad

-- Local imports
import Syntax
import Parser
import Module
import Bytecode
import Compiler
import CBackend
import Semantics

-- Command-line flags
data Flag =
    Run
  | CompileToC String
  | CompileToNIOSII
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['r'] [] (NoArg Run)
      "Run using small-step semantics"
  , Option ['c'] [] (ReqArg CompileToC "DIR")
      "Compile to 32-bit C (experimental)"
  , Option [] ["niosii"] (NoArg CompileToNIOSII)
       "C backend: target NIOS-II"
  ]

getOptions :: [String] -> IO ([Flag], [String])
getOptions argv = 
  case getOpt Permute options argv of
    (o, n, []) | not (null o) -> return (o, n)
    (_, _, errs) -> do
      putStr (usageInfo header options)
      exitFailure
  where
    header = "Usage: elite [OPTION...] MODULE"

main :: IO ()
main = do
  args <- getArgs
  (flags, files) <- getOptions args
  case files of
    [modName] -> do
      prog <- loadModule modName
      -- Evaluate using compiler + small-step semantics
      when (Run `elem` flags) $ do
        putStrLn (run (compile modName prog))
        exitSuccess
      -- Generate C code
      case [dir | CompileToC dir <- flags] of
        [] -> return ()
        [dir] -> do
          let mode = 
                if CompileToNIOSII `elem` flags then Gen_NIOSII_32
                else Gen_C_32
          genC $
            CGenOpts {
                topModName = modName
              , sourceProg = prog
              , targetDir  = dir
              , genMode    = mode
            }
          exitSuccess
        dir:dirs -> do
          putStrLn "Expected on target directory for compilation"
          exitFailure
    other -> return ()
  putStrLn "Expected single module name as argument"
  exitFailure
