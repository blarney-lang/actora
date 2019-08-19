-- Standard imports
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad

-- Local imports
import Syntax
import Parser
import Module
import StackIR
import Compiler
import CBackend
import Bytecode
import Semantics

-- Command-line flags
data Flag =
    Run
  | CompileToC String
  | CompileToNIOSII
  | CompileToStackIR
  | CompileToBytecode
  | CompileToHex
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['r'] [] (NoArg Run)
      "Run using small-step semantics"
  , Option ['c'] [] (ReqArg CompileToC "DIR")
      "Compile to 32-bit C (experimental)"
  , Option [] ["niosii"] (NoArg CompileToNIOSII)
      "C backend: target NIOS-II"
  , Option ['s'] [] (NoArg CompileToStackIR)
      "Genereate stack IR"
  , Option ['b'] [] (NoArg CompileToBytecode)
      "Generate bytecode"
  , Option ['h'] [] (NoArg CompileToHex)
      "Generate bytecode (hex format)"
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
    [fileName] -> do
      (modName, dir) <- case reverse fileName of
        'l':'r':'e':'.':rest -> return
          (reverse $ takeWhile (/= '/') rest,
           reverse $ dropWhile (/= '/') rest)
        other -> do
          putStrLn "Expected '.erl' file"
          exitFailure

      prog <- loadModule (dir ++ "./") modName

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

      -- Generate stack IR
      when (CompileToStackIR `elem` flags) $ do
        mapM_ print (compile modName prog)
        exitSuccess

      -- Generate bytecode
      when (CompileToBytecode `elem` flags) $ do
        print $ encode $ compile modName prog
        exitSuccess

      -- Generate bytecode
      when (CompileToHex `elem` flags) $ do
        mapM_ putStrLn $ encodeHex $ compile modName prog
        exitSuccess

    other -> return ()

  putStrLn "Expected single module name as argument"
  exitFailure
