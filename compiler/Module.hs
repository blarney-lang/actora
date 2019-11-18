module Module where

-- Standard imports
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import qualified Data.Set as S
import qualified Data.Map as M

-- Local imports
import Syntax
import Parser
import Descend

-- A simple module system, with a slow implementation
loadModule :: String -> String -> IO [Decl]
loadModule prefix modName = do
    -- Determine filename
    fileName <- searchPaths >>= locate
    -- Parse file
    prog <- parseFile fileName
    -- Add import of prelude
    let prog' = if modName == "prelude" then prog
                else ImportDecl "prelude" : prog
    -- Perform name resolution
    importAndResolve prog'
  where
    -- Module search paths
    searchPaths :: IO [String]
    searchPaths = do
      libDir <- lookupEnv "ACTORA_ROOT"
      return ([prefix] ++ [dir ++ "/lib" | Just dir <- [libDir]])

    -- Determine module location
    locate :: [String] -> IO String
    locate [] = error ("Can't find module '" ++ modName ++ "'")
    locate (prefix:rest) = do
      let fileName = prefix ++ "/" ++ modName ++ ".erl"
      exists <- doesFileExist fileName
      if exists
        then return fileName
        else locate rest

    -- Name resolution
    importAndResolve :: [Decl] -> IO [Decl]
    importAndResolve ds = do
      -- Determine imports
      let imports = [m | ImportDecl m <- ds]
      -- Recurse
      importedMods <- mapM (loadModule prefix) imports
      -- Remove duplicate definitions
      let imported = removeDups importedMods
      -- Resolve names and remove unused definitions
      let context = [d | d@(FunDecl f args g body) <- ds ++ concat imported]
      return $
        [ FunDecl (modName ++ ":" ++ n) args
                  (fmap (resolveExp modName context) guard)
                  (map (resolveExp modName context) body)
        | FunDecl n args guard body <- ds ] ++ concat imported

-- Name resolution pass for declarations
resolve :: Id -> [Decl] -> [Decl]
resolve m ds = onExp (resolveExp m ds) ds

-- Name resolution pass for expressions
resolveExp :: Id -> [Decl] -> Exp -> Exp
resolveExp m ds (Fun f n) = Fun (resolveName m ds f) n
resolveExp m ds (Id id) = Id (resolveName m ds id)
resolveExp m ds other = descend (resolveExp m ds) other

-- Prepend module name to identifiers, where not already specified
resolveName :: Id -> [Decl] -> Id -> Id
resolveName m ds name
  | ':' `elem` name = name
  | otherwise  =
      if name `elem` [funName d | d <- ds]
      then m ++ ":" ++ name
      else
        case nub [f | f <- map funName ds, (':':name) `isSuffixOf` f] of
          [] -> name
          [f] -> f
          other -> error ("Ambiguous call of function '" ++ name ++
                            "' in module '" ++ m ++ "'")
  where
    funName :: Decl -> Id
    funName (FunDecl v ps g body) = v
    funName (ClosureDecl v env ps g body) = v

-- Remove duplicate declarations
removeDups :: [[Decl]] -> [[Decl]]
removeDups [] = []
removeDups (m:ms) =
    [ FunDecl f args g body
    | FunDecl f args g body <- m, all (notDefined f) ms
    ] : removeDups ms
  where
    notDefined :: Id -> [Decl] -> Bool
    notDefined id ds = null [() | FunDecl f args g body <- ds, f == id]

-- Remove unused declarations
removeUnused :: String -> [Decl] -> [Decl]
removeUnused modName decls = 
  [d | d@(FunDecl f args g body) <- decls, f `S.member` used]
  where
    prog = M.fromListWith (++) [(f, [d]) | d@(FunDecl f args g body) <- decls]
    used = reachable S.empty [modName ++ ":start"]
    reachable set [] = set
    reachable set (f:fs)
      | f `S.member` set = reachable set fs
      | otherwise =
        case M.lookup f prog of
          Nothing -> reachable set fs
          Just ds -> 
            let exprs = concat [ catMaybes [g] ++ body
                               | FunDecl f args g body <- ds ]
                used = [id | e <- exprs, Fun id _ <- universe e]
            in  reachable (S.insert f set) (nub used ++ fs)
