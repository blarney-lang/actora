module Module where

-- Standard imports
import Data.List
import Data.Maybe
import System.Directory
import System.Environment

-- Local imports
import Syntax
import Parser
import Descend

-- A simple module system, with a slow implementation
loadModule :: String -> IO [Decl]
loadModule modName = do
    -- Determine filename
    fileName <- searchPaths >>= locate
    -- Parse file
    prog <- parseFile fileName
    -- Add import of prelude
    let prog' = if modName == "prelude" then prog
                else ImportDecl "prelude" : prog
    -- Perform name resolution
    resolve prog'
  where
    -- Module search paths
    searchPaths :: IO [String]
    searchPaths = do
      libDir <- lookupEnv "ELITE_ROOT"
      return (["."] ++ [dir ++ "/lib" | Just dir <- [libDir]])

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
    resolve :: [Decl] -> IO [Decl]
    resolve ds = do
      -- Determine imports
      let imports = [m | ImportDecl m <- ds]
      -- Recurse
      importedMods <- mapM loadModule imports
      -- Remove duplicate definitions
      let imported = removeDups importedMods
      -- Resolve names and remove unused definitions
      let context = [d | d@(FunDecl f args g body) <- ds ++ concat imported]
      return $ removeUnused $
        [ FunDecl (modName ++ ":" ++ n) args
                  (fmap (resolveExp context) guard)
                  (map (resolveExp context) body)
        | FunDecl n args guard body <- ds ] ++ concat imported

    -- Name resolution pass for expressions
    resolveExp :: [Decl] -> Exp -> Exp
    resolveExp ds (Id id) = Id (resolveName ds id)
    resolveExp ds other = descend (resolveExp ds) other

    -- Prepend module name to identifiers, where not already specified
    resolveName :: [Decl] -> Id -> Id
    resolveName ds name
      | ':' `elem` name = name
      | otherwise  =
          if name `elem` [funName d | d <- ds]
          then modName ++ ":" ++ name
          else
            case nub [f | f <- map funName ds, (':':name) `isSuffixOf` f] of
              [] -> name
              [f] -> f
              other -> error ("Ambiguous call of function '" ++ name ++
                                "' in module '" ++ modName ++ "'")

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
removeUnused :: [Decl] -> [Decl]
removeUnused prog =
    [FunDecl f args g body | FunDecl f args g body <- prog,
       f `elem` used || ":start" `isSuffixOf` f]
  where
    exprs = concat [catMaybes [g] ++ body | FunDecl f args g body <- prog]
    used = [id | e <- exprs, Id id <- universe e]
