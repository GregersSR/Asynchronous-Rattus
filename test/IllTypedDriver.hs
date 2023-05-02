{-# LANGUAGE CPP #-}

module Main where

import GHC
import GHC.Paths ( libdir )
import GHC.Driver.Session
import GHC.Utils.Exception (ExceptionMonad)
import GHC.Utils.Panic

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (SomeException(SomeException))
import Control.Monad.Catch

import System.Process (callCommand)

main :: IO ()
main = do
    buildRattus
    success <- canCompile ["Rattus", "test/IllTyped/IncompatibleAdv.hs"]
    print success

canCompile :: [String] -> IO Bool
canCompile files = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        targets <- mapM (flip guessTarget Nothing) files
        canCompileTargets targets


buildRattus :: IO ()
buildRattus = callCommand "cabal build" >> putStrLn "Did build"

-- inspired by https://parenz.wordpress.com/2013/07/23/on-custom-error-handlers-for-ghc-api/
-- and 
initGhc :: GhcMonad m => m ()
initGhc = do
  dfs <- getSessionDynFlags
  let dfs' = (addPlugin "Rattus.Plugin" . makePluginPkgVisible "Rattus.Plugin" . addPkgDb "dist-newstyle/packagedb/ghc-9.2.5") dfs
  setSessionDynFlags $ dfs' { backend = Interpreter
                            , ghcLink = LinkInMemory
                            , verbosity = 3 }
  --addPkgDb "dist-newstyle/packagedb"
  return ()


-- Attempt to compile a module and report whether this succeeds.
canCompileTargets :: GhcMonad m => [Target] -> m Bool
canCompileTargets targets = do
    initGhc
    setTargets targets
    load LoadAllTargets
    return True

reportSuccess :: (ExceptionMonad m, MonadIO m)
                   => m a -> m Bool
reportSuccess m =
  handleAll (\e -> return False) $
  m >> return True

-- Inspired by: https://parenz.wordpress.com/2013/07/29/ghc-packagedb/
-- | Add a package database to the Ghc monad
addPkgDb :: FilePath -> DynFlags -> DynFlags
addPkgDb fp dfs = dfs { packageDBFlags = pkgFlag : (packageDBFlags dfs) }
  where pkgFlag = PackageDB (PkgDbPath fp)

addPlugin :: String -> DynFlags -> DynFlags
addPlugin p dfs = dfs {pluginModNames = mkModuleName "Rattus.Plugin" : pluginModNames dfs}

makePluginPkgVisible :: String -> DynFlags -> DynFlags
makePluginPkgVisible p dfs = dfs {pluginPackageFlags = pkgFlag : pluginPackageFlags dfs}
  where renaming = ModRenaming True []
        pkgFlag = ExposePackage p (PackageArg p) renaming