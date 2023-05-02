module Main where

import GHC
import GHC.Paths ( libdir )
import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut )
import GHC.Utils.Exception (ExceptionMonad)
import GHC.Utils.Panic

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (SomeException(SomeException))
import Control.Monad.Catch

main :: IO ()
main = do
    success <- canCompile "IllTyped/IncompatibleAdv.hs"
    print success

canCompile :: String -> IO Bool
canCompile file = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        target <- guessTarget file Nothing
        canCompileTargets [ target ]


-- from https://parenz.wordpress.com/2013/07/23/on-custom-error-handlers-for-ghc-api/
initGhc :: GhcMonad m => m ()
initGhc = do
  dfs <- getSessionDynFlags
  setSessionDynFlags $ dfs { backend = Interpreter
                           , ghcLink = LinkInMemory }
  return ()


-- Attempt to compile a module and report whether this succeeds.
canCompileTargets :: GhcMonad m => [Target] -> m Bool
canCompileTargets targets = reportSuccess $ do
    initGhc
    setTargets targets
    load LoadAllTargets

reportSuccess :: (ExceptionMonad m, MonadIO m)
                   => m a -> m Bool
reportSuccess m =
  handleAll (\e -> return False) $
  m >> return True
