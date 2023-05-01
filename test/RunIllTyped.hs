module Main where

import Data.List

import System.Process
import System.Directory
import System.FilePath
import System.Exit
import System.Environment

main :: IO ()
main = do
    dir <- getCurrentDirectory
    putStrLn dir
    let testDir = dir </> "test/IllTyped"
    putStrLn testDir
    dirEntries <- listDirectory testDir
    let qualifiedDirEntries = map (testDir </>) dirEntries
    results <- mapM (doTest (ExitFailure 1)) qualifiedDirEntries
    let (successes, failures) = partition id results
    putStrLn $ "Tests passed: " ++ show (length successes) ++ " | Tests failed: " ++ show (length failures)
    if null failures then exitSuccess else exitFailure

-- run the given file, expect error code
doTest :: ExitCode -> FilePath -> IO Bool
doTest expectedExitCode file = do
    putStrLn $ "Testing " ++ file
    (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal" ["run", file, "--ghc-option=-fplugin=Rattus.Plugin"] ""
    putStrLn "--- stdout ---"
    putStrLn stdout
    putStrLn "--- stderr ---"
    putStrLn stderr
    putStrLn "--- result ---"
    putStrLn $ "Expected exit code: " ++ show expectedExitCode ++ ", got " ++ show exitCode
    return (exitCode == expectedExitCode)
