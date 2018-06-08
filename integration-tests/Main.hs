{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import Control.Exception (throw)
import Control.Monad (unless, when)
import Control.Monad.Except
import Data.Either (isRight)
import Data.Text (Text, unpack)
import Data.Traversable (traverse)
import Data.Yaml
import System.Exit (ExitCode(..), exitFailure)
import System.Process

main :: IO ()
main = do
  (testDefs :: [TestDefinition])  <- either throw pure =<< decodeFileEither "tests.yaml"
  results <- traverse runTest testDefs
  unless (and results) exitFailure

runTest :: TestDefinition -> IO Bool
runTest testDef = do
  putStrLn $ "Running test '" ++ unpack (testName testDef) ++ "'..."
  result <- runExceptT $ runTest' testDef
  either (\e -> putStrLn $ "Failed:\n" ++ e) (const $ putStrLn "Success!") result
  pure $ isRight result

runTest' :: TestDefinition -> ExceptT String IO ()
runTest' (TestDefinition name expectProgExitCode) = do
  let
    testDir = "tests/" ++ unpack name ++ "/"
    sourcePath = testDir ++ unpack name ++ ".amy"
    llvmPath = testDir ++ unpack name ++ ".ll"

  -- Compile program
  (compilerExitCode, compilerStdout, compilerStderr) <- liftIO $ readProcessWithExitCode "amy" ["compile", sourcePath] ""
  when (compilerExitCode /= ExitSuccess) $
    throwError $ "Compilation failed. stdout:\n" ++ compilerStdout ++ "\nstderr:\n" ++ compilerStderr

  -- Print LLVM to file
  liftIO $ writeFile llvmPath compilerStdout

  -- Run llvm program
  (programExitCode, programStdout, programStderr) <- liftIO $ readProcessWithExitCode "lli" [llvmPath] ""
  when (programExitCode /= expectProgExitCode) $
    throwError $
    "Incorrect program exit code. Expected: " ++ show expectProgExitCode ++
    " got: " ++ show programExitCode ++
    (if null programStdout then "" else "\nstdout:\n" ++ programStdout) ++
    (if null programStderr then "" else "\nstderr:\n" ++ programStderr)

data TestDefinition
  = TestDefinition
  { testName :: !Text
  , testProgramExitCode :: !ExitCode
  } deriving (Show, Eq)

instance FromJSON TestDefinition where
  parseJSON = withObject "TestDefinition" $ \o -> do
    testName <- o .: "name"
    exitCode <- o .: "program_exit_code"
    let
      testProgramExitCode =
        case exitCode of
          0 -> ExitSuccess
          _ -> ExitFailure exitCode
    pure TestDefinition{..}
