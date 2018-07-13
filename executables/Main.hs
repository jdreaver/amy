{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import Control.Monad.Except
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (die)

import Amy.Compile
import Amy.Environment

main :: IO ()
main =
  getCommand >>=
    \case
      CompileFile opts -> compileFile opts
      Repl -> error "TODO: Fix REPL"

compileFile :: CompileFileOptions -> IO ()
compileFile CompileFileOptions{..} = T.readFile cfoFilePath >>= process cfoFilePath cfoDumpFlags

process :: FilePath -> DumpFlags -> Text -> IO ()
process filePath dumpFlags input = do
  preludePath <- fromMaybe "stdlib/prelude.amy" <$> lookupEnv "PRELUDE_LOCATION"
  preludeText <- T.readFile preludePath

  eFailure <- runExceptT $ do
    prelude@(CompiledModule preludeEnv _) <- ExceptT $ compileModule primEnvironment preludePath dumpFlags preludeText
    let preludeEnv' = preludeEnv `mergeEnvironments` primEnvironment
    compiledModule <- ExceptT $ compileModule preludeEnv' filePath dumpFlags input

    rtsLL <- fromMaybe "rts/rts.ll" <$> lift (lookupEnv "RTS_LL_LOCATION")
    liftIO $ linkModules [prelude] compiledModule rtsLL

  either (die . intercalate "\n") pure eFailure

-- runRepl :: IO ()
-- runRepl = runInputT defaultSettings loop
--  where
--   loop = do
--     minput <- getInputLine "amy> "
--     case minput of
--       Nothing -> outputStrLn "Goodbye."
--       Just input -> do
--         liftIO $ process "<repl>" dumpFlags (pack input)
--         loop

--
-- Options
--

data Command
  = CompileFile !CompileFileOptions
  | Repl
  deriving (Show, Eq)

getCommand :: IO Command
getCommand =
  execParser $ info (helper <*> commandParser) (fullDesc <> progDesc "Amy compile CLI")

commandParser :: Parser Command
commandParser =
  subparser (
    command "compile"
      (info (helper <*> fmap CompileFile parseCompileFile) (progDesc "Compile a file")) <>
    command "repl"
      (info (helper <*> pure Repl) (progDesc "Run the repl"))
  )

data CompileFileOptions
  = CompileFileOptions
  { cfoFilePath :: !FilePath
  , cfoDumpFlags :: !DumpFlags
  } deriving (Show, Eq)

parseCompileFile :: Parser CompileFileOptions
parseCompileFile =
  CompileFileOptions
  <$> argument str (
        metavar "FILE" <>
        helpDoc (Just "File to compile")
      )
  <*> parseDumpFlags

parseDumpFlags :: Parser DumpFlags
parseDumpFlags =
  DumpFlags
  <$> switch (long "dump-parsed" <> helpDoc (Just "Dump parsed AST"))
  <*> switch (long "dump-typechecked" <> helpDoc (Just "Dump type checked AST"))
  <*> switch (long "dump-core" <> helpDoc (Just "Dump Core AST"))
  <*> switch (long "dump-core-lifted" <> helpDoc (Just "Dump lifted Core AST"))
  <*> switch (long "dump-anf" <> helpDoc (Just "Dump ANF AST"))
  <*> switch (long "dump-llvm-pretty" <> helpDoc (Just "Dump pure LLVM AST from llvm-hs-pretty"))
