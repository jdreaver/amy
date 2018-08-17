{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import Control.Monad.Except
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
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
      CompileFiles opts -> compileFiles opts
      Repl -> error "TODO: Fix REPL"

compileFiles :: CompileFilesOptions -> IO ()
compileFiles CompileFilesOptions{..} = do
  eFailure <- runExceptT $ do
    modules <- foldM (processFile cfoDumpFlags) [] cfoFilePaths
    modulesNE <- maybe (throwError ["Empty list of modules!"]) pure $ NE.nonEmpty modules

    rtsLL <- fromMaybe "rts/rts.ll" <$> lift (lookupEnv "RTS_LL_LOCATION")
    liftIO $ linkModules modulesNE rtsLL

  either (die . intercalate "\n") (\_ -> pure ()) eFailure

processFile :: DumpFlags -> [CompiledModule] -> FilePath -> ExceptT [String] IO [CompiledModule]
processFile flags importModules filePath = do
  fileText <- lift $ T.readFile filePath
  let env = foldl1 mergeEnvironments $ primEnvironment : (compiledModuleEnvironment <$> importModules)
  compiled <- ExceptT $ compileModule env filePath flags fileText
  pure $ importModules ++ [compiled]

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
  = CompileFiles !CompileFilesOptions
  | Repl
  deriving (Show, Eq)

getCommand :: IO Command
getCommand =
  execParser $ info (helper <*> commandParser) (fullDesc <> progDesc "Amy compile CLI")

commandParser :: Parser Command
commandParser =
  subparser (
    command "compile"
      (info (helper <*> fmap CompileFiles parseCompileFiles) (progDesc "Compile a file")) <>
    command "repl"
      (info (helper <*> pure Repl) (progDesc "Run the repl"))
  )

data CompileFilesOptions
  = CompileFilesOptions
  { cfoFilePaths :: ![FilePath]
  , cfoDumpFlags :: !DumpFlags
  } deriving (Show, Eq)

parseCompileFiles :: Parser CompileFilesOptions
parseCompileFiles =
  CompileFilesOptions
  <$> some (argument str (
        metavar "FILE" <>
        helpDoc (Just "File to compile")
      ))
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
