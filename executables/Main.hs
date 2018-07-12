{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit (die)

import Amy.Compile

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
  eResult <- compileModule filePath dumpFlags input
  either (die . intercalate "\n") pure eResult

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
