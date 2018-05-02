{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS8
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Options.Applicative
import System.Console.Haskeline
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec

import Amy.ANF as ANF
import Amy.Codegen
import Amy.Core as C
import Amy.Errors
import Amy.Renamer
import Amy.Syntax as S
import Amy.TypeCheck as T

main :: IO ()
main =
  getCommand >>=
    \case
      CompileFile opts -> compileFile opts
      Repl -> runRepl

compileFile :: CompileFileOptions -> IO ()
compileFile CompileFileOptions{..} = T.readFile cfoFilePath >>= process cfoFilePath

process :: FilePath -> Text -> IO ()
process inputFile input =
  let
    eModule :: Either [Error] T.Module
    eModule = do
      parsed <- first ((:[]) . ParserError) $ parse (runAmyParser parseModule) inputFile input
      renamed <- rename parsed
      first (:[]) $ inferModule renamed
  in do
    eCodegenString <- traverse (generateLLVMIR . codegenModule . normalizeModule . desugarModule) eModule
    either (hPutStrLn stderr . intercalate "\n" . fmap showError) BS8.putStrLn eCodegenString

runRepl :: IO ()
runRepl = runInputT defaultSettings loop
 where
  loop = do
    minput <- getInputLine "amy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        liftIO $ process "<repl>" (pack input)
        loop

--
-- Options
--

data Command
  = CompileFile !CompileFileOptions
  | Repl
  deriving (Show, Eq)

getCommand :: IO Command
getCommand =
  execParser $ info (helper <*> commandParser) (fullDesc <> progDesc "Front Row Ops script")

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
  } deriving (Show, Eq)

parseCompileFile :: Parser CompileFileOptions
parseCompileFile =
  CompileFileOptions
  <$> argument str (
        metavar "FILE" <>
        helpDoc (Just "File to compile")
      )
