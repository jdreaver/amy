{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import Control.Monad (when)
import Control.Monad.Except
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import LLVM.Pretty (ppllvm)
import Options.Applicative
-- import System.Console.Haskeline
import System.Exit (die)
import System.FilePath.Posix (replaceExtension)
import Text.Megaparsec

import Amy.ANF as ANF
import Amy.Codegen
import Amy.Core as C
import Amy.Errors
import Amy.Syntax as S
import Amy.TypeCheck as T

main :: IO ()
main =
  getCommand >>=
    \case
      CompileFile opts -> compileFile opts
      Repl -> error "TODO: Fix REPL"

compileFile :: CompileFileOptions -> IO ()
compileFile CompileFileOptions{..} = T.readFile cfoFilePath >>= process cfoFilePath cfoDumpFlags

process :: FilePath -> DumpFlags -> Text -> IO ()
process filePath DumpFlags{..} input = do
  eResult <- runExceptT $ do
    -- Parse
    tokens' <- liftEither $ first ((:[]) . parseErrorPretty) $ lexer filePath input
    parsed <- liftEither $ first ((:[]) . parseErrorPretty) $ parse (runAmyParser parseModule) filePath tokens'
    when dfDumpParsed $
      lift $ writeFile (filePath `replaceExtension` ".amy-parsed") (show $ S.prettyModule parsed)

    -- Type checking
    typeChecked <- liftEither $ first ((:[]) . showError) $ T.inferModule parsed
    when dfDumpTypeChecked $
      lift $ writeFile (filePath `replaceExtension` ".amy-typechecked") (show $ T.prettyModule typeChecked)

    -- Desugar to Core
    let core = desugarModule typeChecked
    when dfDumpCore $
      lift $ writeFile (filePath `replaceExtension` ".amy-core") (show $ C.prettyModule core)

    -- Normalize to ANF
    let anf = normalizeModule core
    when dfDumpANF $
      lift $ writeFile (filePath `replaceExtension` ".amy-anf") (show $ ANF.prettyModule anf)

    -- Codegen to pure LLVM
    let llvmAST = codegenModule anf
    when dfDumpLLVMPretty $
      lift $ TL.writeFile (filePath `replaceExtension` ".ll-pretty") (ppllvm llvmAST)

    -- Generate LLVM IR using C++ API
    llvm <- lift $ generateLLVMIR llvmAST
    when dfDumpLLVM $
      lift $ BS8.writeFile (filePath `replaceExtension` ".ll") llvm

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

data DumpFlags
  = DumpFlags
  { dfDumpParsed :: !Bool
  , dfDumpTypeChecked :: !Bool
  , dfDumpCore :: !Bool
  , dfDumpANF :: !Bool
  , dfDumpLLVMPretty :: !Bool
  , dfDumpLLVM :: !Bool
  } deriving (Show, Eq)

-- dumpFlags :: DumpFlags
-- dumpFlags =
--   DumpFlags
--   { dfDumpParsed = False
--   , dfDumpTypeChecked = False
--   , dfDumpCore = False
--   , dfDumpANF = False
--   , dfDumpLLVMPretty = False
--   , dfDumpLLVM = False
--   }

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
  <*> switch (long "dump-anf" <> helpDoc (Just "Dump ANF AST"))
  <*> switch (long "dump-llvm-pretty" <> helpDoc (Just "Dump pure LLVM AST from llvm-hs-pretty"))
  <*> switch (long "dump-llvm" <> helpDoc (Just "Dump LLVM IR"))
