{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import Data.List (intercalate)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import LLVM.Pretty (ppllvm)
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
compileFile CompileFileOptions{..} = T.readFile cfoFilePath >>= process cfoFilePath cfoDumpFlags

process :: FilePath -> DumpFlags -> Text -> IO ()
process filePath DumpFlags{..} input = do
  eCodegenString <- runExceptT $ do
    -- Parse
    parsed <- liftEither $ first ((:[]) . ParserError) $ parse (runAmyParser parseModule) filePath input
    when dfDumpParsed $
      lift $ putStrLn "\nParsed:" >> print (S.prettyModule parsed)

    -- Rename
    renamed <- liftEither $ rename parsed

    -- Type checking
    typeChecked <- liftEither $ first (:[]) $ inferModule renamed
    when dfDumpTypeChecked $
      lift $ putStrLn "\nType Checked:" >> print (T.prettyModule typeChecked)

    -- Desugar to Core
    let core = desugarModule typeChecked
    when dfDumpCore $
      lift $ putStrLn "\nCore:" >> print (C.prettyModule core)

    -- Normalize to ANF
    let anf = normalizeModule core
    when dfDumpANF $
      lift $ putStrLn "\nANF:" >> print (ANF.prettyModule anf)

    -- Codegen to pure LLVM
    let llvmAST = codegenModule anf
    when dfDumpLLVMAST $
      lift $ putStrLn "\nLLVM AST:" >> TL.putStrLn (ppllvm llvmAST)

    -- Generate LLVM IR using C++ API
    llvm <- lift $ generateLLVMIR llvmAST
    when dfDumpLLVM $
      lift $ putStrLn "\nLLVM:" >> BS8.putStrLn llvm

    pure llvm

  either showErrors BS8.putStrLn eCodegenString

showErrors :: [Error] -> IO ()
showErrors = hPutStrLn stderr . intercalate "\n" . fmap showError

runRepl :: IO ()
runRepl = runInputT defaultSettings loop
 where
  loop = do
    minput <- getInputLine "amy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        liftIO $ process "<repl>" dumpFlags (pack input)
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
  , dfDumpLLVMAST :: !Bool
  , dfDumpLLVM :: !Bool
  } deriving (Show, Eq)

dumpFlags :: DumpFlags
dumpFlags =
  DumpFlags
  { dfDumpParsed = False
  , dfDumpTypeChecked = False
  , dfDumpCore = False
  , dfDumpANF = False
  , dfDumpLLVMAST = False
  , dfDumpLLVM = False
  }

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
  <*> switch (long "dump-llvm-ast" <> helpDoc (Just "Dump pure LLVM AST"))
  <*> switch (long "dump-llvm" <> helpDoc (Just "Dump LLVM IR"))
