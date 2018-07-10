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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import LLVM.Pretty (ppllvm)
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath.Posix ((</>), dropExtension, replaceExtension, takeDirectory)
import System.Process (callProcess)
import Text.Megaparsec

import Amy.ANF as ANF
import Amy.Codegen
import Amy.Core as C
import Amy.Environment
import Amy.Errors
import Amy.Syntax as S
import Amy.TypeCheck as TC

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
    typeChecked <- liftEither $ first ((:[]) . showError) $ TC.inferModule primEnvironment parsed
    when dfDumpTypeChecked $
      lift $ writeFile (filePath `replaceExtension` ".amy-typechecked") (show $ S.prettyModule typeChecked)

    -- Desugar to Core
    let core = desugarModule typeChecked
    when dfDumpCore $
      lift $ writeFile (filePath `replaceExtension` ".amy-core") (show $ C.prettyModule core)

    -- Prepare for ANF
    let lifted = lambdaLifting core
    when dfDumpCoreLifted $
      lift $ writeFile (filePath `replaceExtension` ".amy-core-lifted") (show $ C.prettyModule lifted)

    -- Normalize to ANF
    let anf = normalizeModule lifted
    when dfDumpANF $
      lift $ writeFile (filePath `replaceExtension` ".amy-anf") (show $ ANF.prettyModule anf)

    -- Codegen to pure LLVM
    let llvmAST = codegenModule anf
    when dfDumpLLVMPretty $
      lift $ TL.writeFile (filePath `replaceExtension` ".ll-pretty") (ppllvm llvmAST)

    -- Generate LLVM IR using C++ API
    let llvmFile = filePath `replaceExtension` ".ll"
    llvm <- lift $ generateLLVMIR llvmAST
    lift $ BS8.writeFile llvmFile llvm

    -- Link RTS
    let linkedLL = dropExtension filePath ++ "-rts-linked.ll"
    rtsLL <- fromMaybe "rts/rts.ll" <$> lift (lookupEnv "RTS_LL_LOCATION")
    linked <- lift $ linkRTS rtsLL llvmFile
    lift $ BS8.writeFile linkedLL linked

    -- Optimize LLVM
    -- TODO: This breaks some examples
    -- let optLL = dropExtension filePath ++ "-rts-opt.ll"
    -- opt <- lift $ optimizeLLVM linkedLL
    -- lift $ BS8.writeFile optLL opt

    -- Compile with clang
    let exeFile = takeDirectory filePath </> "a.out"
    lift $ callProcess "clang" ["-lgc", "-o", exeFile, linkedLL]

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
  , dfDumpCoreLifted :: !Bool
  , dfDumpANF :: !Bool
  , dfDumpLLVMPretty :: !Bool
  --, dfDumpLLVM :: !Bool
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
  <*> switch (long "dump-core-lifted" <> helpDoc (Just "Dump lifted Core AST"))
  <*> switch (long "dump-anf" <> helpDoc (Just "Dump ANF AST"))
  <*> switch (long "dump-llvm-pretty" <> helpDoc (Just "Dump pure LLVM AST from llvm-hs-pretty"))
  -- <*> switch (long "dump-llvm" <> helpDoc (Just "Dump LLVM IR"))
