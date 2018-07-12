{-# LANGUAGE RecordWildCards #-}

module Amy.Compile
  ( compileModule
  , DumpFlags(..)
  ) where

import Control.Monad (when)
import Control.Monad.Except
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TL
import LLVM.Pretty (ppllvm)
import System.Environment (lookupEnv)
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

compileModule
  :: FilePath
     -- ^ Original module file path
  -> DumpFlags
     -- ^ Flags to control intermediate output
  -> Text
     -- ^ Module source code
  -> IO (Either [String] ())
     -- ^ Return any possible errors
compileModule filePath DumpFlags{..} input = runExceptT $ do
  -- Parse
  tokens' <- liftEither $ first ((:[]) . parseErrorPretty) $ lexer filePath input
  parsed <- liftEither $ first ((:[]) . parseErrorPretty) $ parse (runAmyParser parseModule) filePath tokens'
  when dfDumpParsed $
    lift $ writeFile (filePath `replaceExtension` ".amy-parsed") (show $ S.prettyModule parsed)

  -- Type checking
  (typeChecked, typeCheckedEnv) <- liftEither $ first ((:[]) . showError) $ TC.inferModule primEnvironment parsed
  when dfDumpTypeChecked $
    lift $ writeFile (filePath `replaceExtension` ".amy-typechecked") (show $ S.prettyModule typeChecked)

  -- Desugar to Core
  let
    coreEnv = mergeEnvironments primEnvironment typeCheckedEnv
    core = desugarModule coreEnv typeChecked
  when dfDumpCore $
    lift $ writeFile (filePath `replaceExtension` ".amy-core") (show $ C.prettyModule core)

  -- Prepare for ANF
  let lifted = lambdaLifting core
  when dfDumpCoreLifted $
    lift $ writeFile (filePath `replaceExtension` ".amy-core-lifted") (show $ C.prettyModule lifted)

  -- Normalize to ANF
  let
    anfEnv = coreEnv
    (anf, _) = normalizeModule lifted anfEnv
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

data DumpFlags
  = DumpFlags
  { dfDumpParsed :: !Bool
  , dfDumpTypeChecked :: !Bool
  , dfDumpCore :: !Bool
  , dfDumpCoreLifted :: !Bool
  , dfDumpANF :: !Bool
  , dfDumpLLVMPretty :: !Bool
  } deriving (Show, Eq)
