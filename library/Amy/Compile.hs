{-# LANGUAGE RecordWildCards #-}

module Amy.Compile
  ( compileModule
  , linkModules
  , DumpFlags(..)
  , CompiledModule(..)
  ) where

import Control.Monad (when)
import Control.Monad.Except
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TL
import LLVM.Pretty (ppllvm)
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
  :: Environment
     -- ^ 'Environment' for any dependencies
  -> FilePath
     -- ^ Original module file path
  -> DumpFlags
     -- ^ Flags to control intermediate output
  -> Text
     -- ^ Module source code
  -> IO (Either [String] CompiledModule)
     -- ^ Return any possible errors or a compiled module
compileModule depsEnv filePath DumpFlags{..} input = runExceptT $ do
  -- Parse
  tokens' <- liftEither $ first ((:[]) . parseErrorPretty) $ lexer filePath input
  parsed <- liftEither $ first ((:[]) . parseErrorPretty) $ parse (runAmyParser parseModule) filePath tokens'
  when dfDumpParsed $
    lift $ writeFile (filePath `replaceExtension` ".amy-parsed") (show $ S.prettyModule parsed)

  -- Type checking
  (typeChecked, typeCheckedModuleEnv) <- liftEither $ first ((:[]) . showError) $ TC.inferModule depsEnv parsed
  when dfDumpTypeChecked $
    lift $ writeFile (filePath `replaceExtension` ".amy-typechecked") (show $ S.prettyModule typeChecked)

  -- Desugar to Core
  let
    coreEnv = mergeEnvironments depsEnv typeCheckedModuleEnv
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
    (anf, anfModuleEnv) = normalizeModule lifted anfEnv
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

  -- Construct CompiledModule
  let
    moduleEnv = typeCheckedModuleEnv `mergeEnvironments` anfModuleEnv
    compiledModule = CompiledModule moduleEnv llvmFile
  pure compiledModule

linkModules :: [CompiledModule] -> CompiledModule -> FilePath -> IO ()
linkModules depModules module' rtsLL = do
  let
    depFiles = compiledModuleLLVM <$> depModules
    moduleFile = compiledModuleLLVM module'

  -- Link dependencies
  let linkedLL = dropExtension moduleFile ++ "-rts-linked.ll"
  linked <- linkModuleIRs (moduleFile :| rtsLL : depFiles)
  BS8.writeFile linkedLL linked

  -- Optimize LLVM
  -- TODO: This breaks some examples
  -- let optLL = dropExtension filePath ++ "-rts-opt.ll"
  -- opt <- lift $ optimizeLLVM linkedLL
  -- lift $ BS8.writeFile optLL opt

  -- Compile with clang
  let exeFile = takeDirectory moduleFile </> "a.out"
  callProcess "clang" ["-lgc", "-o", exeFile, linkedLL]

data DumpFlags
  = DumpFlags
  { dfDumpParsed :: !Bool
  , dfDumpTypeChecked :: !Bool
  , dfDumpCore :: !Bool
  , dfDumpCoreLifted :: !Bool
  , dfDumpANF :: !Bool
  , dfDumpLLVMPretty :: !Bool
  } deriving (Show, Eq)

data CompiledModule
  = CompiledModule
  { compiledModuleEnvironment :: !Environment
  , compiledModuleLLVM :: !FilePath
  } deriving (Show, Eq)
