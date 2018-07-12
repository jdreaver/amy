module Amy.Codegen.Emit
  ( generateLLVMIR
  , linkModuleIRs
  , optimizeLLVM
  ) where

import Control.Monad (foldM, void)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import LLVM.AST
import LLVM.Context
import LLVM.Module
import LLVM.PassManager

generateLLVMIR :: LLVM.AST.Module -> IO ByteString
generateLLVMIR mod' =
  withContext $ \context ->
    withModuleFromAST context mod' $ \m ->
      moduleLLVMAssembly m

linkModuleIRs :: NonEmpty FilePath -> IO ByteString
linkModuleIRs modules =
  withContext $ \context -> do
    let
      go mod1 mod2IR =
        withModuleFromLLVMAssembly context (File mod2IR) $ \mod2 -> do
          linkModules mod1 mod2
          pure mod1
      mod1IR :| restIR = modules
    withModuleFromLLVMAssembly context (File mod1IR) $ \mod1 -> do
      linked <- foldM go mod1 restIR
      moduleLLVMAssembly linked

optimizeLLVM :: FilePath -> IO ByteString
optimizeLLVM path =
  withContext $ \context ->
  withModuleFromLLVMAssembly context (File path) $ \llvm ->
  withPassManager passSpec $ \passManager -> do
    void $ runPassManager passManager llvm
    moduleLLVMAssembly llvm
 where
  passSpec =
    defaultCuratedPassSetSpec
    { optLevel = Just 3
    }
