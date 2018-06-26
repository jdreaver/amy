module Amy.Codegen.Emit
  ( generateLLVMIR
  , linkRTS
  , optimizeLLVM
  ) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import LLVM.AST
import LLVM.Context
import LLVM.Module
import LLVM.PassManager

generateLLVMIR :: LLVM.AST.Module -> IO ByteString
generateLLVMIR mod' =
  withContext $ \context ->
    withModuleFromAST context mod' $ \m ->
      moduleLLVMAssembly m

linkRTS :: FilePath -> FilePath -> IO ByteString
linkRTS rtsPath irPath =
  withContext $ \context ->
  withModuleFromLLVMAssembly context (File rtsPath) $ \rts ->
  withModuleFromLLVMAssembly context (File irPath) $ \ir -> do
    linkModules rts ir
    moduleLLVMAssembly rts

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
