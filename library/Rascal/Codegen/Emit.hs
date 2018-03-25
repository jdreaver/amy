module Rascal.Codegen.Emit
  ( generateLLVMIR
  ) where

import Data.ByteString (ByteString)
import LLVM.Context
import LLVM.Module

import Rascal.Codegen.Pure
import Rascal.TypeCheck

generateLLVMIR :: TypeCheckAST -> IO ByteString
generateLLVMIR ast =
  withContext $ \context ->
    withModuleFromAST context mod' $ \m ->
      moduleLLVMAssembly m
 where
  mod' = codegenPure ast
