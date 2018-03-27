module Rascal.Codegen.Emit
  ( generateLLVMIR
  ) where

import Data.ByteString (ByteString)
import LLVM.Context
import LLVM.Module
-- import LLVM.Pretty (ppllvm)

import Rascal.AST
import Rascal.Codegen.Pure
import Rascal.Names
import Rascal.Type

generateLLVMIR :: AST IdName Type -> IO ByteString
generateLLVMIR ast =
  withContext $ \context ->
    withModuleFromAST context mod' $ \m ->
      moduleLLVMAssembly m
 where
  mod' = codegenPure ast
