module Amy.Codegen.Emit
  ( generateLLVMIR
  ) where

import Data.ByteString (ByteString)
import LLVM.Context
import LLVM.Module
-- import LLVM.Pretty (ppllvm)

import Amy.AST
import Amy.Codegen.Pure
import Amy.Names
import Amy.Type

generateLLVMIR :: AST ValueName Type -> IO ByteString
generateLLVMIR ast =
  withContext $ \context ->
    withModuleFromAST context mod' $ \m ->
      moduleLLVMAssembly m
 where
  mod' = codegenPure ast
