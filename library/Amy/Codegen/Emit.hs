module Amy.Codegen.Emit
  ( generateLLVMIR
  ) where

import Data.ByteString (ByteString)
import LLVM.AST
import LLVM.Context
import LLVM.Module
-- import LLVM.Pretty (ppllvm)

generateLLVMIR :: LLVM.AST.Module -> IO ByteString
generateLLVMIR mod' =
  withContext $ \context ->
    withModuleFromAST context mod' $ \m ->
      moduleLLVMAssembly m
