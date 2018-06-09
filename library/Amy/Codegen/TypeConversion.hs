{-# LANGUAGE LambdaCase #-}

module Amy.Codegen.TypeConversion
  ( maybeConvertPointer
  , loadPointerToType
  , operandType
  , bindOpToName
  ) where

import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import LLVM.AST
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float as F

import Amy.Codegen.Monad

maybeConvertPointer :: Maybe Name -> Operand -> Type -> BlockGen Operand
maybeConvertPointer mName op targetTy =
  if operandType op == targetTy
  then
    fromMaybe op <$> for mName (\name' -> bindOpToName name' op)
  else
    case (operandType op, targetTy) of
      (PointerType _ _, PointerType _ _) -> maybeBitcast mName targetTy op
      (_, PointerType _ _) -> allocOp op >>= maybeBitcast mName targetTy
      (PointerType _ _, _) -> maybeBitcast Nothing (PointerType targetTy (AddrSpace 0)) op >>= loadPointerToType mName targetTy
      (_, _) -> error $ "Failed to maybeConvertToPointer " ++ show (op, targetTy)

allocOp :: Operand -> BlockGen Operand
allocOp op = do
  -- Store operand in a pointer
  storeName <- freshUnName
  let
    opTy = operandType op
    storeOp = LocalReference (PointerType opTy (AddrSpace 0)) storeName
  addInstruction $ storeName := Alloca opTy Nothing 0 []
  addInstruction $ Do $ Store False storeOp op Nothing 0 []
  pure storeOp

loadPointerToType :: Maybe Name -> Type -> Operand -> BlockGen Operand
loadPointerToType mName targetTy op = do
  resultName <- maybe freshUnName pure mName
  let resultOp = LocalReference targetTy resultName
  addInstruction $ resultName := Load False op Nothing 0 []
  pure resultOp

maybeBitcast :: Maybe Name -> Type -> Operand -> BlockGen Operand
maybeBitcast mName ty op =
  -- Bitcast if we have to
  if operandType op == ty
  then
    fromMaybe op <$> for mName (\name' -> bindOpToName name' op)
  else do
   ptrName <- maybe freshUnName pure mName
   let ptrOp = LocalReference ty ptrName
   addInstruction $ ptrName := BitCast op ty []
   pure ptrOp

operandType :: Operand -> Type
operandType (LocalReference ty _) = ty
operandType (ConstantOperand c) = constantType c
operandType md@(MetadataOperand _) = error $ "Can't get operandType for MetadataOperand: " ++ show md

constantType :: C.Constant -> Type
constantType c =
  case c of
    C.GlobalReference ty _ -> ty
    C.Int bits _ -> IntegerType bits
    C.Float ft -> FloatingPointType (someFloatType ft)
    -- Text constant references
    C.GetElementPtr _ (C.GlobalReference (PointerType (ArrayType _ ty) _) _) _ -> PointerType ty (AddrSpace 0)
    _ -> error $ "Unknown type for constant: " ++ show c

someFloatType :: SomeFloat -> FloatingPointType
someFloatType =
  \case
    Half _ -> HalfFP
    Single _ -> FloatFP
    Double _ -> DoubleFP
    Quadruple _ _ -> FP128FP
    X86_FP80 _ _ -> X86_FP80FP
    PPC_FP128 _ _ -> PPC_FP128FP

-- floatingPointBits :: FloatingPointType -> Int
-- floatingPointBits =
--   \case
--     HalfFP -> 16
--     FloatFP -> 32
--     DoubleFP -> 64
--     FP128FP -> 128
--     X86_FP80FP -> 80
--     PPC_FP128FP -> 128

bindOpToName :: Name -> Operand -> BlockGen Operand
bindOpToName name' op = do
  storeName <- freshUnName
  let
    storeOp = LocalReference (PointerType (operandType op) (AddrSpace 0)) storeName
  addInstruction $ storeName := Alloca (operandType op) Nothing 0 []
  addInstruction $ Do $ Store False storeOp op Nothing 0 []
  addInstruction $ name' := Load False storeOp Nothing 0 []
  pure $ LocalReference (operandType op) name'
