{-# LANGUAGE LambdaCase #-}

module Amy.Codegen.TypeConversion
  ( maybeConvertPointer
  , loadPointerToType
  , operandType
  ) where

import LLVM.AST
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float as F

import Amy.Codegen.Monad

maybeConvertPointer :: Operand -> Type -> BlockGen Operand
maybeConvertPointer op targetTy =
  if operandType op == targetTy
  then pure op
  else
    case (operandType op, targetTy) of
      (PointerType _ _, PointerType _ _) -> maybeBitcast targetTy op
      (_, PointerType _ _) -> allocOp op >>= maybeBitcast targetTy
      (PointerType _ _, _) -> maybeBitcast (PointerType targetTy (AddrSpace 0)) op >>= loadPointerToType targetTy
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

loadPointerToType :: Type -> Operand -> BlockGen Operand
loadPointerToType targetTy op = do
  resultName <- freshUnName
  let resultOp = LocalReference targetTy resultName
  addInstruction $ resultName := Load False op Nothing 0 []
  pure resultOp

maybeBitcast :: Type -> Operand -> BlockGen Operand
maybeBitcast ty op =
  -- Bitcast if we have to
  if operandType op == ty
  then pure op
  else do
   ptrName <- freshUnName
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
