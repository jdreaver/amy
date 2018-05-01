{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amy.Codegen.Pure
  ( codegenModule
  ) where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (for)
import LLVM.AST as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float as F
import LLVM.AST.Global as LLVM
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Linkage as L

import Amy.ANF as ANF
import Amy.Codegen.CaseBlocks
import Amy.Codegen.Monad
import Amy.Codegen.TypeConstructors
import Amy.Literal
import Amy.Prim

codegenModule :: ANF.Module -> LLVM.Module
codegenModule (ANF.Module bindings externs typeDeclarations) =
  let
    topLevelTypes =
      ((\(ANF.Binding name' (ANF.Forall _ ty) _ _ _) -> (name', ty)) <$> bindings)
      ++ ((\(ANF.Extern name' ty) -> (name', ty)) <$> externs)
    definitions = runCodeGen topLevelTypes typeDeclarations $ do
      externs' <- traverse codegenExtern externs
      bindings' <- traverse codegenTopLevelBinding bindings
      pure $ externs' ++ bindings'
  in
    defaultModule
    { moduleName = "amy-module"
    , moduleDefinitions = definitions
    }

codegenExtern :: ANF.Extern -> CodeGen Definition
codegenExtern extern = do
  let
    (paramTypes, returnType') = argAndReturnTypes (ANF.externType extern)
    mkParam ty = do
      ty' <- llvmType ty
      pure $ Parameter ty' (UnName 0) []
  params <- traverse mkParam paramTypes
  retTy <- llvmType returnType'
  pure $
    GlobalDefinition
    functionDefaults
    { name = identToLLVM $ ANF.externName extern
    , parameters = (params, False)
    , LLVM.returnType = retTy
    }

codegenTopLevelBinding :: ANF.Binding -> CodeGen Definition
codegenTopLevelBinding binding = do
  let
    argToParam (ANF.Typed ty ident) = do
      ty' <- llvmType ty
      pure $ Parameter ty' (identToLLVM ident) []
  params <- traverse argToParam (ANF.bindingArgs binding)
  returnType' <- llvmType $ ANF.bindingReturnType binding
  blocks <- codegenExpr $ ANF.bindingBody binding
  pure $
    GlobalDefinition
    functionDefaults
    { name = identToLLVM $ ANF.bindingName binding
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    , basicBlocks = blocks
    , linkage =
        if ANF.identText (ANF.bindingName binding) == "main"
          then L.External
          else L.Private
    }

argAndReturnTypes :: ANF.Type -> ([ANF.Type], ANF.Type)
argAndReturnTypes ty = (NE.init tyNE, NE.last tyNE)
 where
  tyNE = typeToNonEmpty ty

codegenExpr :: ANF.Expr -> CodeGen [BasicBlock]
codegenExpr expr = runBlockGen $ codegenExpr' expr

codegenExpr' :: ANF.Expr -> BlockGen Operand
codegenExpr' (ANF.EVal val) = valOperand val
codegenExpr' (ANF.ELet (ANF.Let bindings expr)) = do
  for_ bindings $ \binding -> do
    op <- codegenExpr' (ANF.bindingBody binding)
    addSymbolToTable (ANF.bindingName binding) op
  codegenExpr' expr
codegenExpr' (ANF.ECase case'@(ANF.Case scrutinee _ _)) = do
  caseId <- freshId

  -- TODO: Move a lot of this logic to ANF. conversion, like finding the default
  -- match and converting match patterns to literals.
  compilationMethods' <- compilationMethods
  let
    mkCaseName nameBase = LLVM.Name $ stringToShortBS $ nameBase ++ show caseId
    (CaseBlocks switchDefaultBlockName mDefaultBlock literalBlocks endBlock) = caseBlocks mkCaseName compilationMethods' case'
    (CaseEndBlock endBlockName endOpName endType) = endBlock

  -- Generate the switch statement
  scrutineeOp <- valOperand scrutinee
  let
    switchNames = (\(CaseLiteralBlock _ name' _ constant) -> (constant, name')) <$> literalBlocks
  terminateBlock (Do $ Switch scrutineeOp switchDefaultBlockName switchNames []) switchDefaultBlockName

  -- Generate case blocks
  let
    generateBlockExpr expr nextBlockName = do
      op <- codegenExpr' expr
      -- N.B. The block name could have changed while generating the
      -- expression, so we need to get the "actual" block name.
      finalBlockName <- currentBlockName
      terminateBlock (Do $ Br endBlockName []) nextBlockName
      pure (op, finalBlockName)
    generateCaseVarBlock (CaseVarBlock expr _ nextBlockName ident) = do
      addSymbolToTable ident scrutineeOp
      generateBlockExpr expr nextBlockName
    generateCaseLiteralBlock (CaseLiteralBlock expr _ nextBlockName _) =
      generateBlockExpr expr nextBlockName

  mDefaultOpAndBlock <- traverse generateCaseVarBlock mDefaultBlock
  matchOpsAndBlocks <- traverse generateCaseLiteralBlock literalBlocks

  -- Generate end block
  endTy <- llvmType endType
  let
    allOpsAndBlocks = maybe id (:) mDefaultOpAndBlock matchOpsAndBlocks
    endOpRef = LocalReference endTy endOpName
  addInstruction $ endOpName := Phi endTy allOpsAndBlocks []
  pure endOpRef
codegenExpr' (ANF.EApp (ANF.App (ANF.VVal (ANF.Typed originalTy ident)) args' returnTy)) = do
  ty <- fromMaybe originalTy <$> topLevelType ident
  funcOperand <- valOperand (ANF.Var $ ANF.VVal $ ANF.Typed ty ident)
  let
    (argTys', returnTy') = argAndReturnTypes ty
  opName <- freshUnName
  -- Convert arguments to pointers if we have to
  argOps <- for (zip args' argTys') $ \(arg, argTy) -> do
    originalOp <- valOperand arg
    argTy' <- llvmType argTy
    convertLLVMType originalOp argTy'

  -- Add call instruction
  addInstruction $ opName := Call Nothing CC.C [] (Right funcOperand) ((\arg -> (arg, [])) <$> argOps) [] []

  -- Return operand, maybe converting it too
  returnTyLLVM <- llvmType returnTy
  returnTyLLVM' <- llvmType returnTy'
  convertLLVMType (LocalReference returnTyLLVM' opName) returnTyLLVM
codegenExpr' (ANF.EApp app@(ANF.App (ANF.VCons (ANF.Typed _ consName)) args' _)) = do
  method <- findCompilationMethod consName <$> compilationMethods
  case method of
    CompileUnboxed _ ->
      case args' of
        [arg] -> valOperand arg
        _ -> error $ "Can't unbox App because there isn't exactly one argument " ++ show app
    CompileEnum i -> valOperand $ ANF.Lit (LiteralInt i)
    CompileTaggedPairs _ -> error $ "Can't compile tagged pairs yet " ++ show app
codegenExpr' (ANF.EPrimOp (ANF.App prim args' returnTy)) = do
  opName <- freshUnName
  argOps <- traverse valOperand args'
  addInstruction $ opName := primitiveFunctionInstruction prim argOps
  returnTy' <- llvmType returnTy
  pure $ LocalReference returnTy' opName

convertLLVMType :: Operand -> LLVM.Type -> BlockGen Operand
convertLLVMType op targetTy
 | opType == targetTy = pure op
 | otherwise =
     case (opType, targetTy) of
       (IntegerType _, PointerType p _) -> convertIntToPointer p op
       (PointerType _ _, IntegerType _) -> convertPointerToInt targetTy op
       (FloatingPointType ft, PointerType p _) -> convertFloatToInt ft op >>= convertIntToPointer p
       (PointerType _ _, FloatingPointType ft) -> convertPointerToInt (IntegerType 64) op >>= convertIntToFloat ft
       (_, _) -> error $ "Failed to convertLLVMType " ++ show (opType, targetTy)
 where
  opType = operandType op

convertIntToPointer :: LLVM.Type -> Operand -> BlockGen Operand
convertIntToPointer pointerType op = do
  opName <- freshUnName
  let ptrTy = PointerType pointerType (AddrSpace 0)
  addInstruction $ opName := IntToPtr op ptrTy []
  pure $ LocalReference ptrTy opName

convertPointerToInt :: LLVM.Type -> Operand -> BlockGen Operand
convertPointerToInt ty op = do
  opName <- freshUnName
  addInstruction $ opName := PtrToInt op ty []
  pure $ LocalReference ty opName

convertFloatToInt :: FloatingPointType -> Operand -> BlockGen Operand
convertFloatToInt ft op = do
  opName <- freshUnName
  let ty = IntegerType (fromIntegral $ floatingPointBits ft)
  addInstruction $ opName := FPToUI op ty []
  pure $ LocalReference ty opName

convertIntToFloat :: FloatingPointType -> Operand -> BlockGen Operand
convertIntToFloat ft op = do
  opName <- freshUnName
  let ty = FloatingPointType ft
  addInstruction $ opName := UIToFP op ty []
  pure $ LocalReference ty opName

operandType :: Operand -> LLVM.Type
operandType (LocalReference ty _) = ty
operandType (ConstantOperand c) =
  case c of
    C.GlobalReference ty _ -> ty
    C.Int bits _ -> IntegerType bits
    C.Float ft -> FloatingPointType (someFloatType ft)
    _ -> error $ "Unknown type for operandType: " ++ show c
operandType md@(MetadataOperand _) = error $ "Can't get operandType for MetadataOperand: " ++ show md

someFloatType :: SomeFloat -> FloatingPointType
someFloatType =
  \case
    Half _ -> HalfFP
    Single _ -> FloatFP
    Double _ -> DoubleFP
    Quadruple _ _ -> FP128FP
    X86_FP80 _ _ -> X86_FP80FP
    PPC_FP128 _ _ -> PPC_FP128FP

floatingPointBits :: FloatingPointType -> Int
floatingPointBits =
  \case
    HalfFP -> 16
    FloatFP -> 32
    DoubleFP -> 64
    FP128FP -> 128
    X86_FP80FP -> 80
    PPC_FP128FP -> 128

valOperand :: ANF.Val -> BlockGen Operand
valOperand (ANF.Var (ANF.VVal (ANF.Typed ty ident))) =
  let
    ident' = identToLLVM ident
  in
    case ident of
      (ANF.Ident _ _ _ True) -> do
        funcTy <- mkFunctionType ty
        pure $ ConstantOperand $ C.GlobalReference funcTy ident'
      _ -> do
        ty' <- llvmType ty
        fromMaybe (LocalReference ty' ident') <$> lookupSymbol ident
valOperand (ANF.Var var@(ANF.VCons (Typed _ consName))) = do
  method <- findCompilationMethod consName <$> compilationMethods
  case method of
    CompileUnboxed _ -> error $ "Attempted to find operand for applied constructor " ++ show var
    CompileEnum i -> valOperand $ ANF.Lit (LiteralInt i)
    CompileTaggedPairs _ -> error $ "Can't compile tagged pairs yet " ++ show var
valOperand (ANF.Lit lit) = pure $ ConstantOperand $ literalConstant lit

primitiveFunctionInstruction
  :: PrimitiveFunctionName
  -> [Operand]
  -> Instruction
primitiveFunctionInstruction primFuncName argumentOperands =
  let
    -- TODO: Better error checking here if number of operands doesn't match
    -- expected number. However, this should be type checked, so a simple panic
    -- should suffice.
    [op] = argumentOperands
    [op0, op1] = argumentOperands
    instruction =
      case primFuncName of
        PrimIAdd -> Add False False op0 op1 []
        PrimISub -> Sub False False op0 op1 []
        PrimIEquals -> ICmp IP.EQ op0 op1 []
        PrimIGreaterThan -> ICmp IP.SGT op0 op1 []
        PrimILessThan -> ICmp IP.SLT op0 op1 []

        PrimDAdd -> FAdd noFastMathFlags op0 op1 []
        PrimDSub -> FSub noFastMathFlags op0 op1 []

        PrimIntToDouble -> UIToFP op (llvmPrimitiveType DoubleType) []
        PrimDoubleToInt -> FPToUI op (llvmPrimitiveType IntType) []
  in instruction

typeToNonEmpty :: ANF.Type -> NonEmpty ANF.Type
typeToNonEmpty (t1 `ANF.TyFun` t2) = NE.cons t1 (typeToNonEmpty t2)
typeToNonEmpty ty = ty :| []

-- TODO: Add tests for this
llvmType :: forall m. (MonadReader CodeGenRead m) => ANF.Type -> m LLVM.Type
llvmType ty = go (typeToNonEmpty ty)
 where
  go :: NonEmpty ANF.Type -> m LLVM.Type
  go (ty' :| []) =
    case ty' of
      ANF.TyCon tyName ->
        case ANF.tyConInfoPrimitiveType tyName of
          Just prim -> pure $ llvmPrimitiveType prim
          Nothing -> do
            unBoxedTy <- isTyConUnboxed tyName
            case unBoxedTy of
              Just prim -> pure $ llvmPrimitiveType prim
              Nothing -> error $ "Can't compile, expected some sort of unboxing " ++ show tyName
      ANF.TyVar _ -> pure $ PointerType (IntegerType 64) (AddrSpace 0)
      ANF.TyFun{} -> mkFunctionType ty
  go _ = mkFunctionType ty

mkFunctionType :: (MonadReader CodeGenRead m) => ANF.Type -> m LLVM.Type
mkFunctionType ty = do
  resType <- llvmType $ NE.last ts
  argTypes <- traverse llvmType (NE.init ts)
  pure $
    PointerType
    FunctionType
    { resultType = resType
    , argumentTypes = argTypes
    , isVarArg = False
    }
    (AddrSpace 0)
 where
  ts = typeToNonEmpty ty

identToLLVM :: ANF.Ident -> LLVM.Name
identToLLVM (ANF.Ident name' _ _ _) = LLVM.Name $ textToShortBS name'

-- | Convert from a amy primitive type to an LLVM type
llvmPrimitiveType :: PrimitiveType -> LLVM.Type
llvmPrimitiveType IntType = IntegerType 64
llvmPrimitiveType DoubleType = FloatingPointType DoubleFP
llvmPrimitiveType BoolType = IntegerType 1

textToShortBS :: Text -> ShortByteString
textToShortBS = BSS.toShort . encodeUtf8

stringToShortBS :: String -> ShortByteString
stringToShortBS = BSS.toShort . BS8.pack
