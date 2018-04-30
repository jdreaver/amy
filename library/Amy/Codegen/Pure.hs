{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Pure
  ( codegenModule
  ) where

import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (for)
import LLVM.AST as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Global as LLVM
import qualified LLVM.AST.Linkage as L

import Amy.ANF as ANF
import Amy.Codegen.Monad
import Amy.Literal
import Amy.Prim

codegenModule :: ANF.Module -> LLVM.Module
codegenModule (ANF.Module bindings externs _) =
  let
    topLevelTypes =
      ((\(ANF.Binding name' (ANF.Forall _ ty) _ _ _) -> (name', ty)) <$> bindings)
      ++ ((\(ANF.Extern name' ty) -> (name', ty)) <$> externs)
  in
    defaultModule
    { moduleName = "amy-module"
    , moduleDefinitions = (codegenExtern <$> externs) ++ (codegenTopLevelBinding topLevelTypes <$> bindings)
    }

codegenExtern :: ANF.Extern -> Definition
codegenExtern extern =
  let
    (paramTypes, returnType') = argAndReturnTypes (ANF.externType extern)
    params =
      (\ty -> Parameter (llvmType ty) (UnName 0) []) <$> paramTypes
  in
    GlobalDefinition
    functionDefaults
    { name = identToLLVM $ ANF.externName extern
    , parameters = (params, False)
    , LLVM.returnType = llvmType returnType'
    }

codegenTopLevelBinding :: [(ANF.Ident, ANF.Type)] -> ANF.Binding -> Definition
codegenTopLevelBinding topLevelTypes binding =
  let
    argToParam (ANF.Typed ty ident) = Parameter (llvmType ty) (identToLLVM ident) []
    params = argToParam <$> ANF.bindingArgs binding
    returnType' = llvmType $ ANF.bindingReturnType binding
    blocks = codegenExpr topLevelTypes $ ANF.bindingBody binding
  in
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

codegenExpr :: [(ANF.Ident, ANF.Type)] -> ANF.Expr -> [BasicBlock]
codegenExpr topLevelTypes expr = runBlockGen topLevelTypes $ codegenExpr' expr

codegenExpr' :: ANF.Expr -> BlockGen Operand
codegenExpr' (ANF.EVal val) = valOperand val
codegenExpr' (ANF.ELet (ANF.Let bindings expr)) = do
  for_ bindings $ \binding -> do
    op <- codegenExpr' (ANF.bindingBody binding)
    addSymbolToTable (ANF.bindingName binding) op
  codegenExpr' expr
codegenExpr' (ANF.ECase (ANF.Case scrutinee matches ty)) = do
  caseId <- freshId

  -- TODO: Move a lot of this logic to ANF. conversion, like finding the default
  -- match and converting match patterns to literals.
  let
    -- Find all matches for literals
    literalMatch :: ANF.Match -> Maybe (Literal, ANF.Expr)
    literalMatch (ANF.Match (ANF.PatternLit lit) body) = Just (lit, body)
    literalMatch _ = Nothing
    literalMatches = mapMaybe literalMatch (NE.toList matches)

    varMatch :: ANF.Match -> Maybe (ANF.Typed ANF.Ident, ANF.Expr)
    varMatch (ANF.Match (ANF.PatternVar ident) body) = Just (ident, body)
    varMatch _ = Nothing

    mDefaultMatch :: Maybe (ANF.Typed ANF.Ident, ANF.Expr)
    mDefaultMatch =
      (\xs -> if null xs then Nothing else Just (head xs)) $
      mapMaybe varMatch (NE.toList matches)

    -- Give names to all the blocks
    mkCaseName nameBase = LLVM.Name $ stringToShortBS $ nameBase ++ show caseId
    matchesAndBlockNames =
      second (mkCaseName . (\i -> "case." ++ i ++ ".") . show)
      <$> zip literalMatches [(0 :: Int)..]

    -- TODO: Have a proper default block if there isn't a natural one,
    -- hopefully something that prints an error? Having a default should
    -- probably be handled in Core/ANF..
    firstBlockName = snd . head $ matchesAndBlockNames
    defaultBlockName =
      case mDefaultMatch of
        Just _ -> mkCaseName "case.default."
        Nothing -> firstBlockName

    endBlockName = mkCaseName "case.end."
    endOpName = mkCaseName "end."
    endTy = llvmType ty
    endOpRef = LocalReference endTy endOpName

  -- Generate the switch statement
  scrutineeOp <- valOperand scrutinee
  let
    switchNames = (\((lit, _), name') -> (literalConstant lit, name')) <$> matchesAndBlockNames
  terminateBlock (Do $ Switch scrutineeOp defaultBlockName switchNames []) defaultBlockName

  -- Generate default block
  mDefaultOpAndBlock <-
    for mDefaultMatch $ \defaultMatch -> do
      let
        (ANF.Typed _ defaultIdent, defaultExpr) = defaultMatch
      addSymbolToTable defaultIdent scrutineeOp
      defaultOp <- codegenExpr' defaultExpr
      defaultBlock <- currentBlockName
      terminateBlock (Do $ Br endBlockName []) firstBlockName
      pure (defaultOp, defaultBlock)

  -- Generate other blocks
  let
    allBlockNames = snd <$> matchesAndBlockNames
    nextBlockNames = drop 1 allBlockNames ++ [endBlockName]
    matchesAndNextBlockNames = zip (fst <$> matchesAndBlockNames) nextBlockNames
  matchOpsAndBlocks <-
    for matchesAndNextBlockNames $ \((_, expr), nextBlockName) -> do
      exprOp <- codegenExpr' expr
      -- N.B. The block name could have changed while generating the
      -- expression, so we need to get the "actual" block name.
      actualBlockName <- currentBlockName
      terminateBlock (Do $ Br endBlockName []) nextBlockName
      pure (exprOp, actualBlockName)

  -- Generate end block
  let allOpsAndBlocks = maybe id (:) mDefaultOpAndBlock matchOpsAndBlocks
  addInstruction $ endOpName := Phi endTy allOpsAndBlocks []
  pure endOpRef
codegenExpr' (ANF.EApp (ANF.App (ANF.Typed originalTy ident) args' returnTy)) = do
  ty <- fromMaybe originalTy <$> topLevelType ident
  funcOperand <- valOperand (ANF.Var $ ANF.Typed ty ident)
  let
    (argTys', returnTy') = argAndReturnTypes ty
  opName <- freshUnName
  -- Convert arguments to pointers if we have to
  argOps <- for (zip args' argTys') $ \(arg, argTy) -> do
    originalOp <- valOperand arg
    convertLLVMType originalOp (llvmType argTy)

  -- Add call instruction
  addInstruction $ opName := Call Nothing CC.C [] (Right funcOperand) ((\arg -> (arg, [])) <$> argOps) [] []

  -- Return operand, maybe converting it too
  convertLLVMType (LocalReference (llvmType returnTy') opName) (llvmType returnTy)
codegenExpr' (ANF.EPrimOp (ANF.App prim args' returnTy)) = do
  opName <- freshUnName
  argOps <- traverse valOperand args'
  addInstruction $ opName := primitiveFunctionInstruction prim argOps
  pure $ LocalReference (llvmType returnTy) opName

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
valOperand (ANF.Var (ANF.Typed ty ident)) =
  let
    ident' = identToLLVM ident
  in
    case ident of
      (ANF.Ident _ _ _ True) -> pure $ ConstantOperand $ C.GlobalReference (mkFunctionType ty) ident'
      _ -> fromMaybe (LocalReference (llvmType ty) ident') <$> lookupSymbol ident
valOperand (ANF.Lit lit) = pure $ ConstantOperand $ literalConstant lit

literalConstant :: Literal -> C.Constant
literalConstant lit =
  case lit of
    LiteralInt i -> C.Int 64 (fromIntegral i)
    LiteralDouble x -> C.Float (F.Double x)
    LiteralBool x -> C.Int 1 $ if x then 1 else 0

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
llvmType :: ANF.Type -> LLVM.Type
llvmType ty = go (typeToNonEmpty ty)
 where
  go (ty' :| []) =
    case ty' of
      ANF.TyCon tyName ->
        let prim = fromMaybe (error $ "Expected primitive TyCon, got " ++ show tyName) (ANF.tyConInfoPrimitiveType tyName)
        in llvmPrimitiveType prim
      ANF.TyVar _ -> PointerType (IntegerType 64) (AddrSpace 0)
      ANF.TyFun{} -> mkFunctionType ty
  go _ = mkFunctionType ty

mkFunctionType :: ANF.Type -> LLVM.Type
mkFunctionType ty =
  PointerType
    FunctionType
    { resultType = llvmType $ NE.last ts
    , argumentTypes = llvmType <$> NE.init ts
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
