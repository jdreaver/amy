{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amy.Codegen.Pure
  ( codegenModule
  ) where

import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Traversable (for)
import LLVM.AST as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as LLVM
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Linkage as L

import Amy.ANF as ANF
import Amy.Codegen.CaseBlocks
import Amy.Codegen.Monad
import Amy.Codegen.TypeConversion
import Amy.Codegen.Utils
import Amy.Prim

codegenModule :: ANF.Module -> LLVM.Module
codegenModule (ANF.Module bindings externs typeDeclarations) =
  let
    topLevelTypes =
      ((\(ANF.Binding name' (ANF.Forall _ ty) _ _ _) -> (name', ty)) <$> bindings)
      ++ ((\(ANF.Extern name' ty) -> (name', ty)) <$> externs)
    definitions = runCodeGen topLevelTypes $ do
      let
        externs' = codegenExtern <$> externs
        typeDefs = mapMaybe codegenTypeDeclaration typeDeclarations
      bindings' <- traverse codegenTopLevelBinding bindings
      pure $ externs' ++ typeDefs ++ bindings'
  in
    defaultModule
    { moduleName = "amy-module"
    , moduleDefinitions = definitions
    }

codegenExtern :: ANF.Extern -> Definition
codegenExtern extern =
  let
    (paramTypes, returnType') = argAndReturnTypes (ANF.externType extern)
    mkParam ty = Parameter (llvmType ty) (UnName 0) []
    params = mkParam <$> paramTypes
    retTy = llvmType returnType'
  in
    GlobalDefinition
    functionDefaults
    { name = identToName $ ANF.externName extern
    , parameters = (params, False)
    , LLVM.returnType = retTy
    }

codegenTypeDeclaration :: ANF.TypeDeclaration -> Maybe Definition
codegenTypeDeclaration (ANF.TypeDeclaration tyCon _) =
  case tyConInfoTypeRep tyCon of
    EnumRep _ -> Nothing
    TaggedUnionRep name' intBits ->
      Just $
        TypeDefinition
        (textToName name')
        (Just $
          StructureType
          False
          [ IntegerType intBits
          , PointerType (IntegerType 64) (AddrSpace 0)
          ]
        )

codegenTopLevelBinding :: ANF.Binding -> CodeGen Definition
codegenTopLevelBinding binding = do
  let
    argToParam (ANF.Typed ty ident) = Parameter (llvmType ty) (identToName ident) []
    params = argToParam <$> ANF.bindingArgs binding
    returnType' = llvmType $ ANF.bindingReturnType binding
  blocks <- codegenExpr $ ANF.bindingBody binding
  pure $
    GlobalDefinition
    functionDefaults
    { name = identToName $ ANF.bindingName binding
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
  for_ bindings $ \(ANF.LetBinding ident _ body) -> do
    op <- codegenExpr' body
    addSymbolToTable ident op
  codegenExpr' expr
codegenExpr' (ANF.ECase case'@(ANF.Case scrutinee (Typed bindingTy _) _ _ _)) = do
  caseId <- freshId
  let
    mkCaseName nameBase = stringToName $ nameBase ++ show caseId
    (CaseBlocks switchDefaultBlockName literalBlocks mDefaultBlock endBlock) = caseBlocks mkCaseName case'
    (CaseEndBlock endBlockName endOpName endType) = endBlock

  -- Generate the switch statement
  scrutineeOp <- valOperand scrutinee

  -- Extract tag from scrutinee op if we have to
  (switchOp, mArgOp) <-
    case bindingTy of
      TyCon tyCon -> unpackConstructor scrutineeOp tyCon
      _ -> pure (scrutineeOp, Nothing)

  let
    switchNames = (\(CaseLiteralBlock _ name' _ constant _) -> (constant, name')) <$> literalBlocks
  terminateBlock (Do $ Switch switchOp switchDefaultBlockName switchNames []) switchDefaultBlockName

  -- Generate case blocks
  let
    generateBlockExpr expr nextBlockName = do
      op <- codegenExpr' expr
      -- N.B. The block name could have changed while generating the
      -- expression, so we need to get the "actual" block name.
      finalBlockName <- currentBlockName
      terminateBlock (Do $ Br endBlockName []) nextBlockName
      pure (op, finalBlockName)
    generateCaseDefaultBlock (CaseDefaultBlock expr _ nextBlockName (Typed _ ident)) = do
      addSymbolToTable ident scrutineeOp
      generateBlockExpr expr nextBlockName
    generateCaseLiteralBlock (CaseLiteralBlock expr _ nextBlockName _ mBind) = do
      for_ mBind $ \(Typed ty ident) -> do
        -- Convert constructor argument and add to symbol table
        let
          argOp = fromMaybe (error "Can't extract argument for tagged union") mArgOp
          ty' = llvmType ty
        dataOp <- maybeConvertPointer argOp ty'
        addSymbolToTable ident dataOp
      generateBlockExpr expr nextBlockName

  mDefaultOpAndBlock <- traverse generateCaseDefaultBlock mDefaultBlock
  matchOpsAndBlocks <- traverse generateCaseLiteralBlock literalBlocks

  -- Generate end block
  let
    endTy = llvmType endType
    allOpsAndBlocks = maybe id (:) mDefaultOpAndBlock matchOpsAndBlocks
    endOpRef = LocalReference endTy endOpName
  addInstruction $ endOpName := Phi endTy allOpsAndBlocks []
  pure endOpRef
codegenExpr' (ANF.EApp (ANF.App (ANF.Typed originalTy ident) args' returnTy)) = do
  ty <- fromMaybe originalTy <$> topLevelType ident
  funcOperand <- valOperand (ANF.Var $ ANF.VVal $ ANF.Typed ty ident)
  let
    (argTys', returnTy') = argAndReturnTypes ty
  -- Convert arguments to pointers if we have to
  argOps <- for (zip args' argTys') $ \(arg, argTy) -> do
    originalOp <- valOperand arg
    let argTy' = llvmType argTy
    maybeConvertPointer originalOp argTy'

  -- Add call instruction
  opName <- freshUnName
  addInstruction $ opName := Call Nothing CC.C [] (Right funcOperand) ((\arg -> (arg, [])) <$> argOps) [] []

  -- Return operand, maybe converting it too
  let
    returnTyLLVM = llvmType returnTy
    returnTyLLVM' = llvmType returnTy'
  maybeConvertPointer (LocalReference returnTyLLVM' opName) returnTyLLVM
codegenExpr' (ANF.ECons app@(ANF.App (ANF.Typed _ (DataConInfo _ con)) args' _)) = do
  let
    mArg =
      case args' of
        [] -> Nothing
        [x] -> Just x
        _ -> error $ "Found too many arguments in ECons " ++ show app
  packConstructor con mArg
codegenExpr' (ANF.EPrimOp (ANF.App prim args' returnTy)) = do
  opName <- freshUnName
  argOps <- traverse valOperand args'
  addInstruction $ opName := primitiveFunctionInstruction prim argOps
  let returnTy' = llvmType returnTy
  pure $ LocalReference returnTy' opName

gepIndex :: Integer -> Operand
gepIndex i = ConstantOperand $ C.Int 32 i

valOperand :: ANF.Val -> BlockGen Operand
valOperand (ANF.Var (ANF.VVal (ANF.Typed ty ident))) =
  let
    ident' = identToName ident
  in
    case ident of
      (ANF.Ident _ _ True) -> do
        let funcTy = mkFunctionType ty
        pure $ ConstantOperand $ C.GlobalReference funcTy ident'
      _ -> do
        let ty' = llvmType ty
        fromMaybe (LocalReference ty' ident') <$> lookupSymbol ident
valOperand (ANF.Var (ANF.VCons (ANF.Typed _ (DataConInfo _ con)))) = packConstructor con Nothing
valOperand (ANF.Lit lit) = pure $ ConstantOperand $ literalConstant lit

packConstructor :: DataConstructor -> Maybe ANF.Val -> BlockGen Operand
packConstructor con mArg = do
  let (ConstructorIndex intIndex) = dataConstructorIndex con
  case tyConInfoTypeRep (dataConstructorType con) of
    EnumRep intBits -> pure $ ConstantOperand $ C.Int intBits (fromIntegral intIndex)
    TaggedUnionRep structName intBits -> do
      let structName' = textToName structName

      -- Allocate struct
      allocName <- freshUnName
      let allocOp = LocalReference (PointerType (NamedTypeReference structName') (AddrSpace 0)) allocName
      addInstruction $ allocName := Alloca (NamedTypeReference structName') Nothing 0 []

      -- Set the tag
      tagPtrName <- freshUnName
      let
        tagPtrOp = LocalReference (PointerType (IntegerType 32) (AddrSpace 0)) tagPtrName
        tagOp = ConstantOperand $ C.Int intBits (fromIntegral intIndex)
      addInstruction $ tagPtrName := GetElementPtr False allocOp [gepIndex 0, gepIndex 0] []
      addInstruction $ Do $ Store False tagPtrOp tagOp Nothing 0 []

      -- Set data
      for_ mArg $ \arg -> do
        argOp <- valOperand arg
        argOp' <- maybeConvertPointer argOp (PointerType (IntegerType 64) (AddrSpace 0))
        dataPtrName <- freshUnName
        let dataPtrOp = LocalReference (PointerType (PointerType (IntegerType 64) (AddrSpace 0)) (AddrSpace 0)) dataPtrName
        addInstruction $ dataPtrName := GetElementPtr False allocOp [gepIndex 0, gepIndex 1] []
        addInstruction $ Do $ Store False dataPtrOp argOp' Nothing 0 []

      -- Return pointer
      pure allocOp

unpackConstructor :: Operand -> TyConInfo -> BlockGen (Operand, Maybe Operand)
unpackConstructor conOp tyCon =
  case tyConInfoTypeRep tyCon of
    EnumRep _ -> pure (conOp, Nothing)
    TaggedUnionRep _ bits -> do
      -- Unpack tag
      tagPtrName <- freshUnName
      tagOpName <- freshUnName
      let
        tagPtrOp = LocalReference (PointerType (IntegerType 32) (AddrSpace 0)) tagPtrName
        tagOp = LocalReference (IntegerType bits) tagOpName
      addInstruction $ tagPtrName := GetElementPtr False conOp [gepIndex 0, gepIndex 0] []
      addInstruction $ tagOpName := Load False tagPtrOp Nothing 0 []

      -- Compute pointer to tagged union value
      dataPtrName <- freshUnName
      let
        dataPtrOp = LocalReference (PointerType (PointerType (IntegerType 64) (AddrSpace 0)) (AddrSpace 0)) dataPtrName
      addInstruction $ dataPtrName := GetElementPtr False conOp [gepIndex 0, gepIndex 1] []

      -- Load value from pointer
      dataName <- freshUnName
      let
        dataOp = LocalReference (PointerType (IntegerType 64) (AddrSpace 0)) dataName
      addInstruction $ dataName := Load False dataPtrOp Nothing 0 []

      pure (tagOp, Just dataOp)

primitiveFunctionInstruction
  :: PrimitiveFunction
  -> [Operand]
  -> Instruction
primitiveFunctionInstruction (PrimitiveFunction primFuncName _ _ _) argumentOperands =
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

        PrimIntToDouble -> UIToFP op (FloatingPointType DoubleFP) []
        PrimDoubleToInt -> FPToUI op (IntegerType 64) []
  in instruction

typeToNonEmpty :: ANF.Type -> NonEmpty ANF.Type
typeToNonEmpty (t1 `ANF.TyFun` t2) = NE.cons t1 (typeToNonEmpty t2)
typeToNonEmpty ty = ty :| []

-- TODO: Add tests for this
llvmType :: ANF.Type -> LLVM.Type
llvmType ty = go (typeToNonEmpty ty)
 where
  go :: NonEmpty ANF.Type -> LLVM.Type
  go (ty' :| []) =
    case ty' of
      ANF.TyCon tyCon ->
        case llvmPrimitiveType tyCon of
          Just prim -> prim
          Nothing ->
            case tyConInfoTypeRep tyCon of
              EnumRep intBits -> IntegerType intBits
              TaggedUnionRep structName _ ->
                PointerType (NamedTypeReference (textToName structName)) (AddrSpace 0)
      ANF.TyVar _ -> PointerType (IntegerType 64) (AddrSpace 0)
      ANF.TyFun{} -> mkFunctionType ty
  go _ = mkFunctionType ty

-- | Convert from an Amy primitive type to an LLVM type
llvmPrimitiveType :: TyConInfo -> Maybe LLVM.Type
llvmPrimitiveType (TyConInfo _ id' _)
  | id' == intTyConId = Just (IntegerType 64)
  | id' == doubleTyConId = Just (FloatingPointType DoubleFP)
  | otherwise = Nothing
 where
  intTyConId = primTyConId intTyCon
  doubleTyConId = primTyConId doubleTyCon

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
