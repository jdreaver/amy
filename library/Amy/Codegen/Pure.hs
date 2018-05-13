{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amy.Codegen.Pure
  ( codegenModule
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import GHC.Word (Word32)
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
      ((\(ANF.Binding name' argTys retTy _) -> (name', FuncType (typedType <$> argTys) retTy)) <$> bindings)
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
    (paramTypes, retTy) =
      case ANF.externType extern of
        FuncType argTys ret -> (argTys, ret)
        _ -> error $ "Found extern with non function type " ++ show extern
    mkParam ty = Parameter (llvmType ty) (UnName 0) []
    params = mkParam <$> paramTypes
    retTy' = llvmType retTy
  in
    GlobalDefinition
    functionDefaults
    { name = identToName $ ANF.externName extern
    , parameters = (params, False)
    , LLVM.returnType = retTy'
    }

codegenTypeDeclaration :: ANF.TypeDeclaration -> Maybe Definition
codegenTypeDeclaration (ANF.TypeDeclaration _ ty _) =
  case ty of
    TaggedUnionType name' intBits ->
      Just $
        TypeDefinition
        (textToName name')
        (Just $
          StructureType
          False
          [ IntegerType intBits
          , LLVM.PointerType (IntegerType 64) (AddrSpace 0)
          ]
        )
    _ -> Nothing

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

codegenExpr :: ANF.Expr -> CodeGen [BasicBlock]
codegenExpr expr = runBlockGen $ codegenExpr' (textToName "ret") expr

codegenExpr' :: Name -> ANF.Expr -> BlockGen Operand
codegenExpr' name' (ANF.EVal val) = do
  op <- valOperand val
  bindOpToName name' op
  pure op
codegenExpr' name' (ANF.ELetVal (ANF.LetVal bindings expr)) = do
  for_ bindings $ \(ANF.LetValBinding ident _ body) ->
    codegenExpr' (identToName ident) body
  codegenExpr' name' expr
codegenExpr' name' (ANF.ECase case'@(ANF.Case scrutinee (Typed bindingTy bindingIdent) _ _ _)) = do
  let
    nameAsString =
      case name' of
        Name s -> s
        UnName i -> BSS.toShort $ BS8.pack $ show i
    mkCaseName nameBase = Name $ BSS.toShort (BS8.pack nameBase) <> nameAsString
    (CaseBlocks switchDefaultBlockName literalBlocks mDefaultBlock endBlock) = caseBlocks mkCaseName case'
    (CaseEndBlock endBlockName endType) = endBlock

  -- Generate the switch statement
  scrutineeOp <- valOperand scrutinee
  bindOpToName (identToName bindingIdent) scrutineeOp

  -- Extract tag from scrutinee op if we have to
  (switchOp, mArgOp) <-
    case bindingTy of
      TaggedUnionType _ bits -> unpackConstructor scrutineeOp bits
      _ -> pure (scrutineeOp, Nothing)

  let
    switchNames = (\(CaseLiteralBlock _ switchName _ constant _) -> (constant, switchName)) <$> literalBlocks
  terminateBlock (Do $ Switch switchOp switchDefaultBlockName switchNames []) switchDefaultBlockName

  -- Generate case blocks
  let
    generateBlockExpr expr nextBlockName = do
      exprName <- freshUnName
      op <- codegenExpr' exprName expr
      -- N.B. The block name could have changed while generating the
      -- expression, so we need to get the "actual" block name.
      finalBlockName <- currentBlockName
      terminateBlock (Do $ Br endBlockName []) nextBlockName
      pure (op, finalBlockName)
    generateCaseDefaultBlock (CaseDefaultBlock expr _ nextBlockName) =
      generateBlockExpr expr nextBlockName
    generateCaseLiteralBlock (CaseLiteralBlock expr _ nextBlockName _ mBind) = do
      for_ mBind $ \(Typed ty ident) -> do
        -- Convert constructor argument and add to symbol table
        let
          argOp = fromMaybe (error "Can't extract argument for tagged union") mArgOp
        maybeConvertPointer (Just $ identToName ident) argOp $ llvmType ty
      generateBlockExpr expr nextBlockName

  mDefaultOpAndBlock <- traverse generateCaseDefaultBlock mDefaultBlock
  matchOpsAndBlocks <- traverse generateCaseLiteralBlock literalBlocks

  -- Generate end block
  let
    endTy = llvmType endType
    allOpsAndBlocks = maybe id (:) mDefaultOpAndBlock matchOpsAndBlocks
    endOpRef = LocalReference endTy name'
  addInstruction $ name' := Phi endTy allOpsAndBlocks []
  pure endOpRef
codegenExpr' name' (ANF.EApp (ANF.App (ANF.Typed originalTy ident) args' returnTy)) = do
  ty <- fromMaybe originalTy <$> topLevelType ident
  funcOperand <- valOperand (ANF.Var $ ANF.VVal $ ANF.Typed ty ident)
  let
    (argTys', returnTy') =
      case ty of
        FuncType argTys ret -> (argTys, ret)
        _ -> error $ "Tried to EApp a non-function type " ++ show ty
  -- Convert arguments to pointers if we have to
  argOps <- for (zip args' argTys') $ \(arg, argTy) -> do
    originalOp <- valOperand arg
    maybeConvertPointer Nothing originalOp $ llvmType argTy

  -- Add call instruction
  callName <- freshUnName
  addInstruction $ callName := Call Nothing CC.C [] (Right funcOperand) ((\arg -> (arg, [])) <$> argOps) [] []

  -- Return operand, maybe converting it too
  let
    returnTyLLVM = llvmType returnTy
    returnTyLLVM' = llvmType returnTy'
  -- TODO: Figure out a priori if we need to convert the result. If we don't,
  -- then we can bind the name directly.
  maybeConvertPointer (Just name') (LocalReference returnTyLLVM' callName) returnTyLLVM
codegenExpr' name' (ANF.ECons app@(ANF.App (ANF.Typed _ (DataConInfo _ con)) args' _)) = do
  let
    mArg =
      case args' of
        [] -> Nothing
        [x] -> Just x
        _ -> error $ "Found too many arguments in ECons " ++ show app
  maybePackConstructor (Just name') con mArg
codegenExpr' name' (ANF.EPrimOp (ANF.App prim args' returnTy)) = do
  argOps <- traverse valOperand args'
  addInstruction $ name' := primitiveFunctionInstruction prim argOps
  let returnTy' = llvmType returnTy
  pure $ LocalReference returnTy' name'

gepIndex :: Integer -> Operand
gepIndex i = ConstantOperand $ C.Int 32 i

valOperand :: ANF.Val -> BlockGen Operand
valOperand (ANF.Var (ANF.VVal (ANF.Typed ty ident))) =
  let
    ident' = identToName ident
  in
    case ident of
      (ANF.Ident _ _ True) ->
        pure $ ConstantOperand $ C.GlobalReference (llvmType ty) ident'
      _ -> do
        let ty' = llvmType ty
        pure $ LocalReference ty' ident'
valOperand (ANF.Var (ANF.VCons (ANF.Typed _ (DataConInfo _ con)))) = maybePackConstructor Nothing con Nothing
valOperand (ANF.Lit lit) = pure $ ConstantOperand $ literalConstant lit

maybePackConstructor :: Maybe Name -> DataConstructor -> Maybe ANF.Val -> BlockGen Operand
maybePackConstructor mName con mArg =
  case dataConstructorType con of
    EnumType intBits -> do
      let
        (ConstructorIndex intIndex) = dataConstructorIndex con
        op = ConstantOperand $ C.Int intBits (fromIntegral intIndex)
      for_ mName $ \name' -> bindOpToName name' op
      pure op
    TaggedUnionType structName intBits -> packConstructor mName con mArg structName intBits
    _ -> error $ "TODO: maybePackConstructor " ++ show con

packConstructor :: Maybe Name -> DataConstructor -> Maybe ANF.Val -> Text -> Word32 -> BlockGen Operand
packConstructor mName con mArg structName intBits = do
  let
    (ConstructorIndex intIndex) = dataConstructorIndex con
    structName' = textToName structName

  -- Allocate struct
  allocName <- maybe freshUnName pure mName
  let allocOp = LocalReference (LLVM.PointerType (NamedTypeReference structName') (AddrSpace 0)) allocName
  addInstruction $ allocName := Alloca (NamedTypeReference structName') Nothing 0 []

  -- Set the tag
  tagPtrName <- freshUnName
  let
    tagPtrOp = LocalReference (LLVM.PointerType (IntegerType 32) (AddrSpace 0)) tagPtrName
    tagOp = ConstantOperand $ C.Int intBits (fromIntegral intIndex)
  addInstruction $ tagPtrName := GetElementPtr False allocOp [gepIndex 0, gepIndex 0] []
  addInstruction $ Do $ Store False tagPtrOp tagOp Nothing 0 []

  -- Set data
  for_ mArg $ \arg -> do
    argOp <- valOperand arg
    argOp' <- maybeConvertPointer Nothing argOp (LLVM.PointerType (IntegerType 64) (AddrSpace 0))
    dataPtrName <- freshUnName
    let dataPtrOp = LocalReference (LLVM.PointerType (LLVM.PointerType (IntegerType 64) (AddrSpace 0)) (AddrSpace 0)) dataPtrName
    addInstruction $ dataPtrName := GetElementPtr False allocOp [gepIndex 0, gepIndex 1] []
    addInstruction $ Do $ Store False dataPtrOp argOp' Nothing 0 []

  -- Return pointer
  pure allocOp

unpackConstructor :: Operand -> Word32 -> BlockGen (Operand, Maybe Operand)
unpackConstructor conOp bits = do
  -- Unpack tag
  tagPtrName <- freshUnName
  tagOpName <- freshUnName
  let
    tagPtrOp = LocalReference (LLVM.PointerType (IntegerType 32) (AddrSpace 0)) tagPtrName
    tagOp = LocalReference (IntegerType bits) tagOpName
  addInstruction $ tagPtrName := GetElementPtr False conOp [gepIndex 0, gepIndex 0] []
  addInstruction $ tagOpName := Load False tagPtrOp Nothing 0 []

  -- Compute pointer to tagged union value
  dataPtrName <- freshUnName
  let
    dataPtrOp = LocalReference (LLVM.PointerType (LLVM.PointerType (IntegerType 64) (AddrSpace 0)) (AddrSpace 0)) dataPtrName
  addInstruction $ dataPtrName := GetElementPtr False conOp [gepIndex 0, gepIndex 1] []

  -- Load value from pointer
  dataName <- freshUnName
  let
    dataOp = LocalReference (LLVM.PointerType (IntegerType 64) (AddrSpace 0)) dataName
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

llvmType :: ANF.Type -> LLVM.Type
llvmType PrimIntType = IntegerType 64
llvmType PrimDoubleType = FloatingPointType DoubleFP
llvmType (ANF.PointerType ty) = LLVM.PointerType (llvmType ty) (AddrSpace 0)
llvmType OpaquePointerType = LLVM.PointerType (IntegerType 64) (AddrSpace 0)
llvmType (FuncType argTys retTy) =
  LLVM.PointerType
  FunctionType
  { resultType = llvmType retTy
  , argumentTypes = llvmType <$> argTys
  , isVarArg = False
  }
  (AddrSpace 0)
llvmType (EnumType intBits) = IntegerType intBits
llvmType (TaggedUnionType structName _) = LLVM.PointerType (NamedTypeReference (textToName structName)) (AddrSpace 0)
