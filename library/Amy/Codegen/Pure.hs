{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amy.Codegen.Pure
  ( codegenModule
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
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
        (textToName $ unTyConName name')
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
        if ANF.bindingName binding == "main"
          then L.External
          else L.Private
    }

codegenExpr :: ANF.Expr -> CodeGen [BasicBlock]
codegenExpr expr = runBlockGen $ codegenExpr' (textToName "ret") expr

codegenExpr' :: Name -> ANF.Expr -> BlockGen Operand
codegenExpr' name' (ANF.EVal val) =
  -- If we get here that means the inliner probably didn't do its job very
  -- well, and there is a primitive value being used by itself in an
  -- expression. No worries, LLVM will most certainly remove these redundant
  -- instructions.
  bindOpToName name' (valOperand val)
codegenExpr' name' (ANF.ERecord (ANF.Typed ty rows)) = do
  -- Allocate struct
  let
    -- TODO: Remove need for irrefutable pattern match
    ty'@(LLVM.PointerType innerTy _) = llvmType ty
    allocOp = LocalReference ty' name'
  addInstruction $ name' := Alloca innerTy Nothing 0 []

  -- Pack rows
  let numberedRows = zip [0..] $ Map.toAscList rows
  for_ numberedRows $ \(i, (_, val)) -> do
    rowPtrName <- freshUnName
    let
      rowOp = valOperand val
      rowPtrOp = LocalReference (LLVM.PointerType (IntegerType 32) (AddrSpace 0)) rowPtrName
    addInstruction $ rowPtrName := GetElementPtr False allocOp [gepIndex 0, gepIndex i] []
    addInstruction $ Do $ Store False rowPtrOp rowOp Nothing 0 []

  pure allocOp
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
  let scrutineeOp = valOperand scrutinee

  -- Extract tag from scrutinee op if we have to
  -- TODO: Should we extract the argument once here or extract it in every
  -- block that needs it? I feel like extracting it here is a bit funny.
  (switchOp, mArgOp) <-
    case bindingTy of
      PrimIntType -> pure (scrutineeOp, Nothing)
      EnumType _ -> pure (scrutineeOp, Nothing)
      TaggedUnionType _ bits -> unpackConstructor scrutineeOp bits
      -- TODO: Proper codegen for case expressions that can't be represented as
      -- switches, like doubles and strings. We'll need to generated a sequence
      -- of comparisons. Using a switch statement is really an optimization for
      -- integers and sum types.
      _ -> error $ "Cannot generate switch expression for doubles" ++ show bindingTy


  let
    switchNames = (\(CaseLiteralBlock _ switchName _ constant _) -> (constant, switchName)) <$> literalBlocks
  terminateBlock (Do $ Switch switchOp switchDefaultBlockName switchNames []) switchDefaultBlockName

  -- Generate case blocks
  let
    generateBlockExpr expr nextBlockName = do
      exprName <- freshUnName
      op <- codegenExpr' exprName expr
      -- N.B. The current block name could have changed while generating the
      -- expression, so we need to fetch it and return it instead of the
      -- original block name.
      finalBlockName <- currentBlockName
      terminateBlock (Do $ Br endBlockName []) nextBlockName
      pure (LocalReference (operandType op) exprName, finalBlockName)
    generateCaseDefaultBlock (CaseDefaultBlock expr _ nextBlockName) = do
      _ <- bindOpToName (identToName bindingIdent) scrutineeOp
      generateBlockExpr expr nextBlockName
    generateCaseLiteralBlock (CaseLiteralBlock expr _ nextBlockName _ mBind) = do
      for_ mBind $ \(Typed ty ident) -> do
        -- Convert constructor argument
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
  addInstruction $ name' := Phi endTy allOpsAndBlocks []
  pure $ LocalReference endTy name'
codegenExpr' name' (ANF.EApp (ANF.App (ANF.Typed originalTy ident) args' returnTy)) = do
  topLevelTy <- topLevelType ident
  let
    (ty, funcOperand) =
      case topLevelTy of
        Nothing -> (originalTy, valOperand (ANF.Var (ANF.Typed originalTy ident) False))
        Just ty' -> (ty', valOperand (ANF.Var (ANF.Typed ty' ident) True))
  let
    (argTys', returnTy') =
      case ty of
        FuncType argTys ret -> (argTys, ret)
        _ -> error $ "Tried to EApp a non-function type " ++ show ty
  -- Convert arguments to pointers if we have to
  argOps <- for (zip args' argTys') $ \(arg, argTy) -> do
    let originalOp = valOperand arg
    maybeConvertPointer Nothing originalOp $ llvmType argTy

  -- Add call instruction
  let
    callInstruction = Call Nothing CC.C [] (Right funcOperand) ((\arg -> (arg, [])) <$> argOps) [] []
    returnTyLLVM = llvmType returnTy
    returnTyLLVM' = llvmType returnTy'
  if returnTyLLVM == returnTyLLVM'
    then do
      addInstruction $ name' := callInstruction
      pure $ LocalReference returnTyLLVM' name'
    else do
      callName <- freshUnName
      addInstruction $ callName := callInstruction
      maybeConvertPointer (Just name') (LocalReference returnTyLLVM' callName) returnTyLLVM
codegenExpr' name' (ANF.EConApp (ANF.ConApp con mArg structName intBits)) =
  packConstructor name' con mArg structName intBits
codegenExpr' name' (ANF.EPrimOp (ANF.App prim args' returnTy)) = do
  let argOps = valOperand <$> args'
  addInstruction $ name' := primitiveFunctionInstruction prim argOps
  let returnTy' = llvmType returnTy
  pure $ LocalReference returnTy' name'

gepIndex :: Integer -> Operand
gepIndex i = ConstantOperand $ C.Int 32 i

valOperand :: ANF.Val -> Operand
valOperand (ANF.Var (ANF.Typed ty ident) True) = ConstantOperand $ C.GlobalReference (llvmType ty) (identToName ident)
valOperand (ANF.Var (ANF.Typed ty ident) False) = LocalReference (llvmType ty) (identToName ident)
valOperand (ANF.Lit lit) = ConstantOperand $ literalConstant lit
valOperand (ANF.ConEnum intBits con) =
  let
    (ConstructorIndex intIndex) = dataConIndex con
  in ConstantOperand $ C.Int intBits (fromIntegral intIndex)

packConstructor :: Name -> DataCon -> Maybe ANF.Val -> TyConName -> Word32 -> BlockGen Operand
packConstructor name' con mArg (TyConName structName) intBits = do
  let
    (ConstructorIndex intIndex) = dataConIndex con
    structName' = textToName structName

  -- Allocate struct
  let allocOp = LocalReference (LLVM.PointerType (NamedTypeReference structName') (AddrSpace 0)) name'
  addInstruction $ name' := Alloca (NamedTypeReference structName') Nothing 0 []

  -- Set the tag
  tagPtrName <- freshUnName
  let
    tagPtrOp = LocalReference (LLVM.PointerType (IntegerType 32) (AddrSpace 0)) tagPtrName
    tagOp = ConstantOperand $ C.Int intBits (fromIntegral intIndex)
  addInstruction $ tagPtrName := GetElementPtr False allocOp [gepIndex 0, gepIndex 0] []
  addInstruction $ Do $ Store False tagPtrOp tagOp Nothing 0 []

  -- Set data
  for_ mArg $ \arg -> do
    let argOp = valOperand arg
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
primitiveFunctionInstruction (PrimitiveFunction primFuncName _ _) argumentOperands =
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
llvmType (TaggedUnionType structName _) = LLVM.PointerType (NamedTypeReference (textToName $ unTyConName structName)) (AddrSpace 0)
llvmType (RecordType rows) = LLVM.PointerType (recordType rows) (AddrSpace 0)

recordType :: [(RowLabel, ANF.Type)] -> LLVM.Type
recordType rows = StructureType False $ llvmType . snd <$> sort rows
