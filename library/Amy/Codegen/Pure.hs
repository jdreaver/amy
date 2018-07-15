{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amy.Codegen.Pure
  ( codegenModule
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_)
import Data.List (elemIndex, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Word (Word32)
import LLVM.AST as LLVM hiding (type')
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as LLVM
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Linkage as L

import Amy.ANF as ANF
import Amy.Codegen.CaseBlocks
import Amy.Codegen.Closures
import Amy.Codegen.Malloc
import Amy.Codegen.Monad
import Amy.Codegen.TypeConversion
import Amy.Codegen.Utils
import Amy.Prim

codegenModule :: ANF.Module -> LLVM.Module
codegenModule (ANF.Module bindings externs typeDeclarations externTypes textPointers closureWrappers) =
  let
    definitions = runCodeGen $ do
      let
        externs' = codegenExtern <$> externs
        typeDefs =
          mapMaybe codegenTypeDeclaration
          $ (typeDeclarationType <$> typeDeclarations) ++ externTypes
        textPointers' = codegenTextPointer <$> textPointers
      bindings' <- traverse codegenTopLevelBinding bindings
      closureWrappers' <- traverse codegenClosureWrapper closureWrappers
      pure $ externs' ++ typeDefs ++ closureWrappers' ++ textPointers' ++ bindings'
  in
    defaultModule
    { moduleName = "amy-module"
    , moduleDefinitions = definitions
    }

codegenExtern :: ANF.Extern -> Definition
codegenExtern (ANF.Extern name' argTys retTy) =
  let
    mkParam ty = Parameter (llvmType ty) (UnName 0) []
    params = mkParam <$> argTys
    retTy' = llvmType retTy
  in
    GlobalDefinition
    functionDefaults
    { name = identToName name'
    , parameters = (params, False)
    , LLVM.returnType = retTy'
    }

codegenTypeDeclaration :: ANF.Type -> Maybe Definition
codegenTypeDeclaration ty =
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

codegenTextPointer :: ANF.TextPointer -> Definition
codegenTextPointer ptr =
  GlobalDefinition
  globalVariableDefaults
  { name = textPointerName ptr
  , LLVM.type' = textPointerType ptr
  , initializer = Just $ textPointerConstant ptr
  , linkage = L.Private
  }

codegenClosureWrapper :: ANF.ClosureWrapper -> CodeGen Definition
codegenClosureWrapper (ANF.ClosureWrapper name' original argTys retTy) =
  closureWrapperDefinition (identToName name') (identToName original) (llvmType <$> argTys) (llvmType retTy)

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
codegenExpr' name' (ANF.ERecord rows) = do
  -- Allocate struct
  let
    rowsTy = Map.toAscList $ typedType <$> rows
    ty = recordType rowsTy
  allocOp <- callMalloc name' Nothing ty

  -- Pack rows
  let numberedRows = zip [0..] $ Map.toAscList rows
  for_ numberedRows $ \(i, (_, Typed _ val)) -> do
    rowPtrName <- freshUnName
    let
      rowOp = valOperand val
      rowPtrOp = LocalReference (LLVM.PointerType (IntegerType 64) (AddrSpace 0)) rowPtrName
    addInstruction $ rowPtrName := GetElementPtr False allocOp [gepIndex 0, gepIndex i] []
    rowPtrOp' <- maybeConvertPointer Nothing rowPtrOp (LLVM.PointerType (operandType rowOp) (AddrSpace 0))
    addInstruction $ Do $ Store False rowPtrOp' rowOp Nothing 0 []

  pure allocOp
codegenExpr' name' (ANF.ERecordSelect val label retTy) = do
  -- Compute what index to use
  let
    tyRows =
      case val of
        Var (Typed (RecordType rows) _) -> rows
        _ -> error $ "Expected record var, got " ++ show val
    index' =
      fromMaybe (error $ "Couldn't find index for label in type " ++ show (label, tyRows))
      . elemIndex label
      $ fst <$> tyRows
    valOp = valOperand val

  -- Compute offset with getelementptr
  dataPtrName <- freshUnName
  let
    dataPtrOp = LocalReference (LLVM.PointerType (LLVM.PointerType (IntegerType 64) (AddrSpace 0)) (AddrSpace 0)) dataPtrName
  addInstruction $ dataPtrName := GetElementPtr False valOp [gepIndex 0, gepIndex (fromIntegral index')] []

  -- Load value from pointer
  let
    dataOp = LocalReference (LLVM.PointerType (llvmType retTy) (AddrSpace 0)) name'
  addInstruction $ name' := Load False dataPtrOp Nothing 0 []
  pure dataOp
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
codegenExpr' name' (ANF.ECreateClosure (CreateClosure func arity)) =
  createClosure name' (identToName func) arity
codegenExpr' name' (ANF.ECallClosure (CallClosure val args' retTy)) =
  callClosure name' (valOperand val) (valOperand <$> args') (llvmType retTy)
codegenExpr' name' (ANF.EKnownFuncApp (ANF.KnownFuncApp ident args' argTys originalReturnTy returnTy)) = do
  let
    ident' = identToName ident
    argVals = valOperand <$> args'
    argTys' = llvmType <$> argTys
    originalReturnTy' = llvmType originalReturnTy
    returnTy' = llvmType returnTy
  knownFunctionApplication name' ident' argVals argTys' originalReturnTy' returnTy'
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
valOperand (ANF.Var (ANF.Typed ty ident)) = LocalReference (llvmType ty) (identToName ident)
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
  allocOp <- callMalloc name' Nothing (NamedTypeReference structName')
  addInstruction $ name' := Alloca (NamedTypeReference structName') Nothing 0 []

  -- Set the tag
  tagPtrName <- freshUnName
  let
    tagPtrOp = LocalReference (LLVM.PointerType (IntegerType 64) (AddrSpace 0)) tagPtrName
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
    -- expected number. That shouldn't ever happen because of type checking and
    -- eta expansion of primops, but a better error would be nice.
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
llvmType PrimTextType = LLVM.PointerType (IntegerType 8) (AddrSpace 0)
llvmType (ANF.PointerType ty) = LLVM.PointerType (llvmType ty) (AddrSpace 0)
llvmType OpaquePointerType = LLVM.PointerType (IntegerType 64) (AddrSpace 0)
llvmType ClosureType = LLVM.PointerType (NamedTypeReference closureStructName) (AddrSpace 0)
llvmType (EnumType intBits) = IntegerType intBits
llvmType (TaggedUnionType structName _) = LLVM.PointerType (NamedTypeReference (textToName $ unTyConName structName)) (AddrSpace 0)
llvmType (RecordType rows) = LLVM.PointerType (recordType rows) (AddrSpace 0)

recordType :: [(RowLabel, ANF.Type)] -> LLVM.Type
recordType rows = StructureType False $ llvmType . snd <$> sort rows
