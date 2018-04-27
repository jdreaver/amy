{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Pure
  ( codegenModule
  ) where

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
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Global as LLVM

import Amy.ANF
import Amy.Codegen.Monad
import Amy.Literal
import Amy.Prim

codegenModule :: ANFModule -> Module
codegenModule (ANFModule bindings externs) =
  let
    topLevelTypes =
      ((\(ANFBinding name' (ANFForall _ ty) _ _ _) -> (name', ty)) <$> bindings)
      ++ ((\(ANFExtern name' ty) -> (name', ty)) <$> externs)
  in
    defaultModule
    { moduleName = "amy-module"
    , moduleDefinitions = (codegenExtern <$> externs) ++ (codegenTopLevelBinding topLevelTypes <$> bindings)
    }

codegenExtern :: ANFExtern -> Definition
codegenExtern extern =
  let
    (paramTypes, returnType') = argAndReturnTypes (anfExternType extern)
    params =
      (\ty -> Parameter (llvmType ty) (UnName 0) []) <$> paramTypes
  in
    GlobalDefinition
    functionDefaults
    { name = identToLLVM $ anfExternName extern
    , parameters = (params, False)
    , LLVM.returnType = llvmType returnType'
    }

codegenTopLevelBinding :: [(ANFIdent, ANFType)] -> ANFBinding -> Definition
codegenTopLevelBinding topLevelTypes binding =
  let
    argToParam (ANFTyped ty ident) = Parameter (llvmType ty) (identToLLVM ident) []
    params = argToParam <$> anfBindingArgs binding
    returnType' = llvmType $ anfBindingReturnType binding
    blocks = codegenExpr topLevelTypes $ anfBindingBody binding
  in
    GlobalDefinition
    functionDefaults
    { name = identToLLVM $ anfBindingName binding
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    , basicBlocks = blocks
    }

argAndReturnTypes :: ANFType -> ([ANFType], ANFType)
argAndReturnTypes ty = (NE.init tyNE, NE.last tyNE)
 where
  tyNE = typeToNonEmpty ty

codegenExpr :: [(ANFIdent, ANFType)] -> ANFExpr -> [BasicBlock]
codegenExpr topLevelTypes expr = runBlockGen topLevelTypes $ codegenExpr' expr

codegenExpr' :: ANFExpr -> BlockGen Operand
codegenExpr' (ANFEVal val) = valOperand val
codegenExpr' (ANFELet (ANFLet bindings expr)) = do
  for_ bindings $ \binding -> do
    op <- codegenExpr' (anfBindingBody binding)
    addSymbolToTable (anfBindingName binding) op
  codegenExpr' expr
codegenExpr' (ANFEIf (ANFIf pred' then' else' ty)) = do
  -- Name blocks and operands
  ifId <- freshId
  let
    mkIfName nameBase = LLVM.Name $ stringToShortBS $ nameBase ++ show ifId
    testOpName = mkIfName "test."
    testOpRef = LocalReference (IntegerType 1) testOpName
    thenBlockName = mkIfName "if.then."
    elseBlockName = mkIfName "if.else."
    endBlockName = mkIfName "if.end."
    endOpName = mkIfName "end."
    ty' = llvmType ty
    endOpRef = LocalReference ty' endOpName

  -- Generate predicate operation
  predOp <- valOperand pred'
  let
    true = ConstantOperand $ C.Int 1 1
  addInstruction (testOpName := ICmp IP.EQ true predOp [])
  terminateBlock (Do $ CondBr testOpRef thenBlockName elseBlockName []) thenBlockName

  -- Generate then block
  thenOp <- codegenExpr' then'
  -- NB: The codegen for the then/else sub-expressions could themselves
  -- produces new blocks. This happens with nested if expressions, for example.
  -- We have to know what block we *actually* came from when generating the phi
  -- function at the end of this if expression.
  thenBlockFinalName <- currentBlockName
  terminateBlock (Do $ Br endBlockName []) elseBlockName

  -- Generate else block
  elseOp <- codegenExpr' else'
  elseBlockFinalName <- currentBlockName
  terminateBlock (Do $ Br endBlockName []) endBlockName

  -- Generate end block
  addInstruction $ endOpName := Phi ty' [(thenOp, thenBlockFinalName), (elseOp, elseBlockFinalName)] []
  pure endOpRef
codegenExpr' (ANFEApp (ANFApp (ANFTyped originalTy ident) args' returnTy)) = do
  ty <- fromMaybe originalTy <$> topLevelType ident
  funcOperand <- valOperand (ANFVar $ ANFTyped ty ident)
  let
    (argTys', returnTy') = argAndReturnTypes ty
  opName <- freshUnName
  -- Convert arguments to pointers if we have to
  argOps <- for (zip args' argTys') $ \(arg, argTy) -> do
    originalOp <- valOperand arg
    maybeConvertPointer originalOp (anfValType arg) argTy

  -- Add call instruction
  addInstruction $ opName := Call Nothing CC.C [] (Right funcOperand) ((\arg -> (arg, [])) <$> argOps) [] []

  -- Return operand, maybe converting it too
  maybeConvertPointer (LocalReference (llvmType returnTy) opName) returnTy' returnTy
codegenExpr' (ANFEPrimOp (ANFApp prim args' returnTy)) = do
  opName <- freshUnName
  argOps <- traverse valOperand args'
  addInstruction $ opName := primitiveFunctionInstruction prim argOps
  pure $ LocalReference (llvmType returnTy) opName

maybeConvertPointer :: Operand -> ANFType -> ANFType -> BlockGen Operand
maybeConvertPointer op originalTy argTy =
  case (originalTy, argTy) of
    (ANFTyVar _, ANFTyVar _) -> pure op
    (_, ANFTyVar _) -> convertToIntPointer op
    (ANFTyVar _, _) -> convertFromIntPointer op argTy
    (_, _) -> pure op

convertToIntPointer :: Operand -> BlockGen Operand
convertToIntPointer op = do
  opName <- freshUnName
  let ptrTy = PointerType (IntegerType 64) (AddrSpace 0)
  addInstruction $ opName := IntToPtr op ptrTy []
  pure $ LocalReference ptrTy opName

convertFromIntPointer :: Operand -> ANFType -> BlockGen Operand
convertFromIntPointer op ty = do
  opName <- freshUnName
  addInstruction $ opName := PtrToInt op (llvmType ty) []
  pure $ LocalReference (llvmType ty) opName

valOperand :: ANFVal -> BlockGen Operand
valOperand (ANFVar (ANFTyped ty ident)) =
  let
    ident' = identToLLVM ident
  in
    case ident of
      (ANFIdent _ _ _ True) -> pure $ ConstantOperand $ C.GlobalReference (mkFunctionType ty) ident'
      _ -> fromMaybe (LocalReference (llvmType ty) ident') <$> lookupSymbol ident
valOperand (ANFLit lit) =
  pure $ ConstantOperand $
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
  in instruction

typeToNonEmpty :: ANFType -> NonEmpty ANFType
typeToNonEmpty (t1 `ANFTyFun` t2) = NE.cons t1 (typeToNonEmpty t2)
typeToNonEmpty ty = ty :| []

-- TODO: Add tests for this
llvmType :: ANFType -> LLVM.Type
llvmType ty = go (typeToNonEmpty ty)
 where
  go (ty' :| []) =
    case ty' of
      ANFTyCon tyName ->
        let prim = fromMaybe (error $ "Expected primitive TyCon, got " ++ show tyName) (anfTypeNamePrimitiveType tyName)
        in llvmPrimitiveType prim
      ANFTyVar _ -> PointerType (IntegerType 64) (AddrSpace 0)
      ANFTyFun{} -> mkFunctionType ty
  go _ = mkFunctionType ty

mkFunctionType :: ANFType -> LLVM.Type
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

identToLLVM :: ANFIdent -> LLVM.Name
identToLLVM (ANFIdent name' _ _ _) = LLVM.Name $ textToShortBS name'

-- | Convert from a amy primitive type to an LLVM type
llvmPrimitiveType :: PrimitiveType -> LLVM.Type
llvmPrimitiveType IntType = IntegerType 64
llvmPrimitiveType DoubleType = FloatingPointType DoubleFP
llvmPrimitiveType BoolType = IntegerType 1

textToShortBS :: Text -> ShortByteString
textToShortBS = BSS.toShort . encodeUtf8

stringToShortBS :: String -> ShortByteString
stringToShortBS = BSS.toShort . BS8.pack
