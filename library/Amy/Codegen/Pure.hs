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
  defaultModule
  { moduleName = "amy-module"
  , moduleDefinitions = (codegenExtern <$> externs) ++ (codegenTopLevelBinding <$> bindings)
  }

codegenExtern :: ANFExtern -> Definition
codegenExtern extern =
  let
    types = anfExternType extern
    paramTypes = llvmType <$> NE.init (typeToNonEmpty types)
    params =
      (\ty -> Parameter ty (UnName 0) []) <$> paramTypes
    returnType' = llvmType . NE.last . typeToNonEmpty $ types
  in
    GlobalDefinition
    functionDefaults
    { name = identToLLVM $ anfExternName extern
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    }

codegenTopLevelBinding :: ANFBinding -> Definition
codegenTopLevelBinding binding =
  let
    argToParam (ANFTyped ty ident) = Parameter (llvmType ty) (identToLLVM ident) []
    params = argToParam <$> anfBindingArgs binding
    returnType' = llvmType $ anfBindingReturnType binding
    blocks = codegenExpr $ anfBindingBody binding
  in
    GlobalDefinition
    functionDefaults
    { name = identToLLVM $ anfBindingName binding
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    , basicBlocks = blocks
    }

codegenExpr :: ANFExpr -> [BasicBlock]
codegenExpr expr = runBlockGen $ codegenExpr' expr

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
codegenExpr' (ANFEApp (ANFApp (ANFTyped ty ident) args' returnTy)) = do
  funcOperand <- valOperand (ANFVar $ ANFTyped ty ident)
  let
    mkInstruction argOps = Call Nothing CC.C [] (Right funcOperand) ((\arg -> (arg, [])) <$> argOps) [] []
  codegenFunctionApp args' returnTy mkInstruction
codegenExpr' (ANFEPrimOp (ANFApp prim args' returnTy)) =
  codegenFunctionApp args' returnTy (primitiveFunctionInstruction prim)

codegenFunctionApp :: [ANFVal] -> ANFType -> ([Operand] -> Instruction) -> BlockGen Operand
codegenFunctionApp args' returnTy mkInstruction = do
  opName <- freshUnName
  argOps <- traverse valOperand args'
  addInstruction $ opName := mkInstruction argOps
  pure $ LocalReference (llvmType returnTy) opName

valOperand :: ANFVal -> BlockGen Operand
valOperand (ANFVar (ANFTyped ty ident)) =
  let
    ty' = llvmType ty
    ident' = identToLLVM ident
  in
    case ident of
      (ANFIdent _ _ _ True) -> pure $ ConstantOperand $ C.GlobalReference ty' ident'
      _ -> fromMaybe (LocalReference ty' ident') <$> lookupSymbol ident
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
llvmType = go . typeToNonEmpty
 where
  go (ty :| []) =
    case ty of
      ANFTyCon tyName ->
        let prim = fromMaybe (error $ "Expected primitive TyCon, got " ++ show tyName) (anfTypeNamePrimitiveType tyName)
        in llvmPrimitiveType prim
      ANFTyVar _ -> error "Can't handle polymorphic type arguments yet"
      t@ANFTyFun{} -> mkFunctionType (t :| [])
  go ts = mkFunctionType ts
  mkFunctionType ts =
    PointerType
      FunctionType
      { resultType = llvmType $ NE.last ts
      , argumentTypes = llvmType <$> NE.init ts
      , isVarArg = False
      }
      (AddrSpace 0)

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
