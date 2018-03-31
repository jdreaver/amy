{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Pure
  ( codegenPure
  ) where

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_, toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import LLVM.AST as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Global as LLVM

import Amy.Codegen.Monad
import Amy.Errors
import Amy.Names
import Amy.Prim
import Amy.Type as T
import Amy.TypeCheck.AST

codegenPure :: TModule -> Either [Error] Module
codegenPure (TModule bindings externs) = do
  let
    -- Extract all the top level names so we can put them in the symbol table.
    topLevelIdentifiers =
      (externIdentifier <$> externs)
      ++ (bindingIdentifier <$> bindings)
  externDefs <- traverse codegenExtern externs
  bindingDefs <- traverse (codegenBinding topLevelIdentifiers) bindings
  pure $
    defaultModule
    { moduleName = "amy-module"
    , moduleDefinitions = externDefs ++ bindingDefs
    }

externIdentifier :: TExtern -> (ValueName, CodegenIdentifier)
externIdentifier extern = (name', GlobalFunction op)
 where
  name' = tExternName extern
  ty = llvmType $ tExternType extern
  op = ConstantOperand $ C.GlobalReference ty (valueNameToLLVM name')

bindingIdentifier :: TBinding -> (ValueName, CodegenIdentifier)
bindingIdentifier binding = (name', ident)
 where
  name' = tBindingName binding
  funcType = llvmType (tBindingType binding)
  op = ConstantOperand $ C.GlobalReference funcType (valueNameToLLVM name')
  ident =
    if null (tBindingArgs binding)
    then GlobalFunctionNoArgs op
    else GlobalFunction op

textToShortBS :: Text -> ShortByteString
textToShortBS = BSS.toShort . encodeUtf8

-- TODO: Add tests for this
llvmType :: T.Type PrimitiveType -> LLVM.Type
llvmType = go . typeToNonEmpty
 where
  go (TVar prim :| []) = llvmPrimitiveType prim
  go ts =
    PointerType
      FunctionType
      { resultType = llvmType $ NE.last ts
      , argumentTypes = llvmType <$> NE.init ts
      , isVarArg = False
      }
      (AddrSpace 0)

-- | Convert from a amy primitive type to an LLVM type
llvmPrimitiveType :: PrimitiveType -> LLVM.Type
llvmPrimitiveType IntType = IntegerType 32
llvmPrimitiveType DoubleType = FloatingPointType DoubleFP
llvmPrimitiveType BoolType = IntegerType 1

codegenExtern :: TExtern -> Either [Error] Definition
codegenExtern extern = do
  let
    types = tExternType extern
    paramTypes = llvmType <$> NE.init (typeToNonEmpty types)
    params =
      (\ty -> Parameter ty (UnName 0) []) <$> paramTypes
    returnType' = llvmType . NE.last . typeToNonEmpty $ types

  pure $
    GlobalDefinition
    functionDefaults
    { name = valueNameToLLVM $ tExternName extern
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    }

codegenBinding :: [(ValueName, CodegenIdentifier)] -> TBinding -> Either [Error] Definition
codegenBinding allTopLevelIdentifiers binding = do
  blocks <- runGenBlocks $ do
    -- Add top-level names to symbol table
    forM_ allTopLevelIdentifiers $ uncurry addNameToSymbolTable

    -- Add args to symbol table
    forM_ (tBindingArgs binding) $ \(Typed argType valueName) ->
      let op = LocalReference (llvmType argType) (valueNameToLLVM valueName)
      in addNameToSymbolTable valueName (LocalOperand op)

    -- Codegen the expression
    codegenExpression $ tBindingBody binding

  let
    params =
      (\(Typed ty valueName) -> Parameter (llvmType ty) (Name . textToShortBS $ valueNameRaw valueName) [])
      <$> tBindingArgs binding
    returnType' = llvmType $ tBindingReturnType binding

  pure $
    GlobalDefinition
    functionDefaults
    { name = valueNameToLLVM $ tBindingName binding
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    , basicBlocks = blocks
    }

valueNameToLLVM :: ValueName -> Name
valueNameToLLVM (ValueName name' _) = Name $ textToShortBS name'

codegenExpression :: TExpr -> FunctionGen Operand
codegenExpression (TELit lit) =
  pure $ ConstantOperand $
    case lit of
      LiteralInt i -> C.Int 32 (fromIntegral i)
      LiteralDouble x -> C.Float (F.Double x)
      LiteralBool x -> C.Int 1 $ if x then 1 else 0
codegenExpression (TEVar (Typed _ valueName)) = do
  -- Check if a value exists in the symbol table
  ident <- lookupSymbolOrError valueName

  case ident of
    LocalOperand op -> pure op
    GlobalFunctionNoArgs op -> functionCallInstruction op []
    GlobalFunction op -> pure op
codegenExpression expr@(TEIf (TIf predicate thenExpression elseExpression)) = do
  let
    one = ConstantOperand $ C.Int 32 1
    --zero = ConstantOperand $ C.Int 32 0
    true = one
    --false = zero

  -- Generate unique block names
  uniqueId <- currentId
  let
    thenBlockName = Name $ BSS.toShort $ BS8.pack $ "if.then." ++ show uniqueId
    elseBlockName = Name $ BSS.toShort $ BS8.pack $ "if.else." ++ show uniqueId
    endBlockName = Name $ BSS.toShort $ BS8.pack $ "if.end." ++ show uniqueId

  -- Generate code for the predicate
  predicateOp <- codegenExpression predicate
  test <- instr (llvmPrimitiveType BoolType) $ ICmp IP.EQ true predicateOp []
  cbr test thenBlockName elseBlockName

  -- Generate code for the "then" block
  startNewBlock thenBlockName
  thenOp <- codegenExpression thenExpression
  br endBlockName

  -- Generate code for the "else" block
  startNewBlock elseBlockName
  elseOp <- codegenExpression elseExpression
  br endBlockName

  -- Generate the code for the ending block
  startNewBlock endBlockName
  let ty' = llvmType $ expressionType expr
  phi ty' [(thenOp, thenBlockName), (elseOp, elseBlockName)]
codegenExpression (TELet (TLet bindings expression)) = do
  -- For each binding value, generate code for the expression
  for_ bindings $ \binding -> do
    -- Generate code for binding expression
    bodyOp <- codegenExpression $ tBindingBody binding

    -- Add operator to symbol table for binding variable name
    addNameToSymbolTable (tBindingName binding) $ LocalOperand bodyOp

  -- Codegen the let expression
  codegenExpression expression
codegenExpression (TEApp app) = do
  -- Evaluate argument expressions
  argOps <- traverse codegenExpression (typedValue <$> tAppArgs app)

  -- Get the function expression variable
  fnVarName <-
    case tAppFunction app of
      (TEVar (Typed _ var)) -> pure var
      _ -> throwError [NoCurrying app]

  -- Generate code for function
  case valueNameId fnVarName of
    PrimitiveFunctionId primName -> codegenPrimitiveFunction primName argOps (tAppReturnType app)
    NameIntId _ -> do
      ident <- lookupSymbolOrError fnVarName

      let
        funcOperand =
          case ident of
            LocalOperand op -> op
            GlobalFunctionNoArgs op -> op
            GlobalFunction op -> op
      functionCallInstruction funcOperand (toList argOps)

codegenPrimitiveFunction
  :: PrimitiveFunctionName
  -> NonEmpty Operand
  -> PrimitiveType
  -> FunctionGen Operand
codegenPrimitiveFunction primFuncName argumentOperands returnType' = do
  let
    -- TODO: Better error checking here if number of operands doesn't match
    -- expected number. However, this should be type checked, so a simple panic
    -- should suffice.
    op0 = NE.head argumentOperands
    op1 = argumentOperands NE.!! 1
  let
    instruction =
      case primFuncName of
        PrimIAdd -> Add False False op0 op1 []
        PrimISub -> Sub False False op0 op1 []
        PrimIEquals -> ICmp IP.EQ op0 op1 []
        PrimIGreaterThan -> ICmp IP.SGT op0 op1 []
        PrimILessThan -> ICmp IP.SLT op0 op1 []

        PrimDAdd -> FAdd noFastMathFlags op0 op1 []
        PrimDSub -> FSub noFastMathFlags op0 op1 []

  instr (llvmPrimitiveType returnType') instruction

functionCallInstruction
  :: Operand
  -> [Operand]
  -> FunctionGen Operand
functionCallInstruction funcOperand argumentOperands = do
  let
    args' = (\arg -> (arg, [])) <$> argumentOperands
    instruction = Call Nothing CC.C [] (Right funcOperand) args' [] []
  resultType' <- operandresultType funcOperand
  instr resultType' instruction

operandresultType :: Operand -> FunctionGen LLVM.Type
operandresultType (LocalReference ty _) = pure ty
operandresultType (ConstantOperand (C.GlobalReference (PointerType ft@FunctionType{} _) _)) =
  pure $ resultType ft
operandresultType op = throwError [UnknownOperandType op]
