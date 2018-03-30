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
    topLevelValueNames = (tExternName <$> externs) ++ (tBindingName <$> bindings)
  externDefs <- traverse codegenExtern externs
  bindingDefs <- traverse (codegenBinding topLevelValueNames) bindings
  pure $
    defaultModule
    { moduleName = "amy-module"
    , moduleDefinitions = externDefs ++ bindingDefs
    }

textToShortBS :: Text -> ShortByteString
textToShortBS = BSS.toShort . encodeUtf8

-- | Convert from a amy primitive type to an LLVM type
llvmPrimitiveType :: PrimitiveType -> LLVM.Type
llvmPrimitiveType IntType = IntegerType 32
llvmPrimitiveType DoubleType = FloatingPointType DoubleFP
llvmPrimitiveType BoolType = IntegerType 1

codegenExtern :: TExtern -> Either [Error] Definition
codegenExtern extern = do
  let
    types = tExternType extern
  paramTypes <-
    traverse (fmap llvmPrimitiveType . assertPrimitiveType) --traverse (fmap llvmPrimitiveType . assertPrimitiveType)
    $ NE.init (typeToNonEmpty types)
  let
    params =
      (\ty -> Parameter ty (UnName 0) []) <$> paramTypes
  returnType' <- fmap llvmPrimitiveType . assertPrimitiveType . NE.last . typeToNonEmpty $ types

  pure $
    GlobalDefinition
    functionDefaults
    { name = valueNameToLLVM $ tExternName extern
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    }

codegenBinding :: [ValueName] -> TBinding -> Either [Error] Definition
codegenBinding allTopLevelNames binding = do
  blocks <- runGenBlocks $ do
    -- Add top-level names to symbol table
    forM_ allTopLevelNames $ \valueName ->
      addNameToSymbolTable valueName (GlobalIdentifier valueName)

    -- Add args to symbol table
    forM_ (tBindingArgs binding) $ \(argType, valueName) ->
      let op = LocalReference (llvmPrimitiveType argType) (valueNameToLLVM valueName)
      in addNameToSymbolTable valueName (LocalOperand op)

    -- Codegen the expression
    codegenExpression $ tBindingBody binding

  let
    params =
      (\(ty, valueName) -> Parameter (llvmPrimitiveType ty) (Name . textToShortBS $ valueNameRaw valueName) [])
      <$> tBindingArgs binding
    returnType' = llvmPrimitiveType $ tBindingReturnType binding

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
codegenExpression (TEVar (Typed ty valueName)) = do
  -- Check if a value exists in the symbol table
  ident <- lookupSymbolOrError valueName

  primTy <- assertPrimitiveType ty
  case ident of
    LocalOperand op -> pure op
    GlobalIdentifier valueName' -> functionCallInstruction valueName' [] [] primTy
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
  ty' <- llvmPrimitiveType <$> assertPrimitiveType (expressionType expr)
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
  let
    fnArgs = tAppArgs app
    fnArgTypes = fst <$> fnArgs
    fnReturnType = tAppReturnType app
  argOps <- mapM codegenExpression (snd <$> fnArgs)

  -- Get the function expression variable
  fnVarName <-
    case tAppFunction app of
      (TEVar (Typed _ var)) -> pure var
      _ -> throwError [NoCurrying app]

  -- Generate code for function
  case valueNameId fnVarName of
    PrimitiveFunctionId primName -> codegenPrimitiveFunction primName argOps fnReturnType
    NameIntId _ -> functionCallInstruction fnVarName (toList argOps) (toList fnArgTypes) fnReturnType

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
  :: ValueName
  -> [Operand]
  -> [PrimitiveType]
  -> PrimitiveType
  -> FunctionGen Operand
functionCallInstruction valueName argumentOperands argumentTypes' returnType' = do
  let
    argTypes' = llvmPrimitiveType <$> argumentTypes'
    fnRef =
      ConstantOperand $
      C.GlobalReference
      (PointerType (LLVM.FunctionType (llvmPrimitiveType returnType') argTypes' False) (AddrSpace 0))
      (valueNameToLLVM valueName)
    toArg arg = (arg, [])
    instruction = Call Nothing CC.C [] (Right fnRef) (toArg <$> argumentOperands) [] []

  instr (llvmPrimitiveType returnType') instruction

assertPrimitiveType :: (MonadError [Error] m) => T.Type PrimitiveType -> m PrimitiveType
assertPrimitiveType t =
  case t of
    (TVar t') -> pure t'
    _ -> throwError [CodegenExpectedPrimitiveType t]
