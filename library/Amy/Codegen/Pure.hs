{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Pure
  ( codegenPure
  ) where

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (forM_, toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import LLVM.AST as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Global as LLVM

import Amy.AST
import Amy.Codegen.Monad
import Amy.Errors
import Amy.Names
import Amy.Prim
import Amy.Type as T

codegenPure :: AST ValueName T.Type -> Either [Error] Module
codegenPure (AST declarations) = do
  let
    -- Extract all the top level names so we can put them in the symbol table.
    topLevelValueNames =
      fmap bindingTypeName
      $ mapMaybe topLevelBindingType declarations
      ++ mapMaybe topLevelExternType declarations
  definitions <- catMaybes <$> mapM (codegenDeclaration topLevelValueNames) declarations
  pure $
    defaultModule
    { moduleName = "amy-module"
    , moduleDefinitions = definitions
    }

textToShortBS :: Text -> ShortByteString
textToShortBS = BSS.toShort . encodeUtf8

-- | Convert from a amy primitive type to an LLVM type
llvmPrimitiveType :: PrimitiveType -> LLVM.Type
llvmPrimitiveType IntType = IntegerType 32
llvmPrimitiveType DoubleType = FloatingPointType DoubleFP
llvmPrimitiveType BoolType = IntegerType 1

codegenDeclaration :: [ValueName] -> TopLevel ValueName T.Type -> Either [Error] (Maybe Definition)
codegenDeclaration allTopLevelNames (TopLevelBindingValue binding) = do
  let
    bindingType = bindingValueType binding

  blocks <- runGenBlocks $ do
    -- Add top-level names to symbol table
    forM_ allTopLevelNames $ \valueName ->
      addNameToSymbolTable valueName (GlobalIdentifier valueName)

    -- Add args to symbol table
    let
      argsAndTypes = zip (bindingValueArgs binding) (argTypes bindingType)
    forM_ argsAndTypes $ \(valueName, argType) ->
      let op = LocalReference (llvmPrimitiveType argType) (valueNameToLLVM valueName)
      in addNameToSymbolTable valueName (LocalOperand op)

    -- Codegen the expression
    codegenExpression $ bindingValueBody binding

  let
    paramTypes = llvmPrimitiveType <$> argTypes bindingType
    params =
      (\(valueName, ty) -> Parameter ty (Name . textToShortBS $ valueNameRaw valueName)  [])
      <$> zip (bindingValueArgs binding) paramTypes
    returnType' = llvmPrimitiveType $ T.returnType bindingType

  pure $
    Just $
    GlobalDefinition
    functionDefaults
    { name = valueNameToLLVM $ bindingValueName binding
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    , basicBlocks = blocks
    }
codegenDeclaration _ (TopLevelExternType extern) = do
  -- TODO: It sucks that we have to interpret these types again. I wish the
  -- renamer handled this for us.
  let
    typeNames = bindingTypeTypeNames extern
  bindingTypes <-
    traverse
    (\mType -> maybe (Left [CodegenUnknownTypeName mType]) pure $ readPrimitiveType mType)
    typeNames

  let
    paramTypes = llvmPrimitiveType <$> NE.init bindingTypes
    params =
      (\ty -> Parameter ty (UnName 0) []) <$> paramTypes
    returnType' = llvmPrimitiveType $ NE.last bindingTypes

  pure $
    Just $
    GlobalDefinition
    functionDefaults
    { name = valueNameToLLVM $ bindingTypeName extern
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    }
codegenDeclaration _ (TopLevelBindingType _) = Right Nothing

valueNameToLLVM :: ValueName -> Name
valueNameToLLVM (ValueName name' _) = Name $ textToShortBS name'

codegenExpression :: Expression ValueName T.Type -> FunctionGen Operand
codegenExpression (ExpressionLiteral lit) =
  pure $ ConstantOperand $
    case lit of
      LiteralInt i -> C.Int 32 (fromIntegral i)
      LiteralDouble x -> C.Float (F.Double x)
      LiteralBool x -> C.Int 1 $ if x then 1 else 0
codegenExpression (ExpressionVariable (Variable valueName ty)) = do
  -- Check if a value exists in the symbol table
  mSymbol <- lookupSymbol valueName
  ident <-
    case mSymbol of
      Nothing -> throwError [CodegenMissingSymbol valueName]
      Just s -> pure s
  case ident of
    LocalOperand op -> pure op
    GlobalIdentifier valueName' -> functionCallInstruction valueName' [] [] (T.returnType ty)
codegenExpression (ExpressionIf (If predicate thenExpression elseExpression ty)) = do
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
  ty' <- llvmPrimitiveType <$> assertPrimitiveType ty
  phi ty' [(thenOp, thenBlockName), (elseOp, elseBlockName)]
codegenExpression (ExpressionLet (Let bindings expression _)) = do
  -- For each binding value, generate code for the expression
  let bindingValues = mapMaybe letBindingValue bindings
  forM_ bindingValues $ \bindingValue -> do
    -- Generate code for binding expression
    bodyOp <- codegenExpression $ bindingValueBody bindingValue

    -- Add operator to symbol table for binding variable name
    addNameToSymbolTable (bindingValueName bindingValue) $ LocalOperand bodyOp

  -- Codegen the let expression
  codegenExpression expression
codegenExpression (ExpressionFunctionApplication app) = do
  -- Evaluate argument expressions
  let
    fnArgs = functionApplicationArgs app
  fnArgTypes <- traverse assertPrimitiveType $ expressionType <$> fnArgs
  fnReturnType <- assertPrimitiveType $ functionApplicationReturnType app
  argOps <- mapM codegenExpression fnArgs

  -- Get the function expression variable
  fnVarName <-
    case functionApplicationFunction app of
      ExpressionVariable var -> pure $ variableName var
      _ -> throwError [NoCurrying app]

  -- Generate code for function
  case valueNameId fnVarName of
    PrimitiveFunctionId primName -> codegenPrimitiveFunction primName argOps fnReturnType
    NameIntId _ -> functionCallInstruction fnVarName (toList argOps) (toList fnArgTypes) fnReturnType
codegenExpression (ExpressionParens expression) = codegenExpression expression

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

-- TODO: This function shouldn't be necessary. The AST that feeds into Codegen
-- should have things that are primitive types statically declared.
assertPrimitiveType :: T.Type -> FunctionGen PrimitiveType
assertPrimitiveType t =
  maybe
    (throwError [CodegenExpectedPrimitiveType t])
    pure
    $ primitiveType t
