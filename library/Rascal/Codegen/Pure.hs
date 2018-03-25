{-# LANGUAGE OverloadedStrings #-}

module Rascal.Codegen.Pure
  ( codegenPure
  ) where

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import LLVM.AST as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.Global

import Rascal.Codegen.Monad
import Rascal.TypeCheck as TC

codegenPure :: TypeCheckAST -> Module
codegenPure (TypeCheckAST declarations) =
  let
    definitions = codegenDeclaration <$> declarations
  in
    defaultModule
    { moduleName = "rascal-module"
    , moduleDefinitions = definitions
    }

textToShortBS :: Text -> ShortByteString
textToShortBS = BSS.toShort . encodeUtf8

-- | Convert from a rascal primitive type to an LLVM type
llvmPrimitiveType :: PrimitiveType -> LLVM.Type
llvmPrimitiveType IntType = IntegerType 32
llvmPrimitiveType DoubleType = FloatingPointType DoubleFP

codegenDeclaration :: TypeCheckASTDeclaration -> Definition
codegenDeclaration (TypeCheckASTBinding binding) =
  let
    block = runGenBlock "entry" (codegenExpression $ typeCheckBindingDeclarationBody binding)
    bindingType = typeCheckBindingDeclarationType binding
    paramTypes = llvmPrimitiveType <$> maybe [] (toList . functionTypeArgTypes) (functionType bindingType)
    params =
      (\(idn, ty) -> Parameter ty (Name . textToShortBS $ idNameText idn)  [])
      <$> zip (typeCheckBindingDeclarationArgs binding) paramTypes
    returnType' = llvmPrimitiveType $ bindingReturnType bindingType
  in
    GlobalDefinition
    functionDefaults
    { name = idNameToLLVM $ typeCheckBindingDeclarationName binding
    , parameters = (params, False)
    , returnType = returnType'
    , basicBlocks = [block]
    }
codegenDeclaration (TypeCheckASTExtern extern) =
  let
    bindingType = typeCheckExternDeclarationType extern
    paramTypes = llvmPrimitiveType <$> maybe [] (toList . functionTypeArgTypes) (functionType bindingType)
    params =
      (\ty -> Parameter ty (UnName 0) []) <$> paramTypes
    returnType' = llvmPrimitiveType $ bindingReturnType bindingType
  in
    GlobalDefinition
    functionDefaults
    { name = idNameToLLVM $ typeCheckExternDeclarationName extern
    , parameters = (params, False)
    , returnType = returnType'
    }

idNameToLLVM :: IdName -> Name
idNameToLLVM (IdName name' _ _) = Name $ textToShortBS name'

codegenExpression :: Typed TypeCheckASTExpression -> FunctionGen Operand
codegenExpression (Typed _ (TypeCheckASTLiteral lit)) =
  pure $ ConstantOperand $
    case lit of
      LiteralInt i -> C.Int 32 (fromIntegral i)
      LiteralDouble x -> C.Float (F.Double x)
codegenExpression (Typed ty (TypeCheckASTVariable idn)) =
  -- We need to use the IdName's provenance to determine whether or not to use
  -- a local reference to a variable or a function call with no arguments.
  case idNameProvenance idn of
    LocalDefinition -> pure $ LocalReference (llvmPrimitiveType ty) (idNameToLLVM idn)
    TopLevelDefinition -> functionCallInstruction idn [] [] ty
codegenExpression (Typed ty (TypeCheckASTFunctionApplication app)) = do
  let
    fnName = typeCheckFunctionApplicationFunctionName app
    fnArgs = typeCheckFunctionApplicationArgs app
    fnArgTypes = toList $ typedType <$> fnArgs
  argOps <- mapM codegenExpression fnArgs
  functionCallInstruction fnName (toList argOps) fnArgTypes ty
codegenExpression (Typed ty (TypeCheckASTExpressionParens expression)) = codegenExpression (Typed ty expression)

functionCallInstruction
  :: IdName
  -> [Operand]
  -> [PrimitiveType]
  -> PrimitiveType
  -> FunctionGen Operand
functionCallInstruction idName argumentOperands argumentTypes' returnType' = do
  let
    argTypes = llvmPrimitiveType <$> argumentTypes'
    fnRef =
      ConstantOperand $
      C.GlobalReference
      (PointerType (LLVM.FunctionType (llvmPrimitiveType returnType') argTypes False) (AddrSpace 0))
      (idNameToLLVM idName)
    toArg arg = (arg, [])
    instruction = Call Nothing CC.C [] (Right fnRef) (toArg <$> argumentOperands) [] []

  instructionName <- addUnNamedInstruction instruction
  pure $ LocalReference (llvmPrimitiveType returnType') instructionName
