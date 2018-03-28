module Amy.Type
  ( Type(..)
  , returnType
  , argTypes
  , primitiveType
  , PrimitiveType(..)
  , readPrimitiveType
  , FunctionType(..)
  , expressionType
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)

import Amy.AST
import Amy.Prim (PrimitiveType(..), readPrimitiveType)

data Type
  = PrimitiveTy !PrimitiveType
  | FunctionTy !FunctionType
  deriving (Show, Eq)

returnType :: Type -> PrimitiveType
returnType (PrimitiveTy ty) = ty
returnType (FunctionTy ft) = functionTypeReturnType ft

argTypes :: Type -> [PrimitiveType]
argTypes (PrimitiveTy _) = []
argTypes (FunctionTy ft) = toList (functionTypeArgTypes ft)

primitiveType :: Type -> Maybe PrimitiveType
primitiveType (PrimitiveTy prim) = Just prim
primitiveType _ = Nothing

data FunctionType
  = FunctionType
  { functionTypeArgTypes :: !(NonEmpty PrimitiveType)
  , functionTypeReturnType :: !PrimitiveType
  } deriving (Show, Eq)

-- | Gets the 'Type' for an expression. All expression nodes have a type!
expressionType :: Expression name Type -> Type
expressionType (ExpressionLiteral lit) = PrimitiveTy $ literalType lit
expressionType (ExpressionVariable (Variable _ ty)) = ty
expressionType (ExpressionIf if') = ifType if'
expressionType (ExpressionLet let') = letType let'
expressionType (ExpressionFunctionApplication app) = functionApplicationReturnType app
expressionType (ExpressionParens expr) = expressionType expr

literalType :: Literal -> PrimitiveType
literalType (LiteralInt _) = IntType
literalType (LiteralDouble _) = DoubleType
literalType (LiteralBool _) = BoolType
