module Amy.Type
  ( Type(..)
  , Typed(..)
  , returnType
  , argTypes
  , primitiveType
  , makeType
  , PrimitiveType(..)
  , readPrimitiveType
  , FunctionType(..)
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Amy.Prim (PrimitiveType(..), readPrimitiveType)

data Type ty
  = PrimitiveTy !ty
  | FunctionTy !(FunctionType ty)
  deriving (Show, Eq)

-- | A value with a type.
data Typed ty a
  = Typed
  { typedType :: !(Type ty)
  , typedValue :: !a
  } deriving (Show, Eq)

returnType :: Type ty -> ty
returnType (PrimitiveTy ty) = ty
returnType (FunctionTy ft) = functionTypeReturnType ft

argTypes :: Type ty -> [ty]
argTypes (PrimitiveTy _) = []
argTypes (FunctionTy ft) = toList (functionTypeArgTypes ft)

primitiveType :: Type ty -> Maybe ty
primitiveType (PrimitiveTy prim) = Just prim
primitiveType _ = Nothing

makeType :: NonEmpty ty -> Type ty
makeType primTypes =
  case primTypes of
    primType :| [] -> PrimitiveTy primType
    types@(firstType :| rest) ->
      FunctionTy
      FunctionType
      { functionTypeArgTypes = firstType :| init rest
      , functionTypeReturnType = NE.last types
      }

data FunctionType ty
  = FunctionType
  { functionTypeArgTypes :: !(NonEmpty ty)
  , functionTypeReturnType :: !ty
  } deriving (Show, Eq)
