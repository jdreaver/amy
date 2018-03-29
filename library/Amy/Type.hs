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

data Type
  = PrimitiveTy !PrimitiveType
  | FunctionTy !FunctionType
  deriving (Show, Eq)

-- | A value with a type.
data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq)

returnType :: Type -> PrimitiveType
returnType (PrimitiveTy ty) = ty
returnType (FunctionTy ft) = functionTypeReturnType ft

argTypes :: Type -> [PrimitiveType]
argTypes (PrimitiveTy _) = []
argTypes (FunctionTy ft) = toList (functionTypeArgTypes ft)

primitiveType :: Type -> Maybe PrimitiveType
primitiveType (PrimitiveTy prim) = Just prim
primitiveType _ = Nothing

makeType :: NonEmpty PrimitiveType -> Type
makeType primTypes =
  case primTypes of
    primType :| [] -> PrimitiveTy primType
    types@(firstType :| rest) ->
      FunctionTy
      FunctionType
      { functionTypeArgTypes = firstType :| init rest
      , functionTypeReturnType = NE.last types
      }

data FunctionType
  = FunctionType
  { functionTypeArgTypes :: !(NonEmpty PrimitiveType)
  , functionTypeReturnType :: !PrimitiveType
  } deriving (Show, Eq)
