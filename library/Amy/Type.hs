{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Amy.Type
  ( Type(..)
  , typeFromNonEmpty
  , typeToNonEmpty
  , Typed(..)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

data Type ty
  = TyCon !ty
    -- ^ A type constructor
  | TyArr !(Type ty) !(Type ty)
    -- ^ Two types linked by "->" (short for Type Array)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Turns a 'NonEmpty' list of 'Type' values into a 'Type' via 'TyArr'.
typeFromNonEmpty :: NonEmpty (Type ty) -> Type ty
typeFromNonEmpty = go . NE.toList
 where
  go [] = error "No empty lists here!"
  go [t] = t
  go (t:ts) = TyArr t (go ts)

-- | Inverse of 'typeFromNonEmpty'
typeToNonEmpty :: Type ty -> NonEmpty (Type ty)
typeToNonEmpty = go
 where
  go ty@(TyCon _) = ty :| []
  go (TyArr t1 t2) = NE.cons t1 (typeToNonEmpty t2)

-- | A value with a type.
data Typed ty a
  = Typed
  { typedType :: !(Type ty)
  , typedValue :: !a
  } deriving (Show, Eq)
