{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Amy.Type
  ( Type(..)
  , TVar(..)
  , typeFromNonEmpty
  , typeToNonEmpty
  , Typed(..)
  , Scheme(..)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)

data Type ty
  = TyCon !ty
    -- ^ A type constructor
  | TyVar !TVar
    -- ^ A type variable, also used in inference
  | TyArr !(Type ty) !(Type ty)
    -- ^ Two types linked by "->" (short for Type Array)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

infixr 0 `TyArr`

newtype TVar = TVar { unTyVar :: Text }
  deriving (Show, Eq, Ord)

-- | Turns a 'NonEmpty' list of 'Type' values into a 'Type' via 'TyArr'.
typeFromNonEmpty :: NonEmpty (Type ty) -> Type ty
typeFromNonEmpty = go . NE.toList
 where
  go [] = error "No empty lists here!"
  go [t] = t
  go (t:ts) = t `TyArr` go ts

-- | Inverse of 'typeFromNonEmpty'
typeToNonEmpty :: Type ty -> NonEmpty (Type ty)
typeToNonEmpty = go
 where
  go ty@(TyCon _) = ty :| []
  go ty@(TyVar _) = ty :| []
  go (t1 `TyArr` t2) = NE.cons t1 (typeToNonEmpty t2)

-- | A value with a type.
data Typed ty a
  = Typed
  { typedType :: !(Type ty)
  , typedValue :: !a
  } deriving (Show, Eq, Ord)

data Scheme ty
  = Forall [TVar] !(Type ty)
  deriving (Show, Eq, Ord)
