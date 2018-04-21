{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import GHC.Exts (IsString)

data Type ty
  = TyCon !ty
    -- ^ A type constructor
  | TyVar !TVar
    -- ^ A type variable, also used in inference
  | TyFun !(Type ty) !(Type ty)
    -- ^ Two types linked by "->" (short for Type Function)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

infixr 0 `TyFun`

newtype TVar = TVar { unTyVar :: Text }
  deriving (Show, Eq, Ord, IsString)

-- | Turns a 'NonEmpty' list of 'Type' values into a 'Type' via 'TyFun'.
typeFromNonEmpty :: NonEmpty (Type ty) -> Type ty
typeFromNonEmpty = foldr1 TyFun

-- | Inverse of 'typeFromNonEmpty'
typeToNonEmpty :: Type ty -> NonEmpty (Type ty)
typeToNonEmpty (t1 `TyFun` t2) = NE.cons t1 (typeToNonEmpty t2)
typeToNonEmpty ty = ty :| []

-- | A value with a type.
data Typed ty a
  = Typed
  { typedType :: !(Type ty)
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)

data Scheme ty
  = Forall [TVar] !(Type ty)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
