{-# LANGUAGE DeriveFunctor #-}

module Amy.Type
  ( Type(..)
  , Typed(..)
  , unfoldTyApp
  , unfoldTyFun
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)

import Amy.Names
import Amy.Syntax.Located

data Type
  = TyCon !TyConName
  | TyVar !TyVarName
  | TyExistVar !TyExistVarName
  | TyApp !Type !Type
  | TyRecord !(Map RowLabel Type) !(Maybe Type)
  | TyFun !Type !Type
  | TyForall !(NonEmpty TyVarName) !Type
  | LocatedType !SourceSpan !Type
  deriving (Show, Eq, Ord)

infixr 0 `TyFun`

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)

unfoldTyApp :: Type -> NonEmpty Type
unfoldTyApp (TyApp app@(TyApp _ _) arg) = unfoldTyApp app <> (arg :| [])
unfoldTyApp (TyApp f arg) = f :| [arg]
unfoldTyApp t = t :| []

unfoldTyFun :: Type -> NonEmpty Type
unfoldTyFun (TyForall _ t) = unfoldTyFun t
unfoldTyFun (t1 `TyFun` t2) = NE.cons t1 (unfoldTyFun t2)
unfoldTyFun ty = ty :| []
