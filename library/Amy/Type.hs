{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Type
  ( -- * Type
    Type(..)
  , Typed(..)
  , unfoldTyApp
  , foldTyApp
  , unfoldTyFun
  , foldTyFun

    -- * Type Traversals
  , traverseType
  , traverseTypeM
  , removeTyExistVar
  , blowUpOnTyUnknown

    -- * Type Declarations
  , TypeDeclaration(..)
  , TyConDefinition(..)
  , DataConDefinition(..)
  , dataConTypes
  ) where

import Control.Monad.Identity (Identity(..), runIdentity)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Text (pack)

import Amy.Names
import Amy.Syntax.Located

--
-- Type
--

data Type
  = TyUnknown
  | TyCon !(MaybeLocated TyConName)
  | TyVar !(MaybeLocated TyVarName)
  | TyExistVar !TyExistVarName
  | TyApp !Type !Type
  | TyRecord !(Map (MaybeLocated RowLabel) Type) !(Maybe Type)
  | TyFun !Type !Type
  | TyForall !(NonEmpty (MaybeLocated TyVarName)) !Type
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

foldTyApp :: NonEmpty Type -> Type
foldTyApp = foldl1 TyApp

unfoldTyFun :: Type -> NonEmpty Type
unfoldTyFun (TyForall _ t) = unfoldTyFun t
unfoldTyFun (t1 `TyFun` t2) = NE.cons t1 (unfoldTyFun t2)
unfoldTyFun ty = ty :| []

foldTyFun :: NonEmpty Type -> Type
foldTyFun = foldr1 TyFun

--
-- Type Traversals
--

traverseType :: (Type -> Type) -> Type -> Type
traverseType f = runIdentity . traverseTypeM (Identity . f)

-- | Single step of a traversal through a 'Type'.
--
-- This function doesn't traverse the entire 'Type'. It applies a function to
-- all the immediate sub nodes of a single node. This is most useful when
-- paired with another mutually recursive function (@f@) that singles out the
-- nodes it cares about, and leaves this function to traverse the ones it
-- doesn't.
--
traverseTypeM :: (Monad m) => (Type -> m Type) -> Type -> m Type
traverseTypeM f = go
 where
  go t@TyUnknown{} = pure t
  go t@TyCon{} = pure t
  go t@TyVar{} = pure t
  go t@TyExistVar{} = pure t
  go (TyApp t1 t2) = TyApp <$> f t1 <*> f t2
  go (TyRecord rows mTail) = TyRecord <$> traverse f rows <*> traverse f mTail
  go (TyFun t1 t2) = TyFun <$> f t1 <*> f t2
  go (TyForall vars ty) = TyForall vars <$> f ty

-- | Replace any 'TyExistVar' nodes with 'TyVar' nodes.
removeTyExistVar :: Type -> Type
removeTyExistVar = go
 where
  go (TyExistVar (TyExistVarName i)) = TyVar $ notLocated $ TyVarName $ "$t" <> pack (show i)
  go t = traverseType go t

-- | Sanity check to run after type checking to make sure all 'TyUnknown'
-- values are gone.
blowUpOnTyUnknown :: Type -> Type
blowUpOnTyUnknown = go
 where
  go TyUnknown = error "TyUnknowns still exist!"
  go t = traverseType go t

--
-- Type Declarations
--

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !TyConDefinition
  , typeDeclarationConstructors :: ![DataConDefinition]
  } deriving (Show, Eq)

data TyConDefinition
  = TyConDefinition
  { tyConDefinitionName :: !(Located TyConName)
  , tyConDefinitionArgs :: ![Located TyVarName]
  } deriving (Show, Eq)

data DataConDefinition
  = DataConDefinition
  { dataConDefinitionName :: !(Located DataConName)
  , dataConDefinitionArgument :: !(Maybe Type)
  } deriving (Show, Eq)
