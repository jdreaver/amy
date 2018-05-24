{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.TypeCheck.Monad
  ( TyEnv(..)
  , substituteEnv
  , Inference(..)
  , runInference
  , freshTypeVariable
  , extendEnvIdentM
  , lookupEnvIdentM
  , instantiate
  , generalize
  , freeTypeVariables
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)

import Amy.Errors
import Amy.Renamer.AST as R
import Amy.TypeCheck.AST as T
import Amy.TypeCheck.Substitution

--
-- Type Environment
--

-- | A 'TyEnv' is the typing environment. It contains known names with their
-- type schemes.
data TyEnv
  = TyEnv
  { identTypes :: !(Map IdentName T.Scheme)
    -- TODO: Should this be constructed in the renamer?
  , typeDefinitions :: !(Map TyConName T.TyConDefinition)
  , dataConstructorTypes :: !(Map DataConName (T.TypeDeclaration, T.DataConDefinition))
  } deriving (Show, Eq)

extendEnvIdent :: TyEnv -> (IdentName, T.Scheme) -> TyEnv
extendEnvIdent env (x, s) =
  env
  { identTypes = Map.insert x s (identTypes env)
  }

extendEnvIdentList :: TyEnv -> [(IdentName, T.Scheme)] -> TyEnv
extendEnvIdentList = foldl' extendEnvIdent

lookupEnvIdent :: IdentName -> TyEnv -> Maybe T.Scheme
lookupEnvIdent key = Map.lookup key . identTypes

substituteEnv :: Subst -> TyEnv -> TyEnv
substituteEnv subst (TyEnv identTys tyDefs dataConTys) =
  TyEnv
    (Map.map (substituteScheme subst) identTys)
    tyDefs
    dataConTys

--
-- Inference Monad
--

-- | Holds a 'TyEnv' variables in a 'ReaderT' and a 'State' 'Int'
-- counter for producing type variables.
newtype Inference a = Inference (ReaderT TyEnv (StateT Int (Except Error)) a)
  deriving (Functor, Applicative, Monad, MonadReader TyEnv, MonadState Int, MonadError Error)

-- TODO: Don't use Except, use Validation

runInference :: TyEnv -> Inference a -> Either Error a
runInference env (Inference action) = runExcept $ evalStateT (runReaderT action env) 0

freshTypeVariable :: Inference T.TyVarInfo
freshTypeVariable = do
  modify' (+ 1)
  id' <- get
  pure $ T.TyVarInfo (TyVarName $ "t" <> pack (show id')) TyVarGenerated

-- | Extends the current typing environment with a list of names and schemes
-- for those names.
extendEnvIdentM :: [(IdentName, T.Scheme)] -> Inference a -> Inference a
extendEnvIdentM tys = local (flip extendEnvIdentList tys)

-- | Lookup type in the environment
lookupEnvIdentM :: IdentName -> Inference T.Type
lookupEnvIdentM name = do
  mTy <- asks (lookupEnvIdent name)
  maybe (throwError $ UnboundVariable name) instantiate mTy

-- | Convert a scheme into a type by replacing all the type variables with
-- fresh names. Types are instantiated when they are looked up so we can make
-- constraints with the type variables and not worry about name collisions.
instantiate :: T.Scheme -> Inference T.Type
instantiate (T.Forall as t) = do
  as' <- fmap T.TyVar <$> traverse (const freshTypeVariable) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ substituteType s t

-- | Generalizing a type is the quantification of that type with all of the
-- free variables of the type minus the free variables in the environment. This
-- is like finding the type variables that should be "bound" by the
-- quantification. This is also called finding the "closure" of a type.
generalize :: TyEnv -> T.Type -> T.Scheme
generalize env t  = T.Forall as t
 where
  as = Set.toList $ freeTypeVariables t `Set.difference` freeEnvTypeVariables env

--
-- Free type variables
--

freeTypeVariables :: T.Type -> Set T.TyVarInfo
freeTypeVariables (T.TyCon _) = Set.empty
freeTypeVariables (T.TyVar var) = Set.singleton var
freeTypeVariables (T.TyApp f arg) = freeTypeVariables f `Set.union` freeTypeVariables arg
freeTypeVariables (T.TyRecord rows mVar) = Set.unions $ Set.fromList (maybeToList mVar) : (freeTypeVariables <$> Map.elems rows)
freeTypeVariables (t1 `T.TyFun` t2) = freeTypeVariables t1 `Set.union` freeTypeVariables t2

freeSchemeTypeVariables :: T.Scheme -> Set T.TyVarInfo
freeSchemeTypeVariables (T.Forall tvs t) = freeTypeVariables t `Set.difference` Set.fromList tvs

freeEnvTypeVariables :: TyEnv -> Set T.TyVarInfo
freeEnvTypeVariables (TyEnv identTys _ _) =
  foldl' Set.union Set.empty $ freeSchemeTypeVariables <$> Map.elems identTys
