{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.TypeCheck.KindInference
  ( inferTypeDeclarationKind
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Traversable (for)

import Amy.Errors
import Amy.Kind
import Amy.TypeCheck.AST

--
-- Inference Monad
--

newtype KindInference a = KindInference (StateT KindInferenceState (Except Error) a)
  deriving (Functor, Applicative, Monad, MonadState KindInferenceState, MonadError Error)

data KindInferenceState
  = KindInferenceState
  { maxId :: !Int
  , tyVarKinds :: !(Map TyVarName Kind)
  , tyConKinds :: !(Map TyConName Kind)
  } deriving (Show, Eq)

runKindInference :: KindInference a -> Either Error a
runKindInference (KindInference action) = runExcept $ evalStateT action (KindInferenceState 0 Map.empty Map.empty)

freshKindVariable :: KindInference Kind
freshKindVariable = do
  modify' (\s -> s { maxId = maxId s + 1 })
  KUnknown <$> gets maxId

addUnknownTyVarKind :: TyVarName -> KindInference Kind
addUnknownTyVarKind name = do
  kind <- freshKindVariable
  modify' (\s -> s { tyVarKinds = Map.insert name kind (tyVarKinds s) })
  pure kind

addUnknownTyConKind :: TyConName -> KindInference Kind
addUnknownTyConKind name = do
  kind <- freshKindVariable
  modify' (\s -> s { tyConKinds = Map.insert name kind (tyConKinds s) })
  pure kind

lookupTyVarKind :: TyVarName -> KindInference Kind
lookupTyVarKind name =
  fromMaybe (error $ "Can't find kind for name, Renamer must have messed up " ++ show name)
  . Map.lookup name
  <$> gets tyVarKinds

lookupTyConKind :: TyConName -> KindInference Kind
lookupTyConKind name =
  fromMaybe (error $ "Can't find kind for name, Renamer must have messed up " ++ show name)
  . Map.lookup name
  <$> gets tyConKinds

--
-- Kind Inference
--

-- TODO: Infer the kinds of mutually recursive type declaration groups at the
-- same time instead of one by one.

inferTypeDeclarationKind :: TypeDeclaration -> Either Error Kind
inferTypeDeclarationKind = runKindInference . inferTypeDeclarationKind'

inferTypeDeclarationKind' :: TypeDeclaration -> KindInference Kind
inferTypeDeclarationKind' (TypeDeclaration (TyConDefinition tyCon tyArgs) constructors) = do
  -- Generate unknown kind variables for the type constructor and all type
  -- variables.
  tyConKindVar <- addUnknownTyConKind tyCon
  tyVarKindVars <- traverse addUnknownTyVarKind tyArgs
  let
    tyConConstraint = Constraint (tyConKindVar, foldr1 KFun $ tyVarKindVars ++ [KStar])

  -- Traverse constructors to collect constraints.
  (consKinds, consCons) <- unzip <$> traverse inferTypeKind (mapMaybe dataConDefinitionArgument constructors)

  -- Solve constraints
  subst <- undefined

  -- Substitute into tyConKindVar and return

  undefined

inferTypeKind :: Type -> KindInference (Kind, [Constraint])
inferTypeKind (TyCon name) = do
  kind <- lookupTyConKind name
  pure (kind, [])
inferTypeKind (TyVar (TyVarInfo name _)) = do
  kind <- lookupTyVarKind name
  pure (kind, [])
inferTypeKind (TyApp t1 t2) = do
  (k1, cons1) <- inferTypeKind t1
  (k2, cons2) <- inferTypeKind t2
  retKind <- freshKindVariable
  let constraint = Constraint (k1, k2 `KFun` retKind)
  pure (KFun k1 k2, cons1 ++ cons2 ++ [constraint])
inferTypeKind (TyFun t1 t2) = do
  (k1, cons1) <- inferTypeKind t1
  (k2, cons2) <- inferTypeKind t2
  let constraint = Constraint (k1, k2 `KFun` KStar)
  pure (KFun k1 k2, cons1 ++ cons2 ++ [constraint])
inferTypeKind (TyRecord fields mVar) = do
  fieldCons <- for (Map.elems fields) $ \ty -> do
    (fieldKind, fieldCons) <- inferTypeKind ty
    pure $ fieldCons ++ [Constraint (fieldKind, KStar)]
  varCons <- for (maybeToList mVar) $ \(TyVarInfo var _) -> do
    varKind <- lookupTyVarKind var
    pure $ Constraint (varKind, KRow)
  pure (KStar, concat fieldCons ++ varCons)

-- | If we have any unknown kinds left, just call them KStar.
starIfUnknown :: Kind -> Kind
starIfUnknown KStar = KStar
starIfUnknown (KUnknown _) = KStar
starIfUnknown KRow = KRow
starIfUnknown (KFun k1 k2) = KFun (starIfUnknown k1) (starIfUnknown k2)

--
-- Unification
--

newtype Constraint = Constraint (Kind, Kind)
  deriving (Show, Eq)
