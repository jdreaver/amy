{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.TypeCheck.KindInference
  ( inferTypeDeclarationKind
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
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

freshKindVariable :: KindInference Int
freshKindVariable = do
  modify' (\s -> s { maxId = maxId s + 1 })
  gets maxId

addUnknownTyVarKind :: TyVarName -> KindInference Int
addUnknownTyVarKind name = do
  i <- freshKindVariable
  modify' (\s -> s { tyVarKinds = Map.insert name (KUnknown i) (tyVarKinds s) })
  pure i

addUnknownTyConKind :: TyConName -> KindInference Int
addUnknownTyConKind name = do
  i <- freshKindVariable
  modify' (\s -> s { tyConKinds = Map.insert name (KUnknown i) (tyConKinds s) })
  pure i

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
    tyConConstraint = Constraint (KUnknown tyConKindVar, foldr1 KFun $ (KUnknown <$> tyVarKindVars) ++ [KStar])

  -- Traverse constructors to collect constraints.
  (_, constructorCons) <- unzip <$> traverse inferTypeKind (mapMaybe dataConDefinitionArgument constructors)

  -- Solve constraints
  (Subst subst) <- solver (emptySubst, tyConConstraint : concat constructorCons)

  -- Substitute into tyConKindVar and return
  let kind = fromMaybe (error "Lost the input kind") $ Map.lookup tyConKindVar subst
  pure $ starIfUnknown kind

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
  retKind <- KUnknown <$> freshKindVariable
  let constraint = Constraint (k1, k2 `KFun` retKind)
  pure (KFun k1 k2, cons1 ++ cons2 ++ [constraint])
inferTypeKind (TyFun t1 t2) = do
  (k1, cons1) <- inferTypeKind t1
  (k2, cons2) <- inferTypeKind t2
  let
    constraints =
      [ Constraint (k1, KStar)
      , Constraint (k2, KStar)
      ]
  pure (KFun k1 k2, cons1 ++ cons2 ++ constraints)
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

solver :: (Subst, [Constraint]) -> KindInference Subst
solver (su, cs) =
  case cs of
    [] -> return su
    (Constraint (t1, t2): cs0) -> do
      su1 <- unify t1 t2
      solver (su1 `composeSubst` su, substituteConstraint su1 <$> cs0)

unify :: Kind -> Kind -> KindInference Subst
unify k1 k2 | k1 == k2 = pure emptySubst
unify (KUnknown i) k = i `bind` k
unify k (KUnknown i) = i `bind` k
unify (KFun k1 k2) (KFun k3 k4) = do
  su1 <- unify k1 k3
  su2 <- unify (substituteKind su1 k2) (substituteKind su1 k4)
  pure (su2 `composeSubst` su1)
unify k1 k2 = throwError $ KindUnificationFail k1 k2

bind :: Int -> Kind -> KindInference Subst
bind i k
  | k == KUnknown i = pure emptySubst
  | occursCheck i k = throwError $ InfiniteKind i k
  | otherwise = pure (singletonSubst i k)

occursCheck :: Int -> Kind -> Bool
occursCheck i k = i `Set.member` unknownKindVariables k

unknownKindVariables :: Kind -> Set Int
unknownKindVariables KStar = Set.empty
unknownKindVariables (KUnknown i) = Set.singleton i
unknownKindVariables KRow = Set.empty
unknownKindVariables (KFun k1 k2) = unknownKindVariables k1 `Set.union` unknownKindVariables k2

--
-- Substitutions
--

newtype Subst = Subst (Map Int Kind)
  deriving (Eq, Show, Semigroup, Monoid)

emptySubst :: Subst
emptySubst = Subst Map.empty

singletonSubst :: Int -> Kind -> Subst
singletonSubst a t = Subst $ Map.singleton a t

composeSubst :: Subst -> Subst -> Subst
(Subst s1) `composeSubst` (Subst s2) = Subst $ Map.map (substituteKind (Subst s1)) s2 `Map.union` s1

substituteKind :: Subst -> Kind -> Kind
substituteKind _ KStar = KStar
substituteKind (Subst subst) k@(KUnknown i) = Map.findWithDefault k i subst
substituteKind _ KRow = KRow
substituteKind subst (KFun k1 k2) = KFun (substituteKind subst k1) (substituteKind subst k2)

substituteConstraint :: Subst -> Constraint -> Constraint
substituteConstraint subst (Constraint (i, kind)) = Constraint (i, substituteKind subst kind)
