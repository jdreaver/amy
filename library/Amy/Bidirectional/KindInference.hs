{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Bidirectional.KindInference
  ( inferTypeDeclarationKind
  , checkTypeKind
  , inferTypeKind
  ) where

import Control.Monad.Except
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)

import Amy.Bidirectional.AST
import Amy.Bidirectional.Monad
import Amy.Errors
import Amy.Kind

--
-- Kind Inference
--

-- TODO: Infer the kinds of mutually recursive type declaration groups at the
-- same time instead of one by one.

inferTypeDeclarationKind :: TypeDeclaration -> Checker Kind
inferTypeDeclarationKind (TypeDeclaration (TyConDefinition tyCon tyArgs) constructors) =
  withNewLexicalScope $ do
    -- Generate unknown kind variables for the type constructor and all type
    -- variables.
    tyConKindVar <- addUnknownTyConKindToScope tyCon
    tyVarKindVars <- traverse addUnknownTyVarKindToScope tyArgs
    let
      tyConConstraint = Constraint (KUnknown tyConKindVar, foldr1 KFun $ (KUnknown <$> tyVarKindVars) ++ [KStar])

    -- Traverse constructors to collect constraints.
    (_, constructorCons) <- unzip <$> traverse inferTypeKind' (mapMaybe dataConDefinitionArgument constructors)

    -- Solve constraints
    (Subst subst) <- solver (emptySubst, tyConConstraint : concat constructorCons)

    -- Substitute into tyConKindVar and return
    let kind = fromMaybe (error "Lost the input kind") $ Map.lookup tyConKindVar subst
    pure $ starIfUnknown kind

checkTypeKind :: Type -> Checker ()
checkTypeKind ty = do
  kind <- inferTypeKind ty
  when (kind /= KStar) $
    error $ "Somehow kind unification passed but we don't have KStar " ++ show (ty, kind)

inferTypeKind :: Type -> Checker Kind
inferTypeKind ty = do
  (kind, cons) <- inferTypeKind' ty
  subst <- solver (emptySubst, cons)
  pure $ starIfUnknown $ substituteKind subst kind

inferTypeKind' :: Type -> Checker (Kind, [Constraint])
inferTypeKind' (TyCon name) = do
  kind <- lookupTyConKind name
  pure (kind, [])
inferTypeKind' (TyVar name) = do
  kind <- lookupTyVarKind name
  pure (kind, [])
inferTypeKind' (TyApp t1 t2) = do
  (k1, cons1) <- inferTypeKind' t1
  (k2, cons2) <- inferTypeKind' t2
  retKind <- KUnknown <$> freshId
  let constraint = Constraint (k1, k2 `KFun` retKind)
  pure (retKind, cons1 ++ cons2 ++ [constraint])
inferTypeKind' (TyFun t1 t2) = do
  (k1, cons1) <- inferTypeKind' t1
  (k2, cons2) <- inferTypeKind' t2
  kind <- KUnknown <$> freshId
  let
    constraints =
      [ Constraint (k1, KStar)
      , Constraint (k2, KStar)
      , Constraint (kind, KStar)
      ]
  pure (kind, cons1 ++ cons2 ++ constraints)
inferTypeKind' (TyRecord fields mTail) = do
  fieldCons <- for (Map.elems fields) $ \ty -> do
    (fieldKind, fieldCons) <- inferTypeKind' ty
    pure $ fieldCons ++ [Constraint (fieldKind, KStar)]
  varCons <- for (maybeToList mTail) $ \tail' -> do
    (kind, cons) <- inferTypeKind' tail'
    pure $ cons ++ [Constraint (kind, KRow)]
  kind <- KUnknown <$> freshId
  pure (kind, concat fieldCons ++ concat varCons ++ [Constraint (kind, KStar)])
inferTypeKind' (TyForall vars ty) = withNewLexicalScope $ do
  traverse_ addUnknownTyVarKindToScope vars
  inferTypeKind' ty
inferTypeKind' v@(TyExistVar _) = error $ "Found existential variable in kind inference " ++ show v

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

solver :: (Subst, [Constraint]) -> Checker Subst
solver (su, cs) =
  case cs of
    [] -> return su
    (Constraint (t1, t2): cs0) -> do
      su1 <- unifyKinds t1 t2
      solver (su1 `composeSubst` su, substituteConstraint su1 <$> cs0)

unifyKinds :: Kind -> Kind -> Checker Subst
unifyKinds k1 k2 | k1 == k2 = pure emptySubst
unifyKinds (KUnknown i) k = i `bind` k
unifyKinds k (KUnknown i) = i `bind` k
unifyKinds (KFun k1 k2) (KFun k3 k4) = do
  su1 <- unifyKinds k1 k3
  su2 <- unifyKinds (substituteKind su1 k2) (substituteKind su1 k4)
  pure (su2 `composeSubst` su1)
unifyKinds k1 k2 = throwError $ KindUnificationFail k1 k2

bind :: Int -> Kind -> Checker Subst
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
substituteConstraint subst (Constraint (k1, k2)) = Constraint (substituteKind subst k1, substituteKind subst k2)
