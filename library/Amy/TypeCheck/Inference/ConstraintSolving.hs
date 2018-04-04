{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Solve collected constraints

module Amy.TypeCheck.Inference.ConstraintSolving
  ( solve
  , closeOver

  -- * Subst
  , Subst
  , substituteType
  ) where

import Control.Monad.Except
import Data.List (delete, find, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Amy.Prim
import Amy.Type
import Amy.TypeCheck.Inference.ConstraintCollection

-- | Canonicalize and return the polymorphic type.
closeOver :: Type PrimitiveType -> Scheme PrimitiveType
closeOver = normalize

generalize :: Set.Set TVar -> Type PrimitiveType -> Scheme PrimitiveType
generalize free t  = Forall as t
 where
  as = Set.toList $ freeTypeVariables t `Set.difference` free

normalize :: Type PrimitiveType -> Scheme PrimitiveType
normalize body = Forall (map snd ord) (normtype body)
 where
  ord = zip (nub $ fv body) (map TVar letters)

  fv (TyVar a) = [a]
  fv (TyArr a b) = fv a ++ fv b
  fv (TyCon _) = []

  normtype (TyArr a b) = TyArr (normtype a) (normtype b)
  normtype (TyCon a) = TyCon a
  normtype (TyVar a) =
    case Prelude.lookup a ord of
      Just x -> TyVar x
      Nothing -> error "type variable not in signature"

instantiateScheme :: Scheme PrimitiveType -> Inference (Type PrimitiveType)
instantiateScheme (Forall as t) = do
  as' <- traverse (fmap TyVar . const freshTypeVariable) as
  let s = substFromList $ zip as as'
  return $ substituteType s t

--
-- Constraint Solving
--

solve :: [Constraint] -> Inference Subst
solve [] = pure emptySubst
solve cs = solve' (nextSolvable cs)

nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs =
  fromMaybe (error "Failed in nextSolvable") $ find solvable (chooseOne xs)
 where
  chooseOne :: [Constraint] -> [(Constraint, [Constraint])]
  chooseOne xs' = (\x -> (x, delete x xs)) <$> xs'
  solvable (EqConstraint{}, _) = True
  solvable (ExplicitInstanceConstraint{}, _) = True
  solvable (ImplicitInstanceConstraint _ ms t2, cs) =
    Set.null ((freeTypeVariables t2 `Set.difference` ms) `Set.intersection` Set.unions (activeTypeVariables <$> cs))

solve' :: (Constraint, [Constraint]) -> Inference Subst
solve' (EqConstraint t1 t2, cs) = do
  su1 <- unifies t1 t2
  su2 <- solve (substituteConstraint su1 <$> cs)
  return (su2 `composeSubst` su1)
solve' (ImplicitInstanceConstraint t1 ms t2, cs) =
  solve (ExplicitInstanceConstraint t1 (generalize ms t2) : cs)
solve' (ExplicitInstanceConstraint t s, cs) = do
  s' <- instantiateScheme s
  solve (EqConstraint t s' : cs)

unifies :: Type PrimitiveType -> Type PrimitiveType -> Inference Subst
unifies t1 t2 | t1 == t2 = pure emptySubst
unifies (TyVar v) t = v `bind` t
unifies t (TyVar v) = v `bind` t
unifies (TyArr t1 t2) (TyArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: [Type PrimitiveType] -> [Type PrimitiveType] -> Inference Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (substituteType su1 <$> ts1) (substituteType su1 <$> ts2)
  return (su2 `composeSubst` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

bind ::  TVar -> Type PrimitiveType -> Inference Subst
bind a t
  | t == TyVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (singletonSubst a t)

occursCheck :: TVar -> Type PrimitiveType -> Bool
occursCheck a t = a `Set.member` freeTypeVariables t

--
-- Substitutions
--

newtype Subst = Subst (Map TVar (Type PrimitiveType))
  deriving (Eq, Show, Semigroup, Monoid)

emptySubst :: Subst
emptySubst = Subst Map.empty

singletonSubst :: TVar -> Type PrimitiveType -> Subst
singletonSubst a t = Subst $ Map.singleton a t

substFromList :: [(TVar, Type PrimitiveType)] -> Subst
substFromList = Subst . Map.fromList

composeSubst :: Subst -> Subst -> Subst
(Subst s1) `composeSubst` (Subst s2) = Subst $ Map.map (substituteType (Subst s1)) s2 `Map.union` s1

substituteScheme :: Subst -> Scheme PrimitiveType -> Scheme PrimitiveType
substituteScheme (Subst subst) (Forall vars ty) = Forall vars $ substituteType s' ty
 where
  s' = Subst $ foldr Map.delete subst vars

substituteType :: Subst -> Type PrimitiveType -> Type PrimitiveType
substituteType (Subst subst) t@(TyVar var) = Map.findWithDefault t var subst
substituteType _ (TyCon a) = TyCon a
substituteType s (t1 `TyArr` t2) = substituteType s t1 `TyArr` substituteType s t2

substituteTVar :: Subst -> TVar -> TVar
substituteTVar (Subst subst) var =
  let
    (TyVar tv) = Map.findWithDefault (TyVar var) var subst
  in tv

substituteConstraint :: Subst -> Constraint -> Constraint
substituteConstraint s (EqConstraint t1 t2) =
  EqConstraint (substituteType s t1) (substituteType s t2)
substituteConstraint s (ExplicitInstanceConstraint t sc) =
  ExplicitInstanceConstraint (substituteType s t) (substituteScheme s sc)
substituteConstraint s (ImplicitInstanceConstraint t1 ms t2) =
  ImplicitInstanceConstraint (substituteType s t1) (Set.map (substituteTVar s) ms) (substituteType s t2)

--
-- Free and Active type variables
--

freeTypeVariables :: Type PrimitiveType -> Set TVar
freeTypeVariables TyCon{} = Set.empty
freeTypeVariables (TyVar var) = Set.singleton var
freeTypeVariables (t1 `TyArr` t2) = freeTypeVariables t1 `Set.union` freeTypeVariables t2

freeSchemeTypeVariables :: Scheme PrimitiveType -> Set TVar
freeSchemeTypeVariables (Forall tvs t) = freeTypeVariables t `Set.difference` Set.fromList tvs

activeTypeVariables :: Constraint -> Set TVar
activeTypeVariables (EqConstraint t1 t2) = freeTypeVariables t1 `Set.union` freeTypeVariables t2
activeTypeVariables (ImplicitInstanceConstraint t1 ms t2) =
  freeTypeVariables t1 `Set.union` (freeTypeVariables t2 `Set.intersection` ms)
activeTypeVariables (ExplicitInstanceConstraint t s) = freeTypeVariables t `Set.union` freeSchemeTypeVariables s
