{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.TypeCheck.Inference
  ( runInference
  , inferExpr'
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (foldl')
import Data.List (delete, find, nub)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)

import Amy.Literal
import Amy.Names
import Amy.Prim
import Amy.Renamer.AST
import Amy.Syntax.Located
import Amy.Type

--
-- Assumptions
--

-- | An Assumption is an assignment of a type variable to a free variable in an
-- expression. An Assumption is created during the inference of a Var in an
-- expression, and assumptions are generally bubbled up the syntax tree during
-- bottom-up constraint collection. There can be more than on assumption for a
-- given variable.
newtype Assumptions = Assumptions { unAssumptions :: [(ValueName, Type PrimitiveType)] }
  deriving (Show, Eq)

emptyAssumptions :: Assumptions
emptyAssumptions = Assumptions []

-- | Add an assumption to the assumption set. This is done when encountering a
-- variable.
extendAssumptions :: Assumptions -> (ValueName, Type PrimitiveType) -> Assumptions
extendAssumptions (Assumptions xs) (x, s) = Assumptions ((x, s) : xs)

-- | Remove an assumption from the assumption set. This is done when we
-- encounter a binding for a variable, like a lambda or let expression
-- variable. The assumption gets "converted" into a constraint.
removeAssumption :: Assumptions -> ValueName -> Assumptions
removeAssumption (Assumptions xs) name = Assumptions (filter (\(n, _) -> n /= name) xs)

lookupAssumption :: ValueName -> Assumptions -> [Type PrimitiveType]
lookupAssumption name (Assumptions xs) = map snd (filter (\(n, _) -> n == name) xs)

concatAssumptions :: [Assumptions] -> Assumptions
concatAssumptions = foldl' mergeAssumptions emptyAssumptions

mergeAssumptions :: Assumptions -> Assumptions -> Assumptions
mergeAssumptions (Assumptions a) (Assumptions b) = Assumptions (a ++ b)

singletonAssumption :: ValueName -> Type PrimitiveType -> Assumptions
singletonAssumption x y = Assumptions [(x, y)]

assumptionKeys :: Assumptions -> [ValueName]
assumptionKeys (Assumptions xs) = map fst xs

--
-- Constraints
--

-- | A 'Constraint' places a restriction on what type is assigned to a
-- variable. Constraints are collected and then solved after collection.
data Constraint
  = EqConstraint !(Type PrimitiveType) !(Type PrimitiveType)
    -- ^ Indicates types should be unified
  | ExplicitInstanceConstraint !(Type PrimitiveType) !(Scheme PrimitiveType)
  | ImplicitInstanceConstraint !(Type PrimitiveType) !(Set TVar) !(Type PrimitiveType)
  deriving (Show, Eq)

--
-- Inference Monad
--

-- | Holds a set of monomorphic variables in a 'ReaderT' and a 'State' 'Int'
-- counter for producing type variables.
newtype Inference a = Inference (ReaderT (Set TVar) (StateT Int (Except TypeError)) a)
  deriving (Functor, Applicative, Monad, MonadReader (Set TVar), MonadState Int, MonadError TypeError)

-- TODO: Separate monads for constraint collection and unification.

-- | Generate a fresh type variable
freshTypeVariable :: Inference (Type PrimitiveType)
freshTypeVariable = do
  modify' (+ 1)
  TyVar . TVar . pack . (letters !!) <$> get

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

instantiateScheme :: Scheme PrimitiveType -> Inference (Type PrimitiveType)
instantiateScheme (Forall as t) = do
  as' <- mapM (const freshTypeVariable) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ substituteType s t

-- | Add a monomorphic variable to the monomorphic set and run a computation
extendMonomorphicSet :: TVar -> Inference a -> Inference a
extendMonomorphicSet x = local (Set.insert x)

generalize :: Set.Set TVar -> Type PrimitiveType -> Scheme PrimitiveType
generalize free t  = Forall as t
 where
  as = Set.toList $ freeTypeVariables t `Set.difference` free

-- TODO: Don't use Except, use Validation

-- TODO: Move these into the main Error type
data TypeError
  = UnificationFail !(Type PrimitiveType) !(Type PrimitiveType)
  | InfiniteType TVar !(Type PrimitiveType)
  | UnboundVariable ValueName
  | Ambigious [Constraint]
  | UnificationMismatch [Type PrimitiveType] [Type PrimitiveType]
  deriving (Show, Eq)

runInference :: Inference a -> Either TypeError a
runInference (Inference action) = runExcept $ evalStateT (runReaderT action Set.empty) 0

--
-- Substitutions
--

newtype Subst = Subst (Map TVar (Type PrimitiveType))
  deriving (Eq, Show, Semigroup, Monoid)

emptySubst :: Subst
emptySubst = Subst Map.empty

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

substituteConstraint s (EqConstraint t1 t2) =
  EqConstraint (substituteType s t1) (substituteType s t2)
substituteConstraint s (ExplicitInstanceConstraint t sc) =
  ExplicitInstanceConstraint (substituteType s t) (substituteScheme s sc)
substituteConstraint s (ImplicitInstanceConstraint t1 ms t2) =
  ImplicitInstanceConstraint (substituteType s t1) (Set.map (substituteTVar s) ms) (substituteType s t2)

--
-- Inference Functions
--

inferType :: RExpr -> Inference (Subst, Type PrimitiveType)
inferType ex = do
  (as, cs, t) <- inferExpr' ex
  let unbounds = Set.fromList (assumptionKeys as) `Set.difference` Set.empty --Set.fromList (Env.keys env)
  unless (Set.null unbounds) $ throwError $ UnboundVariable (Set.findMin unbounds)
  --let cs' = [ExpInstConst t s | (x, s) <- Env.toList env, t <- As.lookup x as]
  subst <- solve cs --(cs ++ cs')
  return (subst, substituteType subst t)

-- TODO: Multiple arguments
inferBinding :: RBinding -> Inference (Assumptions, [Constraint], Type PrimitiveType)
inferBinding (RBinding _ _ args body) = do
  (asBody, consBody, tyBody) <- inferExpr' body
  tyVar <- freshTypeVariable
  let
    argNames = locatedValue <$> args
    argConstraints =
      concatMap (\name' -> (\t -> EqConstraint t tyVar) <$> lookupAssumption name' asBody) argNames
  pure
    ( foldl' removeAssumption asBody argNames
    , consBody ++ argConstraints -- [EqConst t' tv | t' <- As.lookup x as]
    , tyVar `TyArr` tyBody
    )

-- | Collect constraints for an expression.
inferExpr' :: RExpr -> Inference (Assumptions, [Constraint], Type PrimitiveType)
inferExpr' (RELit (Located _ (LiteralInt _))) = pure (emptyAssumptions, [], TyCon IntType)
inferExpr' (RELit (Located _ (LiteralDouble _))) = pure (emptyAssumptions, [], TyCon DoubleType)
inferExpr' (RELit (Located _ (LiteralBool _))) = pure (emptyAssumptions, [], TyCon BoolType)
inferExpr' (REVar (Located _ name)) = do
  -- For a Var, generate a fresh type variable and add it to the assumption set
  tyVar <- freshTypeVariable
  pure (singletonAssumption name tyVar, [], tyVar)
inferExpr' (REIf (RIf pred' then' else')) = do
  -- If statements are simple. Merge all the assumptions/constraints from each
  -- sub expression. Then add a constraint saying the predicate must be a Bool,
  -- and that the then and else branches have equal types.
  (asPred, consPred, tyPred) <- inferExpr' pred'
  (asThen, consThen, tyThen) <- inferExpr' then'
  (asElse, consElse, tyElse) <- inferExpr' else'
  pure
    ( asPred `mergeAssumptions` asThen `mergeAssumptions` asElse
    , consPred ++ consThen ++ consElse ++ [EqConstraint tyPred (TyCon BoolType), EqConstraint tyThen tyElse]
    , tyThen
    )
-- TODO: Multiple arguments and multiple bindings
inferExpr' (RELet (RLet [RBinding (Located _ name) _ [] body] expression)) = do
  (asBody, consBody, tyBody) <- inferExpr' body
  (asExpression, consExpression, tyExpression) <- inferExpr' expression
  monomorphicSet <- ask
  let
    newConstraints =
      (\t -> ImplicitInstanceConstraint t monomorphicSet tyBody)
      <$> lookupAssumption name asExpression
  pure
    ( asBody `mergeAssumptions` asExpression `removeAssumption` name
    , consBody ++ consExpression ++ newConstraints
    , tyExpression
    )
-- TODO: Multiple arguments
inferExpr' (REApp (RApp func (arg :| []))) = do
  -- For an App, we first collect constraints for the function and the
  -- arguments. Then, we instantiate a fresh type variable. The assumption sets
  -- and constraint sets are merged, and the additional constraint that the
  -- function has a type of @tyArg -> tyVar@ is added.
  (asFunc, consFunc, tyFunc) <- inferExpr' func
  (asArg, consArg, tyArg) <- inferExpr' arg
  tyVar <- freshTypeVariable
  pure
    ( asFunc `mergeAssumptions` asArg
    , consFunc ++ consArg ++ [EqConstraint tyFunc (tyArg `TyArr` tyVar)]
    , tyVar
    )

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

--
-- Constraint Solving
--

solve :: [Constraint] -> Inference Subst
solve [] = pure emptySubst
solve cs = solve' (nextSolvable cs)

nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs =
  case find solvable (chooseOne xs) of
    Just cs -> cs
    Nothing -> error "Failed in nextSolvable"
 where
  chooseOne :: [Constraint] -> [(Constraint, [Constraint])]
  chooseOne xs' = [(x, ys) | x <- xs', let ys = delete x xs]
  solvable (EqConstraint{}, _) = True
  solvable (ExplicitInstanceConstraint{}, _) = True
  solvable (ImplicitInstanceConstraint t1 ms t2, cs) =
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
  | otherwise = return (Subst $ Map.singleton a t)

occursCheck :: TVar -> Type PrimitiveType -> Bool
occursCheck a t = a `Set.member` freeTypeVariables t

--
-- Misc
--

l = Located (SourceSpan "" 1 1 1 1)

e1 :: RExpr
e1 =
  RELet $
  RLet
  [RBinding x Nothing [] (RELit (l $ LiteralBool True))]
  (REApp (RApp (REVar x) (RELit (l $ LiteralInt 1) :| [])))
 where
  x = l $ ValueName "x" (NameIntId 1)
