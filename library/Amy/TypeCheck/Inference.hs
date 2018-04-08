{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Amy.TypeCheck.Inference
  ( inferTopLevel
  , TyEnv
  , emptyEnv
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
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
-- Type Environment
--

-- | A 'TyEnv' is the typing environment. It contains known names with their
-- type schemes.
newtype TyEnv = TyEnv { unTyEnv :: Map ValueName (Scheme PrimitiveType) }
  deriving (Show, Eq)

emptyEnv :: TyEnv
emptyEnv = TyEnv Map.empty

extendEnv :: TyEnv -> (ValueName, Scheme PrimitiveType) -> TyEnv
extendEnv (TyEnv env) (x, s) = TyEnv (Map.insert x s env)

extendEnvList :: TyEnv -> [(ValueName, Scheme PrimitiveType)] -> TyEnv
extendEnvList = foldl' extendEnv

-- removeEnv :: TyEnv -> ValueName -> TyEnv
-- removeEnv (TyEnv env) var = TyEnv (Map.delete var env)

lookupEnv :: ValueName -> TyEnv -> Maybe (Scheme PrimitiveType)
lookupEnv key (TyEnv tys) = Map.lookup key tys

-- mergeEnv :: TyEnv -> TyEnv -> TyEnv
-- mergeEnv (TyEnv a) (TyEnv b) = TyEnv (Map.union a b)

-- mergeEnvs :: [TyEnv] -> TyEnv
-- mergeEnvs = foldl' mergeEnv emptyEnv

--
-- Inference Monad
--

-- | Holds a 'TyEnv' variables in a 'ReaderT' and a 'State' 'Int'
-- counter for producing type variables.
newtype Inference a = Inference (ReaderT TyEnv (StateT Int (Except TypeError)) a)
  deriving (Functor, Applicative, Monad, MonadReader TyEnv, MonadState Int, MonadError TypeError)

-- TODO: Don't use Except, use Validation

runInference :: TyEnv -> Inference a -> Either TypeError a
runInference env (Inference action) = runExcept $ evalStateT (runReaderT action env) 0

freshTypeVariable :: Inference TVar
freshTypeVariable = do
  modify' (+ 1)
  TVar . (letters !!) <$> get

-- TODO: Don't use letters for type variables, just use integers. Then at the
-- end of inference we can turn all the type variables into letters so the user
-- gets nice letters to see.
letters :: [Text]
letters = [1..] >>= fmap pack . flip replicateM ['a'..'z']

-- | Extends the current typing environment with a list of names and schemes
-- for those names.
extendEnvM :: [(ValueName, Scheme PrimitiveType)] -> Inference a -> Inference a
extendEnvM tys = local (flip extendEnvList tys)

-- | Lookup type in the environment
lookupEnvM :: ValueName -> Inference (Type PrimitiveType)
lookupEnvM name = do
  mTy <- lookupEnv name <$> ask
  maybe (throwError $ UnboundVariable name) instantiate mTy

-- | Convert a scheme into a type by replacing all the type variables with
-- fresh names. Types are instantiated when they are looked up so we can make
-- constraints with the type variables and not worry about name collisions.
instantiate :: Scheme PrimitiveType -> Inference (Type PrimitiveType)
instantiate (Forall as t) = do
  as' <- traverse (const freshTypeVariable) as
  let s = Subst $ Map.fromList $ zip as (TyVar <$> as')
  return $ substituteType s t

-- | Generalizing a type is the quantification of that type with all of the
-- free variables of the type minus the free variables in the environment. This
-- is like finding the type variables that should be "bound" by the
-- quantification. This is also called finding the "closure" of a type.
generalize :: TyEnv -> Type PrimitiveType -> Scheme PrimitiveType
generalize env t  = Forall as t
 where
  as = Set.toList $ freeTypeVariables t `Set.difference` freeEnvTypeVariables env

-- | Produces a type scheme from a type by finding all the free type variables
-- in the type, replacing them with sequential letters, and collecting the free
-- type variables in the Forall quantifier.
normalize :: Type PrimitiveType -> Scheme PrimitiveType
normalize body = Forall (Map.elems letterMap) (normtype body)
 where
  letterMap = Map.fromList $ zip (Set.toList $ freeTypeVariables body) (TVar <$> letters)

  normtype (TyArr a b) = TyArr (normtype a) (normtype b)
  normtype (TyCon a) = TyCon a
  normtype (TyVar a) =
    case Map.lookup a letterMap of
      Just x -> TyVar x
      Nothing -> error "type variable not in signature"

-- TODO: Move these into the main Error type
-- TODO: Include source spans in these errors
data TypeError
  = UnificationFail !(Type PrimitiveType) !(Type PrimitiveType)
  | InfiniteType TVar !(Type PrimitiveType)
  | UnboundVariable ValueName
  deriving (Show, Eq)

-- | A 'Constraint' is a statement that two types should be equal.
newtype Constraint = Constraint { unConstraint :: (Type PrimitiveType, Type PrimitiveType) }
  deriving (Show, Eq)

--
-- Inference
--

inferTopLevel :: TyEnv -> [RBinding] -> Either TypeError [(ValueName, Scheme PrimitiveType)]
inferTopLevel env bindings =
  case runInference env (inferBindings bindings) of
    Left err -> Left err
    Right results ->
      let
        schemes = (\(name, scheme, _) -> (name, scheme)) <$> results
        constraints = concatMap (\(_, _, cs) -> cs) results
      in
        case runSolve constraints of
          Left err -> Left err
          Right subst -> Right $ (\(name, Forall _ ty) -> (name, normalize $ substituteType subst ty)) <$> schemes

inferBindings :: [RBinding] -> Inference [(ValueName, Scheme PrimitiveType, [Constraint])]
inferBindings bindings = do
  bindingsInference <- traverse inferBinding bindings
  let
    constraints = concatMap snd bindingsInference
  case runSolve constraints of
    Left err -> throwError err
    Right subst -> do
      env <- ask
      pure $ flip fmap (zip bindingsInference bindings) $ \((returnType, bodyCons), binding) ->
        let scheme = generalize (substituteEnv subst env) (substituteType subst returnType)
        in
          ( locatedValue $ rBindingName binding
          , scheme
          , bodyCons
          )

inferBinding :: RBinding -> Inference (Type PrimitiveType, [Constraint])
inferBinding (RBinding _ _ args body) = do
  argsAndTyVars <- traverse (\arg -> (arg,) <$> freshTypeVariable) args
  let argsAndSchemes = (\(Located _ arg, t) -> (arg, Forall [] (TyVar t))) <$> argsAndTyVars
  (bodyTy, bodyCons) <- extendEnvM argsAndSchemes $ inferExpr body
  let
    returnType = typeFromNonEmpty $ NE.fromList $ (TyVar . snd <$> argsAndTyVars) ++ [bodyTy]
  pure
    ( returnType
    , bodyCons
    )

inferExpr :: RExpr -> Inference (Type PrimitiveType, [Constraint])
inferExpr (RELit (Located _ lit)) = pure (TyCon (literalType lit), [])
inferExpr (REVar (Located _ var)) = do
  t <- lookupEnvM var
  pure (t, [])
inferExpr (REIf (RIf pred' then' else')) = do
  (predTy, predCon) <- inferExpr pred'
  (thenTy, thenCon) <- inferExpr then'
  (elseTy, elseCon) <- inferExpr else'
  let
    newConstraints =
      [ Constraint (predTy, TyCon BoolType)
      , Constraint (thenTy, elseTy)
      ]
  pure
    ( thenTy
    , predCon ++ thenCon ++ elseCon ++ newConstraints
    )
inferExpr (RELet (RLet bindings expression)) = do
  bindingsInference <- inferBindings bindings
  let
    bindingSchemes = (\(name, scheme, _) -> (name, scheme)) <$> bindingsInference
    bindingCons = concatMap (\(_, _, cons) -> cons) bindingsInference
  (expTy, expCons) <- extendEnvM bindingSchemes $ inferExpr expression
  pure
    ( expTy
    , bindingCons ++ expCons
    )
inferExpr (REApp (RApp func args)) = do
  (funcTy, funcConstraints) <- inferExpr func
  argsInference <- traverse inferExpr args
  tyVar <- TyVar <$> freshTypeVariable
  let
    argTypes = NE.toList $ fst <$> argsInference
    argConstraints = NE.toList $ snd <$> argsInference
    newConstraint = Constraint (funcTy, typeFromNonEmpty $ NE.fromList (argTypes ++ [tyVar]))
  pure
    ( tyVar
    , funcConstraints ++ concat argConstraints ++ [newConstraint]
    )

--
-- Constraint Solver
--

-- | Constraint solver monad
type Solve a = Except TypeError a

type Unifier = (Subst, [Constraint])

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runExcept $ solver (emptySubst, cs)

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    (Constraint (t1, t2): cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 `composeSubst` su, substituteConstraint su1 <$> cs0)

unifies :: Type PrimitiveType -> Type PrimitiveType -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TyVar v) t = v `bind` t
unifies t (TyVar v) = v `bind` t
unifies (TyArr t1 t2) (TyArr t3 t4) = do
  su1 <- unifies t1 t3
  su2 <- unifies (substituteType su1 t2) (substituteType su1 t4)
  pure (su2 `composeSubst` su1)
unifies t1 t2 = throwError $ UnificationFail t1 t2

bind ::  TVar -> Type PrimitiveType -> Solve Subst
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

-- substFromList :: [(TVar, Type PrimitiveType)] -> Subst
-- substFromList = Subst . Map.fromList

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

substituteConstraint :: Subst -> Constraint -> Constraint
substituteConstraint subst (Constraint (t1, t2)) = Constraint (substituteType subst t1, substituteType subst t2)

substituteEnv :: Subst -> TyEnv -> TyEnv
substituteEnv subst (TyEnv env) = TyEnv $ Map.map (substituteScheme subst) env

--
-- Free and Active type variables
--

freeTypeVariables :: Type PrimitiveType -> Set TVar
freeTypeVariables TyCon{} = Set.empty
freeTypeVariables (TyVar var) = Set.singleton var
freeTypeVariables (t1 `TyArr` t2) = freeTypeVariables t1 `Set.union` freeTypeVariables t2

freeSchemeTypeVariables :: Scheme PrimitiveType -> Set TVar
freeSchemeTypeVariables (Forall tvs t) = freeTypeVariables t `Set.difference` Set.fromList tvs

freeEnvTypeVariables :: TyEnv -> Set TVar
freeEnvTypeVariables (TyEnv env) = foldl' Set.union Set.empty $ freeSchemeTypeVariables <$> Map.elems env

--
-- Misc
--

_l :: a -> Located a
_l = Located (SourceSpan "" 1 1 1 1)

_b1 :: RBinding
_b1 =
  RBinding
  x
  Nothing
  [y, z]
  ( REApp (RApp (REVar y) (NE.fromList $ [REVar z, RELit (_l $ LiteralBool False)]))
  )
 where
  y = _l $ ValueName "y" (NameIntId 1)
  z = _l $ ValueName "z" (NameIntId 2)
  x = _l $ ValueName "x" (NameIntId 3)

_idBind :: RBinding
_idBind =
  RBinding
  id'
  Nothing
  [x]
  (REVar x)
 where
  id' = _l $ ValueName "id" (NameIntId 1)
  x = _l $ ValueName "x" (NameIntId 2)

_constBind :: RBinding
_constBind =
  RBinding
  const'
  Nothing
  [x, y]
  (REVar x)
 where
  const' = _l $ ValueName "const" (NameIntId 1)
  x = _l $ ValueName "x" (NameIntId 2)
  y = _l $ ValueName "y" (NameIntId 3)

-- f x =
--   let
--     g y = x
--   in g 3

_b2 :: RBinding
_b2 =
  RBinding
  f
  Nothing
  [x]
  (RELet (RLet [RBinding g Nothing [y] (REVar x)] (REApp (RApp (REVar g) (NE.fromList [RELit three])))))
 where
  f = _l $ ValueName "f" (NameIntId 1)
  g = _l $ ValueName "g" (NameIntId 2)
  x = _l $ ValueName "x" (NameIntId 3)
  y = _l $ ValueName "y" (NameIntId 4)
  three = _l $ LiteralInt 3
