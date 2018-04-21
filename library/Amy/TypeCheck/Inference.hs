{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Amy.TypeCheck.Inference
  ( inferModule
  , inferTopLevel
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
import Data.Traversable (for)

import Amy.Errors
import Amy.Prim
import Amy.Renamer.AST
import Amy.Syntax.Located
import Amy.Type
import Amy.TypeCheck.AST

--
-- Type Environment
--

-- | A 'TyEnv' is the typing environment. It contains known names with their
-- type schemes.
newtype TyEnv = TyEnv { unTyEnv :: Map TName (Scheme PrimitiveType) }
  deriving (Show, Eq)

emptyEnv :: TyEnv
emptyEnv = TyEnv Map.empty

extendEnv :: TyEnv -> (TName, Scheme PrimitiveType) -> TyEnv
extendEnv (TyEnv env) (x, s) = TyEnv (Map.insert x s env)

extendEnvList :: TyEnv -> [(TName, Scheme PrimitiveType)] -> TyEnv
extendEnvList = foldl' extendEnv

-- removeEnv :: TyEnv -> Name -> TyEnv
-- removeEnv (TyEnv env) var = TyEnv (Map.delete var env)

lookupEnv :: TName -> TyEnv -> Maybe (Scheme PrimitiveType)
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
newtype Inference a = Inference (ReaderT TyEnv (StateT Int (Except Error)) a)
  deriving (Functor, Applicative, Monad, MonadReader TyEnv, MonadState Int, MonadError Error)

-- TODO: Don't use Except, use Validation

runInference :: TyEnv -> Inference a -> Either Error a
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
extendEnvM :: [(TName, Scheme PrimitiveType)] -> Inference a -> Inference a
extendEnvM tys = local (flip extendEnvList tys)

-- | Lookup type in the environment
lookupEnvM :: TName -> Inference (Type PrimitiveType)
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

normalizeTBinding :: TBinding -> TBinding
normalizeTBinding binding =
  let (Forall _ t) = tBindingType binding
  in binding { tBindingType = normalize t }

-- | A 'Constraint' is a statement that two types should be equal.
newtype Constraint = Constraint { unConstraint :: (Type PrimitiveType, Type PrimitiveType) }
  deriving (Show, Eq)

--
-- Inference
--

inferModule :: RModule -> Either Error TModule
inferModule (RModule bindings externs) = do
  let
    externs' = (\(RExtern (Located _ name) ty) -> TExtern (convertRName name) (locatedValue <$> ty)) <$> externs
    externSchemes = (\(TExtern name ty) -> (name, Forall [] ty)) <$> externs'
    mkPrimFuncType prim = Forall [] $ typeFromNonEmpty . fmap TyCon . primitiveFunctionType $ primitiveFunction prim
    primFuncSchemes =
      (\prim -> (TPrimitiveName prim, mkPrimFuncType prim))
      <$> allPrimitiveFunctionNames
    env = TyEnv $ Map.fromList $ externSchemes ++ primFuncSchemes
  bindings' <- inferTopLevel env bindings
  pure (TModule bindings' externs')

inferTopLevel :: TyEnv -> [RBinding] -> Either Error [TBinding]
inferTopLevel env bindings =
  case runInference env (inferBindings bindings) of
    Left err -> Left err
    Right results ->
      let
        bindings' = fst <$> results
        constraints = concatMap snd results
      in
        case runSolve constraints of
          Left err -> Left err
          Right subst -> Right $ normalizeTBinding . substituteTBinding subst <$> bindings'

-- | Infer a group of bindings.
--
-- Binding groups can depend on one another, and even be mutually recursive. We
-- cannot simply just solve them in order. We first have to add all of their
-- types to the environment (either their declared type or a fresh type
-- variable), then we can gather constraints for each binding one by one. We
-- then collect all the constraints and solve them together.
inferBindings :: [RBinding] -> Inference [(TBinding, [Constraint])]
inferBindings bindings = do
  let
    setBindingVariable binding = do
      ty <-
        case rBindingType binding of
          -- There is an explicit type annotation. Use it.
          Just ty -> pure (locatedValue <$> ty)
          -- No explicit type annotation
          Nothing -> Forall [] . TyVar <$> freshTypeVariable
      pure (binding, ty)
  bindingsAndTypes <- traverse setBindingVariable bindings

  -- Add all the binding type variables to the typing environment and then
  -- collect constraints for all bindings.
  let
    bindingNameSchemes =
      (\(binding, ty) -> (TIdentName . convertRIdent . locatedValue $ rBindingName binding, ty)) <$> bindingsAndTypes
  bindingsInference <- extendEnvM bindingNameSchemes $ for bindingsAndTypes $ \(binding, scheme) -> do
    (binding', constraints) <- inferBinding binding
    ty <- instantiate scheme
    let
      -- Add the constraint for the binding itself
      (Forall _ bindingType) = tBindingType binding'
      bindingConstraint = Constraint (ty, bindingType)
    pure (binding', constraints ++ [bindingConstraint])

  -- Solve all constraints together.
  let
    constraints = concatMap snd bindingsInference
  subst <- either throwError pure $ runSolve constraints

  -- Use the produced Substitution and substitute all the type variables we
  -- generated for each binding.
  env <- ask
  pure $ flip fmap bindingsInference $ \(binding, bodyCons) ->
    let
      (Forall _ bindingType) = tBindingType binding
      scheme = generalize (substituteEnv subst env) (substituteType subst bindingType)
      binding' = binding { tBindingType = scheme }
    in
      ( binding'
      , bodyCons
      )

inferBinding :: RBinding -> Inference (TBinding, [Constraint])
inferBinding (RBinding (Located _ name) _ args body) = do
  argsAndTyVars <- traverse (\(Located _ arg) -> (convertRName arg,) <$> freshTypeVariable) args
  let argsAndSchemes = (\(arg, t) -> (arg, Forall [] (TyVar t))) <$> argsAndTyVars
  (body', bodyCons) <- extendEnvM argsAndSchemes $ inferExpr body
  let
    returnType = expressionType body'
    bindingType = typeFromNonEmpty $ NE.fromList $ (TyVar . snd <$> argsAndTyVars) ++ [returnType]
    binding' =
      TBinding
      { tBindingName = convertRIdent name
        -- Type is placeholder until we can solve all constraints and
        -- generalize to get a scheme. If we were really principled we would
        -- have some new intermediate TCBinding type or something with a Type
        -- instead of a Scheme.
      , tBindingType = Forall [] bindingType
      , tBindingArgs = (\(name', tvar) -> Typed (TyVar tvar) name') <$> argsAndTyVars
      , tBindingReturnType = returnType
      , tBindingBody = body'
      }
  pure (binding', bodyCons)

inferExpr :: RExpr -> Inference (TExpr, [Constraint])
inferExpr (RELit (Located _ lit)) = pure (TELit lit, [])
inferExpr (REVar (Located _ var)) = do
  let var' = convertRName var
  t <- lookupEnvM var'
  pure (TEVar (Typed t var'), [])
inferExpr (REIf (RIf pred' then' else')) = do
  (pred'', predCon) <- inferExpr pred'
  (then'', thenCon) <- inferExpr then'
  (else'', elseCon) <- inferExpr else'
  let
    newConstraints =
      [ Constraint (expressionType pred'', TyCon BoolType)
      , Constraint (expressionType then'', expressionType else'')
      ]
  pure
    ( TEIf (TIf pred'' then'' else'')
    , predCon ++ thenCon ++ elseCon ++ newConstraints
    )
inferExpr (RELet (RLet bindings expression)) = do
  bindingsInference <- inferBindings bindings
  let
    bindingSchemes = (\(binding, _) -> (TIdentName $ tBindingName binding, tBindingType binding)) <$> bindingsInference
    bindingCons = concatMap snd bindingsInference
  (expression', expCons) <- extendEnvM bindingSchemes $ inferExpr expression
  pure
    ( TELet (TLet (fst <$> bindingsInference) expression')
    , bindingCons ++ expCons
    )
inferExpr (REApp (RApp func args)) = do
  (func', funcConstraints) <- inferExpr func
  argsInference <- traverse inferExpr args
  tyVar <- TyVar <$> freshTypeVariable
  let
    args' = fst <$> argsInference
    argTypes = NE.toList $ expressionType <$> args'
    argConstraints = NE.toList $ snd <$> argsInference
    newConstraint = Constraint (expressionType func', typeFromNonEmpty $ NE.fromList (argTypes ++ [tyVar]))
  pure
    ( TEApp (TApp func' args' tyVar)
    , funcConstraints ++ concat argConstraints ++ [newConstraint]
    )
inferExpr (REParens expr) = do
  (expr', constraints) <- inferExpr expr
  pure (TEParens expr', constraints)

--
-- Constraint Solver
--

-- | Constraint solver monad
type Solve a = Except Error a

type Unifier = (Subst, [Constraint])

runSolve :: [Constraint] -> Either Error Subst
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

substituteTBinding :: Subst -> TBinding -> TBinding
substituteTBinding subst binding =
  binding
  { tBindingType = substituteScheme subst (tBindingType binding)
  , tBindingArgs = (\(Typed ty arg) -> Typed (substituteType subst ty) arg) <$> tBindingArgs binding
  , tBindingReturnType = substituteType subst (tBindingReturnType binding)
  , tBindingBody = substituteTExpr subst (tBindingBody binding)
  }

substituteTExpr :: Subst -> TExpr -> TExpr
substituteTExpr _ lit@TELit{} = lit
substituteTExpr subst (TEVar (Typed ty name)) = TEVar (Typed (substituteType subst ty) name)
substituteTExpr subst (TEIf (TIf pred' then' else')) =
  TEIf (TIf (substituteTExpr subst pred') (substituteTExpr subst then') (substituteTExpr subst else'))
substituteTExpr subst (TELet (TLet bindings expr)) =
  TELet (TLet (substituteTBinding subst <$> bindings) (substituteTExpr subst expr))
substituteTExpr subst (TEApp (TApp func args returnType)) =
  TEApp (TApp (substituteTExpr subst func) (substituteTExpr subst <$> args) (substituteType subst returnType))
substituteTExpr subst (TEParens expr) = TEParens (substituteTExpr subst expr)

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
-- Names
--

convertRName :: RName -> TName
convertRName (RPrimitiveName prim) = TPrimitiveName prim
convertRName (RIdentName ident) = TIdentName (convertRIdent ident)

convertRIdent :: RIdent -> TIdent
convertRIdent (RIdent name id' isTopLevel) = TIdent name id' isTopLevel
