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
import Amy.TypeCheck.AST

--
-- Type Environment
--

-- | A 'TyEnv' is the typing environment. It contains known names with their
-- type schemes.
newtype TyEnv = TyEnv { unTyEnv :: Map TIdent TScheme }
  deriving (Show, Eq)

emptyEnv :: TyEnv
emptyEnv = TyEnv Map.empty

extendEnv :: TyEnv -> (TIdent, TScheme) -> TyEnv
extendEnv (TyEnv env) (x, s) = TyEnv (Map.insert x s env)

extendEnvList :: TyEnv -> [(TIdent, TScheme)] -> TyEnv
extendEnvList = foldl' extendEnv

-- removeEnv :: TyEnv -> Name -> TyEnv
-- removeEnv (TyEnv env) var = TyEnv (Map.delete var env)

lookupEnv :: TIdent -> TyEnv -> Maybe TScheme
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

freshTypeVariable :: Inference TTypeName
freshTypeVariable = do
  modify' (+ 1)
  id' <- get
  pure $ TTypeName (letters !! id') id' Nothing

-- TODO: Don't use letters for type variables, just use integers. Then at the
-- end of inference we can turn all the type variables into letters so the user
-- gets nice letters to see.
letters :: [Text]
letters = [1..] >>= fmap pack . flip replicateM ['a'..'z']

-- | Extends the current typing environment with a list of names and schemes
-- for those names.
extendEnvM :: [(TIdent, TScheme)] -> Inference a -> Inference a
extendEnvM tys = local (flip extendEnvList tys)

-- | Lookup type in the environment
lookupEnvM :: TIdent -> Inference TType
lookupEnvM name = do
  mTy <- lookupEnv name <$> ask
  maybe (throwError $ UnboundVariable name) instantiate mTy

-- | Convert a scheme into a type by replacing all the type variables with
-- fresh names. Types are instantiated when they are looked up so we can make
-- constraints with the type variables and not worry about name collisions.
instantiate :: TScheme -> Inference TType
instantiate (TForall as t) = do
  as' <- traverse (const freshTypeVariable) as
  let s = Subst $ Map.fromList $ zip as (TTyVar <$> as')
  return $ substituteType s t

-- | Generalizing a type is the quantification of that type with all of the
-- free variables of the type minus the free variables in the environment. This
-- is like finding the type variables that should be "bound" by the
-- quantification. This is also called finding the "closure" of a type.
generalize :: TyEnv -> TType -> TScheme
generalize env t  = TForall as t
 where
  as = Set.toList $ freeTypeVariables t `Set.difference` freeEnvTypeVariables env

-- | Produces a type scheme from a type by finding all the free type variables
-- in the type, replacing them with sequential letters, and collecting the free
-- type variables in the Forall quantifier.
normalize :: TType -> TScheme
normalize body = TForall (Map.elems letterMap) (normtype body)
 where
  -- TODO: Generated type variables should be explicitly marked as generated,
  -- not given fake IDs.
  letterMap = Map.fromList $ zip (Set.toList $ freeTypeVariables body) ((\c -> TTypeName c (-1) Nothing) <$> letters)

  normtype (TTyFun a b) = TTyFun (normtype a) (normtype b)
  normtype (TTyCon a) = TTyCon a
  normtype (TTyVar a) =
    case Map.lookup a letterMap of
      Just x -> TTyVar x
      Nothing -> error "type variable not in signature"

normalizeTBinding :: TBinding -> TBinding
normalizeTBinding binding =
  let (TForall _ t) = tBindingType binding
  in binding { tBindingType = normalize t }

-- | A 'Constraint' is a statement that two types should be equal.
newtype Constraint = Constraint { unConstraint :: (TType, TType) }
  deriving (Show, Eq)

--
-- Inference
--

inferModule :: RModule -> Either Error TModule
inferModule (RModule bindings externs) = do
  let
    externs' = (\(RExtern (Located _ name) ty) -> TExtern (convertRIdent name) (convertRType ty)) <$> externs
    externSchemes = (\(TExtern name ty) -> (name, TForall [] ty)) <$> externs'
    primFuncSchemes = primitiveFunctionScheme <$> allPrimitiveFunctionNamesAndIds
    env = TyEnv $ Map.fromList $ externSchemes ++ primFuncSchemes
  bindings' <- inferTopLevel env bindings
  pure (TModule bindings' externs')

primitiveFunctionScheme :: (Int, PrimitiveFunctionName) -> (TIdent, TScheme)
primitiveFunctionScheme (id', prim) =
  (TIdent (showPrimitiveFunctionName prim) id' (Just prim), mkPrimFunctionScheme prim)

mkPrimFunctionScheme :: PrimitiveFunctionName -> TScheme
mkPrimFunctionScheme prim = TForall [] $ foldr1 TTyFun $ primitiveTyCon <$> primitiveFunctionType (primitiveFunction prim)

primitiveTyCon :: PrimitiveType -> TType
primitiveTyCon prim = TTyCon $ TTypeName (showPrimitiveType prim) (primitiveTypeId prim) (Just prim)

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
      scheme <-
        case rBindingType binding of
          -- There is an explicit type annotation. Use it.
          Just scheme -> pure (convertRScheme scheme)
          -- No explicit type annotation
          Nothing -> TForall [] . TTyVar <$> freshTypeVariable
      pure (binding, scheme)
  bindingsAndTypes <- traverse setBindingVariable bindings

  -- Add all the binding type variables to the typing environment and then
  -- collect constraints for all bindings.
  let
    bindingNameSchemes =
      (\(binding, ty) -> (convertRIdent . locatedValue $ rBindingName binding, ty)) <$> bindingsAndTypes
  bindingsInference <- extendEnvM bindingNameSchemes $ for bindingsAndTypes $ \(binding, scheme) -> do
    (binding', constraints) <- inferBinding binding
    ty <- instantiate scheme
    let
      -- Add the constraint for the binding itself
      (TForall _ bindingType) = tBindingType binding'
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
      (TForall _ bindingType) = tBindingType binding
      scheme = generalize (substituteEnv subst env) (substituteType subst bindingType)
      binding' = binding { tBindingType = scheme }
    in
      ( binding'
      , bodyCons
      )

inferBinding :: RBinding -> Inference (TBinding, [Constraint])
inferBinding (RBinding (Located _ name) _ args body) = do
  argsAndTyVars <- traverse (\(Located _ arg) -> (convertRIdent arg,) <$> freshTypeVariable) args
  let argsAndSchemes = (\(arg, t) -> (arg, TForall [] (TTyVar t))) <$> argsAndTyVars
  (body', bodyCons) <- extendEnvM argsAndSchemes $ inferExpr body
  let
    returnType = expressionType body'
    bindingType = foldr1 TTyFun $ (TTyVar . snd <$> argsAndTyVars) ++ [returnType]
    binding' =
      TBinding
      { tBindingName = convertRIdent name
        -- Type is placeholder until we can solve all constraints and
        -- generalize to get a scheme. If we were really principled we would
        -- have some new intermediate TCBinding type or something with a Type
        -- instead of a Scheme.
      , tBindingType = TForall [] bindingType
      , tBindingArgs = (\(name', tvar) -> TTyped (TTyVar tvar) name') <$> argsAndTyVars
      , tBindingReturnType = returnType
      , tBindingBody = body'
      }
  pure (binding', bodyCons)

inferExpr :: RExpr -> Inference (TExpr, [Constraint])
inferExpr (RELit (Located _ lit)) = pure (TELit lit, [])
inferExpr (REVar (Located _ var)) = do
  let var' = convertRIdent var
  t <- lookupEnvM var'
  pure (TEVar (TTyped t var'), [])
inferExpr (REIf (RIf pred' then' else')) = do
  (pred'', predCon) <- inferExpr pred'
  (then'', thenCon) <- inferExpr then'
  (else'', elseCon) <- inferExpr else'
  let
    newConstraints =
      [ Constraint (expressionType pred'', primitiveTyCon BoolType)
      , Constraint (expressionType then'', expressionType else'')
      ]
  pure
    ( TEIf (TIf pred'' then'' else'')
    , predCon ++ thenCon ++ elseCon ++ newConstraints
    )
inferExpr (RELet (RLet bindings expression)) = do
  bindingsInference <- inferBindings bindings
  let
    bindingSchemes = (\(binding, _) -> (tBindingName binding, tBindingType binding)) <$> bindingsInference
    bindingCons = concatMap snd bindingsInference
  (expression', expCons) <- extendEnvM bindingSchemes $ inferExpr expression
  pure
    ( TELet (TLet (fst <$> bindingsInference) expression')
    , bindingCons ++ expCons
    )
inferExpr (REApp (RApp func args)) = do
  (func', funcConstraints) <- inferExpr func
  argsInference <- traverse inferExpr args
  tyVar <- TTyVar <$> freshTypeVariable
  let
    args' = fst <$> argsInference
    argTypes = NE.toList $ expressionType <$> args'
    argConstraints = NE.toList $ snd <$> argsInference
    newConstraint = Constraint (expressionType func', foldr1 TTyFun (argTypes ++ [tyVar]))
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

unifies :: TType -> TType -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TTyVar v) t = v `bind` t
unifies t (TTyVar v) = v `bind` t
unifies (TTyFun t1 t2) (TTyFun t3 t4) = do
  su1 <- unifies t1 t3
  su2 <- unifies (substituteType su1 t2) (substituteType su1 t4)
  pure (su2 `composeSubst` su1)
unifies t1 t2 = throwError $ UnificationFail t1 t2

bind ::  TTypeName -> TType -> Solve Subst
bind a t
  | t == TTyVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (singletonSubst a t)

occursCheck :: TTypeName -> TType -> Bool
occursCheck a t = a `Set.member` freeTypeVariables t

--
-- Substitutions
--

newtype Subst = Subst (Map TTypeName TType)
  deriving (Eq, Show, Semigroup, Monoid)

emptySubst :: Subst
emptySubst = Subst Map.empty

singletonSubst :: TTypeName -> TType -> Subst
singletonSubst a t = Subst $ Map.singleton a t

composeSubst :: Subst -> Subst -> Subst
(Subst s1) `composeSubst` (Subst s2) = Subst $ Map.map (substituteType (Subst s1)) s2 `Map.union` s1

substituteScheme :: Subst -> TScheme -> TScheme
substituteScheme (Subst subst) (TForall vars ty) = TForall vars $ substituteType s' ty
 where
  s' = Subst $ foldr Map.delete subst vars

substituteType :: Subst -> TType -> TType
substituteType (Subst subst) t@(TTyVar var) = Map.findWithDefault t var subst
substituteType _ (TTyCon a) = TTyCon a
substituteType s (t1 `TTyFun` t2) = substituteType s t1 `TTyFun` substituteType s t2

substituteConstraint :: Subst -> Constraint -> Constraint
substituteConstraint subst (Constraint (t1, t2)) = Constraint (substituteType subst t1, substituteType subst t2)

substituteEnv :: Subst -> TyEnv -> TyEnv
substituteEnv subst (TyEnv env) = TyEnv $ Map.map (substituteScheme subst) env

substituteTBinding :: Subst -> TBinding -> TBinding
substituteTBinding subst binding =
  binding
  { tBindingType = substituteScheme subst (tBindingType binding)
  , tBindingArgs = (\(TTyped ty arg) -> TTyped (substituteType subst ty) arg) <$> tBindingArgs binding
  , tBindingReturnType = substituteType subst (tBindingReturnType binding)
  , tBindingBody = substituteTExpr subst (tBindingBody binding)
  }

substituteTExpr :: Subst -> TExpr -> TExpr
substituteTExpr _ lit@TELit{} = lit
substituteTExpr subst (TEVar (TTyped ty name)) = TEVar (TTyped (substituteType subst ty) name)
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

freeTypeVariables :: TType -> Set TTypeName
freeTypeVariables TTyCon{} = Set.empty
freeTypeVariables (TTyVar var) = Set.singleton var
freeTypeVariables (t1 `TTyFun` t2) = freeTypeVariables t1 `Set.union` freeTypeVariables t2

freeSchemeTypeVariables :: TScheme -> Set TTypeName
freeSchemeTypeVariables (TForall tvs t) = freeTypeVariables t `Set.difference` Set.fromList tvs

freeEnvTypeVariables :: TyEnv -> Set TTypeName
freeEnvTypeVariables (TyEnv env) = foldl' Set.union Set.empty $ freeSchemeTypeVariables <$> Map.elems env

--
-- Names
--

convertRIdent :: RIdent -> TIdent
convertRIdent (RIdent name id' mPrim) = TIdent name id' mPrim

convertRScheme :: RScheme -> TScheme
convertRScheme (RForall vars ty) = TForall (convertRTypeName <$> vars) (convertRType ty)

convertRType :: RType -> TType
convertRType (RTyCon name) = TTyCon (convertRTypeName name)
convertRType (RTyVar name) = TTyVar (convertRTypeName name)
convertRType (RTyFun ty1 ty2) = TTyFun (convertRType ty1) (convertRType ty2)

convertRTypeName :: RTypeName -> TTypeName
convertRTypeName (RTypeName name' _ id' mPrim) = TTypeName name' id' mPrim
