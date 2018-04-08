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

import Amy.Errors
import Amy.Literal
import Amy.Names
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
    externs' = (\(RExtern (Located _ name) ty) -> TExtern name (locatedValue <$> ty)) <$> externs
    externSchemes = (\(TExtern name ty) -> (name, Forall [] ty)) <$> externs'
    mkPrimFuncType prim = Forall [] $ typeFromNonEmpty . fmap TyCon . primitiveFunctionType $ primitiveFunction prim
    primFuncSchemes =
      (\prim -> (ValueName (showPrimitiveFunctionName prim) (PrimitiveFunctionId prim), mkPrimFuncType prim))
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

inferBindings :: [RBinding] -> Inference [(TBinding, [Constraint])]
inferBindings bindings = do
  bindingsAndTyVars <- traverse (\binding -> (binding,) <$> freshTypeVariable) bindings
  let bindingNameSchemes = (\(binding, tvar) -> (locatedValue $ rBindingName binding, Forall [] (TyVar tvar))) <$> bindingsAndTyVars
  bindingsInference <- extendEnvM bindingNameSchemes $ traverse (uncurry inferBinding) bindingsAndTyVars
  let
    constraints = concatMap snd bindingsInference
  case runSolve constraints of
    Left err -> throwError err
    Right subst -> do
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

-- TODO: Explicit types
inferBinding :: RBinding -> TVar -> Inference (TBinding, [Constraint])
inferBinding (RBinding (Located _ name) _ args body) bindingTVar = do
  argsAndTyVars <- traverse (\(Located _ arg) -> (arg,) <$> freshTypeVariable) args
  let argsAndSchemes = (\(arg, t) -> (arg, Forall [] (TyVar t))) <$> argsAndTyVars
  (body', bodyCons) <- extendEnvM argsAndSchemes $ inferExpr body
  let
    returnType = expressionType body'
    bindingType = typeFromNonEmpty $ NE.fromList $ (TyVar . snd <$> argsAndTyVars) ++ [returnType]
    binding' =
      TBinding
      { tBindingName = name
      , tBindingType = Forall [] bindingType
      , tBindingArgs = (\(name', tvar) -> Typed (TyVar tvar) name') <$> argsAndTyVars
      , tBindingReturnType = returnType
      , tBindingBody = body'
      }
    bindingCons = Constraint (TyVar bindingTVar, bindingType)
  pure (binding', bodyCons ++ [bindingCons])

inferExpr :: RExpr -> Inference (TExpr, [Constraint])
inferExpr (RELit (Located _ lit)) = pure (TELit lit, [])
inferExpr (REVar (Located _ var)) = do
  t <- lookupEnvM var
  pure (TEVar (Typed t var), [])
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
