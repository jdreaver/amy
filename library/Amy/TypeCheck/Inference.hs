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
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Traversable (for)

import Amy.Errors
import Amy.Prim
import Amy.Renamer.AST as R
import Amy.Syntax.Located
import Amy.TypeCheck.AST as T

--
-- Type Environment
--

-- | A 'TyEnv' is the typing environment. It contains known names with their
-- type schemes.
newtype TyEnv = TyEnv { unTyEnv :: Map T.Ident T.Scheme }
  deriving (Show, Eq)

emptyEnv :: TyEnv
emptyEnv = TyEnv Map.empty

extendEnv :: TyEnv -> (T.Ident, T.Scheme) -> TyEnv
extendEnv (TyEnv env) (x, s) = TyEnv (Map.insert x s env)

extendEnvList :: TyEnv -> [(T.Ident, T.Scheme)] -> TyEnv
extendEnvList = foldl' extendEnv

-- removeEnv :: TyEnv -> Name -> TyEnv
-- removeEnv (TyEnv env) var = TyEnv (Map.delete var env)

lookupEnv :: T.Ident -> TyEnv -> Maybe T.Scheme
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

freshTypeVariable :: Inference T.Type
freshTypeVariable = do
  modify' (+ 1)
  id' <- get
  pure $ T.TyVar (T.TypeName (letters !! id') id' Nothing) TyVarGenerated

-- TODO: Don't use letters for type variables, just use integers. Then at the
-- end of inference we can turn all the type variables into letters so the user
-- gets nice letters to see.
letters :: [Text]
letters = [1..] >>= fmap pack . flip replicateM ['a'..'z']

-- | Extends the current typing environment with a list of names and schemes
-- for those names.
extendEnvM :: [(T.Ident, T.Scheme)] -> Inference a -> Inference a
extendEnvM tys = local (flip extendEnvList tys)

-- | Lookup type in the environment
lookupEnvM :: T.Ident -> Inference T.Type
lookupEnvM name = do
  mTy <- asks (lookupEnv name)
  maybe (throwError $ UnboundVariable name) instantiate mTy

-- | Convert a scheme into a type by replacing all the type variables with
-- fresh names. Types are instantiated when they are looked up so we can make
-- constraints with the type variables and not worry about name collisions.
instantiate :: T.Scheme -> Inference T.Type
instantiate (T.Forall as t) = do
  as' <- traverse (const freshTypeVariable) as
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

-- | Produces a type scheme from a type by finding all the free type variables
-- in the type, replacing them with sequential letters, and collecting the free
-- type variables in the Forall quantifier.
normalize :: T.Type -> T.Scheme
normalize body = T.Forall (Map.elems letterMap) (normtype body)
 where
  -- TODO: Generated type variables should be explicitly marked as generated,
  -- not given fake IDs.
  letterMap = Map.fromList $ zip (Set.toList $ freeTypeVariables body) ((\c -> T.TypeName c (-1) Nothing) <$> letters)

  normtype (T.TyFun a b) = T.TyFun (normtype a) (normtype b)
  normtype (T.TyCon a) = T.TyCon a
  normtype (T.TyVar a _) =
    case Map.lookup a letterMap of
      Just x -> T.TyVar x TyVarNotGenerated
      Nothing -> error "type variable not in signature"

normalizeTBinding :: T.Binding -> T.Binding
normalizeTBinding binding =
  let (T.Forall _ t) = T.bindingType binding
  in binding { T.bindingType = normalize t }

-- | A 'Constraint' is a statement that two types should be equal.
newtype Constraint = Constraint { unConstraint :: (T.Type, T.Type) }
  deriving (Show, Eq)

--
-- Inference
--

inferModule :: R.Module -> Either Error T.Module
inferModule (R.Module bindings externs _) = do
  let
    externs' = (\(R.Extern (Located _ name) ty) -> T.Extern (convertIdent name) (convertType ty)) <$> externs
    externSchemes = (\(T.Extern name ty) -> (name, T.Forall [] ty)) <$> externs'
    primFuncSchemes = primitiveFunctionScheme <$> allPrimitiveFunctionNamesAndIds
    env = TyEnv $ Map.fromList $ externSchemes ++ primFuncSchemes
  bindings' <- inferTopLevel env bindings
  pure (T.Module bindings' externs')

primitiveFunctionScheme :: (Int, PrimitiveFunctionName) -> (T.Ident, T.Scheme)
primitiveFunctionScheme (id', prim) =
  (T.Ident (showPrimitiveFunctionName prim) id' (Just prim), mkPrimFunctionScheme prim)

mkPrimFunctionScheme :: PrimitiveFunctionName -> T.Scheme
mkPrimFunctionScheme prim = T.Forall [] $ foldr1 T.TyFun $ primitiveTyCon <$> primitiveFunctionType (primitiveFunction prim)

primitiveTyCon :: PrimitiveType -> T.Type
primitiveTyCon prim = T.TyCon $ T.TypeName (showPrimitiveType prim) (primitiveTypeId prim) (Just prim)

inferTopLevel :: TyEnv -> [R.Binding] -> Either Error [T.Binding]
inferTopLevel env bindings = do
  (bindings', constraints) <- unzip <$> runInference env (inferBindings bindings)
  subst <- runSolve (concat constraints)
  pure $ normalizeTBinding . substituteTBinding subst <$> bindings'

-- | Infer a group of bindings.
--
-- Binding groups can depend on one another, and even be mutually recursive. We
-- cannot simply just solve them in order. We first have to add all of their
-- types to the environment (either their declared type or a fresh type
-- variable), then we can gather constraints for each binding one by one. We
-- then collect all the constraints and solve them together.
inferBindings :: [R.Binding] -> Inference [(T.Binding, [Constraint])]
inferBindings bindings = do
  -- Generate constraints separately
  bindingsInference <- generateBindingConstraints bindings

  -- Solve constraints together
  env <- ask
  either throwError pure $ solveBindingConstraints env bindingsInference

generateBindingConstraints :: [R.Binding] -> Inference [(T.Binding, [Constraint])]
generateBindingConstraints bindings = do
  -- Record schemes for each binding
  bindingsAndSchemes <- traverse (\binding -> (binding,) <$> generateBindingScheme binding) bindings

  -- Add all the binding type variables to the typing environment and then
  -- collect constraints for all bindings.
  let
    bindingNameSchemes = first (convertIdent . locatedValue . R.bindingName) <$> bindingsAndSchemes
  extendEnvM bindingNameSchemes $ for bindingsAndSchemes $ \(binding, scheme) -> do
    (binding', constraints) <- inferBinding binding
    let
      -- Add the constraint for the binding itself
      (T.Forall _ bindingTy) = T.bindingType binding'
      (T.Forall _ schemeTy) = scheme
      bindingConstraint = Constraint (schemeTy, bindingTy)
    pure (binding', constraints ++ [bindingConstraint])

generateBindingScheme :: R.Binding -> Inference T.Scheme
generateBindingScheme binding =
  maybe
    -- No explicit type annotation, generate a type variable
    (T.Forall [] <$> freshTypeVariable)
    -- There is an explicit type annotation. Use it.
    (pure . convertScheme)
    (R.bindingType binding)

solveBindingConstraints :: TyEnv -> [(T.Binding, [Constraint])] -> Either Error [(T.Binding, [Constraint])]
solveBindingConstraints env bindingsInference = do
  -- Solve all constraints together.
  let
    constraints = concatMap snd bindingsInference
  subst <- runSolve constraints
  pure $ flip fmap bindingsInference $ \(binding, bodyCons) ->
    let
      (T.Forall _ bindingTy) = T.bindingType binding
      scheme = generalize (substituteEnv subst env) (substituteType subst bindingTy)
      binding' = binding { T.bindingType = scheme }
    in (binding', bodyCons)

inferBinding :: R.Binding -> Inference (T.Binding, [Constraint])
inferBinding (R.Binding (Located _ name) _ args body) = do
  argsAndTyVars <- traverse (\(Located _ arg) -> (convertIdent arg,) <$> freshTypeVariable) args
  let argsAndSchemes = (\(arg, t) -> (arg, T.Forall [] t)) <$> argsAndTyVars
  (body', bodyCons) <- extendEnvM argsAndSchemes $ inferExpr body
  let
    returnType = expressionType body'
    bindingTy = foldr1 T.TyFun $ (snd <$> argsAndTyVars) ++ [returnType]
    binding' =
      T.Binding
      { T.bindingName = convertIdent name
        -- Type is placeholder until we can solve all constraints and
        -- generalize to get a scheme. If we were really principled we would
        -- have some new intermediate TCBinding type or something with a Type
        -- instead of a Scheme.
      , T.bindingType = T.Forall [] bindingTy
      , T.bindingArgs = (\(name', tvar) -> T.Typed tvar name') <$> argsAndTyVars
      , T.bindingReturnType = returnType
      , T.bindingBody = body'
      }
  pure (binding', bodyCons)

inferExpr :: R.Expr -> Inference (T.Expr, [Constraint])
inferExpr (R.ELit (Located _ lit)) = pure (T.ELit lit, [])
inferExpr (R.EVar (Located _ var)) = do
  let var' = convertIdent var
  t <- lookupEnvM var'
  pure (T.EVar (T.Typed t var'), [])
inferExpr (R.EIf (R.If pred' then' else')) = do
  (pred'', predCon) <- inferExpr pred'
  (then'', thenCon) <- inferExpr then'
  (else'', elseCon) <- inferExpr else'
  let
    newConstraints =
      [ Constraint (expressionType pred'', primitiveTyCon BoolType)
      , Constraint (expressionType then'', expressionType else'')
      ]
  pure
    ( T.EIf (T.If pred'' then'' else'')
    , predCon ++ thenCon ++ elseCon ++ newConstraints
    )
inferExpr (R.ECase (R.Case scrutinee matches)) = do
  (scrutinee', scrutineeCons) <- inferExpr scrutinee
  (matches', matchesCons) <- NE.unzip <$> traverse inferMatch matches
  let
    -- Constraint: match patterns have same type as scrutinee
    patternTypes = patternType . T.matchPattern <$> matches'
    scrutineeType = expressionType scrutinee'
    patternConstraints = (\patTy -> Constraint (scrutineeType, patTy)) <$> patternTypes
    -- Constraint: match bodies all have same type. Set all of the body types
    -- equal to the type of the first match body.
    (matchBodyType :| otherMatchBodyTypes) = expressionType . T.matchBody <$> matches'
    bodyConstraints = (\bodyTy -> Constraint (matchBodyType, bodyTy)) <$> otherMatchBodyTypes
  pure
    ( T.ECase (T.Case scrutinee' matches')
    , scrutineeCons ++ concat matchesCons ++ NE.toList patternConstraints ++ bodyConstraints
    )
inferExpr (R.ELet (R.Let bindings expression)) = do
  (bindings', bindingsCons) <- unzip <$> inferBindings bindings
  let
    bindingSchemes = (\binding -> (T.bindingName binding, T.bindingType binding)) <$> bindings'
  (expression', expCons) <- extendEnvM bindingSchemes $ inferExpr expression
  pure
    ( T.ELet (T.Let bindings' expression')
    , concat bindingsCons ++ expCons
    )
inferExpr (R.EApp (R.App func args)) = do
  (func', funcConstraints) <- inferExpr func
  (args', argConstraints) <- NE.unzip <$> traverse inferExpr args
  tyVar <- freshTypeVariable
  let
    argTypes = NE.toList $ expressionType <$> args'
    newConstraint = Constraint (expressionType func', foldr1 T.TyFun (argTypes ++ [tyVar]))
  pure
    ( T.EApp (T.App func' args' tyVar)
    , funcConstraints ++ concat argConstraints ++ [newConstraint]
    )
inferExpr (R.EParens expr) = do
  (expr', constraints) <- inferExpr expr
  pure (T.EParens expr', constraints)

inferMatch :: R.Match -> Inference (T.Match, [Constraint])
inferMatch (R.Match pat body) = do
  pat' <- inferPattern pat
  let patScheme = patternScheme pat'
  (body', bodyCons) <- extendEnvM patScheme $ inferExpr body
  pure
    ( T.Match pat' body'
    , bodyCons
    )

inferPattern :: R.Pattern -> Inference T.Pattern
inferPattern (R.PatternLit (Located _ lit)) = pure $ T.PatternLit lit
inferPattern (R.PatternVar (Located _ ident)) = do
  tvar <- freshTypeVariable
  pure $ T.PatternVar $ T.Typed tvar (convertIdent ident)

patternScheme :: T.Pattern -> [(T.Ident, T.Scheme)]
patternScheme (T.PatternLit _) = []
patternScheme (T.PatternVar (T.Typed ty ident)) = [(ident, T.Forall [] ty)]

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

unifies :: T.Type -> T.Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (T.TyVar v TyVarGenerated) t = v `bind` t
unifies t (T.TyVar v TyVarGenerated) = v `bind` t
unifies (T.TyFun t1 t2) (T.TyFun t3 t4) = do
  su1 <- unifies t1 t3
  su2 <- unifies (substituteType su1 t2) (substituteType su1 t4)
  pure (su2 `composeSubst` su1)
unifies t1 t2 = throwError $ UnificationFail t1 t2

bind ::  T.TypeName -> T.Type -> Solve Subst
bind a t
  | t == T.TyVar a TyVarNotGenerated = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (singletonSubst a t)

occursCheck :: T.TypeName -> T.Type -> Bool
occursCheck a t = a `Set.member` freeTypeVariables t

--
-- Substitutions
--

newtype Subst = Subst (Map T.TypeName T.Type)
  deriving (Eq, Show, Semigroup, Monoid)

emptySubst :: Subst
emptySubst = Subst Map.empty

singletonSubst :: T.TypeName -> T.Type -> Subst
singletonSubst a t = Subst $ Map.singleton a t

composeSubst :: Subst -> Subst -> Subst
(Subst s1) `composeSubst` (Subst s2) = Subst $ Map.map (substituteType (Subst s1)) s2 `Map.union` s1

substituteScheme :: Subst -> T.Scheme -> T.Scheme
substituteScheme (Subst subst) (T.Forall vars ty) = T.Forall vars $ substituteType s' ty
 where
  s' = Subst $ foldr Map.delete subst vars

substituteType :: Subst -> T.Type -> T.Type
substituteType (Subst subst) t@(T.TyVar var _) = Map.findWithDefault t var subst
substituteType _ (T.TyCon a) = T.TyCon a
substituteType s (t1 `T.TyFun` t2) = substituteType s t1 `T.TyFun` substituteType s t2

substituteConstraint :: Subst -> Constraint -> Constraint
substituteConstraint subst (Constraint (t1, t2)) = Constraint (substituteType subst t1, substituteType subst t2)

substituteEnv :: Subst -> TyEnv -> TyEnv
substituteEnv subst (TyEnv env) = TyEnv $ Map.map (substituteScheme subst) env

substituteTBinding :: Subst -> T.Binding -> T.Binding
substituteTBinding subst binding =
  binding
  { T.bindingType = substituteScheme subst (T.bindingType binding)
  , T.bindingArgs = (\(T.Typed ty arg) -> T.Typed (substituteType subst ty) arg) <$> T.bindingArgs binding
  , T.bindingReturnType = substituteType subst (T.bindingReturnType binding)
  , T.bindingBody = substituteTExpr subst (T.bindingBody binding)
  }

substituteTExpr :: Subst -> T.Expr -> T.Expr
substituteTExpr _ lit@T.ELit{} = lit
substituteTExpr subst (T.EVar (T.Typed ty name)) = T.EVar (T.Typed (substituteType subst ty) name)
substituteTExpr subst (T.EIf (T.If pred' then' else')) =
  T.EIf (T.If (substituteTExpr subst pred') (substituteTExpr subst then') (substituteTExpr subst else'))
substituteTExpr subst (T.ECase (T.Case scrutinee matches)) =
  T.ECase (T.Case (substituteTExpr subst scrutinee) (substituteTMatch subst <$> matches))
substituteTExpr subst (T.ELet (T.Let bindings expr)) =
  T.ELet (T.Let (substituteTBinding subst <$> bindings) (substituteTExpr subst expr))
substituteTExpr subst (T.EApp (T.App func args returnType)) =
  T.EApp (T.App (substituteTExpr subst func) (substituteTExpr subst <$> args) (substituteType subst returnType))
substituteTExpr subst (T.EParens expr) = T.EParens (substituteTExpr subst expr)

substituteTMatch :: Subst -> T.Match -> T.Match
substituteTMatch subst (T.Match pat body) =
  T.Match (substituteTPattern subst pat) (substituteTExpr subst body)

substituteTPattern :: Subst -> T.Pattern -> T.Pattern
substituteTPattern _ pat@(T.PatternLit _) = pat
substituteTPattern subst (T.PatternVar (T.Typed ty var)) =
  T.PatternVar (T.Typed (substituteType subst ty) var)

--
-- Free and Active type variables
--

freeTypeVariables :: T.Type -> Set T.TypeName
freeTypeVariables T.TyCon{} = Set.empty
freeTypeVariables (T.TyVar var _) = Set.singleton var
freeTypeVariables (t1 `T.TyFun` t2) = freeTypeVariables t1 `Set.union` freeTypeVariables t2

freeSchemeTypeVariables :: T.Scheme -> Set T.TypeName
freeSchemeTypeVariables (T.Forall tvs t) = freeTypeVariables t `Set.difference` Set.fromList tvs

freeEnvTypeVariables :: TyEnv -> Set T.TypeName
freeEnvTypeVariables (TyEnv env) = foldl' Set.union Set.empty $ freeSchemeTypeVariables <$> Map.elems env

--
-- Names
--

convertIdent :: R.Ident -> T.Ident
convertIdent (R.Ident name id' _ mPrim) = T.Ident name id' mPrim

convertScheme :: R.Scheme -> T.Scheme
convertScheme (R.Forall vars ty) = T.Forall (convertTypeName <$> vars) (convertType ty)

convertType :: R.Type -> T.Type
convertType (R.TyCon name) = T.TyCon (convertTypeName name)
convertType (R.TyVar name) = T.TyVar (convertTypeName name) TyVarNotGenerated
convertType (R.TyFun ty1 ty2) = T.TyFun (convertType ty1) (convertType ty2)

convertTypeName :: R.TypeName -> T.TypeName
convertTypeName (R.TypeName name' _ id' mPrim) = T.TypeName name' id' mPrim
