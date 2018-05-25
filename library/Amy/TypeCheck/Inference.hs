{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Amy.TypeCheck.Inference
  ( inferModule
  , inferTopLevel
  , TyEnv
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable (for_, traverse_)
import Data.List (foldl', foldl1')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Traversable (for)

import Amy.Errors
import Amy.Kind
import Amy.Prim
import Amy.Renamer.AST as R
import Amy.Syntax.Located
import Amy.TypeCheck.AST as T
import Amy.TypeCheck.KindInference
import Amy.TypeCheck.Monad
import Amy.TypeCheck.Substitution

--
-- Inference
--

inferModule :: R.Module -> Either Error T.Module
inferModule (R.Module bindings externs typeDeclarations) = do
  let
    externs' = convertExtern <$> externs
    externSchemes = (\(T.Extern name ty) -> (name, T.Forall [] ty)) <$> externs'
    typeDeclarations' = (convertTypeDeclaration <$> typeDeclarations) ++ (T.fromPrimTypeDef <$> allPrimTypeDefinitions)
    primFuncSchemes = primitiveFunctionScheme <$> allPrimitiveFunctions
    env =
      TyEnv
      { identTypes = Map.fromList $ externSchemes ++ primFuncSchemes
      , dataConstructorTypes = Map.fromList $ concatMap mkDataConTypes typeDeclarations'
      , tyVarKinds = Map.empty
      , tyConKinds = Map.empty
      , maxId = 0
      }
  runInference env $ do
    -- Infer type declaration kinds and add to scope
    for_ typeDeclarations' $ \decl@(T.TypeDeclaration (T.TyConDefinition tyCon _) _) -> do
      kind <- inferTypeDeclarationKind decl
      addTyConKindToScope tyCon kind

    -- Infer all bindings
    bindings' <- inferTopLevel bindings
    pure (T.Module bindings' externs' typeDeclarations')

convertExtern :: R.Extern -> T.Extern
convertExtern (R.Extern (Located _ name) ty) = T.Extern name (convertType ty)

convertTypeDeclaration :: R.TypeDeclaration -> T.TypeDeclaration
convertTypeDeclaration (R.TypeDeclaration tyName cons) =
  T.TypeDeclaration (convertTyConDefinition tyName) (convertDataConDefinition <$> cons)

convertDataConDefinition :: R.DataConDefinition -> T.DataConDefinition
convertDataConDefinition (R.DataConDefinition (Located _ conName) mTyArg) =
  T.DataConDefinition
  { T.dataConDefinitionName = conName
  , T.dataConDefinitionArgument = convertType <$> mTyArg
  }

mkDataConTypes :: T.TypeDeclaration -> [(DataConName, (T.TypeDeclaration, T.DataConDefinition))]
mkDataConTypes tyDecl@(T.TypeDeclaration _ dataConDefs) = mkDataConPair <$> dataConDefs
 where
  mkDataConPair def@(T.DataConDefinition name _) = (name, (tyDecl, def))

dataConstructorScheme :: DataConName -> Inference T.Scheme
dataConstructorScheme con = do
  (T.TypeDeclaration (T.TyConDefinition tyConName tyVars) _, T.DataConDefinition _ mTyArg) <-
    fromMaybe (error $ "No type definition for " ++ show con)
    . Map.lookup con
    <$> gets dataConstructorTypes
  let
    mkTyConInfo v = T.TyVarInfo v TyVarNotGenerated
    tyApp =
      case NE.nonEmpty tyVars of
        Just tyVars' -> foldl1' T.TyApp (T.TyCon tyConName : (T.TyVar . mkTyConInfo <$> NE.toList tyVars'))
        Nothing -> T.TyCon tyConName
    mkTy arg =
      case arg of
        Just ty -> ty `T.TyFun` tyApp
        Nothing -> tyApp
  pure $ T.Forall (mkTyConInfo <$> tyVars) (mkTy mTyArg)

primitiveFunctionScheme :: PrimitiveFunction -> (IdentName, T.Scheme)
primitiveFunctionScheme (PrimitiveFunction _ name ty) =
  ( name
  , T.Forall [] $ foldr1 T.TyFun $ T.TyCon <$> ty
  )

inferTopLevel :: [R.Binding] -> Inference [T.Binding]
inferTopLevel bindings = do
  bindingsAndConstraints <- inferBindings bindings
  let (bindings', constraints) = unzip bindingsAndConstraints
  subst <- runSolve (concat constraints)
  let
    bindings'' =
      flip fmap bindings' $ \binding ->
        let
          (T.Forall _ ty) = T.bindingType binding
          (scheme', letterSubst) = normalize ty
          binding' = binding { T.bindingType = scheme' }
          subst' = composeSubst letterSubst subst
        in substituteBinding subst' binding'
  pure bindings''

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
  env <- get
  solveBindingConstraints env bindingsInference

generateBindingConstraints :: [R.Binding] -> Inference [(T.Binding, [Constraint])]
generateBindingConstraints bindings = do
  -- Record schemes for each binding
  bindingsAndSchemes <- traverse (\binding -> (binding,) <$> generateBindingScheme binding) bindings

  -- Add all the binding type variables to the typing environment and then
  -- collect constraints for all bindings.
  withNewLexicalScope $ do
    -- Add all schemes to typing environment before going into each binding
    for_ bindingsAndSchemes $ \(binding, scheme) ->
      addIdentSchemeToScope (locatedValue $ R.bindingName binding) scheme
    for bindingsAndSchemes $ \(binding, scheme) -> do
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
    (T.Forall [] . T.TyVar <$> freshTypeVariable)
    -- There is an explicit type annotation. Use it.
    convertExisting
    (R.bindingType binding)
 where
  convertExisting scheme = do
    let scheme' = convertScheme scheme
    checkSchemeKind scheme'
    pure scheme'

checkSchemeKind :: T.Scheme -> Inference ()
checkSchemeKind scheme@(T.Forall tyVars ty) =
  withNewLexicalScope $ do
    traverse_ (addUnknownTyVarKindToScope . tyVarInfoName) tyVars
    kind <- inferTypeKind ty
    when (kind /= KStar) $
      error $ "Somehow kind unification passed bu we don't have KStar " ++ show (scheme, kind)

solveBindingConstraints :: TyEnv -> [(T.Binding, [Constraint])] -> Inference [(T.Binding, [Constraint])]
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
  argsAndTyVars <- traverse (\(Located _ arg) -> (arg,) . T.TyVar <$> freshTypeVariable) args
  (body', bodyCons) <- withNewLexicalScope $ do
    for_ argsAndTyVars $ \(arg, ty) ->
      addIdentSchemeToScope arg (T.Forall [] ty)
    inferExpr body
  let
    returnType = expressionType body'
    bindingTy = foldr1 T.TyFun $ (snd <$> argsAndTyVars) ++ [returnType]
    binding' =
      T.Binding
      { T.bindingName = name
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
inferExpr (R.ERecord rows) = do
  (rows', rowsCons) <- unzip <$> traverse (uncurry inferRow) (Map.toList rows)
  let typedRows = (\r -> Typed (expressionType r) r) <$> Map.fromList rows'
  pure
    ( T.ERecord typedRows
    , concat rowsCons
    )
inferExpr (R.ERecordSelect expr (Located _ label)) = do
  (expr', exprCons) <- inferExpr expr
  retVar <- freshTypeVariable
  polyVar <- freshTypeVariable
  let exprTy = T.TyRecord (Map.singleton label (T.TyVar retVar)) (Just polyVar)
  pure
    ( T.ERecordSelect expr' label (T.TyVar retVar)
    , exprCons ++ [Constraint (expressionType expr', exprTy)]
    )
inferExpr (R.EVar var) =
  case var of
    R.VVal (Located _ valVar) -> do
      t <- lookupIdentScheme valVar >>= instantiate
      pure (T.EVar $ T.VVal (T.Typed t valVar), [])
    R.VCons (Located _ con) -> do
      t <- instantiate =<< dataConstructorScheme con
      pure (T.EVar $ T.VCons (T.Typed t con), [])
inferExpr (R.EIf (R.If pred' then' else')) = do
  (pred'', predCon) <- inferExpr pred'
  (then'', thenCon) <- inferExpr then'
  (else'', elseCon) <- inferExpr else'
  let
    newConstraints =
      [ Constraint (expressionType pred'', T.TyCon boolTyCon)
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
  (expression', expCons) <- withNewLexicalScope $ do
    for_ bindings' $ \binding ->
      addIdentSchemeToScope (T.bindingName binding) (T.bindingType binding)
    inferExpr expression
  pure
    ( T.ELet (T.Let bindings' expression')
    , concat bindingsCons ++ expCons
    )
inferExpr (R.EApp f arg) = do
  (f', funcConstraints) <- inferExpr f
  (arg', argConstraints) <- inferExpr arg
  tyVar <- T.TyVar <$> freshTypeVariable
  let
    argType = expressionType arg'
    newConstraint = Constraint (expressionType f', argType `T.TyFun` tyVar)
  pure
    ( T.EApp (T.App f' arg' tyVar)
    , funcConstraints ++ argConstraints ++ [newConstraint]
    )
inferExpr (R.EParens expr) = do
  (expr', constraints) <- inferExpr expr
  pure (T.EParens expr', constraints)

inferRow :: Located RowLabel -> R.Expr -> Inference ((RowLabel, T.Expr), [Constraint])
inferRow (Located _ label) expr = do
  (expr', exprCons) <- inferExpr expr
  pure
    ( (label, expr')
    , exprCons
    )

inferMatch :: R.Match -> Inference (T.Match, [Constraint])
inferMatch (R.Match pat body) = do
  (pat', patCons) <- inferPattern pat
  (body', bodyCons) <- withNewLexicalScope $ do
    traverse_ (uncurry addIdentSchemeToScope) (patternScheme pat')
    inferExpr body
  pure
    ( T.Match pat' body'
    , patCons ++ bodyCons
    )

inferPattern :: R.Pattern -> Inference (T.Pattern, [Constraint])
inferPattern (R.PLit (Located _ lit)) = pure (T.PLit lit, [])
inferPattern (R.PVar (Located _ ident)) = do
  tvar <- T.TyVar <$> freshTypeVariable
  pure (T.PVar $ T.Typed tvar ident, [])
inferPattern (R.PCons (R.PatCons (Located _ con) mArg)) = do
  conTy <- instantiate =<< dataConstructorScheme con
  case mArg of
    -- Convert argument and add a constraint on argument plus constructor
    Just arg -> do
      (arg', argCons) <- inferPattern arg
      let argTy = patternType arg'
      retTy <- T.TyVar <$> freshTypeVariable
      let constraint = Constraint (argTy `T.TyFun` retTy, conTy)
      pure
        ( T.PCons $ T.PatCons con (Just arg') retTy
        , argCons ++ [constraint]
        )
    -- No argument. The return type is just the data constructor type.
    Nothing ->
      pure
        ( T.PCons $ T.PatCons con Nothing conTy
        , []
        )
inferPattern (R.PParens pat) = do
  (pat', patCons) <- inferPattern pat
  pure
    ( T.PParens pat'
    , patCons
    )

patternScheme :: T.Pattern -> Maybe (IdentName, T.Scheme)
patternScheme (T.PLit _) = Nothing
patternScheme (T.PVar (T.Typed ty ident)) = Just (ident, T.Forall [] ty)
patternScheme (T.PCons (T.PatCons _ mArg _)) = patternScheme =<< mArg
patternScheme (T.PParens pat) = patternScheme pat

--
-- Utilities
--

-- | Produces a type scheme from a type by finding all the free type variables
-- in the type, replacing them with sequential letters, and collecting the free
-- type variables in the Forall quantifier.
normalize :: T.Type -> (T.Scheme, Subst)
normalize body =
 ( T.Forall (Map.elems letterMap) (normType body)
 , Subst $ T.TyVar <$> letterMap
 )
 where
  letterMap =
    Map.fromList
    $ replaceGensWithLetters (Set.toList $ freeTypeVariables body) letters

  normType (T.TyFun a b) = T.TyFun (normType a) (normType b)
  normType (T.TyCon con) = T.TyCon con
  normType (T.TyApp f arg) = T.TyApp (normType f) (normType arg)
  normType (T.TyRecord rows mVar) = T.TyRecord (normType <$> rows) (normTyVar <$> mVar)
  normType (T.TyVar a) = T.TyVar $ normTyVar a
  normTyVar var = fromMaybe (error "type variable not in signature") (Map.lookup var letterMap)

letters :: [Text]
letters = [1..] >>= fmap pack . flip replicateM ['a'..'z']

replaceGensWithLetters :: [T.TyVarInfo] -> [Text] -> [(T.TyVarInfo, T.TyVarInfo)]
replaceGensWithLetters _ [] = error "Ran out of letters, how???"
replaceGensWithLetters [] _ = []
replaceGensWithLetters (var@(T.TyVarInfo _ TyVarGenerated):vars) (l:ls) =
  (var, T.TyVarInfo (TyVarName l) TyVarNotGenerated) : replaceGensWithLetters vars ls
replaceGensWithLetters (var@(T.TyVarInfo _ TyVarNotGenerated):vars) ls =
  (var, var) : replaceGensWithLetters vars ls

-- | Convert a scheme into a type by replacing all the type variables with
-- fresh names. Types are instantiated when they are looked up so we can make
-- constraints with the type variables and not worry about name collisions.
instantiate :: T.Scheme -> Inference T.Type
instantiate (T.Forall as t) = do
  as' <- fmap T.TyVar <$> traverse (const freshTypeVariable) as
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

--
-- Free type variables
--

freeTypeVariables :: T.Type -> Set T.TyVarInfo
freeTypeVariables (T.TyCon _) = Set.empty
freeTypeVariables (T.TyVar var) = Set.singleton var
freeTypeVariables (T.TyApp f arg) = freeTypeVariables f `Set.union` freeTypeVariables arg
freeTypeVariables (T.TyRecord rows mVar) = Set.unions $ Set.fromList (maybeToList mVar) : (freeTypeVariables <$> Map.elems rows)
freeTypeVariables (t1 `T.TyFun` t2) = freeTypeVariables t1 `Set.union` freeTypeVariables t2

freeSchemeTypeVariables :: T.Scheme -> Set T.TyVarInfo
freeSchemeTypeVariables (T.Forall tvs t) = freeTypeVariables t `Set.difference` Set.fromList tvs

freeEnvTypeVariables :: TyEnv -> Set T.TyVarInfo
freeEnvTypeVariables env =
  foldl' Set.union Set.empty $ freeSchemeTypeVariables <$> Map.elems (identTypes env)

--
-- Constraint Solver
--

-- | A 'Constraint' is a statement that two types should be equal.
newtype Constraint = Constraint { unConstraint :: (T.Type, T.Type) }
  deriving (Show, Eq)

substituteConstraint :: Subst -> Constraint -> Constraint
substituteConstraint subst (Constraint (t1, t2)) = Constraint (substituteType subst t1, substituteType subst t2)

type Unifier = (Subst, [Constraint])

runSolve :: [Constraint] -> Inference Subst
runSolve cs = solver (emptySubst, cs)

-- Unification solver
solver :: Unifier -> Inference Subst
solver (su, cs) =
  case cs of
    [] -> return su
    (Constraint (t1, t2): cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 `composeSubst` su, substituteConstraint su1 <$> cs0)

unifies :: T.Type -> T.Type -> Inference Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (T.TyVar v@(T.TyVarInfo _ TyVarGenerated)) t = v `bind` t
unifies t (T.TyVar v@(T.TyVarInfo _ TyVarGenerated)) = v `bind` t
unifies (T.TyFun t1 t2) (T.TyFun t3 t4) = do
  su1 <- unifies t1 t3
  su2 <- unifies (substituteType su1 t2) (substituteType su1 t4)
  pure (su2 `composeSubst` su1)
unifies (T.TyApp f1 arg1) (T.TyApp f2 arg2) = do
  su1 <- unifies f1 f2
  su2 <- unifies (substituteType su1 arg1) (substituteType su1 arg2)
  pure (su2 `composeSubst` su1)
unifies (T.TyCon name1) (T.TyCon name2)
  | name1 == name2 = return emptySubst
unifies t1@(T.TyRecord rows1 mVar1) t2@(T.TyRecord rows2 mVar2) = do
  let
    commonFields = Map.intersectionWith (,) rows1 rows2
    justFields1 = Map.difference rows1 rows2
    justFields2 = Map.difference rows2 rows1
    unifyRecordWithVar subst rows mVar unifyVar = do
      subst' <-
        case (Map.null rows, mVar, unifyVar) of
          -- TODO: This special case for user-generated type variables seems
          -- super janky. Is there something more principled here? The reason
          -- for this is to allow user-defined extensible records in type
          -- signatures. Normal unification would only allow generated type
          -- variables to unify with the empty record otherwise. We can only do
          -- this for empty records because if we didn't then we would be too
          -- permissive.
          (True, Just var, T.TyVarInfo _ TyVarNotGenerated) -> pure $ singletonSubst var (T.TyVar unifyVar)
          _ -> unifies (substituteType subst $ T.TyRecord rows mVar) (substituteType subst $ T.TyVar unifyVar)
      pure $ subst' `composeSubst` subst

  substCommon <- uncurry unifyMany $ unzip $ snd <$> Map.toAscList commonFields
  case (mVar1, mVar2) of
    -- Neither record is extensible
    (Nothing, Nothing) ->
      if Map.keysSet rows1 == Map.keysSet rows2
      then pure substCommon
      else throwError $ UnificationFail t1 t2
    -- Both records are extensible
    (Just var1, Just var2) ->
      -- Side condition: if the tails are equal but the prefixes are not equal,
      -- then don't unify. This prevents us from unifying rows with the same
      -- tail but not the same prefix. For example, {x::Int|a} and {y::Int|a}
      -- shouldn't unify.
      if var1 == var2 && not (Map.null justFields1 || Map.null justFields2)
      then throwError $ UnificationFail t1 t2
      else do
        fresh1 <- freshTypeVariable
        subst' <- unifyRecordWithVar substCommon justFields1 (Just fresh1) var2
        fresh2 <- freshTypeVariable
        unifyRecordWithVar subst' justFields2 (Just fresh2) var1
    -- Only one record is extensible
    (Just var1, Nothing) ->
      if null justFields1
      then unifyRecordWithVar substCommon justFields2 Nothing var1
      else throwError $ UnificationFail t1 t2
    (Nothing, Just var2) ->
      if null justFields2
      then unifyRecordWithVar substCommon justFields1 Nothing var2
      else throwError $ UnificationFail t1 t2
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: [T.Type] -> [T.Type] -> Inference Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (substituteType su1 <$> ts1) (substituteType su1 <$> ts2)
  return (su2 `composeSubst` su1)
unifyMany t1 t2 = error $ "unifyMany lists different length " ++ show (t1, t2)

bind ::  T.TyVarInfo -> T.Type -> Inference Subst
bind a t
  | t == T.TyVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (singletonSubst a t)

occursCheck :: T.TyVarInfo -> T.Type -> Bool
occursCheck a t = a `Set.member` freeTypeVariables t

--
-- Names
--

convertScheme :: R.Scheme -> T.Scheme
convertScheme (R.Forall vars ty) = T.Forall (convertTyVarInfo <$> vars) (convertType ty)

convertType :: R.Type -> T.Type
convertType (R.TyCon (Located _ con)) = T.TyCon con
convertType (R.TyVar var) = T.TyVar (convertTyVarInfo var)
convertType (R.TyApp f arg) = T.TyApp (convertType f) (convertType arg)
convertType (R.TyRecord rows mVar) =
  T.TyRecord
    (Map.mapKeys locatedValue $ convertType <$> rows)
    (flip T.TyVarInfo TyVarNotGenerated . locatedValue <$> mVar)
convertType (R.TyFun ty1 ty2) = T.TyFun (convertType ty1) (convertType ty2)

convertTyConDefinition :: R.TyConDefinition -> T.TyConDefinition
convertTyConDefinition (R.TyConDefinition name' args _) = T.TyConDefinition name' (locatedValue <$> args)

convertTyVarInfo :: Located TyVarName -> T.TyVarInfo
convertTyVarInfo (Located _ name') = T.TyVarInfo name' TyVarNotGenerated
