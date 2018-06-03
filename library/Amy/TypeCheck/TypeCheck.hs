{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Amy.TypeCheck.TypeCheck
  ( inferModule
  , inferBindingGroup
  , inferExpr
  , checkBinding
  , checkExpr
  ) where

import Control.Monad (replicateM)
import Data.Foldable (for_, traverse_)
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack)
import Data.Traversable (for)

import Amy.Errors
import Amy.Prim
import Amy.Renamer.AST as R
import Amy.Syntax.Located
import Amy.TypeCheck.AST as T
import Amy.TypeCheck.KindInference
import Amy.TypeCheck.Monad
import Amy.TypeCheck.Subtyping

--
-- Infer
--

inferModule :: R.Module -> Either Error T.Module
inferModule (R.Module bindings externs typeDeclarations) = do
  let
    externs' = convertExtern <$> externs
    typeDeclarations' = (convertTypeDeclaration <$> typeDeclarations) ++ (T.fromPrimTypeDef <$> allPrimTypeDefinitions)

    externTypes = (\(T.Extern name ty) -> (name, ty)) <$> externs'
    primFuncTypes = primitiveFunctionType' <$> allPrimitiveFunctions
    identTypes = externTypes ++ primFuncTypes
    dataConstructorTypes = concatMap mkDataConTypes typeDeclarations'
  runChecker identTypes dataConstructorTypes $ do
    -- Infer type declaration kinds and add to scope
    for_ typeDeclarations' $ \decl@(T.TypeDeclaration (T.TyConDefinition tyCon _) _) -> do
      kind <- inferTypeDeclarationKind decl
      addTyConKindToScope tyCon kind

    -- Infer all bindings
    bindings' <- inferBindingGroup bindings
    pure (T.Module bindings' externs' typeDeclarations')

inferBindingGroup :: [R.Binding] -> Checker [T.Binding]
inferBindingGroup bindings = do
  -- Add all binding group types to context
  bindingsWithTypes <- for bindings $ \binding@(R.Binding (Located _ name) mTy _ _) -> do
    ty <-
      case mTy of
        Just ty' -> do
          let ty'' = convertScheme ty'
          checkTypeKind ty''
          pure ty''
        Nothing -> T.TyExistVar <$> freshTyExistVar
    addValueTypeToScope name ty
    pure (binding, ty)

  -- Infer each binding individually
  bindings' <- for bindingsWithTypes $ \(binding, ty) -> do
    binding' <- checkBinding binding ty
    case R.bindingType binding of
      Nothing -> pure binding'
      Just ty' -> pure binding' { T.bindingType = convertScheme ty' }

  -- Generalize and apply substitutions to bindings
  context <- getContext
  pure $ flip fmap bindings' $ \binding ->
    let
      ty = T.bindingType binding
      (ty', context') = generalize context ty
    in
      contextSubstBinding context' $ binding { T.bindingType = ty' }

inferUntypedBinding :: R.Binding -> Checker T.Binding
inferUntypedBinding (R.Binding (Located _ name) _ args expr) = withNewLexicalScope $ do
  argsAndVars <- for args $ \(Located _ arg) -> do
    ty <- freshTyExistVar
    addValueTypeToScope arg (TyExistVar ty)
    pure (arg, ty)
  exprVar <- freshTyExistVar
  marker <- freshTyExistMarkerVar

  expr' <- checkExpr expr (TyExistVar exprVar)

  -- Generalize (the Hindley-Milner extension in the paper)
  (contextL, contextR) <- findMarkerHole marker
  let ty = foldr1 T.TyFun $ T.TyExistVar <$> ((snd <$> argsAndVars) ++ [exprVar])
  putContext contextL

  -- Convert binding
  args' <- for argsAndVars $ \(arg, var) -> do
    argTy <- currentContextSubst (T.TyExistVar var)
    pure $ T.Typed argTy arg
  retTy <- currentContextSubst (T.TyExistVar exprVar)

  pure $ contextSubstBinding (contextL <> contextR) $ T.Binding name ty args' retTy expr'

generalize :: Context -> T.Type -> (T.Type, Context)
generalize context ty =
  let
    -- Find unsolved existential variables in the order found in the type. Note
    -- that in general there can be more unsolved existential variables in the
    -- context then there actually are in the type.
    freeVars = freeTEVars $ contextSubst context ty
    -- unsolvedEVars = contextUnsolved context
    -- unsolvedFreeEVars = filter (`elem` freeVars) unsolvedEVars

    -- Replace these with nice letters. TODO: Make sure these letters aren't
    -- already in scope.
    varsWithLetters = zip freeVars (TyVarName <$> letters)
    solutions = uncurry ContextSolved . fmap T.TyVar <$> varsWithLetters
    context' = context <> (Context $ Seq.fromList solutions)

    -- Build the forall type
    tyVars = snd <$> varsWithLetters
    ty' = contextSubst context' ty
    tyForall = maybe ty' (\varsNE -> TyForall varsNE ty') $ NE.nonEmpty tyVars
  in (tyForall, context')

letters :: [Text]
letters = [1..] >>= fmap pack . flip replicateM ['a'..'z']

inferExpr :: R.Expr -> Checker T.Expr
inferExpr (R.ELit (Located _ lit)) = pure $ T.ELit lit
inferExpr (R.EVar var) =
  case var of
    R.VVal (Located _ valVar) -> do
      t <- currentContextSubst =<< lookupValueType valVar
      pure $ T.EVar $ T.VVal (T.Typed t valVar)
    R.VCons (Located _ con) -> do
      t <- currentContextSubst =<< lookupDataConType con
      pure (T.EVar $ T.VCons (T.Typed t con))
inferExpr (R.EIf (R.If pred' then' else')) = do
  -- TODO: Is this the right way to do this? Should we actually infer the types
  -- and then unify with expected types? I'm thinking instead we should
  -- instantiate a variable for then/else and check both of them against it,
  -- instead of inferring "then" and using that type to check "else".
  pred'' <- checkExpr pred' (T.TyCon boolTyCon)
  then'' <- inferExpr then'
  else'' <- checkExpr else' (expressionType then'')
  pure $ T.EIf $ T.If pred'' then'' else''
inferExpr (R.ELet (R.Let bindings expression)) = do
  bindings' <- inferBindingGroup bindings
  expression' <- withNewLexicalScope $ do
    for_ bindings' $ \binding ->
      addValueTypeToScope (T.bindingName binding) (T.bindingType binding)
    inferExpr expression
  pure $ T.ELet (T.Let bindings' expression')
inferExpr (R.EApp f e) = do
  f' <- inferExpr f
  tfSub <- currentContextSubst (expressionType f')
  (e', retTy) <- inferApp tfSub e
  pure (T.EApp $ T.App f' e' retTy)
inferExpr (R.ERecord rows) = do
  rows' <- for (Map.mapKeys locatedValue rows) $ \expr -> do
    expr' <- inferExpr expr
    pure (T.Typed (expressionType expr') expr')
  pure $ T.ERecord rows'
inferExpr (R.ERecordSelect expr (Located _ label)) = do
  retVar <- freshTyExistVar
  polyVar <- freshTyExistVar
  let exprTy = T.TyRecord (Map.singleton label $ T.TyExistVar retVar) (Just $ T.TyExistVar polyVar)
  expr' <- checkExpr expr exprTy
  retTy <- currentContextSubst $ T.TyExistVar retVar
  pure $ T.ERecordSelect expr' label retTy
inferExpr (R.EParens expr) = T.EParens <$> inferExpr expr
inferExpr (R.ECase (R.Case scrutinee matches)) = do
  scrutineeVar <- freshTyExistVar
  matchVar <- freshTyExistVar
  scrutinee' <- checkExpr scrutinee (T.TyExistVar scrutineeVar)
  matches' <- for matches $ \match -> checkMatch (T.TyExistVar scrutineeVar) match (T.TyExistVar matchVar)
  pure $ T.ECase $ T.Case scrutinee' matches'

inferApp :: T.Type -> R.Expr -> Checker (T.Expr, T.Type)
inferApp (TyForall as t) e = do
  as' <- traverse (const freshTyExistVar) as
  let t' = foldl' (\ty (a, a') -> instantiate a (TyExistVar a') ty) t $ NE.zip as as'
  inferApp t' e
inferApp (TyExistVar a) e = do
  (a1, a2) <- articulateTyFunExist a
  e' <- checkExpr e (TyExistVar a1)
  t <- currentContextSubst (TyExistVar a2)
  pure (e', t)
inferApp (T.TyFun t1 t2) e = do
  e' <- checkExpr e t1
  t <- currentContextSubst t2
  pure (e', t)
inferApp t e = error $ "Cannot inferApp for " ++ show (t, e)

inferMatch :: T.Type ->  R.Match -> Checker T.Match
inferMatch scrutineeTy (R.Match pat body) = do
  pat' <- checkPattern pat scrutineeTy
  body' <- withNewLexicalScope $ do
    traverse_ (uncurry addValueTypeToScope) (patternBinderType pat')
    inferExpr body
  pure $ T.Match pat' body'

inferPattern :: R.Pattern -> Checker T.Pattern
inferPattern (R.PLit (Located _ lit)) = pure $ T.PLit lit
inferPattern (R.PVar (Located _ ident)) = do
  tvar <- freshTyExistVar
  pure $ T.PVar $ T.Typed (T.TyExistVar tvar) ident
inferPattern (R.PCons (R.PatCons (Located _ con) mArg)) = do
  conTy <- currentContextSubst =<< lookupDataConType con
  case mArg of
    -- Convert argument and add a constraint on argument plus constructor
    Just arg -> do
      arg' <- inferPattern arg
      let argTy = patternType arg'
      retTy <- freshTyExistVar
      subtype conTy (argTy `T.TyFun` T.TyExistVar retTy) -- Is this right?
      pure $ T.PCons $ T.PatCons con (Just arg') (T.TyExistVar retTy)
    -- No argument. The return type is just the data constructor type.
    Nothing ->
      pure $ T.PCons $ T.PatCons con Nothing conTy
inferPattern (R.PParens pat) = T.PParens <$> inferPattern pat

patternBinderType :: T.Pattern -> Maybe (IdentName, T.Type)
patternBinderType (T.PLit _) = Nothing
patternBinderType (T.PVar (T.Typed ty ident)) = Just (ident, ty)
patternBinderType (T.PCons (T.PatCons _ mArg _)) = patternBinderType =<< mArg
patternBinderType (T.PParens pat) = patternBinderType pat

--
-- Checking
--

checkBinding :: R.Binding -> T.Type -> Checker T.Binding
checkBinding binding (TyForall as t) =
  withContextUntilNE (ContextVar <$> as) $
    checkBinding binding t
checkBinding binding t = do
  binding' <- inferUntypedBinding binding
  tSub <- currentContextSubst t
  subtype (T.bindingType binding') tSub
  context <- getContext
  pure $ contextSubstBinding context binding'

checkExpr :: R.Expr -> T.Type -> Checker T.Expr
checkExpr e (TyForall as t) =
  withContextUntilNE (ContextVar <$> as) $
    checkExpr e t
checkExpr e t = do
  e' <- inferExpr e
  tSub <- currentContextSubst t
  eTy' <- currentContextSubst $ expressionType e'
  subtype eTy' tSub
  pure e'

checkMatch :: T.Type -> R.Match -> T.Type -> Checker T.Match
checkMatch scrutineeTy m t = do
  m' <- inferMatch scrutineeTy m
  tSub <- currentContextSubst t
  mTy' <- currentContextSubst $ matchType m'
  subtype mTy' tSub
  pure m'

checkPattern :: R.Pattern -> T.Type -> Checker T.Pattern
checkPattern pat t = do
  pat' <- inferPattern pat
  tSub <- currentContextSubst t
  patTy' <- currentContextSubst $ patternType pat'
  subtype patTy' tSub
  pure pat'

--
-- Converting types
--

primitiveFunctionType' :: PrimitiveFunction -> (IdentName, T.Type)
primitiveFunctionType' (PrimitiveFunction _ name ty) =
  ( name
  , foldr1 T.TyFun $ T.TyCon <$> ty
  )

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

mkDataConTypes :: T.TypeDeclaration -> [(DataConName, T.Type)]
mkDataConTypes (T.TypeDeclaration (T.TyConDefinition tyConName tyVars) dataConDefs) = mkDataConPair <$> dataConDefs
 where
  mkDataConPair (T.DataConDefinition name mTyArg) =
    let
      tyVars' = T.TyVar <$> tyVars
      tyApp = foldl1 T.TyApp (T.TyCon tyConName : tyVars')
      ty = foldl1 T.TyFun (maybeToList mTyArg ++ [tyApp])
      tyForall = maybe ty (\varsNE -> TyForall varsNE ty) (NE.nonEmpty tyVars)
    in (name, tyForall)

convertScheme :: R.Scheme -> T.Type
convertScheme (R.Forall vars ty) =
  case NE.nonEmpty vars of
    Just vars' -> T.TyForall (convertTyVarInfo <$> vars') (convertType ty)
    Nothing -> convertType ty

convertType :: R.Type -> T.Type
convertType (R.TyCon (Located _ con)) = T.TyCon con
convertType (R.TyVar var) = T.TyVar (convertTyVarInfo var)
convertType (R.TyApp f arg) = T.TyApp (convertType f) (convertType arg)
convertType (R.TyRecord rows mTail) =
  T.TyRecord
    (Map.mapKeys locatedValue $ convertType <$> rows)
    (T.TyVar . locatedValue <$> mTail)
convertType (R.TyFun ty1 ty2) = T.TyFun (convertType ty1) (convertType ty2)

convertTyConDefinition :: R.TyConDefinition -> T.TyConDefinition
convertTyConDefinition (R.TyConDefinition name' args _) = T.TyConDefinition name' (locatedValue <$> args)

convertTyVarInfo :: Located TyVarName -> T.TyVarName
convertTyVarInfo (Located _ name') = name'

--
-- Substitution
--

contextSubstBinding :: Context -> T.Binding -> T.Binding
contextSubstBinding context (T.Binding name ty args retTy body) =
  T.Binding
  { T.bindingName = name
  , T.bindingType = contextSubst context ty
  , T.bindingArgs = contextSubstTyped context <$> args
  , T.bindingReturnType = contextSubst context retTy
  , T.bindingBody = contextSubstExpr context body
  }

contextSubstExpr :: Context -> T.Expr -> T.Expr
contextSubstExpr _ lit@T.ELit{} = lit
contextSubstExpr context (T.ERecord rows) =
  T.ERecord $ (\(Typed ty expr) -> Typed (contextSubst context ty) (contextSubstExpr context expr)) <$> rows
contextSubstExpr context (T.ERecordSelect expr label ty) =
  T.ERecordSelect (contextSubstExpr context expr) label (contextSubst context ty)
contextSubstExpr context (T.EVar var) =
  T.EVar $
    case var of
      T.VVal var' -> T.VVal $ contextSubstTyped context var'
      T.VCons (T.Typed ty con) -> T.VCons (T.Typed (contextSubst context ty) con)
contextSubstExpr context (T.EIf (T.If pred' then' else')) =
  T.EIf (T.If (contextSubstExpr context pred') (contextSubstExpr context then') (contextSubstExpr context else'))
contextSubstExpr context (T.ECase (T.Case scrutinee matches)) =
  T.ECase (T.Case (contextSubstExpr context scrutinee) (contextSubstMatch context <$> matches))
contextSubstExpr context (T.ELet (T.Let bindings expr)) =
  T.ELet (T.Let (contextSubstBinding context <$> bindings) (contextSubstExpr context expr))
contextSubstExpr context (T.EApp (T.App f arg returnType)) =
  T.EApp (T.App (contextSubstExpr context f) (contextSubstExpr context arg) (contextSubst context returnType))
contextSubstExpr context (T.EParens expr) = T.EParens (contextSubstExpr context expr)

contextSubstTyped :: Context -> T.Typed a -> T.Typed a
contextSubstTyped subst (T.Typed ty x) = T.Typed (contextSubst subst ty) x

contextSubstMatch :: Context -> T.Match -> T.Match
contextSubstMatch context (T.Match pat body) =
  T.Match (contextSubstPattern context pat) (contextSubstExpr context body)

contextSubstPattern :: Context -> T.Pattern -> T.Pattern
contextSubstPattern _ pat@(T.PLit _) = pat
contextSubstPattern context (T.PVar var) = T.PVar $ contextSubstTyped context var
contextSubstPattern context (T.PCons (T.PatCons con mArg retTy)) =
  let
    mArg' = contextSubstPattern context <$> mArg
    retTy' = contextSubst context retTy
  in T.PCons (T.PatCons con mArg' retTy')
contextSubstPattern context (T.PParens pat) = T.PParens (contextSubstPattern context pat)
