{-# LANGUAGE OverloadedStrings #-}

module Amy.TypeCheck.TypeCheck
  ( inferModule
  , inferBindingGroup
  , inferExpr
  , checkBinding
  , checkExpr
  ) where

import Control.Monad (replicateM)
import Control.Monad.Except
import Data.Foldable (for_, traverse_, toList)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text, pack)
import Data.Traversable (for, traverse)

import Amy.Environment
import Amy.Errors
import Amy.Prim
import Amy.Syntax.AST
import Amy.Syntax.BindingGroups
import Amy.TypeCheck.KindInference
import Amy.TypeCheck.Monad
import Amy.TypeCheck.Subtyping

--
-- Infer
--

inferModule :: Environment -> Module -> Either Error (Module, Environment)
inferModule env (Module filePath typeDeclarations externs bindings) =
  runChecker env filePath $ do
    -- Add data constructor types to scope
    let dataConstructorInfos = concatMap dataConInfos typeDeclarations
    for_ dataConstructorInfos $ uncurry addDataConInfoToScope

    -- Infer type declaration kinds and add to scope
    kinds <- for typeDeclarations $ \decl@(TypeDeclaration (TyConDefinition tyCon _) _) -> do
      kind <- inferTypeDeclarationKind decl
      addTyConKindToScope tyCon kind
      pure (locatedValue tyCon, kind)

    -- Add extern types to scope
    for_ externs $ \(Extern name ty) ->
      addValueTypeToScope name ty

    -- Infer all bindings
    bindings' <- inferBindings True (concatMap toList bindings)

    let
      module' = Module filePath typeDeclarations externs bindings'
      identTypes =
        ((\binding -> (locatedValue $ bindingName binding, bindingType binding)) <$> concatMap toList bindings')
        ++ ((\extern -> (locatedValue $ externName extern, externType extern)) <$> externs)
      moduleEnv =
        emptyEnvironment
        { environmentIdentTypes = Map.fromList identTypes
        , environmentDataConInfos = Map.mapKeys locatedValue $ Map.fromList dataConstructorInfos
        , environmentTyConKinds = Map.fromList kinds
        }
    pure (module', moduleEnv)

-- | Compute binding groups and infer each group separately.
inferBindings :: Bool -> [Binding] -> Checker [NonEmpty Binding]
inferBindings isTopLevel bindings =
  traverse (inferBindingGroup isTopLevel) (bindingGroups bindings)

data BindingTypeStatus
  = TypedBinding !Binding
  | UntypedBinding !Binding
  deriving (Show, Eq)

compareBindingTypeStatus :: BindingTypeStatus -> BindingTypeStatus -> Ordering
compareBindingTypeStatus TypedBinding{} UntypedBinding{} = LT
compareBindingTypeStatus UntypedBinding{} TypedBinding{} = GT
compareBindingTypeStatus _ _ = EQ

inferBindingGroup :: Bool -> NonEmpty Binding -> Checker (NonEmpty Binding)
inferBindingGroup isTopLevel bindings = do
  -- Add all binding types to contex Also record whether binding is typed or
  -- untyped
  bindings' <- for bindings $ \binding@(Binding name ty _ _ _) ->
    case ty of
      TyUnknown -> do
        ty' <- TyExistVar <$> freshTyExistVar
        addValueTypeToScope name ty'
        pure $ UntypedBinding binding { bindingType = ty' }
      _ -> do
        checkTypeKind ty
        addValueTypeToScope name ty
        pure $ TypedBinding binding

  -- Check/infer each binding. We sort to make sure typed bindings are checked first
  bindings'' <- for (NE.sortBy compareBindingTypeStatus bindings') $ \bindingAndTy ->
    case bindingAndTy of
      TypedBinding binding -> checkBinding binding (bindingType binding)
      UntypedBinding binding -> do
        binding' <- inferBinding binding
        tySub <- currentContextSubst (bindingType binding)
        subtype (bindingType binding') tySub
        pure binding'

  -- Generalize and apply substitutions to binding N.B. we only generalize
  -- top-level bindings, not let binding
  context <- getContext
  pure $ flip fmap bindings'' $ \binding ->
    let
      ty = bindingType binding
      -- TODO: Only try to generalize untyped bindings. We can keep them in
      -- their BindingTypeStatus after inference/checking and use that to
      -- decide to generalize.
      (ty', context') =
        if isTopLevel
        then generalize context ty
        else (ty, context)
    in
      contextSubstBinding context' $ binding { bindingType = ty' }

inferBinding :: Binding -> Checker Binding
inferBinding (Binding name@(Located nameSpan _) _ args _ body) = do
  (args', body', ty, retTy, context) <- inferAbs args body nameSpan
  pure $ contextSubstBinding context $ Binding name ty args' retTy body'

-- | Helper to infer both bindings and lambdas
inferAbs
  :: (Traversable t)
  => t (Typed (Located IdentName))
  -> Expr
  -> SourceSpan
  -> Checker (t (Typed (Located IdentName)), Expr, Type, Type, Context)
inferAbs args body span' = withSourceSpan span' $ withNewLexicalScope $ do
  argsAndVars <- for args $ \(Typed _ arg) -> do
    ty <- freshTyExistVar
    addValueTypeToScope arg (TyExistVar ty)
    pure (arg, ty)
  bodyVar <- freshTyExistVar
  marker <- freshTyExistMarkerVar

  -- Infer body
  body' <- checkExpr body (TyExistVar bodyVar)

  -- Construct the type from arg/body variables
  let ty = foldTyFun $ NE.fromList $ TyExistVar <$> (toList (snd <$> argsAndVars) ++ [bodyVar])

  -- Convert binding
  args' <- for argsAndVars $ \(arg, var) -> do
    argTy <- currentContextSubst (TyExistVar var)
    pure $ Typed argTy arg
  retTy <- currentContextSubst (TyExistVar bodyVar)

  (contextL, contextR) <- findMarkerHole marker
  putContext contextL
  pure (args', body', ty, retTy, contextL <> contextR)

generalize :: Context -> Type -> (Type, Context)
generalize context ty =
  let
    -- Find unsolved existential variables in the order found in the type. Note
    -- that in general there can be more unsolved existential variables in the
    -- context then there actually are in the type.
    freeVars = freeTEVars $ contextSubst context ty

    -- Replace these with nice letters. TODO: Make sure these letters aren't
    -- already in scope.
    varsWithLetters = zip freeVars (TyVarName <$> letters)
    solutions = uncurry ContextSolved . fmap (TyVar . notLocated) <$> varsWithLetters
    context' = context <> (Context $ Seq.fromList solutions)

    -- Build the forall type
    tyVars = notLocated . snd <$> varsWithLetters
    ty' = contextSubst context' ty
    tyForall = maybe ty' (\varsNE -> TyForall varsNE ty') $ NE.nonEmpty tyVars
  in (tyForall, context')

letters :: [Text]
letters = [1..] >>= fmap pack . flip replicateM ['a'..'z']

inferExpr :: Expr -> Checker Expr
inferExpr expr = withSourceSpan (expressionSpan expr) $ inferExpr' expr

inferExpr' :: Expr -> Checker Expr
inferExpr' (ELit lit) = pure $ ELit lit
inferExpr' (EVar (Typed _ var)) = do
    t <- currentContextSubst =<< lookupValueType var
    pure $ EVar $ Typed t var
inferExpr' (ECon (Typed _ con)) = do
    t <- currentContextSubst =<< lookupDataConType con
    pure $ ECon $ Typed t con
inferExpr' (EIf (If pred' then' else' span')) = do
  -- TODO: Is this the right way to do this? Should we actually infer the types
  -- and then unify with expected types? I'm thinking instead we should
  -- instantiate a variable for then/else and check both of them against it,
  -- instead of inferring "then" and using that type to check "else".
  pred'' <- checkExpr pred' (TyCon $ notLocated boolTyCon)
  then'' <- inferExpr then'
  else'' <- checkExpr else' (expressionType then'')
  pure $ EIf $ If pred'' then'' else'' span'
inferExpr' (ELet (Let bindings expression span')) =
  withNewLexicalScope $ do
    bindings'' <- inferBindings False (concatMap toList bindings)
    expression' <- inferExpr expression
    pure $ ELet (Let bindings'' expression' span')
inferExpr' (ELam (Lambda args body span' _)) = do
  (args', body', ty, _, context) <- inferAbs args body span'
  pure $ contextSubstExpr context $ ELam $ Lambda args' body' span' ty
inferExpr' (EApp (App f e _)) = do
  f' <- inferExpr f
  tfSub <- currentContextSubst (expressionType f')
  (e', retTy) <- inferApp tfSub e
  pure (EApp $ App f' e' retTy)
inferExpr' (ERecord span' rows) = do
  rows' <- fmap Map.fromList $ for (Map.toList rows) $ \(label, Typed _ expr) -> do
    expr' <- inferExpr expr
    pure (label, Typed (expressionType expr') expr')
  pure $ ERecord span' rows'
inferExpr' (ERecordSelect expr label _) = do
  retVar <- freshTyExistVar
  polyVar <- freshTyExistVar
  let exprTy = TyRecord (Map.singleton (notLocated $ locatedValue label) $ TyExistVar retVar) (Just $ TyExistVar polyVar)
  expr' <- checkExpr expr exprTy
  retTy <- currentContextSubst $ TyExistVar retVar
  pure $ ERecordSelect expr' label retTy
inferExpr' (EParens expr) = EParens <$> inferExpr expr
inferExpr' (ECase (Case scrutinee matches span')) = do
  scrutineeVar <- freshTyExistVar
  matchVar <- freshTyExistVar
  scrutinee' <- checkExpr scrutinee (TyExistVar scrutineeVar)
  matches' <- for matches $ \match -> checkMatch (TyExistVar scrutineeVar) match (TyExistVar matchVar)
  pure $ ECase $ Case scrutinee' matches' span'

inferApp :: Type -> Expr -> Checker (Expr, Type)
inferApp (TyForall as t) e = do
  as' <- traverse (const freshTyExistVar) as
  let t' = foldl' (\ty (MaybeLocated _ a, a') -> instantiate a (TyExistVar a') ty) t $ NE.zip as as'
  inferApp t' e
inferApp (TyExistVar a) e = do
  (a1, a2) <- articulateTyFunExist a
  e' <- checkExpr e (TyExistVar a1)
  t <- currentContextSubst (TyExistVar a2)
  pure (e', t)
inferApp (TyFun t1 t2) e = do
  e' <- checkExpr e t1
  t <- currentContextSubst t2
  pure (e', t)
inferApp t e = error $ "Cannot inferApp for " ++ show (t, e)

inferMatch :: Type ->  Match -> Checker Match
inferMatch scrutineeTy match@(Match pat body) =
  withSourceSpan (matchSpan match) $ do
    pat' <- checkPattern pat scrutineeTy
    body' <- withNewLexicalScope $ do
      let
        mBinderNameAndTy = do
          Typed ty ident <- patternBinderIdent pat'
          pure (ident, ty)
      traverse_ (uncurry addValueTypeToScope) mBinderNameAndTy
      inferExpr body
    pure $ Match pat' body'

inferPattern :: Pattern -> Checker Pattern
inferPattern pat = withSourceSpan (patternSpan pat) $ inferPattern' pat

inferPattern' :: Pattern -> Checker Pattern
inferPattern' (PLit lit) = pure $ PLit lit
inferPattern' (PVar (Typed _ ident)) = do
  tvar <- freshTyExistVar
  pure $ PVar $ Typed (TyExistVar tvar) ident
inferPattern' (PCons (PatCons con mArg _)) = do
  conTy <- currentContextSubst =<< lookupDataConType con
  case mArg of
    -- Convert argument and add a constraint on argument plus constructor
    Just arg -> do
      arg' <- inferPattern arg
      let argTy = patternType arg'
      retTy <- freshTyExistVar
      subtype conTy (argTy `TyFun` TyExistVar retTy) -- Is this right?
      pure $ PCons $ PatCons con (Just arg') (TyExistVar retTy)
    -- No argumen The return type is just the data constructor type.
    Nothing ->
      pure $ PCons $ PatCons con Nothing conTy
inferPattern' (PParens pat) = PParens <$> inferPattern pat

patternBinderIdent :: Pattern -> Maybe (Typed (Located IdentName))
patternBinderIdent (PLit _) = Nothing
patternBinderIdent (PVar ident) = Just ident
patternBinderIdent (PCons (PatCons _ mArg _)) = patternBinderIdent =<< mArg
patternBinderIdent (PParens pat) = patternBinderIdent pat

--
-- Checking
--

checkBinding :: Binding -> Type -> Checker Binding
checkBinding binding t@(TyForall as t') =
  withContextUntilNE (ContextVar . maybeLocatedValue <$> as) $ do
    binding' <- checkBinding binding t'
    pure binding' { bindingType = t }
checkBinding (Binding name@(Located span' _) _ args _ body) t = do
  (args', body', bodyTy, context) <- checkAbs args body t span'
  pure $ contextSubstBinding context $ Binding name t args' bodyTy body'

-- | Helper to check bindings and lambdas
checkAbs
  :: [Typed (Located IdentName)]
  -> Expr
  -> Type
  -> SourceSpan
  -> Checker ([Typed (Located IdentName)], Expr, Type, Context)
checkAbs args body t span' =
  withSourceSpan span' $ do
    -- Split out argument and body types
    let
      unfoldedTy = unfoldTyFun t
      numArgs = length args
    when (length unfoldedTy < numArgs + 1) $
      throwAmyError $ TooManyBindingArguments (length unfoldedTy - 1) numArgs
    let
      (argTys, bodyTys) = NE.splitAt numArgs unfoldedTy
      bodyTy = foldTyFun $ NE.fromList bodyTys

    withNewLexicalScope $ withNewContextScope $ do
      -- Add argument types to scope
      args' <- for (zip args argTys) $ \(Typed _ arg, ty) -> do
        addValueTypeToScope arg ty
        pure $ Typed ty arg

      -- Check body
      body' <- checkExpr body bodyTy

      context <- getContext
      pure (args', body', bodyTy, context)

checkExpr :: Expr -> Type -> Checker Expr
checkExpr e t = withSourceSpan (expressionSpan e) $ checkExpr' e t

checkExpr' :: Expr -> Type -> Checker Expr
checkExpr' e (TyForall as t) =
  withContextUntilNE (ContextVar . maybeLocatedValue <$> as) $
    checkExpr e t
checkExpr' (ELam (Lambda args body span' _)) t@TyFun{} = do
  (args', body', _, context) <- checkAbs (toList args) body t span'
  pure $ contextSubstExpr context $ ELam $ Lambda (NE.fromList args') body' span' t
checkExpr' e t = do
  e' <- inferExpr e
  tSub <- currentContextSubst t
  eTy' <- currentContextSubst $ expressionType e'
  subtype eTy' tSub
  pure e'

checkMatch :: Type -> Match -> Type -> Checker Match
checkMatch scrutineeTy m t =
  withSourceSpan (matchSpan m) $ do
    m' <- inferMatch scrutineeTy m
    tSub <- currentContextSubst t
    mTy' <- currentContextSubst $ matchType m'
    subtype mTy' tSub
    pure m'

checkPattern :: Pattern -> Type -> Checker Pattern
checkPattern pat t =
  withSourceSpan (patternSpan pat) $ do
    pat' <- inferPattern pat
    tSub <- currentContextSubst t
    patTy' <- currentContextSubst $ patternType pat'
    subtype patTy' tSub
    pure pat'

--
-- Substitution
--

contextSubstBinding :: Context -> Binding -> Binding
contextSubstBinding context (Binding name ty args retTy body) =
  Binding
  { bindingName = name
  , bindingType = contextSubst context ty
  , bindingArgs = contextSubstTyped context <$> args
  , bindingReturnType = contextSubst context retTy
  , bindingBody = contextSubstExpr context body
  }

contextSubstExpr :: Context -> Expr -> Expr
contextSubstExpr _ lit@ELit{} = lit
contextSubstExpr context (ERecord span' rows) =
  ERecord span' $ (\(Typed ty expr) -> Typed (contextSubst context ty) (contextSubstExpr context expr)) <$> rows
contextSubstExpr context (ERecordSelect expr label ty) =
  ERecordSelect (contextSubstExpr context expr) label (contextSubst context ty)
contextSubstExpr context (EVar var) = EVar $ contextSubstTyped context var
contextSubstExpr context (ECon con) = ECon $ contextSubstTyped context con
contextSubstExpr context (EIf (If pred' then' else' span')) =
  EIf (If (contextSubstExpr context pred') (contextSubstExpr context then') (contextSubstExpr context else') span')
contextSubstExpr context (ECase (Case scrutinee matches span')) =
  ECase (Case (contextSubstExpr context scrutinee) (contextSubstMatch context <$> matches) span')
contextSubstExpr context (ELet (Let bindings expr span')) =
  ELet (Let (fmap (contextSubstBinding context) <$> bindings) (contextSubstExpr context expr) span')
contextSubstExpr context (ELam (Lambda args body span' ty)) =
  ELam (Lambda (contextSubstTyped context <$> args) (contextSubstExpr context body) span' (contextSubst context ty))
contextSubstExpr context (EApp (App f arg returnType)) =
  EApp (App (contextSubstExpr context f) (contextSubstExpr context arg) (contextSubst context returnType))
contextSubstExpr context (EParens expr) = EParens (contextSubstExpr context expr)

contextSubstTyped :: Context -> Typed a -> Typed a
contextSubstTyped subst (Typed ty x) = Typed (contextSubst subst ty) x

contextSubstMatch :: Context -> Match -> Match
contextSubstMatch context (Match pat body) =
  Match (contextSubstPattern context pat) (contextSubstExpr context body)

contextSubstPattern :: Context -> Pattern -> Pattern
contextSubstPattern _ pat@(PLit _) = pat
contextSubstPattern context (PVar var) = PVar $ contextSubstTyped context var
contextSubstPattern context (PCons (PatCons con mArg retTy)) =
  let
    mArg' = contextSubstPattern context <$> mArg
    retTy' = contextSubst context retTy
  in PCons (PatCons con mArg' retTy')
contextSubstPattern context (PParens pat) = PParens (contextSubstPattern context pat)
