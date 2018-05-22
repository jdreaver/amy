{-# LANGUAGE NamedFieldPuns #-}

module Amy.Renamer.Renamer
  ( rename
  ) where

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Traversable (for, mapAccumL)
import Data.Validation

import Amy.Errors
import Amy.Renamer.AST as R
import Amy.Renamer.Monad
import Amy.Syntax.AST as S

-- | Gives a unique identity to all names in the AST
rename :: S.Module -> Either [Error] R.Module
rename ast = toEither . runRenamer emptyRenamerState $ rename' ast

rename' :: S.Module -> Renamer (Validation [Error] R.Module)
rename' (S.Module declarations) = do
  -- Rename type declarations
  -- TODO: Add all type names to scope before renaming each declaration
  typeDeclarations <- traverse renameTypeDeclaration (mapMaybe declType declarations)

  -- Rename extern declarations
  rModuleExterns <- traverse renameExtern (mapMaybe declExtern declarations)

  -- Rename binding value declarations
  let
    bindings = mapMaybe declBinding declarations
    bindingTypes = mapMaybe declBindingType declarations
  rModuleBindings <- renameBindingGroup bindings bindingTypes
  pure
    $ R.Module
    <$> rModuleBindings
    <*> sequenceA rModuleExterns
    <*> sequenceA typeDeclarations

renameTypeDeclaration :: S.TypeDeclaration -> Renamer (Validation [Error] R.TypeDeclaration)
renameTypeDeclaration (S.TypeDeclaration (S.TyConDefinition name args span') constructors) = do
  -- Rename type name
  name' <- fmap locatedValue <$> addTypeConstructorToScope (Located span' name)
  let
    tyDef =
      R.TyConDefinition
      <$> name'
      <*> pure args
      <*> pure (Just span')

  liftValidation tyDef $ \tyDef' -> do
    -- Rename data constructors
    constructors' <- for constructors $ \(S.DataConDefinition con mArgTy) -> do
      let
        renameTyVar lTyVar@(Located tyVarSpan tyVar) =
          -- Find the constructor's type variable argument corresponding to
          -- this type variable
          let mTyVar = find ((== tyVar) . locatedValue) args
          in pure $ maybe (Failure [UnknownTypeVariable lTyVar]) (Success . Located tyVarSpan . locatedValue) mTyVar
        renameArg (S.TyCon tyCon) = fmap R.TyCon <$> lookupTypeConstructorInScopeOrError tyCon
        renameArg (S.TyVar tvar) = fmap R.TyVar <$> renameTyVar tvar
        renameArg (S.TyFun t1 t2) = do
          ty1' <- renameArg t1
          ty2' <- renameArg t2
          pure
            $ R.TyFun
            <$> ty1'
            <*> ty2'
        renameArg (S.TyRecord rows mVar) = do
          rows' <- sequenceA <$> traverse renameArg rows
          mVar' <- sequenceA <$> traverse renameTyVar mVar
          pure
            $ R.TyRecord
            <$> rows'
            <*> mVar'
        renameArg (S.TyApp appCon appArgs) = do
          appCon' <- lookupTypeConstructorInScopeOrError appCon
          appArgs' <- traverse renameArg appArgs
          pure
            $ R.TyApp
            <$> appCon'
            <*> sequenceA appArgs'
      mArgTy' <- traverse renameArg mArgTy
      liftValidation (sequenceA mArgTy') $ \mArgTy'' ->
        addDataConDefinitionToScope $ R.DataConDefinition con mArgTy''
    let
      decl' =
        R.TypeDeclaration
        <$> pure tyDef'
        <*> sequenceA constructors'
    liftValidation decl' $ \decl'' ->
      addTypeDeclarationToScope decl'' span' <* pure (checkForDuplicateTypeVariables args)

-- | We don't allow duplicate type variables in type definitions, like if we
-- said @Either a a = ...@
checkForDuplicateTypeVariables :: [Located TyVarName] -> Validation [Error] ()
checkForDuplicateTypeVariables tyVars = if null dups then Success () else Failure dups
 where
  checkDup :: Located TyVarName -> [Located TyVarName] -> Maybe Error
  checkDup var@(Located _ name) prev =
    case find ((== name) . locatedValue) prev of
      Just dupVar -> Just (DuplicateTypeVariable var dupVar)
      Nothing -> Nothing
  dups :: [Error]
  dups = catMaybes $ snd $ mapAccumL (\prev var -> (var:prev, checkDup var prev)) [] tyVars

renameExtern :: S.Extern -> Renamer (Validation [Error] R.Extern)
renameExtern extern = do
  name' <- addValueToScope (S.externName extern)
  type' <- renameType (S.externType extern)
  pure
    $ R.Extern
    <$> name'
    <*> type'

renameBindingGroup :: [S.Binding] -> [S.BindingType] -> Renamer (Validation [Error] [R.Binding])
renameBindingGroup bindings bindingTypes = do
  -- Add each binding name to scope since they can be mutually recursive
  bindingNames <- traverse addValueToScope (S.bindingName <$> bindings)

  -- Rename each individual binding
  bindings' <- traverse (uncurry $ renameBinding (bindingTypesMap bindingTypes)) (zip bindingNames bindings)
  pure $ sequenceA bindings'

bindingTypesMap :: [BindingType] -> Map IdentName S.Scheme
bindingTypesMap = Map.fromList . fmap (\(BindingType (Located _ name) ts) -> (name, ts))

renameBinding
  :: Map IdentName S.Scheme
  -> Validation [Error] (Located IdentName)
  -> S.Binding
  -> Renamer (Validation [Error] R.Binding)
renameBinding typeMap name binding = withNewScope $ do -- Begin new scope
  type' <- traverse renameScheme $ Map.lookup (locatedValue $ S.bindingName binding) typeMap
  args <- traverse addValueToScope (S.bindingArgs binding)
  body <- renameExpression (S.bindingBody binding)
  pure $
    R.Binding
    <$> name
    <*> sequenceA type'
    <*> sequenceA args
    <*> body

renameScheme :: S.Scheme -> Renamer (Validation [Error] R.Scheme)
renameScheme (S.Forall vars ty) = do
  vars' <- traverse addTypeVariableToScope vars
  ty' <- renameType ty
  pure $
    R.Forall
    <$> sequenceA vars'
    <*> ty'

renameType :: S.Type -> Renamer (Validation [Error] R.Type)
renameType (S.TyCon con) = fmap R.TyCon <$> lookupTypeConstructorInScopeOrError con
renameType (S.TyVar var) = fmap R.TyVar <$> lookupTypeVariableInScopeOrError var
renameType (S.TyApp con args) = do
  con' <- lookupTypeConstructorInScopeOrError con
  args' <- traverse renameType args
  pure
    $ R.TyApp
    <$> con'
    <*> sequenceA args'
renameType (S.TyRecord rows mVar) = do
  rows' <- sequenceA <$> traverse renameType rows
  mVar' <- sequenceA <$> traverse lookupTypeVariableInScopeOrError mVar
  pure
    $ R.TyRecord
    <$> rows'
    <*> mVar'
renameType (S.TyFun ty1 ty2) = do
  ty1' <- renameType ty1
  ty2' <- renameType ty2
  pure $
    R.TyFun
    <$> ty1'
    <*> ty2'

renameExpression :: S.Expr -> Renamer (Validation [Error] R.Expr)
renameExpression (S.ELit lit) = pure $ Success $ R.ELit lit
renameExpression (S.ERecord rows) =
  fmap R.ERecord . sequenceA <$> traverse renameExpression rows
renameExpression (S.EVar var) =
  fmap (fmap R.EVar) $
    case var of
      S.VVal name -> fmap R.VVal <$> lookupValueInScopeOrError name
      S.VCons name -> fmap R.VCons <$> lookupDataConInScopeOrError name
renameExpression (S.EIf (S.If predicate thenExpression elseExpression)) = do
  pred' <- renameExpression predicate
  then' <- renameExpression thenExpression
  else' <- renameExpression elseExpression
  pure
    $ fmap R.EIf
    $ R.If
    <$> pred'
    <*> then'
    <*> else'
renameExpression (S.ECase (S.Case scrutinee matches)) = do
  scrutinee' <- renameExpression scrutinee
  matches' <- traverse renameMatch matches
  pure
    $ fmap R.ECase
    $ R.Case
    <$> scrutinee'
    <*> sequenceA matches'
renameExpression (S.ELet (S.Let bindings expression)) =
  withNewScope $ do
    let
      bindings' = mapMaybe letBinding bindings
      bindingTypes' = mapMaybe letBindingType bindings
    rLetBindings <- renameBindingGroup bindings' bindingTypes'
    rLetExpression <- renameExpression expression
    pure
      $ fmap R.ELet
      $ R.Let
      <$> rLetBindings
      <*> rLetExpression
renameExpression (S.EApp (S.App func args)) = do
  func' <- renameExpression func
  args' <- traverse renameExpression args
  pure
    $ fmap R.EApp
    $ R.App
    <$> func'
    <*> sequenceA args'
renameExpression (S.EParens expr) = fmap R.EParens <$> renameExpression expr

renameMatch :: S.Match -> Renamer (Validation [Error] R.Match)
renameMatch (S.Match pat body) =
  withNewScope $ do
    pat' <- renamePattern pat
    body' <- renameExpression body
    pure
      $ R.Match
      <$> pat'
      <*> body'

renamePattern :: S.Pattern -> Renamer (Validation [Error] R.Pattern)
renamePattern (S.PLit lit) = pure . Success $ R.PLit lit
renamePattern (S.PVar var) = do
  var' <- addValueToScope var
  pure $ R.PVar <$> var'
renamePattern (S.PCons (S.PatCons cons mArg)) = do
  cons' <- lookupDataConInScopeOrError cons
  mArg' <- traverse renamePattern mArg
  pure $ fmap R.PCons $ R.PatCons <$> cons' <*> sequenceA mArg'
renamePattern (S.PParens pat) = fmap R.PParens <$> renamePattern pat
