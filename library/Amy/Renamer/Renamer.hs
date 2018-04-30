{-# LANGUAGE NamedFieldPuns #-}

module Amy.Renamer.Renamer
  ( rename
  ) where

import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
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
renameTypeDeclaration (S.TypeDeclaration tyName dataCon argTy) = do
  tyName' <- addTypeConstructorToScope tyName
  dataCon' <- addDataConstructorToScope dataCon
  argTy' <- lookupTypeConstructorInScopeOrError argTy
  pure
    $ R.TypeDeclaration
    <$> tyName'
    <*> dataCon'
    <*> argTy'

renameExtern :: S.Extern -> Renamer (Validation [Error] R.Extern)
renameExtern extern = do
  name' <- addValueToScope (S.externName extern)
  type' <- renameType (S.externType extern)
  pure $
    R.Extern
      <$> name'
      <*> type'

renameBindingGroup :: [S.Binding] -> [S.BindingType] -> Renamer (Validation [Error] [R.Binding])
renameBindingGroup bindings bindingTypes = do
  -- Add each binding name to scope since they can be mutually recursive
  traverse_ addValueToScope (S.bindingName <$> bindings)
  -- Rename each individual binding
  bindings' <- traverse (renameBinding $ bindingTypesMap bindingTypes) bindings
  pure $ sequenceA bindings'

bindingTypesMap :: [BindingType] -> Map Text S.Scheme
bindingTypesMap = Map.fromList . fmap (\(BindingType (Located _ name) ts) -> (name, ts))

renameBinding :: Map Text S.Scheme -> S.Binding -> Renamer (Validation [Error] R.Binding)
renameBinding typeMap binding = withNewScope $ do -- Begin new scope
  -- N.B. Assumes the binding name was already added to scope in
  -- renameBindingGroup
  name <- lookupValueInScopeOrError (S.bindingName binding)
  let
    name' =
      case name of
        (Success (Located l ident)) -> pure (Located l ident)
        Failure f -> Failure f
  type' <- traverse renameScheme $ Map.lookup (locatedValue $ S.bindingName binding) typeMap
  args <- traverse addValueToScope (S.bindingArgs binding)
  body <- renameExpression (S.bindingBody binding)
  pure $
    R.Binding
    <$> name'
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
renameType (S.TyCon name) = fmap R.TyCon <$> lookupTypeConstructorInScopeOrError name
renameType (S.TyVar name) = fmap R.TyVar <$> lookupTypeVariableInScopeOrError name
renameType (S.TyFun ty1 ty2) = do
  ty1' <- renameType ty1
  ty2' <- renameType ty2
  pure $
    R.TyFun
    <$> ty1'
    <*> ty2'

renameExpression :: S.Expr -> Renamer (Validation [Error] R.Expr)
renameExpression (S.ELit lit) = pure $ Success $ R.ELit lit
renameExpression (S.EVar var) =
  case var of
    Variable name -> fmap R.EVar <$> lookupValueInScopeOrError name
    DataConstructor name -> fmap R.EVar <$> lookupDataConstructorInScopeOrError name
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
    pat' <-
     case pat of
        S.PatternLit lit -> pure . Success $ R.PatternLit lit
        S.PatternVar var -> do
          var' <- addValueToScope var
          pure $ R.PatternVar <$> var'
        S.PatternCons (S.ConstructorPattern cons mArg) -> do
          cons' <- lookupDataConstructorInScopeOrError cons
          mArg' <- traverse addValueToScope mArg
          pure $ fmap R.PatternCons $ R.ConstructorPattern <$> cons' <*> sequenceA mArg'
    body' <- renameExpression body
    pure
      $ R.Match
      <$> pat'
      <*> body'
