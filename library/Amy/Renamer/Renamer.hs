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
import Amy.Renamer.AST
import Amy.Renamer.Monad
import Amy.Prim
import Amy.Syntax.AST
import Amy.Type

-- | Gives a unique identity to all names in the AST
rename :: Module -> Either [Error] RModule
rename ast = toEither . runRenamer emptyRenamerState $ rename' ast

rename' :: Module -> Renamer (Validation [Error] RModule)
rename' (Module declarations) = do
  -- Rename extern declarations
  rModuleExterns <- traverse renameExtern (mapMaybe declExtern declarations)

  -- Rename binding value declarations
  let
    bindings = mapMaybe declBinding declarations
    bindingTypeMap = bindingTypesMap $ mapMaybe declBindingType declarations
  traverse_ (addValueToScope True) (bindingName <$> bindings)
  rModuleBindings <- traverse (renameBinding bindingTypeMap) bindings

  pure
    $ RModule
    <$> sequenceA rModuleBindings
    <*> sequenceA rModuleExterns

bindingTypesMap :: [BindingType] -> Map Text (Scheme (Located Text))
bindingTypesMap = Map.fromList . fmap (\(BindingType (Located _ name) ts) -> (name, ts))

renameExtern :: Extern -> Renamer (Validation [Error] RExtern)
renameExtern extern = do
  rExternName <- addValueToScope True (externName extern)
  let rExternType = traverse renameType (externType extern)
  pure $
    RExtern
      <$> rExternName
      <*> rExternType

renameType :: Located Text -> Validation [Error] (Located PrimitiveType)
renameType name =
  maybe (Failure [UnknownTypeName name]) Success $ traverse readPrimitiveType name

renameBinding :: Map Text (Scheme (Located Text)) -> Binding -> Renamer (Validation [Error] RBinding)
renameBinding typeMap binding = withNewScope $ do -- Begin new scope
  name <- lookupValueInScopeOrError (bindingName binding)
  let
    rBindingName =
      case name of
        (Success (Located l (RIdentName ident))) -> pure (Located l ident)
        (Success _) -> Failure [NonIdentifierName (bindingName binding)]
        Failure f -> Failure f
  let rBindingType = traverse (traverse renameType) $ Map.lookup (locatedValue $ bindingName binding) typeMap
  rBindingArgs <- traverse (addValueToScope False) (bindingArgs binding)
  rBindingBody <- renameExpression (bindingBody binding)
  pure $
    RBinding
    <$> rBindingName
    <*> rBindingType
    <*> sequenceA rBindingArgs
    <*> rBindingBody

renameExpression :: Expr -> Renamer (Validation [Error] RExpr)
renameExpression (ELit lit) = pure $ Success $ RELit lit
renameExpression (EVar var) = fmap REVar <$> lookupValueInScopeOrError var
renameExpression (EIf (If predicate thenExpression elseExpression)) = do
  rIfPredicate <- renameExpression predicate
  rIfThen <- renameExpression thenExpression
  rIfElse <- renameExpression elseExpression
  pure
    $ fmap REIf
    $ RIf
    <$> rIfPredicate
    <*> rIfThen
    <*> rIfElse
renameExpression (ELet (Let bindings expression)) =
  withNewScope $ do
    let
      bindings' = mapMaybe letBinding bindings
      bindingTypeMap = bindingTypesMap $ mapMaybe letBindingType bindings
    traverse_ (addValueToScope False) (bindingName <$> bindings')
    rLetBindings <- traverse (renameBinding bindingTypeMap) bindings'
    rLetExpression <- renameExpression expression
    pure
      $ fmap RELet
      $ RLet
      <$> sequenceA rLetBindings
      <*> rLetExpression
renameExpression (EApp app) = do
  rAppFunction <- renameExpression (appFunction app)
  rAppArgs <- traverse renameExpression (appArgs app)
  pure
    $ fmap REApp
    $ RApp
    <$> rAppFunction
    <*> sequenceA rAppArgs
renameExpression (EParens expr) = fmap REParens <$> renameExpression expr
