{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Amy.Renamer.Renamer
  ( rename
  ) where

import Control.Monad.Except
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Amy.Errors
import Amy.Renamer.AST
import Amy.Renamer.Monad
import Amy.Prim
import Amy.Syntax.AST
import Amy.Type

-- | Gives a unique identity to all names in the AST
rename :: Module -> Either [Error] RModule
rename ast = runRenamer emptyRenamerState $ rename' ast

rename' :: Module -> Renamer RModule
rename' (Module declarations) = do
  -- TODO: Try to do each of these steps in such a way that we can get as many
  -- errors as possible. For example, we should be able to validate all binding
  -- declarations "in parallel" so if more than one has an error we can show
  -- all the errors. Currently we fail on the first error. Maybe this should be
  -- applicative? I think MonadPlus or Errors
  -- (http://hackage.haskell.org/package/transformers-0.5.2.0/docs/Control-Applicative-Lift.html#t:Errors)
  -- might be the way to go.

  -- Rename extern declarations
  rModuleExterns <- traverse renameExtern (mapMaybe declExtern declarations)

  -- Rename binding value declarations
  let
    bindings = mapMaybe declBinding declarations
    bindingTypeMap = bindingTypesMap $ mapMaybe declBindingType declarations
  traverse_ addValueToScope (bindingName <$> bindings)
  rModuleBindings <- traverse (renameBinding bindingTypeMap) bindings

  pure RModule{..}

bindingTypesMap :: [BindingType] -> Map Text (Type (Located Text))
bindingTypesMap = Map.fromList . fmap (\(BindingType (Located _ name) ts) -> (name, ts))

renameExtern :: BindingType -> Renamer RExtern
renameExtern bindingType = do
  rExternName <- addValueToScope (bindingTypeName bindingType)
  rExternType <- traverse renameType (bindingTypeTypeNames bindingType)
  pure RExtern{..}

renameType :: Located Text -> Renamer (Located PrimitiveType)
renameType name = maybe (throwError [UnknownTypeName name]) pure $ traverse readPrimitiveType name

renameBinding :: Map Text (Type (Located Text)) -> Binding -> Renamer RBinding
renameBinding typeMap binding = withNewScope $ do -- Begin new scope
  rBindingName <- lookupValueInScopeOrError (bindingName binding)
  rBindingType <- traverse (traverse renameType) $ Map.lookup (locatedValue $ bindingName binding) typeMap
  rBindingArgs <- traverse addValueToScope (bindingArgs binding)
  rBindingBody <- renameExpression (bindingBody binding)
  pure RBinding{..}
renameExpression :: Expr -> Renamer RExpr
renameExpression (ELit lit) = pure $ RELit lit
renameExpression (EVar var) = REVar <$> lookupValueInScopeOrError var
renameExpression (EIf (If predicate thenExpression elseExpression)) = do
  rIfPredicate <- renameExpression predicate
  rIfThen <- renameExpression thenExpression
  rIfElse <- renameExpression elseExpression
  pure $ REIf RIf {..}
renameExpression (ELet (Let bindings expression)) =
  withNewScope $ do
    let
      bindings' = mapMaybe letBinding bindings
      bindingTypeMap = bindingTypesMap $ mapMaybe letBindingType bindings
    traverse_ addValueToScope (bindingName <$> bindings')
    rLetBindings <- traverse (renameBinding bindingTypeMap) bindings'
    rLetExpression <- renameExpression expression
    pure $ RELet RLet{..}
renameExpression (EApp app) = do
  rAppFunction <- renameExpression (appFunction app)
  rAppArgs <- traverse renameExpression (appArgs app)
  pure $ REApp RApp{..}
renameExpression (EParens expr) = renameExpression expr
