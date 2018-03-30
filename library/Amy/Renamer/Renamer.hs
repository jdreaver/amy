{-# LANGUAGE NamedFieldPuns #-}

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
  externs' <- traverse renameExtern (mapMaybe declExtern declarations)

  -- Rename binding value declarations
  let
    bindings = mapMaybe declBinding declarations
    bindingTypeMap = bindingTypesMap $ mapMaybe declBindingType declarations
  traverse_ addValueToScope (bindingName <$> bindings)
  bindingValues' <- traverse (renameBinding bindingTypeMap) bindings

  pure
    RModule
    { rModuleBindings = bindingValues'
    , rModuleExterns = externs'
    }

bindingTypesMap :: [BindingType] -> Map Text (Type Text)
bindingTypesMap = Map.fromList . fmap (\(BindingType name ts) -> (name, ts))

renameExtern :: BindingType -> Renamer RExtern
renameExtern bindingType =
  RExtern
  -- Add extern name to scope
  <$> addValueToScope (bindingTypeName bindingType)
  -- Look up types
  <*> renameTypes (bindingTypeTypeNames bindingType)

renameTypes :: (Traversable t) => t Text -> Renamer (t PrimitiveType)
renameTypes = traverse (\name -> maybe (throwError [UnknownTypeName name]) pure $ readPrimitiveType name)

renameBinding :: Map Text (Type Text) -> Binding -> Renamer RBinding
renameBinding typeMap binding = withNewScope $ do -- Begin new scope
  -- Look up binding name
  name <- lookupValueInScopeOrError (bindingName binding)

  -- Look up types
  let mTypeNames = Map.lookup (bindingName binding) typeMap
  types <- traverse renameTypes mTypeNames

  -- Add binding arguments to scope
  args <- mapM addValueToScope (bindingArgs binding)

  -- Run renamer on expression
  body <- renameExpression (bindingBody binding)

  pure
    RBinding
    { rBindingName = name
    , rBindingType = types
    , rBindingArgs = args
    , rBindingBody = body
    }

renameExpression :: Expr -> Renamer RExpr
renameExpression (ELit lit) = pure $ RELit lit
renameExpression (EVar var) = REVar <$> lookupValueInScopeOrError var
renameExpression (EIf (If predicate thenExpression elseExpression)) =
  fmap REIf
  $ RIf
  <$> renameExpression predicate
  <*> renameExpression thenExpression
  <*> renameExpression elseExpression
renameExpression (ELet (Let bindings expression)) =
  withNewScope $ do
    let
      bindings' = mapMaybe letBinding bindings
      bindingTypeMap = bindingTypesMap $ mapMaybe letBindingType bindings

    traverse_ addValueToScope (bindingName <$> bindings')
    bindings'' <- traverse (renameBinding bindingTypeMap) bindings'
    expression' <- renameExpression expression

    pure $
      RELet
      RLet
      { rLetBindings = bindings''
      , rLetExpression = expression'
      }
renameExpression (EApp app) =
  fmap REApp $
  RApp
  <$> renameExpression (appFunction app)
  <*> mapM renameExpression (appArgs app)
renameExpression (EParens expr) = renameExpression expr
