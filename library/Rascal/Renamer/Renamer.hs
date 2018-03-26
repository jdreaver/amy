{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Rascal.Renamer.Renamer
  ( rename
  ) where

import Control.Monad (void)
import Control.Monad.Except
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Exts (toList)

import Rascal.AST
import Rascal.Names
import Rascal.Renamer.Monad

-- | Gives a unique identity to all names in the AST
rename :: AST Text -> Either [RenamerError] (AST IdName)
rename ast = runRenamer emptyRenamerState $ setupDefaultEnvironment >> rename' ast

-- | Add known type definitions
setupDefaultEnvironment :: Renamer ()
setupDefaultEnvironment = do
  void $ addTypeToScope "Int"
  void $ addTypeToScope "Double"

rename' :: AST Text -> Renamer (AST IdName)
rename' (AST declarations) = do
  -- TODO: Try to do each of these steps in such a way that we can get as many
  -- errors as possible. For example, we should be able to validate all binding
  -- declarations "in parallel" so if more than one has an error we can show
  -- all the errors. Currently we fail on the first error. Maybe this should be
  -- applicative?

  -- Rename extern declarations
  let externs = mapMaybe topLevelExternType (toList declarations)
  externs' <- fmap TopLevelExternType <$> mapM renameBindingType externs

  -- Rename binding type declarations
  let bindingTypes = mapMaybe topLevelBindingType (toList declarations)
  bindingTypes' <- fmap TopLevelBindingType <$> mapM renameBindingType bindingTypes

  -- Add all top-level binding names to scope. This is necessary so that
  -- binding value expressions can reference any other top-level binding.
  let bindingValues = mapMaybe topLevelBindingValue (toList declarations)
  mapM_ (addValueToScope TopLevelDefinition . bindingValueName) bindingValues

  -- Rename binding value declarations
  bindingValues' <- fmap TopLevelBindingValue <$> mapM renameBindingValue bindingValues

  pure $ AST $ externs' ++ bindingTypes' ++ bindingValues'

renameBindingType
  :: BindingType Text
  -> Renamer (BindingType IdName)
renameBindingType declaration = do
  -- Add extern name to scope
  idName <- addValueToScope TopLevelDefinition $ bindingTypeName declaration

  -- Add types to scope
  typeIds <- ensureTypesExist $ bindingTypeType declaration

  pure
    BindingType
    { bindingTypeName = idName
    , bindingTypeType = typeIds
    }

ensureTypesExist :: NonEmpty Text -> Renamer (NonEmpty IdName)
ensureTypesExist typeNames = do
  let lookupTypeWithError typeName = maybe (Left $ UnknownType typeName) Right <$> lookupTypeInScope typeName
  eTypeIds <- mapM lookupTypeWithError (toList typeNames)
  case partitionEithers eTypeIds of
    ([], ids) -> pure (NE.fromList ids) -- NE.fromList shouldn't fail since all IDs exist
    (errors, _) -> throwError errors

renameBindingValue
  :: BindingValue Text
  -> Renamer (BindingValue IdName)
renameBindingValue binding = withNewScope $ do -- Begin new scope
  -- Get binding name ID
  idName <- lookupValueInScopeOrError (bindingValueName binding)

  -- Add binding arguments to scope
  args <- mapM (addValueToScope LocalDefinition) (bindingValueArgs binding)

  -- Run renamer on expression
  expression <- renameExpression (bindingValueBody binding)

  pure
    BindingValue
    { bindingValueName = idName
    , bindingValueArgs = args
    , bindingValueBody = expression
    }

renameExpression :: Expression Text -> Renamer (Expression IdName)
renameExpression (ExpressionLiteral lit) = pure $ ExpressionLiteral lit
renameExpression (ExpressionVariable var) = ExpressionVariable <$> lookupValueInScopeOrError var
renameExpression (ExpressionFunctionApplication app) = do
  let funcName = functionApplicationFunctionName app
  funcNameId <- maybe (throwError [UnknownVariable funcName]) pure =<< lookupValueInScope funcName
  expressions <- mapM renameExpression (functionApplicationArgs app)
  pure $
    ExpressionFunctionApplication
    FunctionApplication
    { functionApplicationFunctionName = funcNameId
    , functionApplicationArgs = expressions
    }
renameExpression (ExpressionParens expr) = ExpressionParens <$> renameExpression expr
