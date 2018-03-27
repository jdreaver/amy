{-# LANGUAGE NamedFieldPuns #-}

module Rascal.Renamer.Renamer
  ( rename
  ) where

import Control.Monad.Except
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Exts (toList)

import Rascal.AST
import Rascal.Names
import Rascal.Renamer.Monad

-- | Gives a unique identity to all names in the AST
rename :: AST Text () -> Either [RenamerError] (AST IdName ())
rename ast = runRenamer emptyRenamerState $ rename' ast

rename' :: AST Text () -> Renamer (AST IdName ())
rename' (AST declarations) = do
  -- TODO: Try to do each of these steps in such a way that we can get as many
  -- errors as possible. For example, we should be able to validate all binding
  -- declarations "in parallel" so if more than one has an error we can show
  -- all the errors. Currently we fail on the first error. Maybe this should be
  -- applicative?

  -- Rename extern declarations
  let externs = mapMaybe topLevelExternType (toList declarations)
  externs' <- fmap TopLevelExternType <$> mapM renameBindingType externs

  -- Rename binding type declarations and add value bindings to scope
  let bindingTypes = mapMaybe topLevelBindingType (toList declarations)
  bindingTypes' <- fmap TopLevelBindingType <$> mapM renameBindingType bindingTypes

  -- Rename binding value declarations
  let bindingValues = mapMaybe topLevelBindingValue (toList declarations)
  bindingValues' <- fmap TopLevelBindingValue <$> mapM renameBindingValue bindingValues

  pure $ AST $ externs' ++ bindingTypes' ++ bindingValues'

renameBindingType
  :: BindingType Text
  -> Renamer (BindingType IdName)
renameBindingType bindingType = do
  -- Add extern name to scope
  idName <- addValueToScope TopLevelDefinition $ bindingTypeName bindingType

  pure
    bindingType
    { bindingTypeName = idName
    }

renameBindingValue
  :: BindingValue Text ()
  -> Renamer (BindingValue IdName ())
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
    , bindingValueType = ()
    , bindingValueBody = expression
    }

renameExpression :: Expression Text () -> Renamer (Expression IdName ())
renameExpression (ExpressionLiteral lit) = pure $ ExpressionLiteral lit
renameExpression (ExpressionVariable var) = do
  idName <- lookupValueInScopeOrError (variableName var)
  pure $
    ExpressionVariable
    Variable
    { variableName = idName
    , variableType = ()
    }
renameExpression (ExpressionFunctionApplication app) = do
  let funcName = functionApplicationFunctionName app
  funcNameId <- maybe (throwError [UnknownVariable funcName]) pure =<< lookupValueInScope funcName
  expressions <- mapM renameExpression (functionApplicationArgs app)
  pure $
    ExpressionFunctionApplication
    FunctionApplication
    { functionApplicationFunctionName = funcNameId
    , functionApplicationType = ()
    , functionApplicationArgs = expressions
    }
renameExpression (ExpressionParens expr) = ExpressionParens <$> renameExpression expr
