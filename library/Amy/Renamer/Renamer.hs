{-# LANGUAGE NamedFieldPuns #-}

module Amy.Renamer.Renamer
  ( rename
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Exts (toList)

import Amy.AST
import Amy.Names
import Amy.Renamer.Monad

-- | Gives a unique identity to all names in the AST
rename :: AST Text () -> Either [RenamerError] (AST IdName ())
rename ast = runRenamer emptyRenamerState $ rename' ast

rename' :: AST Text () -> Renamer (AST IdName ())
rename' (AST declarations) = do
  -- TODO: Try to do each of these steps in such a way that we can get as many
  -- errors as possible. For example, we should be able to validate all binding
  -- declarations "in parallel" so if more than one has an error we can show
  -- all the errors. Currently we fail on the first error. Maybe this should be
  -- applicative? I think MonadPlus or Errors
  -- (http://hackage.haskell.org/package/transformers-0.5.2.0/docs/Control-Applicative-Lift.html#t:Errors)
  -- might be the way to go.

  -- Rename extern declarations
  let externs = mapMaybe topLevelExternType (toList declarations)
  externs' <- fmap TopLevelExternType <$> mapM (renameBindingType TopLevelDefinition) externs

  -- Rename binding type declarations and add value bindings to scope
  let bindingTypes = mapMaybe topLevelBindingType (toList declarations)
  bindingTypes' <- fmap TopLevelBindingType <$> mapM (renameBindingType TopLevelDefinition) bindingTypes

  -- Rename binding value declarations
  let bindingValues = mapMaybe topLevelBindingValue (toList declarations)
  bindingValues' <- fmap TopLevelBindingValue <$> mapM renameBindingValue bindingValues

  pure $ AST $ externs' ++ bindingTypes' ++ bindingValues'

renameBindingType
  :: IdNameProvenance
  -> BindingType Text
  -> Renamer (BindingType IdName)
renameBindingType provenance bindingType = do
  -- Add extern name to scope
  idName <- addValueToScope provenance $ bindingTypeName bindingType

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
renameExpression (ExpressionIf (If predicate thenExpression elseExpression _)) =
  fmap ExpressionIf
  $ If
  <$> renameExpression predicate
  <*> renameExpression thenExpression
  <*> renameExpression elseExpression
  <*> pure ()
renameExpression (ExpressionLet (Let bindings expression _)) =
  withNewScope $ do
    -- Rename binding types
    bindingTypes' <- fmap LetBindingType <$> mapM (renameBindingType LocalDefinition) (mapMaybe letBindingType bindings)

    -- Rename binding values
    bindingValues' <- fmap LetBindingValue <$> mapM renameBindingValue (mapMaybe letBindingValue bindings)

    -- Rename expression
    expression' <- renameExpression expression

    pure $
      ExpressionLet
      Let
      { letBindings = bindingTypes' ++ bindingValues'
      , letExpression = expression'
      , letType = ()
      }
renameExpression (ExpressionFunctionApplication app) = do
  function <- renameExpression $ functionApplicationFunction app
  expressions <- mapM renameExpression (functionApplicationArgs app)
  pure $
    ExpressionFunctionApplication
    FunctionApplication
    { functionApplicationFunction = function
    , functionApplicationArgs = expressions
    , functionApplicationReturnType = ()
    }
renameExpression (ExpressionParens expr) = ExpressionParens <$> renameExpression expr
