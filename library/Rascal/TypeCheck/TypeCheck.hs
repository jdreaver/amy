{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rascal.TypeCheck.TypeCheck
  ( typeCheck
  ) where

import Control.Monad (forM, when)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import GHC.Exts (toList)

import Rascal.AST
import Rascal.Names
import Rascal.Type
import Rascal.TypeCheck.Monad

typeCheck :: AST IdName () -> Either [TypeCheckError] (AST IdName Type)
typeCheck ast = runTypeCheck emptyTypeCheckState $ typeCheck' ast

typeCheck' :: AST IdName () -> TypeCheck (AST IdName Type)
typeCheck' (AST declarations) = do
  -- Check that all extern types exist
  let externs = mapMaybe topLevelExternType (toList declarations)
  mapM_ typeCheckBindingType externs

  -- Check that all binding types exist and set binding types
  let bindingTypes = mapMaybe topLevelBindingType (toList declarations)
  mapM_ typeCheckBindingType bindingTypes

  -- Type check binding values
  let bindingValues = mapMaybe topLevelBindingValue (toList declarations)
  bindingValues' <- mapM typeCheckBindingValue bindingValues

  pure $ AST $
    (TopLevelExternType <$> externs)
    ++ (TopLevelBindingType <$> bindingTypes)
    ++ (TopLevelBindingValue <$> bindingValues')

typeCheckBindingType
  :: BindingType IdName
  -> TypeCheck ()
typeCheckBindingType bindingType = do
  -- Validate type names
  primTypes <- forM (bindingTypeTypeNames bindingType) $ \typeName ->
    maybe (throwError [UnknownTypeName typeName]) pure (readPrimitiveType typeName)

  -- Set value type for binding
  let
    type' =
      case primTypes of
        primType :| [] -> PrimitiveTy primType
        types@(firstType :| rest) ->
          FunctionTy
          FunctionType
          { functionTypeArgTypes = firstType :| init rest
          , functionTypeReturnType = NE.last types
          }

  setValueType (idNameId $ bindingTypeName bindingType) type'

typeCheckBindingValue
  :: BindingValue IdName ()
  -> TypeCheck (BindingValue IdName Type)
typeCheckBindingValue bindingValue = do
  -- Look up binding type
  type' <- lookupValueTypeOrError (bindingValueName bindingValue)

  -- Make sure binding has same number of args as binding type
  let
    argNames = bindingValueArgs bindingValue
    argTypes' = argTypes type'

  -- Look up types for arguments
  forM_ (zip argNames argTypes') $ \(argName, argType) ->
    setValuePrimitiveType (idNameId argName) argType

  -- Get type of expression
  expression <- typeCheckExpression (bindingValueBody bindingValue)

  -- Make sure expression type matches binding return type
  let expType = expressionType expression
  expressionType' <-
    maybe (throwError [ExpectedPrimitiveType (bindingValueName bindingValue) expType]) pure $
    primitiveType expType
  let
    returnType' = returnType type'
  when (expressionType' /= returnType') $
    throwError [TypeMismatch (PrimitiveTy expressionType') (PrimitiveTy returnType')]

  pure
    bindingValue
    { bindingValueType = type'
    , bindingValueBody = expression
    }

typeCheckExpression
  :: Expression IdName ()
  -> TypeCheck (Expression IdName Type)
typeCheckExpression (ExpressionLiteral lit) = pure $ ExpressionLiteral lit
typeCheckExpression (ExpressionVariable var) = do
  ty <- lookupValuePrimitiveTypeOrError (variableName var)
  pure $
    ExpressionVariable
    var
    { variableType = PrimitiveTy ty
    }
typeCheckExpression (ExpressionFunctionApplication app) = do
  -- Compute function return type
  funcType <- lookupValueFunctionTypeOrError $ functionApplicationFunctionName app

  -- Compute types of args
  args <- mapM typeCheckExpression $ functionApplicationArgs app
  argTypes' <- forM args $ \arg ->
    let argType = expressionType arg
    in maybe (throwError [ExpectedPrimitiveType (functionApplicationFunctionName app) argType]) pure $
       primitiveType argType

  -- Make sure there is the right number of arguments
  let
    funcName = functionApplicationFunctionName app
    funcArgTypes = functionTypeArgTypes funcType
  unless (length argTypes' == length funcArgTypes) $
    throwError [WrongNumberOfArguments funcName (length argTypes') (length funcArgTypes)]

  -- Make sure arg types make function types
  let
    mismatchedTypes =
      fmap (\(p1, p2) -> TypeMismatch (PrimitiveTy p1) (PrimitiveTy p2))
      . NE.filter (uncurry (/=))
      $ NE.zip argTypes' funcArgTypes
  unless (null mismatchedTypes) $
    throwError mismatchedTypes

  -- Put it all together
  pure $
    ExpressionFunctionApplication
    app
    { functionApplicationType = PrimitiveTy $ functionTypeReturnType funcType
    , functionApplicationArgs = args
    }
typeCheckExpression (ExpressionParens expr) =
  ExpressionParens <$> typeCheckExpression expr
