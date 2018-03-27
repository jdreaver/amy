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
    maybe (throwError [ExpectedPrimitiveType (Just $ bindingValueName bindingValue) expType]) pure $
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
  ty <- lookupValueTypeOrError (variableName var)
  pure $
    ExpressionVariable
    var
    { variableType = ty
    }
typeCheckExpression (ExpressionIf (If predicate thenExpression elseExpression ())) = do
  predicate' <- typeCheckExpression predicate
  thenExpression' <- typeCheckExpression thenExpression
  elseExpression' <- typeCheckExpression elseExpression

  let
    predicateType = expressionType predicate'
    thenType = expressionType thenExpression'
    elseType = expressionType elseExpression'

  -- Predicate needs to be Bool
  when (predicateType /= PrimitiveTy BoolType) $
    throwError [TypeMismatch predicateType (PrimitiveTy BoolType)]

  -- then/else branches need to have the same type
  when (thenType /= elseType) $
    throwError [TypeMismatch thenType elseType]

  pure $
    ExpressionIf
    If
    { ifPredicate = predicate'
    , ifThen = thenExpression'
    , ifElse = elseExpression'
    , ifType = thenType
    }
typeCheckExpression (ExpressionFunctionApplication app) = do
  -- Type check the function expression
  function <- typeCheckExpression $ functionApplicationFunction app
  let
    funcType = expressionType function

  -- Type check the arguments
  args <- mapM typeCheckExpression $ functionApplicationArgs app
  argTypes' <- forM args $ \arg ->
    let argType = expressionType arg
    in maybe (throwError [ExpectedPrimitiveType Nothing argType]) pure $
       primitiveType argType

  -- Make sure there is the right number of arguments
  funcType' <-
    case funcType of
      FunctionTy ft -> pure ft
      _ -> throwError [ExpectedFunctionType funcType]
  let
    funcArgTypes = functionTypeArgTypes funcType'
  unless (length argTypes' == length funcArgTypes) $
    throwError [WrongNumberOfArguments (length argTypes') (length funcArgTypes)]

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
    FunctionApplication
    { functionApplicationFunction = function
    , functionApplicationArgs = args
    , functionApplicationReturnType = PrimitiveTy $ functionTypeReturnType funcType'
    }
typeCheckExpression (ExpressionParens expr) =
  ExpressionParens <$> typeCheckExpression expr
