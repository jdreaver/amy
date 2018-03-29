{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amy.TypeCheck.TypeCheck
  ( typeCheck
  ) where

import Control.Monad (forM, when)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import qualified Data.List.NonEmpty as NE

import Amy.Errors
import Amy.Literal
import Amy.Names
import Amy.Renamer.AST
import Amy.Type
import Amy.TypeCheck.AST
import Amy.TypeCheck.Monad

typeCheck :: RModule -> Either [Error] TModule
typeCheck ast = runTypeCheck emptyTypeCheckState $ typeCheck' ast

typeCheck' :: RModule -> TypeCheck TModule
typeCheck' (RModule bindings externs) = do
  -- Add all extern types to scope
  externs' <- traverse typeCheckExtern externs

  -- Add all binding types to scope
  traverse_ addBindingTypeToScope bindings

  -- Type check bindings
  bindings' <- traverse typeCheckBinding bindings

  pure
    TModule
    { tModuleExterns = externs'
    , tModuleBindings = bindings'
    }

typeCheckExtern
  :: RExtern
  -> TypeCheck TExtern
typeCheckExtern extern = do
  -- Set value type for binding
  setValueType (valueNameId $ rExternName extern) (makeType $ rExternType extern)

  pure
    TExtern
    { tExternName = rExternName extern
    , tExternType = rExternType extern
    }

addBindingTypeToScope :: RBinding -> TypeCheck ()
addBindingTypeToScope binding =
  maybe (pure ()) (setValueType (valueNameId $ rBindingName binding) . makeType) (rBindingType binding)

typeCheckBinding :: RBinding -> TypeCheck TBinding
typeCheckBinding binding = do
  -- Get binding type (all bindings must have types)
  bindingType <- maybe (throwError [BindingLacksTypeSignature binding]) (pure . makeType) $ rBindingType binding

  -- Make sure binding has same number of args as binding type
  args <-
    for (zip (rBindingArgs binding) (argTypes bindingType)) $ \(argName, argType) -> do
      setValuePrimitiveType (valueNameId argName) argType
      pure (argType, argName)

  -- Get type of body expression
  body' <- typeCheckExpression (rBindingBody binding)

  -- Make sure expression type matches binding return type
  let expType = typedType body'
  expressionType' <-
    maybe (throwError [ExpectedPrimitiveType (Just $ rBindingName binding) expType]) pure $
    primitiveType expType
  let
    returnType' = returnType bindingType
  when (expressionType' /= returnType') $
    throwError [TypeMismatch (PrimitiveTy expressionType') (PrimitiveTy returnType')]

  pure
    TBinding
    { tBindingName = rBindingName binding
    , tBindingArgs = args
    , tBindingReturnType = returnType'
    , tBindingBody = body'
    }

typeCheckExpression :: RExpr -> TypeCheck (Typed TExpr)
typeCheckExpression (RELit lit) = pure $ Typed (PrimitiveTy $ literalType lit) (TELit lit)
typeCheckExpression (REVar var) = do
  ty <- lookupValueTypeOrError var
  pure $ Typed ty (TEVar var)
typeCheckExpression (REIf (RIf predicate thenExpression elseExpression)) = do
  predicate' <- typeCheckExpression predicate
  thenExpression' <- typeCheckExpression thenExpression
  elseExpression' <- typeCheckExpression elseExpression

  let
    predicateType = typedType predicate'
    thenType = typedType thenExpression'
    elseType = typedType elseExpression'

  -- Predicate needs to be Bool
  when (predicateType /= PrimitiveTy BoolType) $
    throwError [TypeMismatch predicateType (PrimitiveTy BoolType)]

  -- then/else branches need to have the same type
  when (thenType /= elseType) $
    throwError [TypeMismatch thenType elseType]

  pure $
    Typed thenType $
      TEIf
      TIf
      { tIfPredicate = predicate'
      , tIfThen = thenExpression'
      , tIfElse = elseExpression'
      }
typeCheckExpression (RELet (RLet bindings expression)) = do
  -- Add binding types to scope
  traverse_ addBindingTypeToScope bindings

  -- Type check bindings
  bindings' <- traverse typeCheckBinding bindings

  -- Type check expression
  expression' <- typeCheckExpression expression

  pure $
    Typed (typedType expression') $
    TELet
    TLet
    { tLetBindings = bindings'
    , tLetExpression = expression'
    }
typeCheckExpression (REApp app) = do
  -- Type check the function expression
  function <- typeCheckExpression $ rAppFunction app

  -- Type check the arguments
  args <- mapM typeCheckExpression $ rAppArgs app
  argTypes' <- forM args $ \arg ->
    let argType = typedType arg
    in maybe (throwError [ExpectedPrimitiveType Nothing argType]) pure $
       primitiveType argType

  -- Make sure there is the right number of arguments
  let funcType = typedType function
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
    Typed (PrimitiveTy $ functionTypeReturnType funcType') $
    TEApp
    TApp
    { tAppFunction = function
    , tAppArgs = args
    }
