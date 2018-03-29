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
  let expType = expressionType body'
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

typeCheckExpression :: RExpr -> TypeCheck TExpr
typeCheckExpression (RELit lit) = pure $ TELit lit
typeCheckExpression (REVar var) = do
  ty <- lookupValueTypeOrError var
  pure $ TEVar $ Typed ty var
typeCheckExpression (REIf (RIf predicate thenExpression elseExpression)) = do
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
  typedArgs <- forM args $ \arg -> do
    let argType = expressionType arg
    ty <- maybe (throwError [ExpectedPrimitiveType Nothing argType]) pure $ primitiveType argType
    pure (ty, arg)

  -- Make sure there is the right number of arguments
  let funcType = expressionType function
  funcType' <-
    case funcType of
      FunctionTy ft -> pure ft
      _ -> throwError [ExpectedFunctionType funcType]
  let
    funcArgTypes = functionTypeArgTypes funcType'
  unless (length typedArgs == length funcArgTypes) $
    throwError [WrongNumberOfArguments (length typedArgs) (length funcArgTypes)]

  -- Make sure arg types make function types
  let
    mismatchedTypes =
      fmap (\(p1, p2) -> TypeMismatch (PrimitiveTy p1) (PrimitiveTy p2))
      . NE.filter (uncurry (/=))
      . fmap (\((p1, _), p2) -> (p1, p2))
      $ NE.zip typedArgs funcArgTypes
  unless (null mismatchedTypes) $
    throwError mismatchedTypes

  -- Put it all together
  pure $
    TEApp
    TApp
    { tAppFunction = function
    , tAppArgs = typedArgs
    , tAppReturnType = functionTypeReturnType funcType'
    }
