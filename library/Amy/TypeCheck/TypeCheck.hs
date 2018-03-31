{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amy.TypeCheck.TypeCheck
  ( typeCheck
  ) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import qualified Data.List.NonEmpty as NE

import Amy.Errors
import Amy.Names
import Amy.Prim
import Amy.Renamer.AST
import Amy.Syntax.Located
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
  let
    name' = locatedValue $ rExternName extern
    type' = locatedValue <$> rExternType extern
  setValueType type' name'

  pure
    TExtern
    { tExternName = name'
    , tExternType = type'
    }

addBindingTypeToScope :: RBinding -> TypeCheck ()
addBindingTypeToScope binding =
  maybe
    (pure ())
    (flip setValueType (locatedValue $ rBindingName binding))
    (fmap locatedValue <$> rBindingType binding)

typeCheckBinding :: RBinding -> TypeCheck TBinding
typeCheckBinding binding = do
  -- Get binding type (all bindings must have types until we have type
  -- inference)
  bindingType <- maybe (throwError [BindingLacksTypeSignature binding]) pure $ rBindingType binding

  -- Sort out which types are for arguments and what the return type is
  let
    typeNE = typeToNonEmpty bindingType
    args = rBindingArgs binding
    (argTypes, returnTypeList) = NE.splitAt (length args) typeNE

  -- Make sure there aren't too many arguments
  returnType <-
    case NE.nonEmpty returnTypeList of
      Nothing -> throwError []-- TODO: Too many arguments to function
      Just returnTypeNE -> pure . fmap locatedValue . typeFromNonEmpty $ returnTypeNE

  -- Set types for arguments
  args' <-
    for (zip args argTypes) $ \(Located _ argName, argType) -> do
      setValueType (locatedValue <$> argType) argName
      pure $ Typed (locatedValue <$> argType) argName

  -- Get type of body expression
  body' <- typeCheckExpression (rBindingBody binding)

  -- Make sure expression type matches binding return type
  let expType = expressionType body'
  when (expType /= returnType) $
    throwError [TypeMismatch expType returnType]

  pure
    TBinding
    { tBindingName = locatedValue $ rBindingName binding
    , tBindingType = locatedValue <$> bindingType
    , tBindingArgs = args'
    , tBindingReturnType = returnType
    , tBindingBody = body'
    }

typeCheckExpression :: RExpr -> TypeCheck TExpr
typeCheckExpression (RELit lit) = pure $ TELit (locatedValue lit)
typeCheckExpression (REVar (Located _ var)) = do
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
  when (predicateType /= TVar BoolType) $
    throwError [TypeMismatch predicateType (TVar BoolType)]

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
  args <- traverse typeCheckExpression $ rAppArgs app
  let
    typedArgs = (\arg -> Typed (expressionType arg) arg) <$> args

  -- Make sure there is the right number of arguments
  let
    funcType = expressionType function
    funcTypeNE = typeToNonEmpty funcType
  (funcArgTypes, funcReturnType) <-
    if NE.length funcTypeNE == 1
    then throwError [ExpectedFunctionType funcType]
    else pure (NE.fromList (NE.init funcTypeNE), NE.last funcTypeNE)
  unless (length typedArgs == length funcArgTypes) $
    throwError [WrongNumberOfArguments (length typedArgs) (length funcArgTypes)]

  -- Make sure arg types match function types
  let
    mismatchedTypes =
      fmap (uncurry TypeMismatch)
      . NE.filter (uncurry (/=))
      . fmap (\(Typed p1 _, p2) -> (p1, p2))
      $ NE.zip typedArgs funcArgTypes
  unless (null mismatchedTypes) $
    throwError mismatchedTypes

  -- Make sure return type is primitive
  funcReturnType' <- assertPrimitiveType Nothing funcReturnType

  -- Put it all together
  pure $
    TEApp
    TApp
    { tAppFunction = function
    , tAppArgs = typedArgs
    , tAppReturnType = funcReturnType'
    }

assertPrimitiveType
  :: (MonadError [Error] m)
  => Maybe (Located ValueName)
  -> Type PrimitiveType
  -> m PrimitiveType
assertPrimitiveType mName t =
  case t of
    (TVar t') -> pure t'
    _ -> throwError [ExpectedPrimitiveType mName t]
