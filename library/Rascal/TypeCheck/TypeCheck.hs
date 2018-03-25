{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Rascal.TypeCheck.TypeCheck
  ( typeCheck
  ) where

import Control.Monad (forM, when)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import GHC.Exts (toList)

import Rascal.Renamer
import Rascal.TypeCheck.AST
import Rascal.TypeCheck.Monad

typeCheck :: RenamerAST -> Either [TypeCheckError] TypeCheckAST
typeCheck ast = runTypeCheck emptyTypeCheckState $ typeCheckDefaultNames >> typeCheck' ast

typeCheckDefaultNames :: TypeCheck ()
typeCheckDefaultNames = do
  -- TODO: Assumes Int is type ID 0. Make this more principled please!
  setValuePrimitiveType 0 IntType
  setValuePrimitiveType 1 DoubleType

typeCheck' :: RenamerAST -> TypeCheck TypeCheckAST
typeCheck' (RenamerAST declarations) = do
  let
    bindingDeclarations = mapMaybe getBinding (toList declarations)
    getBinding (RenamerASTBinding f) = Just f
    getBinding _ = Nothing

    externDeclarations = mapMaybe getExtern (toList declarations)
    getExtern (RenamerASTExtern f) = Just f
    getExtern _ = Nothing

  -- Check that all extern types exist
  externs <- mapM typeCheckExtern externDeclarations

  -- Check that all declaration types exist and set types for binding arguments
  declarationsWithTypedArgs <- mapM (\d -> (d,) <$> setBindingArgTypes d) bindingDeclarations

  -- Type check declarations now
  declarations' <- mapM typeCheckDeclaration declarationsWithTypedArgs

  pure $ TypeCheckAST $
    (TypeCheckASTExtern <$> externs)
    ++ (TypeCheckASTBinding <$> declarations')

setBindingArgTypes :: RenamerBindingDeclaration -> TypeCheck Type
setBindingArgTypes declaration = do
  let
    typeNames = renamerBindingDeclarationTypeNames declaration
    argNames = renamerBindingDeclarationArgs declaration

    returnTypeName = NE.last typeNames
    argTypeNames = NE.init typeNames

  -- Lookup binding argument types and assign these types to the arguments.
  -- Also ensure that binding types are primitive (no higher order functions...
  -- yet!)
  argTypes <- forM (zip argNames argTypeNames) $ \(argName, argTypeName) -> do
    argType <- lookupValuePrimitiveTypeOrError argTypeName
    setValuePrimitiveType (idNameId argName) argType
    pure argType

  -- Look up return type and ensure it is primitive
  returnType <- lookupValuePrimitiveTypeOrError returnTypeName

  -- Set binding return type
  let
    bindingType = Type $ NE.fromList $ argTypes ++ [returnType]
  setValueType (idNameId $ renamerBindingDeclarationName declaration) bindingType
  pure bindingType

typeCheckExtern
  :: RenamerExternDeclaration
  -> TypeCheck TypeCheckExternDeclaration
typeCheckExtern declaration = do
  -- Validate types
  type' <- mapM lookupValuePrimitiveTypeOrError $ renamerExternDeclarationTypeNames declaration

  -- Set extern return type
  let
    externType = Type type'
  setValueType (idNameId $ renamerExternDeclarationName declaration) externType

  pure
    TypeCheckExternDeclaration
    { typeCheckExternDeclarationName = renamerExternDeclarationName declaration
    , typeCheckExternDeclarationType = externType
    }

typeCheckDeclaration
  :: (RenamerBindingDeclaration, Type)
  -> TypeCheck TypeCheckBindingDeclaration
typeCheckDeclaration (declaration, bindingType) = do
  -- Get type of expression
  expression <- typeCheckExpression (renamerBindingDeclarationBody declaration)

  -- Make sure expression type matches binding return type
  let
    expressionType = typedType expression
    returnType = bindingReturnType bindingType
  when (expressionType /= returnType) $
    throwError [TypeMismatch (unPrimitiveType expressionType) (unPrimitiveType returnType)]

  pure
    TypeCheckBindingDeclaration
    { typeCheckBindingDeclarationName = renamerBindingDeclarationName declaration
    , typeCheckBindingDeclarationArgs = renamerBindingDeclarationArgs declaration
    , typeCheckBindingDeclarationType = bindingType
    , typeCheckBindingDeclarationBody = expression
    }

typeCheckExpression :: RenamerASTExpression -> TypeCheck (Typed TypeCheckASTExpression)
typeCheckExpression (RenamerASTLiteral lit) = pure $ Typed (litType lit) (TypeCheckASTLiteral lit)
 where
  litType (LiteralInt _) = IntType
  litType (LiteralDouble _) = DoubleType
typeCheckExpression (RenamerASTVariable idName) = do
  ty <- lookupValuePrimitiveTypeOrError idName
  pure $ Typed ty (TypeCheckASTVariable idName)
typeCheckExpression (RenamerASTFunctionApplication app) = do
  -- Compute function return type
  funcType <- lookupValueFunctionTypeOrError $ renamerFunctionApplicationFunctionName app

  -- Compute types of args
  args <- mapM typeCheckExpression $ renamerFunctionApplicationArgs app

  let
    funcName = renamerFunctionApplicationFunctionName app
    argTypes = unPrimitiveType . typedType <$> args
    funcArgTypes = unPrimitiveType <$> functionTypeArgTypes funcType

  -- Make sure there is the right number of arguments
  unless (length argTypes == length funcArgTypes) $
    throwError [WrongNumberOfArguments funcName (length funcArgTypes) (length argTypes)]

  -- Make sure arg types make function types
  let
    mismatchedTypes = fmap (uncurry TypeMismatch) . NE.filter (uncurry (/=)) $ NE.zip argTypes funcArgTypes
  unless (null mismatchedTypes) $
    throwError mismatchedTypes

  -- Put it all together
  let
    ty = functionTypeReturnType funcType
    funcApp =
      TypeCheckASTFunctionApplication
      TypeCheckFunctionApplication
      { typeCheckFunctionApplicationFunctionName = funcName
      , typeCheckFunctionApplicationArgs = args
      }
  pure $ Typed ty funcApp
typeCheckExpression (RenamerASTExpressionParens expr) =
  fmap TypeCheckASTExpressionParens <$> typeCheckExpression expr
