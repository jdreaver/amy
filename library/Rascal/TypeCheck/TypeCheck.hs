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
typeCheckDefaultNames =
  -- TODO: Assumes Int is type ID 0. Make this more principled please!
  setPrimitiveType 0 IntType

typeCheck' :: RenamerAST -> TypeCheck TypeCheckAST
typeCheck' (RenamerAST declarations) = do
  let
    functionDeclarations = mapMaybe getFunction (toList declarations)
    getFunction (RenamerASTFunction f) = Just f

  -- Check that all declaration types exist and set types for function
  -- arguments
  declarationsWithTypedArgs <- mapM (\d -> (d,) <$> setFunctionArgTypes d) functionDeclarations

  -- Type check declarations now
  declarations' <- mapM typeCheckDeclaration declarationsWithTypedArgs

  pure $ TypeCheckAST $ TypeCheckASTFunction <$> declarations'

setFunctionArgTypes :: RenamerFunctionDeclaration -> TypeCheck FunctionType
setFunctionArgTypes declaration = do
  let
    typeNames = renamerFunctionDeclarationTypeNames declaration
    argNames = renamerFunctionDeclarationArgs declaration

    returnTypeName = NE.last typeNames
    argTypeNames = NE.init typeNames

  -- Lookup function argument types and assign these types to the arguments
  argTypes <- forM (zip argNames argTypeNames) $ \(argName, argTypeName) -> do
    primType <- lookupPrimitiveTypeOrError argTypeName
    setPrimitiveType (idNameId argName) primType
    pure $ Typed primType argName

  -- Look up return type
  returnType <- lookupPrimitiveTypeOrError returnTypeName

  -- Set function return type
  let
    funcType =
      FunctionType
      { functionTypeArgTypes = argTypes
      , functionTypeReturnType = Typed returnType returnTypeName
      }
  setFunctionType (idNameId $ renamerFunctionDeclarationName declaration) funcType
  pure funcType

typeCheckDeclaration
  :: (RenamerFunctionDeclaration, FunctionType)
  -> TypeCheck TypeCheckFunctionDeclaration
typeCheckDeclaration (declaration, funcType) = do
  -- Get type of expression
  expression <- typeCheckExpression (renamerFunctionDeclarationBody declaration)

  -- Make sure expression type matches function return type
  let
    expressionType = typedType expression
    returnType = typedType $ functionTypeReturnType funcType
  when (expressionType /= returnType) $
    throwError [TypeMismatch expressionType returnType]

  pure
    TypeCheckFunctionDeclaration
    { typeCheckFunctionDeclarationName = renamerFunctionDeclarationName declaration
    , typeCheckFunctionDeclarationType = funcType
    , typeCheckFunctionDeclarationBody = expression
    }

typeCheckExpression :: RenamerASTExpression -> TypeCheck (Typed TypeCheckASTExpression)
typeCheckExpression (RenamerASTLiteral lit) = pure $ Typed (litType lit) (TypeCheckASTLiteral lit)
 where
  litType (LiteralInt _) = IntType
typeCheckExpression (RenamerASTVariable idName) = do
  ty <- lookupPrimitiveTypeOrError idName
  pure $ Typed ty (TypeCheckASTVariable idName)
typeCheckExpression (RenamerASTFunctionApplication app) = do
  -- Compute function return type
  funcType <- lookupFunctionTypeOrError $ renamerFunctionApplicationFunctionName app

  -- Compute types of args
  args <- mapM typeCheckExpression $ renamerFunctionApplicationArgs app

  -- Make sure arg types make function types
  let
    argTypes = typedType <$> toList args
    funcArgTypes = typedType <$> functionTypeArgTypes funcType
    mismatchedTypes = fmap (uncurry TypeMismatch) . filter (uncurry (/=)) $ zip argTypes funcArgTypes
  unless (null mismatchedTypes) $
    throwError mismatchedTypes

  -- Put it all together
  let
    ty = typedType $ functionTypeReturnType funcType
    funcApp =
      TypeCheckASTFunctionApplication
      TypeCheckFunctionApplication
      { typeCheckFunctionApplicationFunctionName = renamerFunctionApplicationFunctionName app
      , typeCheckFunctionApplicationArgs = args
      }
  pure $ Typed ty funcApp
typeCheckExpression (RenamerASTExpressionParens expr) =
  fmap TypeCheckASTExpressionParens <$> typeCheckExpression expr
