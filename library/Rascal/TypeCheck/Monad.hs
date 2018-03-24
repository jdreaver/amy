{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rascal.TypeCheck.Monad
  ( TypeCheck
  , runTypeCheck
  , TypeCheckError(..)
  , emptyTypeCheckState
  , setPrimitiveType
  , setFunctionType
  , lookupPrimitiveType
  , lookupPrimitiveTypeOrError
  , lookupFunctionType
  , lookupFunctionTypeOrError
  , module Control.Monad.Except
  , module Control.Monad.State.Strict
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Rascal.Renamer
import Rascal.TypeCheck.AST

newtype TypeCheck a = TypeCheck (ExceptT [TypeCheckError] (State TypeCheckState) a)
  deriving (Functor, Applicative, Monad, MonadState TypeCheckState, MonadError [TypeCheckError])

runTypeCheck :: TypeCheckState -> TypeCheck a -> Either [TypeCheckError] a
runTypeCheck initialState (TypeCheck action) = evalState (runExceptT action) initialState

data TypeCheckError
  = TypeMismatch !PrimitiveType !PrimitiveType
  | FunctionTypeMismatch !FunctionType !FunctionType -- Does this make sense? How could this happen?
  | UnknownType !IdName
  | CantFindType !IdName
  deriving (Show, Eq)

data TypeCheckState
  = TypeCheckState
  { -- TODO: The distinction between primitive types and function types might
    -- be muddied with proper currying
    typeCheckStatePrimitiveTypeMap :: Map NameId PrimitiveType
    -- ^ Type for all values
  , typeCheckStateFunctionTypeMap :: Map NameId FunctionType
    -- ^ Type for all functions
  } deriving (Show, Eq)

emptyTypeCheckState :: TypeCheckState
emptyTypeCheckState =
  TypeCheckState
  { typeCheckStatePrimitiveTypeMap = Map.empty
  , typeCheckStateFunctionTypeMap = Map.empty
  }

setPrimitiveType :: NameId -> PrimitiveType -> TypeCheck ()
setPrimitiveType nameId ty = do
  mExistingType <- lookupPrimitiveType nameId
  case mExistingType of
    Just ty' -> throwError [TypeMismatch ty ty']
    Nothing ->
      modify' (\s -> s { typeCheckStatePrimitiveTypeMap = Map.insert nameId ty (typeCheckStatePrimitiveTypeMap s) })

setFunctionType :: NameId -> FunctionType -> TypeCheck ()
setFunctionType nameId ty = do
  mExistingType <- lookupFunctionType nameId
  case mExistingType of
    Just ty' -> throwError [FunctionTypeMismatch ty ty']
    Nothing ->
      modify' (\s -> s { typeCheckStateFunctionTypeMap = Map.insert nameId ty (typeCheckStateFunctionTypeMap s) })

lookupPrimitiveType :: NameId -> TypeCheck (Maybe PrimitiveType)
lookupPrimitiveType nameId = Map.lookup nameId <$> gets typeCheckStatePrimitiveTypeMap

lookupPrimitiveTypeOrError :: IdName -> TypeCheck PrimitiveType
lookupPrimitiveTypeOrError idName =
  lookupPrimitiveType (idNameId idName) >>= maybe (throwError [err]) pure
 where
  err = UnknownType idName

lookupFunctionType :: NameId -> TypeCheck (Maybe FunctionType)
lookupFunctionType nameId = Map.lookup nameId <$> gets typeCheckStateFunctionTypeMap

lookupFunctionTypeOrError :: IdName -> TypeCheck FunctionType
lookupFunctionTypeOrError idName =
  lookupFunctionType (idNameId idName) >>= maybe (throwError [err]) pure
 where
  err = CantFindType idName
