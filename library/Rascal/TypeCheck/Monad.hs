{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rascal.TypeCheck.Monad
  ( TypeCheck
  , runTypeCheck
  , TypeCheckError(..)
  , emptyTypeCheckState
  , setValueType
  , setValuePrimitiveType
  , lookupValueType
  , lookupValueTypeOrError
  , lookupValuePrimitiveTypeOrError
  , lookupValueFunctionTypeOrError
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
  = TypeMismatch !Type !Type
  | UnknownType !IdName
  | CantFindType !IdName
  | WrongNumberOfArguments !IdName !Int !Int
  | ExpectedPrimitiveType !IdName !Type
  | ExpectedFunctionType !IdName !Type
  deriving (Show, Eq)

data TypeCheckState
  = TypeCheckState
  { typeCheckStateValueTypeMap :: Map NameId Type
    -- ^ Type for all values
  } deriving (Show, Eq)

emptyTypeCheckState :: TypeCheckState
emptyTypeCheckState =
  TypeCheckState
  { typeCheckStateValueTypeMap = Map.empty
  }

setValueType :: NameId -> Type -> TypeCheck ()
setValueType nameId ty = do
  mExistingType <- lookupValueType nameId
  case mExistingType of
    Just ty' -> throwError [TypeMismatch ty ty']
    Nothing ->
      modify' (\s -> s { typeCheckStateValueTypeMap = Map.insert nameId ty (typeCheckStateValueTypeMap s) })

setValuePrimitiveType :: NameId -> PrimitiveType -> TypeCheck ()
setValuePrimitiveType nameId = setValueType nameId . unPrimitiveType

lookupValueType :: NameId -> TypeCheck (Maybe Type)
lookupValueType nameId = Map.lookup nameId <$> gets typeCheckStateValueTypeMap

lookupValueTypeOrError :: IdName -> TypeCheck Type
lookupValueTypeOrError idName =
  lookupValueType (idNameId idName) >>= maybe (throwError [err]) pure
 where
  err = UnknownType idName

lookupValuePrimitiveTypeOrError :: IdName -> TypeCheck PrimitiveType
lookupValuePrimitiveTypeOrError idName = do
  ty <- lookupValueTypeOrError idName
  maybe (throwError [ExpectedPrimitiveType idName ty]) pure $ primitiveType ty

lookupValueFunctionTypeOrError :: IdName -> TypeCheck FunctionType
lookupValueFunctionTypeOrError idName = do
  ty <- lookupValueTypeOrError idName
  maybe (throwError [ExpectedFunctionType idName ty]) pure $ functionType ty
