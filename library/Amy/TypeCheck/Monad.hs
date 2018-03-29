{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.TypeCheck.Monad
  ( TypeCheck
  , runTypeCheck
  , TypeCheckError(..)
  , emptyTypeCheckState
  , setValueType
  , setValuePrimitiveType
  , lookupValueType
  , lookupValueTypeOrError
  , module Control.Monad.Except
  , module Control.Monad.State.Strict
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Amy.Names
import Amy.Prim
import Amy.Type

newtype TypeCheck a = TypeCheck (ExceptT [TypeCheckError] (State TypeCheckState) a)
  deriving (Functor, Applicative, Monad, MonadState TypeCheckState, MonadError [TypeCheckError])

runTypeCheck :: TypeCheckState -> TypeCheck a -> Either [TypeCheckError] a
runTypeCheck initialState (TypeCheck action) = evalState (runExceptT action) initialState

data TypeCheckError
  = TypeMismatch !Type !Type
  | UnknownTypeName !Text
  | CantFindType !ValueName
  | WrongNumberOfArguments !Int !Int
  | ExpectedPrimitiveType !(Maybe ValueName) !Type
  | ExpectedFunctionType !Type
  deriving (Show, Eq)

data TypeCheckState
  = TypeCheckState
  { typeCheckStateValueTypeMap :: Map NameId Type
    -- ^ Type for all values
  } deriving (Show, Eq)

emptyTypeCheckState :: TypeCheckState
emptyTypeCheckState =
  TypeCheckState
  { typeCheckStateValueTypeMap = primitiveFunctionTypes
  }

primitiveFunctionTypes :: Map NameId Type
primitiveFunctionTypes =
  Map.fromList $ (\prim -> (PrimitiveFunctionId prim, mkType prim)) <$> allPrimitiveFunctionNames
 where
  mkType prim = makeType $ primitiveFunctionType $ primitiveFunction prim

setValueType :: NameId -> Type -> TypeCheck ()
setValueType nameId ty = do
  mExistingType <- lookupValueType nameId
  case mExistingType of
    Just ty' -> throwError [TypeMismatch ty ty']
    Nothing ->
      modify' (\s -> s { typeCheckStateValueTypeMap = Map.insert nameId ty (typeCheckStateValueTypeMap s) })

setValuePrimitiveType :: NameId -> PrimitiveType -> TypeCheck ()
setValuePrimitiveType nameId = setValueType nameId . PrimitiveTy

lookupValueType :: NameId -> TypeCheck (Maybe Type)
lookupValueType nameId = Map.lookup nameId <$> gets typeCheckStateValueTypeMap

lookupValueTypeOrError :: ValueName -> TypeCheck Type
lookupValueTypeOrError valueName =
  lookupValueType (valueNameId valueName) >>= maybe (throwError [err]) pure
 where
  err = CantFindType valueName