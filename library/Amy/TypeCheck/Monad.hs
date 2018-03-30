{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.TypeCheck.Monad
  ( TypeCheck
  , runTypeCheck
  , emptyTypeCheckState
  , setValueType
  , lookupValueType
  , lookupValueTypeOrError
  , module Control.Monad.Except
  , module Control.Monad.State.Strict
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Amy.Errors
import Amy.Names
import Amy.Prim
import Amy.Type

newtype TypeCheck a = TypeCheck (ExceptT [Error] (State TypeCheckState) a)
  deriving (Functor, Applicative, Monad, MonadState TypeCheckState, MonadError [Error])

runTypeCheck :: TypeCheckState -> TypeCheck a -> Either [Error] a
runTypeCheck initialState (TypeCheck action) = evalState (runExceptT action) initialState

data TypeCheckState
  = TypeCheckState
  { typeCheckStateValueTypeMap :: Map NameId (Type PrimitiveType)
    -- ^ Type for all values
  } deriving (Show, Eq)

emptyTypeCheckState :: TypeCheckState
emptyTypeCheckState =
  TypeCheckState
  { typeCheckStateValueTypeMap = primitiveFunctionTypes
  }

primitiveFunctionTypes :: Map NameId (Type PrimitiveType)
primitiveFunctionTypes =
  Map.fromList $ (\prim -> (PrimitiveFunctionId prim, mkType prim)) <$> allPrimitiveFunctionNames
 where
  mkType prim = typeFromNonEmpty . fmap TVar . primitiveFunctionType $ primitiveFunction prim

setValueType :: NameId -> Type PrimitiveType -> TypeCheck ()
setValueType nameId ty = do
  mExistingType <- lookupValueType nameId
  case mExistingType of
    Just ty' -> throwError [TypeMismatch ty ty']
    Nothing ->
      modify' (\s -> s { typeCheckStateValueTypeMap = Map.insert nameId ty (typeCheckStateValueTypeMap s) })

lookupValueType :: NameId -> TypeCheck (Maybe (Type PrimitiveType))
lookupValueType nameId = Map.lookup nameId <$> gets typeCheckStateValueTypeMap

lookupValueTypeOrError :: ValueName -> TypeCheck (Type PrimitiveType)
lookupValueTypeOrError valueName =
  lookupValueType (valueNameId valueName) >>= maybe (throwError [err]) pure
 where
  err = CantFindType valueName
