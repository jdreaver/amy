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
import Amy.Syntax.Located
import Amy.Type

newtype TypeCheck a = TypeCheck (ExceptT [Error] (State TypeCheckState) a)
  deriving (Functor, Applicative, Monad, MonadState TypeCheckState, MonadError [Error])

runTypeCheck :: TypeCheckState -> TypeCheck a -> Either [Error] a
runTypeCheck initialState (TypeCheck action) = evalState (runExceptT action) initialState

data TypeCheckState
  = TypeCheckState
  { typeCheckStateValueTypeMap :: Map ValueName (Type PrimitiveType)
    -- ^ Type for all values
  } deriving (Show, Eq)

emptyTypeCheckState :: TypeCheckState
emptyTypeCheckState =
  TypeCheckState
  { typeCheckStateValueTypeMap = primitiveFunctionTypes
  }

primitiveFunctionTypes :: Map ValueName (Type PrimitiveType)
primitiveFunctionTypes =
  Map.fromList
    $ (\prim -> (ValueName (showPrimitiveFunctionName prim) (PrimitiveFunctionId prim), mkType prim))
    <$> allPrimitiveFunctionNames
 where
  mkType prim = typeFromNonEmpty . fmap TyCon . primitiveFunctionType $ primitiveFunction prim

setValueType :: Type PrimitiveType -> ValueName -> TypeCheck ()
setValueType ty valueName = do
  mExistingType <- lookupValueType valueName
  case mExistingType of
    Just ty' -> throwError [TypeMismatch ty ty']
    Nothing ->
      modify' (\s -> s { typeCheckStateValueTypeMap = Map.insert valueName ty (typeCheckStateValueTypeMap s) })

lookupValueType :: ValueName -> TypeCheck (Maybe (Type PrimitiveType))
lookupValueType valueName = Map.lookup valueName <$> gets typeCheckStateValueTypeMap

lookupValueTypeOrError :: Located ValueName -> TypeCheck (Type PrimitiveType)
lookupValueTypeOrError valueName =
  lookupValueType (locatedValue valueName) >>= maybe (throwError [err]) pure
 where
  err = CantFindType valueName
