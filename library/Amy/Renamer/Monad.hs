{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Renamer.Monad
  ( Renamer
  , runRenamer
  , emptyRenamerState
  , freshId
  , addValueToScope
  , lookupValueInScope
  , lookupValueInScopeOrError
  , withNewScope
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Amy.Errors
import Amy.Names
import Amy.Prim

newtype Renamer a = Renamer (ExceptT [Error] (State RenamerState) a)
  deriving (Functor, Applicative, Monad, MonadState RenamerState, MonadError [Error])

runRenamer :: RenamerState -> Renamer a -> Either [Error] a
runRenamer initialState (Renamer action) = evalState (runExceptT action) initialState

data RenamerState
  = RenamerState
  { renamerStateLastId :: !NameIntId
    -- ^ Last 'NameIntId' generated
  , renamerStateValues :: !(Map Text ValueName)
    -- ^ Values in scope
  } deriving (Show, Eq)

emptyRenamerState :: RenamerState
emptyRenamerState =
  RenamerState
  { renamerStateLastId = -1
  , renamerStateValues = primitiveFunctionNames
  }

primitiveFunctionNames :: Map Text ValueName
primitiveFunctionNames =
  Map.fromList $
    (\prim ->
       ( showPrimitiveFunctionName prim
       , ValueName (showPrimitiveFunctionName prim) (PrimitiveFunctionId prim)
       ))
    <$> allPrimitiveFunctionNames

-- | Generate a new 'NameIntId'
freshId :: Renamer NameId
freshId = do
  modify' (\s -> s { renamerStateLastId = 1 + renamerStateLastId s })
  NameIntId <$> gets renamerStateLastId

addValueToScope :: Text -> Renamer ValueName
addValueToScope name = do
  nameId <- freshId
  let valueName = ValueName name nameId
  mExistingId <- lookupValueInScope name
  case mExistingId of
    Just nid -> throwError [VariableShadowed name nid]
    Nothing -> modify' (\s -> s { renamerStateValues = Map.insert name valueName (renamerStateValues s) })
  pure valueName

lookupValueInScope :: Text -> Renamer (Maybe ValueName)
lookupValueInScope name = Map.lookup name <$> gets renamerStateValues

lookupValueInScopeOrError :: Text -> Renamer ValueName
lookupValueInScopeOrError name =
  lookupValueInScope name >>= maybe (throwError [UnknownVariable name]) pure

-- | Runs a 'Renamer' action in a fresh scope and restores the original scope
-- when the action is done.
withNewScope :: Renamer a -> Renamer a
withNewScope action = do
  originalState <- get
  result <- action
  modify'
    (\s -> s
      { renamerStateValues = renamerStateValues originalState
      }
    )
  pure result
