{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Renamer.Monad
  ( Renamer
  , runRenamer
  , emptyRenamerState
  , RenamerError(..)
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

import Amy.Names

newtype Renamer a = Renamer (ExceptT [RenamerError] (State RenamerState) a)
  deriving (Functor, Applicative, Monad, MonadState RenamerState, MonadError [RenamerError])

runRenamer :: RenamerState -> Renamer a -> Either [RenamerError] a
runRenamer initialState (Renamer action) = evalState (runExceptT action) initialState

data RenamerError
  = TypeSignatureLacksBinding !Text
  | BindingLacksTypeSignature !Text
  | UnknownVariable !Text
  | VariableShadowed !Text !ValueName
  deriving (Show, Eq)

data RenamerState
  = RenamerState
  { renamerStateLastId :: !NameId
    -- ^ Last 'NameId' generated
  , renamerStateValues :: !(Map Text ValueName)
    -- ^ Values in scope
  } deriving (Show, Eq)

emptyRenamerState :: RenamerState
emptyRenamerState =
  RenamerState
  { renamerStateLastId = -1
  , renamerStateValues = Map.empty
  }

-- | Generate a new 'NameId'
freshId :: Renamer NameId
freshId = do
  modify' (\s -> s { renamerStateLastId = 1 + renamerStateLastId s })
  gets renamerStateLastId

addValueToScope :: ValueNameProvenance -> Text -> Renamer ValueName
addValueToScope provenance name = do
  nameId <- freshId
  let valueName = ValueName name nameId provenance
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
