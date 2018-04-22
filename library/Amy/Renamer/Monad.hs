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

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Validation

import Amy.Errors
import Amy.Prim
import Amy.Renamer.AST
import Amy.Syntax.Located

newtype Renamer a = Renamer (State RenamerState a)
  deriving (Functor, Applicative, Monad, MonadState RenamerState)

runRenamer :: RenamerState -> Renamer a -> a
runRenamer initialState (Renamer action) = evalState action initialState

data RenamerState
  = RenamerState
  { renamerStateLastId :: !Int
    -- ^ Last 'NameIntId' generated
  , renamerStateValues :: !(Map Text RIdent)
    -- ^ Values in scope
  } deriving (Show, Eq)

emptyRenamerState :: RenamerState
emptyRenamerState =
  RenamerState
  { renamerStateLastId = maximum (fst <$> allPrimitiveFunctionNamesAndIds) + 1
  , renamerStateValues = primitiveFunctionNames
  }

primitiveFunctionNames :: Map Text RIdent
primitiveFunctionNames =
  Map.fromList $
    (\(id', prim) ->
       ( showPrimitiveFunctionName prim
       , RIdent (showPrimitiveFunctionName prim) id' (Just prim) False
       ))
    <$> allPrimitiveFunctionNamesAndIds

-- | Generate a new 'NameIntId'
freshId :: Renamer Int
freshId = do
  modify' (\s -> s { renamerStateLastId = 1 + renamerStateLastId s })
  gets renamerStateLastId

addValueToScope :: Bool -> Located Text -> Renamer (Validation [Error] (Located RIdent))
addValueToScope isTopLevel lName@(Located span' name) = do
  nameId <- freshId
  let
    ident = RIdent name nameId Nothing isTopLevel
  mExistingName <- lookupValueInScope name
  case mExistingName of
    Just existingName -> pure $ Failure [VariableShadowed lName existingName]
    Nothing -> do
      modify' (\s -> s { renamerStateValues = Map.insert name ident (renamerStateValues s) })
      pure $ Success (Located span' ident)

lookupValueInScope :: Text -> Renamer (Maybe RIdent)
lookupValueInScope name = Map.lookup name <$> gets renamerStateValues

lookupValueInScopeOrError :: Located Text -> Renamer (Validation [Error] (Located RIdent))
lookupValueInScopeOrError name@(Located span' name') =
  maybe (Failure [UnknownVariable name]) (Success . Located span') <$> lookupValueInScope name'

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
