{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Renamer.Monad
  ( Renamer
  , runRenamer
  , emptyRenamerState
  , freshId
  , addValueToScope
  , lookupValueInScope
  , lookupValueInScopeOrError
  , addTypeToScope
  , lookupTypeInScope
  , lookupTypeInScopeOrError
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
  , renamerStateValuesInScope :: !(Map Text Ident)
  , renamerStateTypesInScope :: !(Map Text TypeName)
  } deriving (Show, Eq)

emptyRenamerState :: RenamerState
emptyRenamerState =
  RenamerState
  { renamerStateLastId = maximum (fst <$> allPrimitiveFunctionNamesAndIds) + 1
  , renamerStateValuesInScope = primitiveFunctionNames
  , renamerStateTypesInScope = Map.empty
  }

primitiveFunctionNames :: Map Text Ident
primitiveFunctionNames =
  Map.fromList $
    (\(id', prim) ->
       ( showPrimitiveFunctionName prim
       , Ident (showPrimitiveFunctionName prim) id' (Just prim)
       ))
    <$> allPrimitiveFunctionNamesAndIds

-- | Generate a new 'NameIntId'
freshId :: Renamer Int
freshId = do
  modify' (\s -> s { renamerStateLastId = 1 + renamerStateLastId s })
  gets renamerStateLastId

addValueToScope :: Located Text -> Renamer (Validation [Error] (Located Ident))
addValueToScope lName@(Located span' name) = do
  nameId <- freshId
  let
    ident = Ident name nameId Nothing
  mExistingName <- lookupValueInScope name
  case mExistingName of
    Just existingName -> pure $ Failure [VariableShadowed lName existingName]
    Nothing -> do
      modify' (\s -> s { renamerStateValuesInScope = Map.insert name ident (renamerStateValuesInScope s) })
      pure $ Success (Located span' ident)

lookupValueInScope :: Text -> Renamer (Maybe Ident)
lookupValueInScope name = Map.lookup name <$> gets renamerStateValuesInScope

lookupValueInScopeOrError :: Located Text -> Renamer (Validation [Error] (Located Ident))
lookupValueInScopeOrError name@(Located span' name') =
  maybe (Failure [UnknownVariable name]) (Success . Located span') <$> lookupValueInScope name'

addTypeToScope :: Located Text -> Renamer (Validation [Error] TypeName)
addTypeToScope (Located span' name) = do
  nameId <- freshId
  let
    tname = TypeName name span' nameId Nothing
  mExistingName <- lookupTypeInScope name
  case mExistingName of
    Just _ -> pure () -- These will be set to equal during inference
    Nothing ->
      modify' (\s -> s { renamerStateTypesInScope = Map.insert name tname (renamerStateTypesInScope s) })
  pure $ Success tname

lookupTypeInScope :: Text -> Renamer (Maybe TypeName)
lookupTypeInScope name = Map.lookup name <$> gets renamerStateTypesInScope

lookupTypeInScopeOrError :: Located Text -> Renamer (Validation [Error] TypeName)
lookupTypeInScopeOrError name@(Located _ name') =
  maybe (Failure [UnknownTypeName name]) Success <$> lookupTypeInScope name'

-- | Runs a 'Renamer' action in a fresh scope and restores the original scope
-- when the action is done.
withNewScope :: Renamer a -> Renamer a
withNewScope action = do
  originalState <- get
  result <- action
  modify'
    (\s -> s
      { renamerStateValuesInScope = renamerStateValuesInScope originalState
      , renamerStateTypesInScope = renamerStateTypesInScope originalState
      }
    )
  pure result
