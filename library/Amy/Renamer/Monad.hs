{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Renamer.Monad
  ( Renamer
  , runRenamer
  , emptyRenamerState
  , freshId
  , addValueToScope
  , lookupValueInScope
  , lookupValueInScopeOrError
  , addTypeConstructorToScope
  , lookupTypeConstructorInScope
  , lookupTypeConstructorInScopeOrError
  , addTypeVariableToScope
  , lookupTypeVariableInScope
  , lookupTypeVariableInScopeOrError
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
  , renamerStateTypeConstructorsInScope :: !(Map Text TypeName)
  , renamerStateTypeVariablesInScope :: !(Map Text TypeName)
  } deriving (Show, Eq)

emptyRenamerState :: RenamerState
emptyRenamerState =
  RenamerState
  { renamerStateLastId = maximum (fst <$> allPrimitiveFunctionNamesAndIds) + 1
  , renamerStateValuesInScope = primitiveFunctionNames
  , renamerStateTypeConstructorsInScope = primitiveTypeNames
  , renamerStateTypeVariablesInScope = Map.empty
  }

primitiveFunctionNames :: Map Text Ident
primitiveFunctionNames =
  Map.fromList $
    (\(id', prim) ->
       ( showPrimitiveFunctionName prim
       , Ident (showPrimitiveFunctionName prim) id' ValueName (Just prim)
       ))
    <$> allPrimitiveFunctionNamesAndIds

primitiveTypeNames :: Map Text TypeName
primitiveTypeNames =
  Map.fromList $
    (\(id', prim) ->
       ( showPrimitiveType prim
       , TypeName (showPrimitiveType prim) Nothing id' (Just prim)
       )
    )
    <$> allPrimitiveTypesAndIds

-- | Generate a new 'NameIntId'
freshId :: Renamer Int
freshId = do
  modify' (\s -> s { renamerStateLastId = 1 + renamerStateLastId s })
  gets renamerStateLastId

addValueToScope :: Namespace -> Located Text -> Renamer (Validation [Error] (Located Ident))
addValueToScope namespace lName@(Located span' name) = do
  nameId <- freshId
  let
    ident = Ident name nameId namespace Nothing
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

addTypeConstructorToScope :: Located Text -> Renamer (Validation [Error] TypeName)
addTypeConstructorToScope lname@(Located span' name) = do
  nameId <- freshId
  let
    tname = TypeName name (Just span') nameId Nothing
  mExistingName <- lookupTypeConstructorInScope name
  case mExistingName of
    Just existingName -> pure $ Failure [TypeNameAlreadyExists lname existingName]
    Nothing -> do
      modify' (\s -> s { renamerStateTypeConstructorsInScope = Map.insert name tname (renamerStateTypeConstructorsInScope s) })
      pure $ Success tname

lookupTypeConstructorInScope :: Text -> Renamer (Maybe TypeName)
lookupTypeConstructorInScope name = Map.lookup name <$> gets renamerStateTypeConstructorsInScope

lookupTypeConstructorInScopeOrError :: Located Text -> Renamer (Validation [Error] TypeName)
lookupTypeConstructorInScopeOrError name@(Located _ name') =
  maybe (Failure [UnknownTypeName name]) Success <$> lookupTypeConstructorInScope name'

addTypeVariableToScope :: Located Text -> Renamer (Validation [Error] TypeName)
addTypeVariableToScope (Located span' name) = do
  nameId <- freshId
  let
    tname = TypeName name (Just span') nameId Nothing
  mExistingName <- lookupTypeVariableInScope name
  case mExistingName of
    Just _ -> pure () -- These will be set to equal during inference
    Nothing ->
      modify' (\s -> s { renamerStateTypeVariablesInScope = Map.insert name tname (renamerStateTypeVariablesInScope s) })
  pure $ Success tname

lookupTypeVariableInScope :: Text -> Renamer (Maybe TypeName)
lookupTypeVariableInScope name = Map.lookup name <$> gets renamerStateTypeVariablesInScope

lookupTypeVariableInScopeOrError :: Located Text -> Renamer (Validation [Error] TypeName)
lookupTypeVariableInScopeOrError name@(Located _ name') =
  maybe (Failure [UnknownTypeName name]) Success <$> lookupTypeVariableInScope name'

-- | Runs a 'Renamer' action in a fresh scope and restores the original scope
-- when the action is done.
withNewScope :: Renamer a -> Renamer a
withNewScope action = do
  originalState <- get
  result <- action
  modify'
    (\s -> s
      { renamerStateValuesInScope = renamerStateValuesInScope originalState
      , renamerStateTypeConstructorsInScope = renamerStateTypeConstructorsInScope originalState
      , renamerStateTypeVariablesInScope = renamerStateTypeVariablesInScope originalState
      }
    )
  pure result
