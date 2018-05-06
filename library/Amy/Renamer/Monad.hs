{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Renamer.Monad
  ( Renamer
  , runRenamer
  , emptyRenamerState
  , freshId

    -- * Values
  , addValueToScope
  , lookupValueInScope
  , lookupValueInScopeOrError

    -- * Data Constructors
  , addDataConstructorToScope
  , lookupDataConstructorInScope
  , lookupDataConstructorInScopeOrError

    -- * Type Constructors
  , addTypeConstructorToScope
  , lookupTypeConstructorInScope
  , lookupTypeConstructorInScopeOrError

    -- * Type Variables
  , addTypeVariableToScope
  , lookupTypeVariableInScope
  , lookupTypeVariableInScopeOrError

    -- * Scoping
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
  , renamerStateDataConstructorsInScope :: !(Map Text DataConstructor)
  , renamerStateTypeConstructorsInScope :: !(Map Text TyConInfo)
  , renamerStateTypeVariablesInScope :: !(Map Text TyVarInfo)
  } deriving (Show, Eq)

emptyRenamerState :: RenamerState
emptyRenamerState =
  RenamerState
  { renamerStateLastId = maxPrimId + 1
  , renamerStateValuesInScope = primitiveFunctionNames
  , renamerStateDataConstructorsInScope = primitiveDataConNames
  , renamerStateTypeConstructorsInScope = primitiveTypeNames
  , renamerStateTypeVariablesInScope = Map.empty
  }

primitiveFunctionNames :: Map Text Ident
primitiveFunctionNames =
  Map.fromList $
    (\(PrimitiveFunction _ name id' _) ->
       ( name
       , Ident name id'
       ))
    <$> allPrimitiveFunctions

primitiveTypeNames :: Map Text TyConInfo
primitiveTypeNames =
  let
    infos = fromPrimTyCon <$> allPrimTyCons
    nameTuples = (\tyCon -> (tyConInfoText tyCon, tyCon)) <$> infos
  in Map.fromList nameTuples

primitiveDataConNames :: Map Text DataConstructor
primitiveDataConNames =
  let
    infos = fromPrimDataCon <$> allPrimDataCons
    nameTuples = (\dataCon -> (locatedValue (dataConstructorName dataCon), dataCon)) <$> infos
  in Map.fromList nameTuples

-- | Generate a new 'NameIntId'
freshId :: Renamer Int
freshId = do
  modify' (\s -> s { renamerStateLastId = 1 + renamerStateLastId s })
  gets renamerStateLastId

addValueToScope :: Located Text -> Renamer (Validation [Error] (Located Ident))
addValueToScope lName@(Located span' name) = do
  nameId <- freshId
  let
    ident = Ident name nameId
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

addDataConstructorToScope
  :: Located Text
  -> Maybe (Located Text)
  -> Validation [Error] TyConInfo
  -> ConstructorSpan
  -> ConstructorIndex
  -> Renamer (Validation [Error] DataConstructor)
addDataConstructorToScope lname@(Located _ name) mArgTy tyCon span' index = do
  nameId <- freshId
  mExistingName <- lookupDataConstructorInScope name
  case mExistingName of
    Just existingName -> pure $ Failure [DuplicateDataConstructorName lname existingName]
    Nothing -> do
      mArgTy' <- traverse lookupTypeConstructorInScopeOrError mArgTy
      let
        cons =
          DataConstructor
          <$> pure lname
          <*> pure nameId
          <*> sequenceA mArgTy'
          <*> tyCon
          <*> pure span'
          <*> pure index
      case cons of
        Failure e -> pure $ Failure e
        Success cons' -> do
          modify' (\s -> s { renamerStateDataConstructorsInScope = Map.insert name cons' (renamerStateDataConstructorsInScope s) })
          pure $ Success cons'

lookupDataConstructorInScope :: Text -> Renamer (Maybe DataConstructor)
lookupDataConstructorInScope name = Map.lookup name <$> gets renamerStateDataConstructorsInScope

lookupDataConstructorInScopeOrError :: Located Text -> Renamer (Validation [Error] DataConstructor)
lookupDataConstructorInScopeOrError name@(Located _ name') =
  maybe (Failure [UnknownVariable name]) Success <$> lookupDataConstructorInScope name'

addTypeConstructorToScope :: Located Text -> Renamer (Validation [Error] TyConInfo)
addTypeConstructorToScope lname@(Located span' name) = do
  nameId <- freshId
  let
    info = TyConInfo name (Just span') nameId
  mExistingName <- lookupTypeConstructorInScope name
  case mExistingName of
    Just existingName -> pure $ Failure [TypeConstructorAlreadyExists lname existingName]
    Nothing -> do
      modify' (\s -> s { renamerStateTypeConstructorsInScope = Map.insert name info (renamerStateTypeConstructorsInScope s) })
      pure $ Success info

lookupTypeConstructorInScope :: Text -> Renamer (Maybe TyConInfo)
lookupTypeConstructorInScope name = Map.lookup name <$> gets renamerStateTypeConstructorsInScope

lookupTypeConstructorInScopeOrError :: Located Text -> Renamer (Validation [Error] TyConInfo)
lookupTypeConstructorInScopeOrError name@(Located _ name') =
  maybe (Failure [UnknownTypeConstructor name]) Success <$> lookupTypeConstructorInScope name'

addTypeVariableToScope :: Located Text -> Renamer (Validation [Error] TyVarInfo)
addTypeVariableToScope (Located span' name) = do
  nameId <- freshId
  let
    info = TyVarInfo name nameId span'
  mExistingName <- lookupTypeVariableInScope name
  case mExistingName of
    Just _ -> pure () -- These will be set to equal during inference
    Nothing ->
      modify' (\s -> s { renamerStateTypeVariablesInScope = Map.insert name info (renamerStateTypeVariablesInScope s) })
  pure $ Success info

lookupTypeVariableInScope :: Text -> Renamer (Maybe TyVarInfo)
lookupTypeVariableInScope name = Map.lookup name <$> gets renamerStateTypeVariablesInScope

lookupTypeVariableInScopeOrError :: Located Text -> Renamer (Validation [Error] TyVarInfo)
lookupTypeVariableInScopeOrError name@(Located _ name') =
  maybe (Failure [UnknownTypeVariable name]) Success <$> lookupTypeVariableInScope name'

-- | Runs a 'Renamer' action in a fresh scope and restores the original scope
-- when the action is done.
withNewScope :: Renamer a -> Renamer a
withNewScope action = do
  originalState <- get
  result <- action
  modify'
    (\s -> s
      { renamerStateValuesInScope = renamerStateValuesInScope originalState
      , renamerStateDataConstructorsInScope = renamerStateDataConstructorsInScope originalState
      , renamerStateTypeConstructorsInScope = renamerStateTypeConstructorsInScope originalState
      , renamerStateTypeVariablesInScope = renamerStateTypeVariablesInScope originalState
      }
    )
  pure result
