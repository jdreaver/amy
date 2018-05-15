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

    -- * Type Constructors
  , addTypeConstructorToScope
  , lookupTypeConstructorInScope
  , lookupTypeConstructorInScopeOrError

    -- * Type Declarations and DataConInfo
  , addTypeDeclarationToScope
  , lookupDataConInfoInScopeOrError

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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Validation

import Amy.Errors
import Amy.Prim
import Amy.Renamer.AST as R
import Amy.Syntax.AST as S

newtype Renamer a = Renamer (State RenamerState a)
  deriving (Functor, Applicative, Monad, MonadState RenamerState)

runRenamer :: RenamerState -> Renamer a -> a
runRenamer initialState (Renamer action) = evalState action initialState

data RenamerState
  = RenamerState
  { renamerStateLastId :: !Int
    -- ^ Last 'NameIntId' generated
  , renamerStateValuesInScope :: !(Map Text (Located Ident))
  , renamerStateDataConstructorsInScope :: !(Map Text R.DataConstructor)
  , renamerStateTypeConstructorsInScope :: !(Map Text R.TyConInfo)
  , renamerStateTypeDeclarations :: !(Map R.TyConInfo R.TypeDeclaration)
  , renamerStateTypeVariablesInScope :: !(Map Text R.TyVarInfo)
  } deriving (Show, Eq)

emptyRenamerState :: RenamerState
emptyRenamerState =
  RenamerState
  { renamerStateLastId = maxPrimId + 1
  , renamerStateValuesInScope = primitiveFunctionNames
  , renamerStateDataConstructorsInScope = primitiveDataConNames
  , renamerStateTypeConstructorsInScope = primitiveTypeNames
  , renamerStateTypeDeclarations = primitiveTypeDeclNames
  , renamerStateTypeVariablesInScope = Map.empty
  }


primitiveTypeDeclarations :: [R.TypeDeclaration]
primitiveTypeDeclarations = fromPrimTypeDef <$> allPrimTypeDefinitions

allPrimTyCons :: [R.TyConInfo]
allPrimTyCons = R.typeDeclarationTypeName <$> primitiveTypeDeclarations

allPrimDataCons :: [R.DataConstructor]
allPrimDataCons = concatMap R.typeDeclarationConstructors primitiveTypeDeclarations

primitiveTypeDeclNames :: Map R.TyConInfo R.TypeDeclaration
primitiveTypeDeclNames =
  Map.fromList $ (\tyDef -> (R.typeDeclarationTypeName tyDef, tyDef)) <$> primitiveTypeDeclarations

primitiveTypeNames :: Map Text R.TyConInfo
primitiveTypeNames =
  Map.fromList $ (\tyCon -> (tyConInfoText tyCon, tyCon)) <$> allPrimTyCons

primitiveDataConNames :: Map Text R.DataConstructor
primitiveDataConNames =
  Map.fromList $ (\dataCon -> (locatedValue (R.dataConstructorName dataCon), dataCon)) <$> allPrimDataCons

primitiveFunctionNames :: Map Text (Located Ident)
primitiveFunctionNames =
  Map.fromList $
    (\(PrimitiveFunction _ name id' _) ->
       ( name
       , Located (SourceSpan "" 1 1 1 1) (Ident name id')
       ))
    <$> allPrimitiveFunctions


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
      let located = Located span' ident
      modify' (\s -> s { renamerStateValuesInScope = Map.insert name located (renamerStateValuesInScope s) })
      pure $ Success located

lookupValueInScope :: Text -> Renamer (Maybe (Located Ident))
lookupValueInScope name = Map.lookup name <$> gets renamerStateValuesInScope

lookupValueInScopeOrError :: Located Text -> Renamer (Validation [Error] (Located Ident))
lookupValueInScopeOrError name@(Located span' name') =
  maybe (Failure [UnknownVariable name]) (\(Located _ ident) -> Success $ Located span' ident) <$> lookupValueInScope name'

addDataConstructorToScope
  :: Located Text
  -> Maybe S.TyConInfo
  -> Validation [Error] R.TyConInfo
  -> ConstructorSpan
  -> ConstructorIndex
  -> Renamer (Validation [Error] R.DataConstructor)
addDataConstructorToScope lname@(Located _ name) mArgTy tyCon span' index = do
  nameId <- freshId
  mExistingName <- lookupDataConstructorInScope name
  case mExistingName of
    Just existingName -> pure $ Failure [DuplicateDataConstructorName lname existingName]
    Nothing -> do
      mArgTy' <- traverse lookupTypeConstructorInScopeOrError mArgTy
      let
        cons =
          R.DataConstructor
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

lookupDataConstructorInScope :: Text -> Renamer (Maybe R.DataConstructor)
lookupDataConstructorInScope name = Map.lookup name <$> gets renamerStateDataConstructorsInScope

lookupDataConstructorInScopeOrError :: Located Text -> Renamer (Validation [Error] R.DataConstructor)
lookupDataConstructorInScopeOrError name@(Located _ name') =
  maybe (Failure [UnknownVariable name]) Success <$> lookupDataConstructorInScope name'

addTypeConstructorToScope :: S.TyConInfo -> Renamer (Validation [Error] R.TyConInfo)
addTypeConstructorToScope info@(S.TyConInfo lname@(Located span' name)) = do
  nameId <- freshId
  let
    info' = R.TyConInfo name (Just span') nameId
  mExistingName <- lookupTypeConstructorInScope info
  case mExistingName of
    Just existingName -> pure $ Failure [TypeConstructorAlreadyExists lname existingName]
    Nothing -> do
      modify' (\s -> s { renamerStateTypeConstructorsInScope = Map.insert name info' (renamerStateTypeConstructorsInScope s) })
      pure $ Success info'

lookupTypeConstructorInScope :: S.TyConInfo -> Renamer (Maybe R.TyConInfo)
lookupTypeConstructorInScope (S.TyConInfo (Located _ name)) = Map.lookup name <$> gets renamerStateTypeConstructorsInScope

lookupTypeConstructorInScopeOrError :: S.TyConInfo -> Renamer (Validation [Error] R.TyConInfo)
lookupTypeConstructorInScopeOrError info@(S.TyConInfo name) =
  maybe (Failure [UnknownTypeConstructor name]) Success <$> lookupTypeConstructorInScope info

addTypeDeclarationToScope :: Validation [Error] R.TypeDeclaration -> Renamer (Validation [Error] R.TypeDeclaration)
addTypeDeclarationToScope decl =
  case decl of
    Failure f -> pure $ Failure f
    Success decl' -> do
      let tyCon = R.typeDeclarationTypeName decl'
      modify' (\s -> s { renamerStateTypeDeclarations = Map.insert tyCon decl' (renamerStateTypeDeclarations s) })
      pure decl

lookupTypeDeclaration :: R.TyConInfo -> Renamer R.TypeDeclaration
lookupTypeDeclaration info =
  fromMaybe (error $ "Couldn't find declaration for " ++ show info)
  . Map.lookup info
  <$> gets renamerStateTypeDeclarations

lookupDataConInfoInScopeOrError :: Located Text -> Renamer (Validation [Error] DataConInfo)
lookupDataConInfoInScopeOrError dataCon = do
  dataCon' <- lookupDataConstructorInScopeOrError dataCon
  case dataCon' of
    Failure f -> pure $ Failure f
    Success dataCon'' -> do
      typeDef <- lookupTypeDeclaration (dataConstructorType dataCon'')
      pure
        $ DataConInfo
        <$> pure typeDef
        <*> pure dataCon''

addTypeVariableToScope :: S.TyVarInfo -> Renamer (Validation [Error] R.TyVarInfo)
addTypeVariableToScope info@(S.TyVarInfo (Located span' name)) = do
  nameId <- freshId
  let
    info' = R.TyVarInfo name nameId span'
  mExistingName <- lookupTypeVariableInScope info
  case mExistingName of
    Just _ -> pure () -- These will be set to equal during inference
    Nothing ->
      modify' (\s -> s { renamerStateTypeVariablesInScope = Map.insert name info' (renamerStateTypeVariablesInScope s) })
  pure $ Success info'

lookupTypeVariableInScope :: S.TyVarInfo -> Renamer (Maybe R.TyVarInfo)
lookupTypeVariableInScope (S.TyVarInfo (Located _ name)) = Map.lookup name <$> gets renamerStateTypeVariablesInScope

lookupTypeVariableInScopeOrError :: S.TyVarInfo -> Renamer (Validation [Error] R.TyVarInfo)
lookupTypeVariableInScopeOrError info@(S.TyVarInfo name) =
  maybe (Failure [UnknownTypeVariable name]) Success <$> lookupTypeVariableInScope info

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
