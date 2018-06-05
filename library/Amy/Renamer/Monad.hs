{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Renamer.Monad
  ( Renamer
  , runRenamer
  , emptyRenamerState

    -- * Values
  , addValueToScope
  , lookupValueInScope
  , lookupValueInScopeOrError

    -- * Data Constructors
  , addDataConDefinitionToScope
  , lookupDataConDefinitionInScope
  , lookupDataConInScopeOrError

    -- * Type Constructors
  , addTypeConstructorToScope
  , lookupTypeConstructorInScopeOrError

    -- * Type Declarations
  , addTypeDeclarationToScope

    -- * Type Variables
  , addTypeVariableToScope
  , lookupTypeVariableInScope
  , lookupTypeVariableInScopeOrError

    -- * Scoping
  , withNewScope

    -- * validation helpers
  , liftValidation
  ) where

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
  { renamerStateValuesInScope :: !(Map IdentName (Located IdentName))
  , renamerStateDataConDefinitionsInScope :: !(Map DataConName R.DataConDefinition)
  , renamerStateTypeConstructorsInScope :: !(Set TyConName)
  , renamerStateTypeDeclarations :: !(Map TyConName R.TypeDeclaration)
  , renamerStateTypeVariablesInScope :: !(Set TyVarName)
  } deriving (Show, Eq)

emptyRenamerState :: RenamerState
emptyRenamerState =
  RenamerState
  { renamerStateValuesInScope = primitiveFunctionNames
  , renamerStateDataConDefinitionsInScope = primitiveDataConNames
  , renamerStateTypeConstructorsInScope = primitiveTypeNames
  , renamerStateTypeDeclarations = primitiveTypeDeclNames
  , renamerStateTypeVariablesInScope = Set.empty
  }

primitiveTypeDeclarations :: [R.TypeDeclaration]
primitiveTypeDeclarations = fromPrimTypeDef <$> allPrimTypeDefinitions

allPrimTyCons :: [R.TyConDefinition]
allPrimTyCons = R.typeDeclarationTypeName <$> primitiveTypeDeclarations

allPrimDataCons :: [R.DataConDefinition]
allPrimDataCons = concatMap R.typeDeclarationConstructors primitiveTypeDeclarations

primitiveTypeDeclNames :: Map TyConName R.TypeDeclaration
primitiveTypeDeclNames =
  Map.fromList $ (\tyDef -> (R.tyConDefinitionName $ R.typeDeclarationTypeName tyDef, tyDef)) <$> primitiveTypeDeclarations

primitiveTypeNames :: Set TyConName
primitiveTypeNames =
  Set.fromList $ R.tyConDefinitionName <$> allPrimTyCons

primitiveDataConNames :: Map DataConName R.DataConDefinition
primitiveDataConNames =
  Map.fromList $ (\dataCon -> (locatedValue (R.dataConDefinitionName dataCon), dataCon)) <$> allPrimDataCons

primitiveFunctionNames :: Map IdentName (Located IdentName)
primitiveFunctionNames =
  Map.fromList $
    (\(PrimitiveFunction _ name _) ->
       ( name
       , Located (SourceSpan "" 1 1 1 1) name
       ))
    <$> allPrimitiveFunctions

addValueToScope :: Located IdentName -> Renamer (Validation [Error] (Located IdentName))
addValueToScope lName@(Located span' name) = do
  mExistingName <- lookupValueInScope name
  case mExistingName of
    Just existingName -> pure $ Failure [VariableShadowed lName existingName]
    Nothing -> do
      let located = Located span' name
      modify' (\s -> s { renamerStateValuesInScope = Map.insert name located (renamerStateValuesInScope s) })
      pure $ Success located

lookupValueInScope :: IdentName -> Renamer (Maybe (Located IdentName))
lookupValueInScope name = Map.lookup name <$> gets renamerStateValuesInScope

lookupValueInScopeOrError :: Located IdentName -> Renamer (Validation [Error] (Located IdentName))
lookupValueInScopeOrError name@(Located span' name') =
  maybe (Failure [UnknownVariable name]) (\(Located _ ident) -> Success $ Located span' ident) <$> lookupValueInScope name'

addDataConDefinitionToScope
  :: R.DataConDefinition
  -> Renamer (Validation [Error] R.DataConDefinition)
addDataConDefinitionToScope con@(R.DataConDefinition lname@(Located _ name) _) = do
  mExistingName <- lookupDataConDefinitionInScope name
  case mExistingName of
    Just existingName -> pure $ Failure [DuplicateDataConstructorName lname existingName]
    Nothing -> do
      modify' (\s -> s { renamerStateDataConDefinitionsInScope = Map.insert name con (renamerStateDataConDefinitionsInScope s) })
      pure $ Success con

lookupDataConDefinitionInScope :: DataConName -> Renamer (Maybe R.DataConDefinition)
lookupDataConDefinitionInScope name = Map.lookup name <$> gets renamerStateDataConDefinitionsInScope

lookupDataConInScopeOrError :: Located DataConName -> Renamer (Validation [Error] (Located DataConName))
lookupDataConInScopeOrError name@(Located span' name') =
  maybe (Failure [UnknownDataCon name]) (Success . mkDataCon) <$> lookupDataConDefinitionInScope name'
 where
  mkDataCon (R.DataConDefinition (Located _ name'') _) = Located span' name''

addTypeConstructorToScope :: Located TyConName -> Renamer (Validation [Error] (Located TyConName))
addTypeConstructorToScope lname@(Located _ name) = do
  exists <- isTypeConstructorInScope name
  if exists
    then pure $ Failure [TypeConstructorAlreadyExists lname]
    else do
      modify' (\s -> s { renamerStateTypeConstructorsInScope = Set.insert name (renamerStateTypeConstructorsInScope s) })
      pure $ Success lname

lookupTypeConstructorInScopeOrError :: Located TyConName -> Renamer (Validation [Error] (Located TyConName))
lookupTypeConstructorInScopeOrError lname@(Located _ name) = do
  exists <- isTypeConstructorInScope name
  if exists
    then pure $ Success lname
    else pure $ Failure [UnknownTypeConstructor lname]

isTypeConstructorInScope :: TyConName -> Renamer Bool
isTypeConstructorInScope name = Set.member name <$> gets renamerStateTypeConstructorsInScope

addTypeDeclarationToScope :: R.TypeDeclaration -> SourceSpan -> Renamer (Validation [Error] R.TypeDeclaration)
addTypeDeclarationToScope decl span' = do
  let tyConName = R.tyConDefinitionName $ R.typeDeclarationTypeName decl
  exists <- isTypeDeclarationInScope tyConName
  if exists
    then pure $ Failure [TypeConstructorAlreadyExists (Located span' $ R.tyConDefinitionName $ R.typeDeclarationTypeName decl)]
    else do
      modify' (\s -> s { renamerStateTypeDeclarations = Map.insert tyConName decl (renamerStateTypeDeclarations s) })
      pure $ Success decl

isTypeDeclarationInScope :: TyConName -> Renamer Bool
isTypeDeclarationInScope name = Map.member name <$> gets renamerStateTypeDeclarations

addTypeVariableToScope :: Located TyVarName -> Renamer (Validation [Error] (Located TyVarName))
addTypeVariableToScope lname@(Located _ name) = do
  mExistingName <- lookupTypeVariableInScope lname
  case mExistingName of
    Just _ -> pure () -- These will be set to equal during inference
    Nothing ->
      modify' (\s -> s { renamerStateTypeVariablesInScope = Set.insert name (renamerStateTypeVariablesInScope s) })
  pure $ Success lname

lookupTypeVariableInScope :: Located TyVarName -> Renamer (Maybe (Located TyVarName))
lookupTypeVariableInScope lname@(Located _ name) = do
  isMember <- Set.member name <$> gets renamerStateTypeVariablesInScope
  pure
    $ if isMember
        then Just lname
        else Nothing

lookupTypeVariableInScopeOrError :: Located TyVarName -> Renamer (Validation [Error] (Located TyVarName))
lookupTypeVariableInScopeOrError name =
  maybe (Failure [UnknownTypeVariable name]) Success <$> lookupTypeVariableInScope name

-- | Runs a 'Renamer' action in a fresh scope and restores the original scope
-- when the action is done.
withNewScope :: Renamer a -> Renamer a
withNewScope action = do
  originalState <- get
  result <- action
  modify'
    (\s -> s
      { renamerStateValuesInScope = renamerStateValuesInScope originalState
      , renamerStateDataConDefinitionsInScope = renamerStateDataConDefinitionsInScope originalState
      , renamerStateTypeVariablesInScope = renamerStateTypeVariablesInScope originalState
      }
    )
  pure result

liftValidation :: (Applicative f) => Validation err a -> (a -> f (Validation err b)) -> f (Validation err b)
liftValidation x f =
  case x of
    Failure err -> pure $ Failure err
    Success x' -> f x'
