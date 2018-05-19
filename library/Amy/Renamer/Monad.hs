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
  , addTypeDefinitionToScope
  , lookupTypeConstructorInScope
  , lookupTypeConstructorInScopeOrError
  , lookupTypeTerm

    -- * Type Declarations
  , addTypeDeclarationToScope
  , lookupTypeDeclaration

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
  { renamerStateValuesInScope :: !(Map Text (Located Ident))
  , renamerStateDataConDefinitionsInScope :: !(Map Text R.DataConDefinition)
  , renamerStateTypeConstructorsInScope :: !(Map Text R.TyConDefinition)
  , renamerStateTypeDeclarations :: !(Map Text R.TypeDeclaration)
  , renamerStateTypeVariablesInScope :: !(Map Text R.TyVarInfo)
  } deriving (Show, Eq)

emptyRenamerState :: RenamerState
emptyRenamerState =
  RenamerState
  { renamerStateValuesInScope = primitiveFunctionNames
  , renamerStateDataConDefinitionsInScope = primitiveDataConNames
  , renamerStateTypeConstructorsInScope = primitiveTypeNames
  , renamerStateTypeDeclarations = primitiveTypeDeclNames
  , renamerStateTypeVariablesInScope = Map.empty
  }


primitiveTypeDeclarations :: [R.TypeDeclaration]
primitiveTypeDeclarations = fromPrimTypeDef <$> allPrimTypeDefinitions

allPrimTyCons :: [R.TyConDefinition]
allPrimTyCons = R.typeDeclarationTypeName <$> primitiveTypeDeclarations

allPrimDataCons :: [R.DataConDefinition]
allPrimDataCons = concatMap R.typeDeclarationConstructors primitiveTypeDeclarations

primitiveTypeDeclNames :: Map Text R.TypeDeclaration
primitiveTypeDeclNames =
  Map.fromList $ (\tyDef -> (R.tyConDefinitionName $ R.typeDeclarationTypeName tyDef, tyDef)) <$> primitiveTypeDeclarations

primitiveTypeNames :: Map Text R.TyConDefinition
primitiveTypeNames =
  Map.fromList $ (\tyCon -> (R.tyConDefinitionName tyCon, tyCon)) <$> allPrimTyCons

primitiveDataConNames :: Map Text R.DataConDefinition
primitiveDataConNames =
  Map.fromList $ (\dataCon -> (locatedValue (R.dataConDefinitionName dataCon), dataCon)) <$> allPrimDataCons

primitiveFunctionNames :: Map Text (Located Ident)
primitiveFunctionNames =
  Map.fromList $
    (\(PrimitiveFunction _ name _) ->
       ( name
       , Located (SourceSpan "" 1 1 1 1) (Ident name)
       ))
    <$> allPrimitiveFunctions

addValueToScope :: Located Text -> Renamer (Validation [Error] (Located Ident))
addValueToScope lName@(Located span' name) = do
  let
    ident = Ident name
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

addDataConDefinitionToScope
  :: Located Text
  -> Maybe R.TypeTerm
  -> Renamer (Validation [Error] R.DataConDefinition)
addDataConDefinitionToScope lname@(Located _ name) mArgTy = do
  mExistingName <- lookupDataConDefinitionInScope name
  let
    con =
      R.DataConDefinition
      { R.dataConDefinitionName = lname
      , R.dataConDefinitionArgument = mArgTy
      }
  case mExistingName of
    Just existingName -> pure $ Failure [DuplicateDataConstructorName lname existingName]
    Nothing -> do
      modify' (\s -> s { renamerStateDataConDefinitionsInScope = Map.insert name con (renamerStateDataConDefinitionsInScope s) })
      pure $ Success con

lookupDataConDefinitionInScope :: Text -> Renamer (Maybe R.DataConDefinition)
lookupDataConDefinitionInScope name = Map.lookup name <$> gets renamerStateDataConDefinitionsInScope

lookupDataConInScopeOrError :: Located Text -> Renamer (Validation [Error] R.DataCon)
lookupDataConInScopeOrError name@(Located _ name') =
  maybe (Failure [UnknownVariable name]) (Success . mkDataCon) <$> lookupDataConDefinitionInScope name'
 where
  mkDataCon (R.DataConDefinition name'' _) = R.DataCon name''

addTypeDefinitionToScope :: S.TyConDefinition -> Renamer (Validation [Error] R.TyConDefinition)
addTypeDefinitionToScope (S.TyConDefinition name tyArgs span') = do
  -- Rename ty args
  let
    tyArgs' = (\(S.TyVarInfo (Located argSpan argName)) -> R.TyVarInfo argName argSpan) <$> tyArgs
    tyDef =
      R.TyConDefinition
      { R.tyConDefinitionName = name
      , R.tyConDefinitionArgs = tyArgs'
      , R.tyConDefinitionLocation = Just span'
      }
  mExistingName <- lookupTypeConstructorInScope name
  case mExistingName of
    Just existingName -> pure $ Failure [TypeConstructorAlreadyExists (Located span' name) existingName]
    Nothing -> do
      modify' (\s -> s { renamerStateTypeConstructorsInScope = Map.insert name tyDef (renamerStateTypeConstructorsInScope s) })
      pure $ Success tyDef

lookupTypeConstructorInScope :: Text -> Renamer (Maybe R.TyConInfo)
lookupTypeConstructorInScope name = fmap tyConDefinitionToInfo . Map.lookup name <$> gets renamerStateTypeConstructorsInScope

lookupTypeConstructorInScopeOrError :: S.TyConInfo -> Renamer (Validation [Error] R.TyConInfo)
lookupTypeConstructorInScopeOrError (S.TyConInfo name args span') = do
  mTyCon <- lookupTypeConstructorInScope name
  case mTyCon of
    Nothing -> pure $ Failure [UnknownTypeConstructor (Located span' name)]
    Just (R.TyConInfo name' _ span'') -> do
      args' <- traverse lookupTypeTerm args
      pure $
        R.TyConInfo
        <$> pure name'
        <*> sequenceA args'
        <*> pure span''

lookupTypeTerm :: S.TypeTerm -> Renamer (Validation [Error] R.TypeTerm)
lookupTypeTerm (S.TyCon info) = fmap R.TyCon <$> lookupTypeConstructorInScopeOrError info
lookupTypeTerm (S.TyVar info) = fmap R.TyVar <$> lookupTypeVariableInScopeOrError info
lookupTypeTerm (S.TyParens t) = fmap R.TyParens <$> lookupTypeTerm t

addTypeDeclarationToScope :: R.TypeDeclaration -> Renamer R.TypeDeclaration
addTypeDeclarationToScope decl = do
  let tyConName = R.tyConDefinitionName $ R.typeDeclarationTypeName decl
  modify' (\s -> s { renamerStateTypeDeclarations = Map.insert tyConName decl (renamerStateTypeDeclarations s) })
  pure decl

lookupTypeDeclaration :: R.TyConInfo -> Renamer R.TypeDeclaration
lookupTypeDeclaration info =
  fromMaybe (error $ "Couldn't find declaration for " ++ show info)
  . Map.lookup (R.tyConInfoName info)
  <$> gets renamerStateTypeDeclarations

addTypeVariableToScope :: S.TyVarInfo -> Renamer (Validation [Error] R.TyVarInfo)
addTypeVariableToScope info@(S.TyVarInfo (Located span' name)) = do
  let
    info' = R.TyVarInfo name span'
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
      , renamerStateDataConDefinitionsInScope = renamerStateDataConDefinitionsInScope originalState
      , renamerStateTypeConstructorsInScope = renamerStateTypeConstructorsInScope originalState
      , renamerStateTypeVariablesInScope = renamerStateTypeVariablesInScope originalState
      }
    )
  pure result

liftValidation :: (Applicative f) => Validation err a -> (a -> f (Validation err b)) -> f (Validation err b)
liftValidation x f =
  case x of
    Failure err -> pure $ Failure err
    Success x' -> f x'
