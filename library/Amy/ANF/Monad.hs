{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Amy.ANF.Monad
  ( ANFConvert
  , runANFConvert
  , ANFConvertState
  , freshId
  , freshIdent
  , convertType
  , getTyConDefinitionType
  , getDataConInfo
  , getKnownFuncType
  , makeTextPointer
  , getTextPointers
  , putClosureWrapper
  , getClosureWrappers
  , getExternFunctions
  , getExternTypes
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)

import Amy.ANF.AST as ANF
import Amy.ANF.ConvertType
import Amy.Core.AST as C
import Amy.Environment

newtype ANFConvert a = ANFConvert (ReaderT ANFConvertRead (State ANFConvertState) a)
  deriving (Functor, Applicative, Monad, MonadReader ANFConvertRead, MonadState ANFConvertState)

runANFConvert :: Environment -> Environment -> ANFConvert a -> a
runANFConvert depsEnv moduleEnv (ANFConvert action) =
  let
    combinedEnv = depsEnv `mergeEnvironments` moduleEnv
    modTyCons = Set.fromList . Map.keys $ environmentANFTypeReps moduleEnv
    read' = ANFConvertRead combinedEnv moduleEnv modTyCons
  in evalState (runReaderT action read') (ANFConvertState 0 [] Map.empty Map.empty Map.empty)

data ANFConvertRead
  = ANFConvertRead
  { combinedEnvironment :: !Environment
  , moduleEnvironment :: !Environment
  , moduleTyCons :: !(Set TyConName)
  } deriving (Show, Eq)

data ANFConvertState
  = ANFConvertState
  { lastId :: !Int
  , textPointers :: ![TextPointer]
  , closureWrappers :: !(Map IdentName ClosureWrapper)
  , externFunctions :: !(Map IdentName ANF.Extern)
  , externTypes :: !(Map TyConName ANF.Type)
  } deriving (Show, Eq)

freshId :: ANFConvert Int
freshId = do
  modify' (\s -> s { lastId = 1 + lastId s })
  gets lastId

freshIdent :: Text -> ANFConvert IdentName
freshIdent t = do
  id' <- freshId
  -- TODO: Give these a special name to ensure the name doesn't conflict with
  -- user-defined type variables. Prefix with "$"?
  pure $ IdentName (t <> pack (show id'))

convertType :: C.Type -> ANFConvert ANF.Type
convertType ty = do
  read' <- ask

  -- Compute ANF type
  let
    combinedTyMap = environmentANFTypeReps $ combinedEnvironment read'
    ty' = convertANFType combinedTyMap ty

  -- Record any types from outside the current module
  let
    tyCons = typeTyCons ty
    externalTyCons = tyCons `Set.difference` moduleTyCons read'
    externalTyMap =
      Map.fromList
      . fmap (\n -> maybe (error $ "Couldn't find TyCon rep " ++ show n) (n,) $ Map.lookup n combinedTyMap)
      $ Set.toList externalTyCons
  modify' $ \s -> s { externTypes = externTypes s <> externalTyMap }

  pure ty'

getTyConDefinitionType :: C.TyConDefinition -> ANFConvert ANF.Type
getTyConDefinitionType tyCon =
  fromMaybe err
  . Map.lookup (locatedValue $ tyConDefinitionName tyCon)
  <$> asks (environmentANFTypeReps . combinedEnvironment)
  where
   err = error $ "Couldn't find TypeCompilationMethod of TyConDefinition " ++ show tyCon

getDataConInfo :: DataConName -> ANFConvert DataConInfo
getDataConInfo con = fromMaybe err . Map.lookup con <$> asks (environmentDataConInfos . combinedEnvironment)
  where
   err = error $ "Couldn't find TypeCompilationMethod of TyConDefinition " ++ show con

getKnownFuncType :: IdentName -> ANFConvert (Maybe ([ANF.Type], ANF.Type))
getKnownFuncType ident = do
  read' <- ask

  -- Look up known function type from current module
  let
    moduleFuncs = environmentANFFunctionTypes $ moduleEnvironment read'
    mFunc = Map.lookup ident moduleFuncs
  case mFunc of
    Just f -> pure (Just f)
    Nothing -> do
      -- Look up known function type from external module
      let
        combinedFuncs = environmentANFFunctionTypes $ combinedEnvironment read'
        mExternalFunc = Map.lookup ident combinedFuncs

      -- Record external function use
      for_ mExternalFunc $ \(argTys, retTy) -> do
        let extern = ANF.Extern ident argTys retTy
        modify' $ \s -> s { externFunctions = Map.insert ident extern (externFunctions s) }

      pure mExternalFunc

makeTextPointer :: Text -> ANFConvert ANF.TextPointer
makeTextPointer text = do
  id' <- freshId
  let ptr = ANF.TextPointer id' text
  modify' $ \s -> s { textPointers = ptr : textPointers s }
  pure ptr

getTextPointers :: ANFConvert [TextPointer]
getTextPointers = reverse <$> gets textPointers

putClosureWrapper :: IdentName -> [ANF.Type] -> ANF.Type -> ANFConvert IdentName
putClosureWrapper original@(IdentName t) argTys retTy = do
  let
    name = IdentName $ t <> "_closure_wrapper"
    wrapper = ANF.ClosureWrapper name original argTys retTy
  -- TODO: Maybe check for duplicates and ensure they are equal
  modify' $ \s -> s { closureWrappers = Map.insert name wrapper (closureWrappers s) }
  pure name

getClosureWrappers :: ANFConvert [ClosureWrapper]
getClosureWrappers = fmap snd . Map.toAscList <$> gets closureWrappers

getExternFunctions :: ANFConvert [ANF.Extern]
getExternFunctions = fmap snd . Map.toAscList <$> gets externFunctions

getExternTypes :: ANFConvert [ANF.Type]
getExternTypes = fmap snd . Map.toAscList <$> gets externTypes
