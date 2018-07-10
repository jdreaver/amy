{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Monad
  ( ANFConvert
  , runANFConvert
  , ANFConvertRead
  , anfConvertRead
  , ANFConvertState
  , freshId
  , freshIdent
  , getTyConDefinitionType
  , getTyConType
  , getDataConInfo
  , getKnownFuncType
  , makeTextPointer
  , getTextPointers
  , putClosureWrapper
  , getClosureWrappers
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Amy.ANF.AST as ANF
import Amy.ANF.TypeRep
import Amy.Core.AST as C
import Amy.Prim

newtype ANFConvert a = ANFConvert (ReaderT ANFConvertRead (State ANFConvertState) a)
  deriving (Functor, Applicative, Monad, MonadReader ANFConvertRead, MonadState ANFConvertState)

runANFConvert :: ANFConvertRead -> ANFConvert a -> a
runANFConvert read' (ANFConvert action) = evalState (runReaderT action read') (ANFConvertState 0 [] Map.empty)

data ANFConvertRead
  = ANFConvertRead
  { anfConvertReadTypeReps :: !(Map TyConName ANF.Type)
  , anfConvertReadDataConInfos :: !(Map DataConName (ANF.Type, ConstructorIndex))
  , anfConvertReadFuncTypes :: !(Map IdentName ([C.Type], C.Type))
  } deriving (Show, Eq)

anfConvertRead :: [(IdentName, ([C.Type], C.Type))] -> [C.TypeDeclaration] -> ANFConvertRead
anfConvertRead funcs typeDeclarations =
  let
    allTypeDecls = typeDeclarations ++ (fst <$> allPrimTypeDefinitions)
    typeRepMap =
      Map.fromList
      $ (\t -> (locatedValue . C.tyConDefinitionName . C.typeDeclarationTypeName $ t, typeRep t))
      <$> allTypeDecls
    dataConInfos = Map.fromList $ concatMap mkDataConInfo allTypeDecls
  in
    ANFConvertRead
    { anfConvertReadTypeReps = typeRepMap
    , anfConvertReadDataConInfos = dataConInfos
    , anfConvertReadFuncTypes = Map.fromList funcs
    }

mkDataConInfo :: C.TypeDeclaration -> [(DataConName, (ANF.Type, ConstructorIndex))]
mkDataConInfo decl@(C.TypeDeclaration _ cons) = mkInfo <$> zip cons [0..]
 where
  rep = typeRep decl
  mkInfo (C.DataConDefinition (Located _ name) _, index) = (name, (rep, ConstructorIndex index))

data ANFConvertState
  = ANFConvertState
  { lastId :: !Int
  , textPointers :: ![TextPointer]
  , closureWrappers :: !(Map IdentName ClosureWrapper)
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

getTyConDefinitionType :: C.TyConDefinition -> ANFConvert ANF.Type
getTyConDefinitionType tyCon =
  fromMaybe err
  . Map.lookup (locatedValue $ tyConDefinitionName tyCon)
  <$> asks anfConvertReadTypeReps
  where
   err = error $ "Couldn't find TypeCompilationMethod of TyConDefinition " ++ show tyCon

getTyConType :: TyConName -> ANFConvert ANF.Type
getTyConType con = fromMaybe err . Map.lookup con <$> asks anfConvertReadTypeReps
  where
   err = error $ "Couldn't find TypeCompilationMethod of TyConName " ++ show con

getDataConInfo :: DataConName -> ANFConvert (ANF.Type, ConstructorIndex)
getDataConInfo con = fromMaybe err . Map.lookup con <$> asks anfConvertReadDataConInfos
  where
   err = error $ "Couldn't find TypeCompilationMethod of TyConDefinition " ++ show con

getKnownFuncType :: IdentName -> ANFConvert (Maybe ([C.Type], C.Type))
getKnownFuncType ident = Map.lookup ident <$> asks anfConvertReadFuncTypes

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
