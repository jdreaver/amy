{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.ANF.Monad
  ( ANFConvert
  , runANFConvert
  , ANFConvertRead
  , anfConvertRead
  , ANFConvertState
  , anfConvertState
  , freshId
  , freshIdent
  , getTyConInfoType
  , isIdentTopLevel
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import GHC.Exts (fromList)

import Amy.ANF.AST as ANF
import Amy.ANF.TypeRep
import Amy.Core.AST as C
import Amy.Prim

newtype ANFConvert a = ANFConvert (ReaderT ANFConvertRead (State ANFConvertState) a)
  deriving (Functor, Applicative, Monad, MonadReader ANFConvertRead, MonadState ANFConvertState)

runANFConvert :: ANFConvertRead -> ANFConvertState -> ANFConvert a -> a
runANFConvert read' state' (ANFConvert action) = evalState (runReaderT action read') state'

data ANFConvertRead
  = ANFConvertRead
  { anfConvertReadTypeReps :: !(Map Int ANF.Type)
  , anfConvertReadTopLevelNames :: !(Set C.Ident)
  } deriving (Show, Eq)

anfConvertRead :: [C.Ident] -> [C.TypeDeclaration] -> ANFConvertRead
anfConvertRead topLevelNames typeDeclarations =
  let
    allTypeDecls = typeDeclarations ++ (fromPrimTypeDefinition <$> allPrimTypeDefinitions)
    typeRepMap = Map.fromList $ (\t -> (C.tyConInfoId $ C.typeDeclarationTypeName t, typeRep t)) <$> allTypeDecls
  in
    ANFConvertRead
    { anfConvertReadTypeReps = typeRepMap
    , anfConvertReadTopLevelNames = fromList topLevelNames
    }

data ANFConvertState
  = ANFConvertState
  { lastId :: Int
  } deriving (Show, Eq)

anfConvertState :: Int -> ANFConvertState
anfConvertState id' = ANFConvertState id'

freshId :: ANFConvert Int
freshId = do
  modify' (\s -> s { lastId = 1 + lastId s })
  gets lastId

freshIdent :: Text -> ANFConvert ANF.Ident
freshIdent t = do
  id' <- freshId
  pure $ ANF.Ident (t <> pack (show id')) id' False

getTyConInfoType :: C.TyConInfo -> ANFConvert ANF.Type
getTyConInfoType tyCon = fromMaybe err . Map.lookup (tyConInfoId tyCon) <$> asks anfConvertReadTypeReps
  where
   err = error $ "Couldn't find TypeCompilationMethod of TyConInfo " ++ show tyCon

isIdentTopLevel :: C.Ident -> ANFConvert Bool
isIdentTopLevel ident = Set.member ident <$> asks anfConvertReadTopLevelNames
