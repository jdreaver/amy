{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.ANF.Monad
  ( ANFConvert
  , runANFConvert
  , anfConvertState
  , freshId
  , freshIdent
  , isIdentTopLevel
  , unboxDataConstructor
  , unboxTyCon
  ) where

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import GHC.Exts (fromList)

import Amy.ANF.AST as ANF
import Amy.Core.AST as C
import Amy.Prim

newtype ANFConvert a = ANFConvert (State ANFConvertState a)
  deriving (Functor, Applicative, Monad, MonadState ANFConvertState)

runANFConvert :: ANFConvertState -> ANFConvert a -> a
runANFConvert state' (ANFConvert action) = evalState action state'

data ANFConvertState
  = ANFConvertState
  { lastId :: Int
  , topLevelNames :: !(Set C.Ident)
  , dataConstructorsToUnbox :: !(Map ANF.Ident ANF.TyConInfo)
  , typeConstructorsToUnbox :: !(Map ANF.TyConInfo ANF.TyConInfo)
  }
  deriving (Show, Eq)

anfConvertState :: Int -> [C.Ident] -> [ANF.TypeDeclaration] -> ANFConvertState
anfConvertState id' names typeDeclarations = ANFConvertState id' (fromList names) dataConUnboxMap tyConUnboxMap
 where
  assertPrimitiveType info@(ANF.TyConInfo _ _ mPrim) =
    maybe (error $ "Cannot unbox, not a primitive type! " ++ show info) primitiveTyConInfo mPrim
  dataConUnboxMap =
    Map.fromList
    $ (\(ANF.TypeDeclaration _ dataCon tyArg) -> (dataCon, assertPrimitiveType tyArg))
    <$> typeDeclarations
  tyConUnboxMap =
    Map.fromList
    $ (\(ANF.TypeDeclaration tyCon _ tyArg) -> (tyCon, assertPrimitiveType tyArg))
    <$> typeDeclarations

primitiveTyConInfo :: PrimitiveType -> ANF.TyConInfo
primitiveTyConInfo prim = ANF.TyConInfo (showPrimitiveType prim) (primitiveTypeId prim) (Just prim)

freshId :: ANFConvert Int
freshId = do
  modify' (\s -> s { lastId = 1 + lastId s })
  gets lastId

freshIdent :: Text -> ANFConvert ANF.Ident
freshIdent t = do
  id' <- freshId
  pure $ ANF.Ident (t <> pack (show id')) id' Nothing False

isIdentTopLevel :: C.Ident -> ANFConvert Bool
isIdentTopLevel ident = Set.member ident <$> gets topLevelNames

unboxDataConstructor :: ANF.Ident -> ANFConvert Bool
unboxDataConstructor ident = gets (isJust . Map.lookup ident . dataConstructorsToUnbox)

unboxTyCon :: ANF.TyConInfo -> ANFConvert (Maybe ANF.TyConInfo)
unboxTyCon info = gets (Map.lookup info . typeConstructorsToUnbox)
