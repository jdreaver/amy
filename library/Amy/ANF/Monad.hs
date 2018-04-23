{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.ANF.Monad
  ( ANFConvert
  , runANFConvert
  , anfConvertState
  , freshId
  , freshIdent
  , isIdentTopLevel
  ) where

import Control.Monad.State.Strict
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import GHC.Exts (fromList)

import Amy.ANF.AST
import Amy.TypeCheck.AST

newtype ANFConvert a = ANFConvert (State ANFConvertState a)
  deriving (Functor, Applicative, Monad, MonadState ANFConvertState)

runANFConvert :: ANFConvertState -> ANFConvert a -> a
runANFConvert state' (ANFConvert action) = evalState action state'

data ANFConvertState
  = ANFConvertState
  { lastId :: Int
  , topLevelNames :: !(Set TIdent)
  }
  deriving (Show, Eq)

anfConvertState :: Int -> [TIdent] -> ANFConvertState
anfConvertState id' names = ANFConvertState id' (fromList names)

freshId :: ANFConvert Int
freshId = do
  modify' (\s -> s { lastId = 1 + lastId s })
  gets lastId

freshIdent :: Text -> ANFConvert ANFIdent
freshIdent t = do
  id' <- freshId
  pure $ ANFIdent (t <> pack (show id')) id' Nothing False

isIdentTopLevel :: TIdent -> ANFConvert Bool
isIdentTopLevel ident = Set.member ident <$> gets topLevelNames
