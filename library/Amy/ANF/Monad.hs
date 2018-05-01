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

import Amy.ANF.AST as ANF
import Amy.Core.AST as C

newtype ANFConvert a = ANFConvert (State ANFConvertState a)
  deriving (Functor, Applicative, Monad, MonadState ANFConvertState)

runANFConvert :: ANFConvertState -> ANFConvert a -> a
runANFConvert state' (ANFConvert action) = evalState action state'

data ANFConvertState
  = ANFConvertState
  { lastId :: Int
  , topLevelNames :: !(Set C.Ident)
  } deriving (Show, Eq)

anfConvertState :: Int -> [C.Ident] -> ANFConvertState
anfConvertState id' names = ANFConvertState id' (fromList names)

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
