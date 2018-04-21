{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.ANF.Monad
  ( ANFConvert
  , runANFConvert
  , freshId
  , freshIdent
  ) where

import Control.Monad.State.Strict
import Data.Text (Text, pack)

import Amy.Names

newtype ANFConvert a = ANFConvert (State ANFConvertState a)
  deriving (Functor, Applicative, Monad, MonadState ANFConvertState)

runANFConvert :: Int -> ANFConvert a -> a
runANFConvert startingId (ANFConvert action) = evalState action (ANFConvertState startingId)

newtype ANFConvertState = ANFConvertState { lastId :: Int }
  deriving (Show, Eq)

freshId :: ANFConvert IdentId
freshId = do
  modify' (\s -> s { lastId = 1 + lastId s })
  gets lastId

freshIdent :: Text -> ANFConvert Ident
freshIdent t = do
  id' <- freshId
  pure $ Ident (t <> pack (show id')) id' False
