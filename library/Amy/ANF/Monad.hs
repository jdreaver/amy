{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.ANF.Monad
  ( ANFConvert
  , runANFConvert
  , freshIdent
  ) where

import Control.Monad.State.Strict
import Data.Text (Text)

import Amy.Names

newtype ANFConvert a = ANFConvert (State ANFConvertState a)
  deriving (Functor, Applicative, Monad, MonadState ANFConvertState)

runANFConvert :: Int -> ANFConvert a -> a
runANFConvert startingId (ANFConvert action) = evalState action (ANFConvertState startingId)

newtype ANFConvertState = ANFConvertState { lastId :: Int }
  deriving (Show, Eq)

freshIdent :: Text -> ANFConvert Ident
freshIdent t = do
  modify' (\s -> s { lastId = 1 + lastId s })
  id' <- gets lastId
  pure $ Ident t id' False
