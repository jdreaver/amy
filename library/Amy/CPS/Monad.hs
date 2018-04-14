{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.CPS.Monad
  ( CPSConvert
  , runCPSConvert
  , freshIdent
  ) where

import Control.Monad.State.Strict
import Data.Text (Text)

import Amy.Names

newtype CPSConvert a = CPSConvert (State CPSConvertState a)
  deriving (Functor, Applicative, Monad, MonadState CPSConvertState)

runCPSConvert :: Int -> CPSConvert a -> a
runCPSConvert startingId (CPSConvert action) = evalState action (CPSConvertState startingId)

newtype CPSConvertState = CPSConvertState { lastId :: Int }
  deriving (Show, Eq)

freshIdent :: Text -> CPSConvert Ident
freshIdent t = do
  modify' (\s -> s { lastId = 1 + lastId s })
  Ident t <$> gets lastId
