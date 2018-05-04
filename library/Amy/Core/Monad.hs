{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Core.Monad
  ( Desugar
  , runDesugar
  , freshId
  , freshIdent
  ) where

import Control.Monad.State.Strict
import Data.Text (Text, pack)

import Amy.Core.AST as C

newtype Desugar a = Desugar (State Int a)
  deriving (Functor, Applicative, Monad, MonadState Int)

runDesugar :: Int -> Desugar a -> a
runDesugar maxId (Desugar action) = evalState action maxId

freshId :: Desugar Int
freshId = do
  modify' (+ 1)
  get

freshIdent :: Text -> Desugar C.Ident
freshIdent t = do
  id' <- freshId
  pure $ C.Ident (t <> pack (show id')) id'
