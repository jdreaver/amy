{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Core.Monad
  ( Desugar
  , runDesugar
  , freshId
  , freshIdent
  , lookupDataConType
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Amy.Core.AST as C
import Amy.Environment

newtype Desugar a = Desugar (ReaderT Environment (State Int) a)
  deriving (Functor, Applicative, Monad, MonadReader Environment, MonadState Int)

runDesugar :: Environment -> [TypeDeclaration] -> Desugar a -> a
runDesugar env decls (Desugar action) = evalState (runReaderT action env') 0
 where
  -- TODO: Compute all DataConInfos for a module once and use in type checking,
  -- Core, ANF, etc.
  declInfos = Map.fromList $ first locatedValue <$> concatMap dataConInfos decls
  env' =
    env
    { environmentDataConInfos = environmentDataConInfos env <> declInfos
    }

freshId :: Desugar Int
freshId = do
  modify' (+ 1)
  get

freshIdent :: Text -> Desugar IdentName
freshIdent t = do
  id' <- freshId
  pure $ IdentName (t <> pack (show id'))

lookupDataConType :: DataConName -> Desugar TypeDeclaration
lookupDataConType con =
  asks
  $ dataConInfoTypeDeclaration
  . fromMaybe (error $ "No type definition for " ++ show con)
  . Map.lookup con
  . environmentDataConInfos
