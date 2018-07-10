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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Amy.Core.AST as C
import Amy.Prim

newtype Desugar a = Desugar (ReaderT (Map DataConName TypeDeclaration) (State Int) a)
  deriving (Functor, Applicative, Monad, MonadReader (Map DataConName TypeDeclaration), MonadState Int)

runDesugar :: [TypeDeclaration] -> Desugar a -> a
runDesugar decls (Desugar action) = evalState (runReaderT action dataConMap) 0
 where
  allTypeDecls = decls ++ (fst <$> allPrimTypeDefinitions)
  dataConMap = Map.fromList $ concatMap mkDataConTypes allTypeDecls

freshId :: Desugar Int
freshId = do
  modify' (+ 1)
  get

freshIdent :: Text -> Desugar IdentName
freshIdent t = do
  id' <- freshId
  pure $ IdentName (t <> pack (show id'))

-- TODO: Compute this in the Renamer so we don't have to keep recomputing it
-- here
mkDataConTypes :: TypeDeclaration -> [(DataConName, TypeDeclaration)]
mkDataConTypes tyDecl@(TypeDeclaration _ dataConDefs) = mkDataConPair <$> dataConDefs
 where
  mkDataConPair (DataConDefinition (Located _ name) _) = (name, tyDecl)

lookupDataConType :: DataConName -> Desugar TypeDeclaration
lookupDataConType con =
  asks
  $ fromMaybe (error $ "No type definition for " ++ show con)
  . Map.lookup con
