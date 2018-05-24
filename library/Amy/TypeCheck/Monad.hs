{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.TypeCheck.Monad
  ( TyEnv(..)
  , substituteEnv
  , Inference(..)
  , runInference
  , freshTypeVariable
  , withNewLexicalScope
  , addIdentSchemeToScope
  , lookupIdentScheme
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (pack)

import Amy.Errors
import Amy.Renamer.AST as R
import Amy.TypeCheck.AST as T
import Amy.TypeCheck.Substitution

--
-- Type Environment
--

-- | A 'TyEnv' is the typing environment. It contains known names with their
-- type schemes.
data TyEnv
  = TyEnv
  { identTypes :: !(Map IdentName T.Scheme)
    -- TODO: Should this be constructed in the renamer?
  , typeDefinitions :: !(Map TyConName T.TyConDefinition)
  , dataConstructorTypes :: !(Map DataConName (T.TypeDeclaration, T.DataConDefinition))
  , maxId :: !Int
  } deriving (Show, Eq)

substituteEnv :: Subst -> TyEnv -> TyEnv
substituteEnv subst env = env { identTypes = Map.map (substituteScheme subst) (identTypes env) }

--
-- Inference Monad
--

-- | Holds a 'TyEnv' variables in a 'ReaderT' and a 'State' 'Int'
-- counter for producing type variables.
newtype Inference a = Inference (StateT TyEnv (Except Error) a)
  deriving (Functor, Applicative, Monad, MonadState TyEnv, MonadError Error)

-- TODO: Don't use Except, use Validation

runInference :: TyEnv -> Inference a -> Either Error a
runInference env (Inference action) = runExcept $ evalStateT action env

freshId :: Inference Int
freshId = do
  modify' (\s -> s { maxId = maxId s + 1 })
  gets maxId

freshTypeVariable :: Inference T.TyVarInfo
freshTypeVariable = do
  id' <- freshId
  pure $ T.TyVarInfo (TyVarName $ "t" <> pack (show id')) TyVarGenerated

withNewLexicalScope :: Inference a -> Inference a
withNewLexicalScope action = do
  orig <- get
  result <- action
  modify' $ \s ->
    s
    { identTypes = identTypes orig
    }
  pure result

addIdentSchemeToScope :: IdentName -> T.Scheme -> Inference ()
addIdentSchemeToScope name scheme = modify' $ \env -> env { identTypes = Map.insert name scheme (identTypes env) }

lookupIdentScheme :: IdentName -> Inference T.Scheme
lookupIdentScheme name = do
  mScheme <- gets (Map.lookup name . identTypes)
  maybe (throwError $ UnboundVariable name) pure mScheme
