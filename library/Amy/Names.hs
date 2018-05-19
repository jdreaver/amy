{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Names
  ( IdentName(..)
  , DataConName(..)
  , TyConName(..)
  , TyVarName(..)
  ) where

import Data.Text (Text)
import GHC.Exts (IsString)

newtype IdentName = IdentName { unIdentName :: Text }
  deriving (Show, Eq, Ord, IsString)

newtype DataConName = DataConName { unDataConName :: Text }
  deriving (Show, Eq, Ord, IsString)

newtype TyConName = TyConName { unTyConName :: Text }
  deriving (Show, Eq, Ord, IsString)

newtype TyVarName = TyVarName { unTyVarName :: Text }
  deriving (Show, Eq, Ord, IsString)
