module Amy.Names
  ( ValueName(..)
  , NameIntId
  , NameId(..)
  ) where

import Data.Text (Text)

import Amy.Prim

-- | An 'ValueName' is a program name tagged with a unique ID.
data ValueName
  = ValueName
  { valueNameRaw :: !Text
  , valueNameId :: !NameId
  } deriving (Show, Eq, Ord)

-- | How a name is identified after renaming.
data NameId
  = NameIntId !NameIntId
  | PrimitiveFunctionId !PrimitiveFunctionName
  deriving (Show, Eq, Ord)

type NameIntId = Int
