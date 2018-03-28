module Amy.Names
  ( ValueName(..)
  , NameId
  ) where

import Data.Text (Text)

-- | An 'ValueName' is a program name tagged with a unique ID.
data ValueName
  = ValueName
  { valueNameRaw :: !Text
  , valueNameId :: !NameId
  } deriving (Show, Eq, Ord)

type NameId = Int
