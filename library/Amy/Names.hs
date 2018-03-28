module Amy.Names
  ( ValueName(..)
  , ValueNameProvenance(..)
  , NameId
  ) where

import Data.Text (Text)

-- | An 'ValueName' is a program name tagged with a unique ID.
data ValueName
  = ValueName
  { valueNameRaw :: !Text
  , valueNameId :: !NameId
  , valueNameProvenance :: !ValueNameProvenance
  } deriving (Show, Eq, Ord)

-- TODO: Get rid of this. It is only used for the LLVM code generator to decide
-- to use a global or local reference. The code generator itself should track
-- this, especially since local let expressions might get moved to the
-- top-level, for example.
data ValueNameProvenance
  = LocalDefinition
  | TopLevelDefinition
  deriving (Show, Eq, Ord)

type NameId = Int
