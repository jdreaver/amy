module Amy.Names
  ( IdName(..)
  , IdNameProvenance(..)
  , NameId
  ) where

import Data.Text (Text)

-- | An 'IdName' is a program name tagged with a unique ID.
data IdName
  = IdName
  { idNameRaw :: !Text
  , idNameId :: !NameId
  , idNameProvenance :: !IdNameProvenance
  } deriving (Show, Eq, Ord)

-- TODO: Get rid of this. It is only used for the LLVM code generator to decide
-- to use a global or local reference. The code generator itself should track
-- this, especially since local let expressions might get moved to the
-- top-level, for example.
data IdNameProvenance
  = LocalDefinition
  | TopLevelDefinition
  deriving (Show, Eq, Ord)

type NameId = Int
