module Amy.Names
  ( Name(..)
  , identName
  , Ident(..)
  , IdentId
  ) where

import Data.Text (Text)

import Amy.Prim

-- | A 'Name' is an identified name of something from the source code after renaming.
data Name
  = PrimitiveName !PrimitiveFunctionName
  | IdentName !Ident
  deriving (Show, Eq, Ord)

identName :: Name -> Maybe Ident
identName (IdentName ident) = Just ident
identName _ = Nothing

-- | An identifier from source code
data Ident
  = Ident
  { identText :: !Text
  , identId :: !IdentId
  } deriving (Show, Eq, Ord)

type IdentId = Int
