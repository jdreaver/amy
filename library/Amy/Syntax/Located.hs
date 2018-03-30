{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Amy.Syntax.Located
  ( Located(..)
  , SourceSpan(..)
  ) where

-- | Location of something in source code.
data Located a
  = Located
  { locatedSpan :: !SourceSpan
  , locatedValue :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | A file path along with a start and end 'SourcePos'.
data SourceSpan
  = SourceSpan
  { sourceSpanFile :: !FilePath
  , sourceSpanStartLine :: !Int
  , sourceSpanStartColumn :: !Int
  , sourceSpanEndLine :: !Int
  , sourceSpanEndColumn :: !Int
  } deriving (Show, Eq)
