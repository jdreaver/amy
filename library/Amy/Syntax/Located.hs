{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Amy.Syntax.Located
  ( Located(..)
  , SourceSpan(..)
  , mergeSpans
  ) where

-- | Location of something in source code.
data Located a
  = Located
  { locatedSpan :: !SourceSpan
  , locatedValue :: !a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | A file path along with a start and end 'SourcePos'.
data SourceSpan
  = SourceSpan
  { sourceSpanFile :: !FilePath
  , sourceSpanStartLine :: !Int
  , sourceSpanStartColumn :: !Int
  , sourceSpanEndLine :: !Int
  , sourceSpanEndColumn :: !Int
  } deriving (Show, Eq, Ord)

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SourceSpan file startLine startCol _ _) (SourceSpan _ _ _ endLine endCol) =
  SourceSpan file startLine startCol endLine endCol
