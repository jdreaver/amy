{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Amy.Syntax.Located
  ( Located(..)
  , SourceSpan(..)
  , mergeSpans
  , mkSourcePos
  , module Text.Megaparsec.Pos
  ) where

import Text.Megaparsec.Pos

-- | Location of something in source code.
data Located a
  = Located
  { locatedSpan :: !SourceSpan
  , locatedValue :: !a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | A file path along with a start and end 'SourcePos'.
data SourceSpan
  = SourceSpan
  { sourceSpanStart :: !SourcePos
  , sourceSpanEnd :: !SourcePos
  } deriving (Show, Eq, Ord)

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SourceSpan start _) (SourceSpan _ end) = SourceSpan start end

mkSourcePos :: FilePath -> Int -> Int -> SourcePos
mkSourcePos fp line col = SourcePos fp (mkPos line) (mkPos col)
