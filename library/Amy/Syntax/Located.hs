{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Amy.Syntax.Located
  ( Located(..)
  , MaybeLocated(..)
  , fromLocated
  , notLocated
  , SourceSpan(..)
  , mergeSpans
  , mkSourcePos
  , mkSourceSpan
  , module Text.Megaparsec.Pos
  ) where

import Text.Megaparsec.Pos

-- | Location of something in source code.
data Located a
  = Located
  { locatedSpan :: !SourceSpan
  , locatedValue :: !a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Possible location of something in source code.
data MaybeLocated a
  = MaybeLocated
  { maybeLocatedSpan :: !(Maybe SourceSpan)
  , maybeLocatedValue :: !a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

fromLocated :: Located a -> MaybeLocated a
fromLocated (Located span' x) = MaybeLocated (Just span') x

notLocated :: a -> MaybeLocated a
notLocated = MaybeLocated Nothing

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

mkSourceSpan :: FilePath -> Int -> Int -> Int -> Int -> SourceSpan
mkSourceSpan fp startLine startCol endLine endCol = SourceSpan (mkSourcePos fp startLine startCol) (mkSourcePos fp endLine endCol)
