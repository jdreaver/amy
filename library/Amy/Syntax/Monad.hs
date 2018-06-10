{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Syntax.Monad
  ( AmyParser
  , runAmyParser
  , withBlockIndentation
  , currentIndentation
  , assertIndented
  , assertSameIndentation
  , indentedBlock
  , indentedBlockNonEmpty
  ) where

import Control.Applicative (Alternative)
import qualified Control.Applicative.Combinators.NonEmpty as CNE
import Control.Monad.State.Strict
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec hiding (State)

import Amy.Syntax.Lexer

newtype AmyParser a = AmyParser (StateT Int (Parsec Void AmyTokens) a)
  deriving (Functor, Applicative, Alternative, MonadPlus, Monad, MonadState Int, MonadParsec Void AmyTokens)

runAmyParser :: AmyParser a -> Parsec Void AmyTokens a
runAmyParser (AmyParser action) = evalStateT action 0

withBlockIndentation :: AmyParser a -> AmyParser a
withBlockIndentation action = do
  originalIndent <- currentIndentation
  nextIndent <- maybe (unexpected EndOfInput) (pure . unPos . sourceColumn) =<< getNextTokenPosition
  setIndentation nextIndent
  result <- action
  setIndentation originalIndent
  pure result

currentIndentation :: AmyParser Int
currentIndentation = get

setIndentation :: Int -> AmyParser ()
setIndentation x = modify' (const x)

-- | Check that the current indentation level is past the stored indentation
assertIndented :: AmyParser ()
assertIndented = checkIndentation "indentation past column" (>)

-- | Check that the current indentation level is the same as the stored indentation
assertSameIndentation :: AmyParser ()
assertSameIndentation = checkIndentation "indentation at column" (==)

-- | Check that the current identation level matches a predicate
checkIndentation
  :: Text
  -> (Int -> Int -> Bool)
  -> AmyParser ()
checkIndentation msg rel = do
  currentIndent <- currentIndentation
  nextIndent <- maybe (unexpected EndOfInput) (pure . unPos . sourceColumn) =<< getNextTokenPosition
  guard (nextIndent `rel` currentIndent) <?> unpack (msg <> " " <> pack (show currentIndent))

-- A block is defined as a group of things that are at the same indentation
-- level. Block items can also be separated by semicolons.
indentedBlock
  :: AmyParser a
  -> AmyParser [a]
indentedBlock p =
  fmap concat $ withBlockIndentation $ many $
    assertSameIndentation *> p `sepBy1` semiColon

indentedBlockNonEmpty
  :: AmyParser a
  -> AmyParser (NonEmpty a)
indentedBlockNonEmpty p =
  fmap join $ withBlockIndentation $ CNE.some $
    assertSameIndentation *> p `CNE.sepBy1` semiColon
