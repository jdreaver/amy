{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Syntax.Monad
  ( AmyParser
  , runAmyParser
  , withBlockIndentation
  , currentIndentation
  , assertIndented
  , assertSameIndentation
  ) where

import Control.Applicative (Alternative)
import Control.Monad.State.Strict
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec hiding (State)

newtype AmyParser a = AmyParser (StateT Pos (Parsec Void Text) a)
  deriving (Functor, Applicative, Alternative, MonadPlus, Monad, MonadState Pos, MonadParsec Void Text)

runAmyParser :: AmyParser a -> Parsec Void Text a
runAmyParser (AmyParser action) = evalStateT action (mkPos 1)

withBlockIndentation :: AmyParser a -> AmyParser a
withBlockIndentation action = do
  originalIndent <- currentIndentation
  currentIndent <- sourceColumn <$> getPosition
  setIndentation currentIndent
  result <- action
  setIndentation originalIndent
  pure result

currentIndentation :: AmyParser Pos
currentIndentation = get

setIndentation :: Pos -> AmyParser ()
setIndentation x = modify' (\_ -> x)

-- | Check that the current indentation level is past the stored indentation
assertIndented :: AmyParser ()
assertIndented = checkIndentation "indentation past column" (>)

-- | Check that the current indentation level is the same as the stored indentation
assertSameIndentation :: AmyParser ()
assertSameIndentation = checkIndentation "indentation at column" (==)

-- | Check that the current identation level matches a predicate
checkIndentation
  :: Text
  -> (Pos -> Pos -> Bool)
  -> AmyParser ()
checkIndentation msg rel = do
  col <- sourceColumn <$> getPosition
  current <- currentIndentation
  guard (col `rel` current) <?> unpack (msg <> " " <> pack (show $ unPos current))
