{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Syntax.Monad
  ( AmyParser
  , runAmyParser
  , withBlockIndentation
  , currentIndentation
  ) where

import Control.Applicative (Alternative)
import Control.Monad.State.Strict
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (State)

newtype AmyParser a = AmyParser (StateT BlockStack (Parsec Void Text) a)
  deriving (Functor, Applicative, Alternative, MonadPlus, Monad, MonadState BlockStack, MonadParsec Void Text)

runAmyParser :: AmyParser a -> Parsec Void Text a
runAmyParser (AmyParser action) = evalStateT action blockStack

-- | Stores block indentation columns in a stack.
newtype BlockStack = BlockStack (NonEmpty Int)
  deriving (Show, Eq)

blockStack :: BlockStack
blockStack = BlockStack (0 :| [])

pushBlockStack :: Int -> AmyParser ()
pushBlockStack x = modify' (\(BlockStack xs) -> BlockStack (x <| xs))

popBlockStack :: AmyParser ()
popBlockStack =
  modify' (\(BlockStack xs) -> BlockStack (fromMaybe err . snd . NE.uncons $ xs))
 where
  err = error "Tried to pop last block indentation off of the stack!"

withBlockIndentation :: Int -> AmyParser a -> AmyParser a
withBlockIndentation x action = do
  pushBlockStack x
  result <- action
  popBlockStack
  pure result

currentIndentation :: AmyParser Int
currentIndentation = gets $ \(BlockStack (x :| _)) -> x
