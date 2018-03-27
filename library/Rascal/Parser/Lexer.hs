{-# LANGUAGE OverloadedStrings #-}

module Rascal.Parser.Lexer
  ( integer
  , double
  , bool
  , symbol
  , identifier
  , typeIdentifier
  , extern
  , lparen
  , rparen
  , comma
  , semicolon
  , doubleColon
  , equals
  , typeSeparatorArrow
  , text
  , sepByNonEmpty
  , someNonEmpty
  ) where

import Control.Monad (MonadPlus, void)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Lexer = Parsec Void Text

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Lexer ()
spaceConsumer = L.space space1 lineComment blockComment
 where
  lineComment  = L.skipLineComment "#"
  blockComment = empty

integer :: Lexer Int
integer = lexeme L.decimal

double :: Lexer Double
double = lexeme L.float

bool :: Lexer Bool
bool = lexeme $
  (string "True" >> pure True)
  <|> (string "False" >> pure False)

symbol :: Text -> Lexer Text
symbol = L.symbol spaceConsumer

identifier :: Lexer Text
identifier = (lexeme . try) (p >>= check)
 where
  p = ((:) <$> lowerChar <*> many alphaNumChar) <* spaceConsumer
  check x =
    if x `elem` reservedWords
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return (pack x)

reservedWords :: [String]
reservedWords =
  [ "extern"
  ]

extern :: Lexer ()
extern = void $ symbol "extern"

-- | Type names are upper-case, like Int and Double
typeIdentifier :: Lexer Text
typeIdentifier = fmap pack $ ((:) <$> upperChar <*> many alphaNumChar) <* spaceConsumer

lparen :: Lexer ()
lparen = char '(' >> spaceConsumer

rparen :: Lexer ()
rparen = char ')' >> spaceConsumer

comma :: Lexer ()
comma = char ',' >> spaceConsumer

semicolon :: Lexer ()
semicolon = char ';' >> spaceConsumer

doubleColon :: Lexer ()
doubleColon = char ':' >> char ':' >> spaceConsumer

equals :: Lexer ()
equals = char '=' >> spaceConsumer

typeSeparatorArrow :: Lexer ()
typeSeparatorArrow = string "->" >> spaceConsumer

text :: Lexer Text
text = fmap pack $ char '"' >> manyTill L.charLiteral (char '"')

-- | Like 'sepBy1', except shoves the result in a 'NonEmpty'
sepByNonEmpty :: (MonadPlus m) => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty p sep = NE.fromList <$> sepBy1 p sep

-- | Like 'some' but puts result in a 'NonEmpty'
someNonEmpty :: (MonadPlus f) => f a -> f (NonEmpty a)
someNonEmpty = fmap NE.fromList . some
