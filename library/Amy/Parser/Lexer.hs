{-# LANGUAGE OverloadedStrings #-}

module Amy.Parser.Lexer
  ( spaceConsumer
  , integer
  , double
  , bool
  , symbol
  , identifier
  , typeIdentifier
  , extern
  , if'
  , then'
  , else'
  , lparen
  , rparen
  , comma
  , semicolon
  , doubleColon
  , equals
  , typeSeparatorArrow
  , text
  ) where

import Control.Monad (void)
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
  , "if"
  , "then"
  , "else"
  ]

extern :: Lexer ()
extern = void $ symbol "extern"

if' :: Lexer ()
if' = void $ symbol "if"

then' :: Lexer ()
then' = void $ symbol "then"

else' :: Lexer ()
else' = void $ symbol "else"

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
