{-# LANGUAGE OverloadedStrings #-}

module Amy.Parser.Lexer
  ( spaceConsumer
  , spaceConsumerNewlines
  , noIndent
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
  , doubleColon
  , equals
  , typeSeparatorArrow
  , text
  , indentedBlock
  , lineFold
  ) where

import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Lexer = Parsec Void Text

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Lexer ()
spaceConsumer = L.space (void $ char ' ') lineComment blockComment

spaceConsumerNewlines :: Lexer ()
spaceConsumerNewlines = L.space space1 lineComment blockComment

lineComment :: Lexer ()
lineComment  = L.skipLineComment "#"

blockComment :: Lexer ()
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

doubleColon :: Lexer ()
doubleColon = char ':' >> char ':' >> spaceConsumer

equals :: Lexer ()
equals = char '=' >> spaceConsumer

typeSeparatorArrow :: Lexer ()
typeSeparatorArrow = string "->" >> spaceConsumer

text :: Lexer Text
text = fmap pack $ char '"' >> manyTill L.charLiteral (char '"')

noIndent :: Lexer a -> Lexer a
noIndent = L.nonIndented spaceConsumer

-- | Parse a list of things at the current indentation level, no more no less.
indentedBlock
  :: Parsec Void Text a
  -> Parsec Void Text [a]
indentedBlock p = do
  blockIndentation <- L.indentLevel
  many $ do
    currentIndentation <- L.indentLevel
    if currentIndentation == blockIndentation
    then p <* spaceConsumerNewlines
    else L.incorrectIndent EQ blockIndentation currentIndentation

-- | Parse something that can bleed over newlines as long as the indentation on
-- the next lines is strictly greater than the first line.
lineFold :: Parsec Void Text a -> Parsec Void Text (NonEmpty a)
lineFold p = do
  startingIndent <- L.indentLevel
  first <- p
  spaceConsumerNewlines
  rest <- many (L.indentGuard spaceConsumerNewlines GT startingIndent >> p)
  -- rest <- many $ do
  --   item <- p
  --   _ <- L.indentGuard spaceConsumerNewlines GT startingIndent
  --   pure item
  pure $ first :| rest
