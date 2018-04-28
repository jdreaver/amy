{-# LANGUAGE OverloadedStrings #-}

module Amy.Syntax.Lexer
  ( spaceConsumer
  , spaceConsumerNewlines
  , noIndent
  , number
  , bool
  , symbol
  , identifier
  , typeIdentifier
  , extern
  , forall
  , if'
  , then'
  , else'
  , let'
  , in'
  , lparen
  , rparen
  , parens
  , optionalParens
  , comma
  , dot
  , doubleColon
  , equals
  , typeSeparatorArrow
  , text
  , indentedBlock
  , lineFold
  ) where

import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (floatingOrInteger)
import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Amy.Syntax.Located
import Amy.Syntax.Monad

lexeme :: AmyParser a -> AmyParser (Located a)
lexeme p = do
  (SourcePos fp startLine startCol) <- getPosition
  val <- p
  (SourcePos _ endLine endCol) <- getPosition
  spaceConsumer
  let
    sourceSpan =
      SourceSpan
      fp
      (unPos startLine)
      (unPos startCol)
      (unPos endLine)
      (unPos endCol - 1)
  pure (Located sourceSpan val)

spaceConsumer :: AmyParser ()
spaceConsumer = L.space (void $ char ' ') lineComment blockComment

spaceConsumerNewlines :: AmyParser ()
spaceConsumerNewlines = L.space space1 lineComment blockComment

lineComment :: AmyParser ()
lineComment  = L.skipLineComment "#"

blockComment :: AmyParser ()
blockComment = empty

-- TODO: Fix this for Doubles with a ".0". Fails on 1.0, thinks it is an Int
number :: AmyParser (Located (Either Double Int))
number = fmap floatingOrInteger <$> lexeme L.scientific

bool :: AmyParser (Located Bool)
bool = lexeme $
  (string "True" >> pure True)
  <|> (string "False" >> pure False)

symbol :: Text -> AmyParser Text
symbol = L.symbol spaceConsumer

identifier :: AmyParser (Located Text)
identifier = try (p >>= traverse check)
 where
  p = lexeme ((:) <$> lowerChar <*> many (alphaNumChar <|> char '\''))
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
  , "let"
  , "in"
  ]

extern :: AmyParser ()
extern = void $ symbol "extern"

forall :: AmyParser ()
forall = void $ symbol "forall"

if' :: AmyParser ()
if' = void $ symbol "if"

then' :: AmyParser ()
then' = void $ symbol "then"

else' :: AmyParser ()
else' = void $ symbol "else"

let' :: AmyParser ()
let' = void $ symbol "let"

in' :: AmyParser ()
in' = void $ symbol "in"

-- | Type names are upper-case, like Int and Double
typeIdentifier :: AmyParser (Located Text)
typeIdentifier = lexeme $ pack <$> ((:) <$> upperChar <*> many alphaNumChar)

lparen :: AmyParser ()
lparen = char '(' >> spaceConsumer

rparen :: AmyParser ()
rparen = char ')' >> spaceConsumer

parens :: AmyParser a -> AmyParser a
parens = between lparen rparen

optionalParens :: AmyParser a -> AmyParser a
optionalParens p = parens p <|> p

comma :: AmyParser ()
comma = char ',' >> spaceConsumer

dot :: AmyParser ()
dot = char '.' >> spaceConsumer

doubleColon :: AmyParser ()
doubleColon = char ':' >> char ':' >> spaceConsumer

equals :: AmyParser ()
equals = char '=' >> spaceConsumer

typeSeparatorArrow :: AmyParser ()
typeSeparatorArrow = string "->" >> spaceConsumer

text :: AmyParser Text
text = fmap pack $ char '"' >> manyTill L.charLiteral (char '"')

noIndent :: AmyParser a -> AmyParser a
noIndent = L.nonIndented spaceConsumer

-- | Parse a list of things at the current indentation level, no more no less.
indentedBlock
  :: AmyParser a
  -> AmyParser [a]
indentedBlock p = do
  blockIndentation <- L.indentLevel
  many $ do
    currentIndentation' <- L.indentLevel
    if currentIndentation' == blockIndentation
    then p <* spaceConsumerNewlines
    else L.incorrectIndent EQ blockIndentation currentIndentation'

-- | Parse something that can bleed over newlines as long as the indentation on
-- the next lines is strictly greater than the first line.
lineFold :: AmyParser a -> AmyParser (NonEmpty a)
lineFold p = do
  startingIndent <- L.indentLevel
  first <- p
  spaceConsumerNewlines
  rest <- many $ L.indentGuard spaceConsumerNewlines GT startingIndent >> p <* spaceConsumerNewlines
  pure $ first :| rest
