{-# LANGUAGE OverloadedStrings #-}

module Amy.Syntax.Lexer
  ( spaceConsumer
  , spaceConsumerNewlines
  , noIndent
  , number
  , symbol
  , identifier
  , dataConName
  , tyConName
  , tyVarName
  , rowLabel
  , pipe
  , extern
  , forall
  , if'
  , then'
  , else'
  , case'
  , of'
  , let'
  , in'
  , parens
  , braces
  , optionalParens
  , comma
  , dot
  , semiColon
  , doubleColon
  , equals
  , rightArrow
  , typeSeparatorArrow
  , text
  , indentedBlock
  , indentedBlockNonEmpty
  ) where

import qualified Control.Applicative.Combinators.NonEmpty as CNE
import Control.Monad (join, void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (floatingOrInteger)
import Data.Text (Text, pack, unpack)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Amy.Names
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

symbol :: Text -> AmyParser Text
symbol sym = L.symbol spaceConsumer sym <?> unpack sym

identifier :: AmyParser (Located IdentName)
identifier = fmap IdentName <$> try (p >>= traverse check)
 where
  p = lexeme $ do
    firstChar <- lowerChar
    otherChars <- many (alphaNumChar <|> oneOf ['\''])
    hash <- unpack <$> option "" (string "#")
    pure $ firstChar : otherChars ++ hash
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
  , "case"
  , "of"
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

case' :: AmyParser ()
case' = void $ symbol "case"

of' :: AmyParser ()
of' = void $ symbol "of"

let' :: AmyParser ()
let' = void $ symbol "let"

in' :: AmyParser ()
in' = void $ symbol "in"

-- | Type names are upper-case, like Int and Double
typeIdentifier :: AmyParser (Located Text)
typeIdentifier = lexeme (pack <$> ((:) <$> upperChar <*> many alphaNumChar)) <?> "type identifier"

dataConName :: AmyParser (Located DataConName)
dataConName = fmap DataConName <$> typeIdentifier

tyConName :: AmyParser (Located TyConName)
tyConName = fmap TyConName <$> typeIdentifier

tyVarName :: AmyParser (Located TyVarName)
tyVarName = fmap (TyVarName . unIdentName) <$> identifier

rowLabel :: AmyParser (Located RowLabel)
rowLabel = fmap (RowLabel . unIdentName) <$> identifier

pipe :: AmyParser ()
pipe = char '|' >> spaceConsumer

lparen :: AmyParser ()
lparen = char '(' >> spaceConsumer

rparen :: AmyParser ()
rparen = char ')' >> spaceConsumer

parens :: AmyParser a -> AmyParser a
parens = between lparen rparen

optionalParens :: AmyParser a -> AmyParser a
optionalParens p = parens p <|> p

lbrace :: AmyParser ()
lbrace = char '{' >> spaceConsumer

rbrace :: AmyParser ()
rbrace = char '}' >> spaceConsumer

braces :: AmyParser a -> AmyParser a
braces = between lbrace rbrace

comma :: AmyParser ()
comma = char ',' >> spaceConsumer

dot :: AmyParser ()
dot = char '.' >> spaceConsumer

semiColon :: AmyParser ()
semiColon = char ';' >> spaceConsumer

doubleColon :: AmyParser ()
doubleColon = char ':' >> char ':' >> spaceConsumer

equals :: AmyParser ()
equals = char '=' >> spaceConsumer

rightArrow :: AmyParser ()
rightArrow = string "->" >> spaceConsumer

typeSeparatorArrow :: AmyParser ()
typeSeparatorArrow = rightArrow

text :: AmyParser Text
text = fmap pack $ char '"' >> manyTill L.charLiteral (char '"')

noIndent :: AmyParser a -> AmyParser a
noIndent = L.nonIndented spaceConsumer

-- | Parse a block of items into a list.
--
-- A block is defined as a group of things that are at the same indentation
-- level. Block items can also be separated by semicolons.
indentedBlock
  :: AmyParser a
  -> AmyParser [a]
indentedBlock p =
  fmap concat $ withBlockIndentation $ many $
    assertSameIndentation *> p `sepBy1` semiColon <* spaceConsumerNewlines

indentedBlockNonEmpty
  :: AmyParser a
  -> AmyParser (NonEmpty a)
indentedBlockNonEmpty p =
  fmap join $ withBlockIndentation $ CNE.some $
    assertSameIndentation *> p `CNE.sepBy1` semiColon <* spaceConsumerNewlines
