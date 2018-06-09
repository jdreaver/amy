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
  , optionalParens
  , lbrace
  , rbrace
  , comma
  , dot
  , colon
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
  spaceConsumerNewlines
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

extern :: AmyParser SourceSpan
extern = fmap locatedSpan . lexeme $ symbol "extern"

forall :: AmyParser SourceSpan
forall = fmap locatedSpan . lexeme $ symbol "forall"

if' :: AmyParser SourceSpan
if' = fmap locatedSpan . lexeme $ symbol "if"

then' :: AmyParser SourceSpan
then' = fmap locatedSpan . lexeme $ symbol "then"

else' :: AmyParser SourceSpan
else' = fmap locatedSpan . lexeme $ symbol "else"

case' :: AmyParser SourceSpan
case' = fmap locatedSpan . lexeme $ symbol "case"

of' :: AmyParser SourceSpan
of' = fmap locatedSpan . lexeme $ symbol "of"

let' :: AmyParser SourceSpan
let' = fmap locatedSpan . lexeme $ symbol "let"

in' :: AmyParser SourceSpan
in' = fmap locatedSpan . lexeme $ symbol "in"

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

pipe :: AmyParser SourceSpan
pipe = fmap locatedSpan . lexeme $ char '|'

lparen :: AmyParser SourceSpan
lparen = fmap locatedSpan . lexeme $ char '('

rparen :: AmyParser SourceSpan
rparen = fmap locatedSpan . lexeme $ char ')'

parens :: AmyParser a -> AmyParser a
parens = between lparen rparen

optionalParens :: AmyParser a -> AmyParser a
optionalParens p = parens p <|> p

lbrace :: AmyParser SourceSpan
lbrace = fmap locatedSpan . lexeme $ char '{'

rbrace :: AmyParser SourceSpan
rbrace = fmap locatedSpan . lexeme $ char '}'

comma :: AmyParser SourceSpan
comma = fmap locatedSpan . lexeme $ char ','

dot :: AmyParser SourceSpan
dot = fmap locatedSpan . lexeme $ char '.'

colon :: AmyParser SourceSpan
colon = fmap locatedSpan . lexeme $ char ':'

semiColon :: AmyParser SourceSpan
semiColon = fmap locatedSpan . lexeme $ char ';'

doubleColon :: AmyParser SourceSpan
doubleColon = fmap locatedSpan . lexeme $ char ':' >> char ':'

equals :: AmyParser SourceSpan
equals = fmap locatedSpan . lexeme $ char '='

rightArrow :: AmyParser SourceSpan
rightArrow = fmap locatedSpan . lexeme $ string "->"

typeSeparatorArrow :: AmyParser SourceSpan
typeSeparatorArrow = rightArrow

text :: AmyParser (Located Text)
text = do
  (SourcePos fp startLine startCol) <- getPosition
  text' <- fmap pack $ char '"' >> manyTill L.charLiteral (char '"')
  (SourcePos _ endLine endCol) <- getPosition
  let
    span' =
      SourceSpan
        fp
        (unPos startLine)
        (unPos startCol)
        (unPos endLine)
        (unPos endCol - 1)
  spaceConsumerNewlines
  pure $ Located span' text'

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
