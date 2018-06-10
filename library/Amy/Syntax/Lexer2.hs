{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lexer2
  ( -- * Lexer
    lexer

    -- * Parsers on AmyTokens
  , int
  , double
  , text
  , ident
  , tyCon
  , tyVar
  , dataCon
  , if'
  , then'
  , else'
  , case'
  , of'
  , let'
  , in'
  , pipe
  , lParen
  , rParen
  , parens
  , lBrace
  , rBrace
  , comma
  , dot
  , colon
  , doubleColon
  , semiColon
  , equals
  , rArrow

    -- * Indentation
  , noIndent
  ) where

import Control.Monad (void)
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.Scientific (floatingOrInteger)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Amy.Names
import Amy.Syntax.Located

--
-- Token type and tokens stream
--

data AmyToken
  = IntToken !Int
  | DoubleToken !Double
  | TextToken !Text
  | IdentToken !IdentName
  | TyConToken !TyConName
  | TyVarToken !TyVarName
  | DataConToken !DataConName
  | RowLabelToken !RowLabel
  | IfToken
  | ThenToken
  | ElseToken
  | CaseToken
  | OfToken
  | LetToken
  | InToken
  | PipeToken
  | LParenToken
  | RParenToken
  | LBraceToken
  | RBraceToken
  | CommaToken
  | DotToken
  | ColonToken
  | DoubleColonToken
  | SemiColonToken
  | EqualsToken
  | RArrowToken
  deriving (Show, Eq, Ord)

prettyToken :: AmyToken -> Text
prettyToken (IntToken i) = pack (show i)
prettyToken (DoubleToken i) = pack (show i)
prettyToken (TextToken t) = "\"" <> t <> "\""
prettyToken (IdentToken (IdentName name)) = name
prettyToken (TyConToken (TyConName name)) = name
prettyToken (TyVarToken (TyVarName name)) = name
prettyToken (DataConToken (DataConName name)) = name
prettyToken (RowLabelToken (RowLabel name)) = name
prettyToken IfToken = "if"
prettyToken ThenToken = "then"
prettyToken ElseToken = "else"
prettyToken CaseToken = "case"
prettyToken OfToken = "of"
prettyToken LetToken = "let"
prettyToken InToken = "in"
prettyToken PipeToken = "|"
prettyToken LParenToken = "("
prettyToken RParenToken = ")"
prettyToken LBraceToken = "{"
prettyToken RBraceToken = "}"
prettyToken CommaToken = ","
prettyToken DotToken = "."
prettyToken ColonToken = ":"
prettyToken DoubleColonToken = "::"
prettyToken SemiColonToken = ";"
prettyToken EqualsToken = "="
prettyToken RArrowToken = "->"

newtype AmyTokens = AmyTokens { unAmyTokens :: [Located AmyToken] }
  deriving (Show, Eq, Ord)

instance Stream AmyTokens where
  type Token AmyTokens = Located AmyToken
  type Tokens AmyTokens = AmyTokens
  tokenToChunk Proxy = AmyTokens . (:[])
  tokensToChunk Proxy = AmyTokens
  chunkToTokens Proxy = unAmyTokens
  chunkLength Proxy = length . unAmyTokens
  chunkEmpty Proxy = null . unAmyTokens
  positionAt1 Proxy _ (Located (SourceSpan fp l c _ _) _) = SourcePos fp (mkPos l) (mkPos c)
  positionAtN Proxy pos (AmyTokens []) = pos
  positionAtN Proxy _ (AmyTokens (Located (SourceSpan fp l c _ _) _ : _)) = SourcePos fp (mkPos l) (mkPos c)
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w cp (AmyTokens ts) = foldl' (defaultAdvance1 w) cp ts
  take1_ (AmyTokens []) = Nothing
  take1_ (AmyTokens (t:ts)) = Just (t, AmyTokens ts)
  takeN_ n (AmyTokens s)
    | n <= 0    = Just (AmyTokens [], AmyTokens s)
    | null s    = Nothing
    | otherwise = Just ((\(t1, t2) -> (AmyTokens t1, AmyTokens t2)) $ splitAt n s)
  takeWhile_ f (AmyTokens ts) = (\(t1, t2) -> (AmyTokens t1, AmyTokens t2)) $ span f ts

instance ShowToken (Located AmyToken) where
  showTokens = unwords . fmap (unpack . prettyToken . locatedValue) . NE.toList

defaultAdvance1
  :: Pos               -- ^ Tab width
  -> SourcePos         -- ^ Current position
  -> Located AmyToken  -- ^ Current token
  -> SourcePos         -- ^ Incremented position
defaultAdvance1 _ _ (Located (SourceSpan fp _ _ l c) _) = SourcePos fp (mkPos l) (mkPos c)
{-# INLINE defaultAdvance1 #-}

--
-- Lexer
--

type Lexer a = Parsec Void Text a

lexer :: FilePath -> Text -> Either (ParseError Char Void) AmyTokens
lexer = parse lexer'

lexer' :: Lexer AmyTokens
lexer' = fmap AmyTokens $ spaceConsumerNewlines *> many lexToken <* eof

--
-- Space consumption
--

spaceConsumer :: Lexer ()
spaceConsumer = L.space (void $ char ' ') lineComment blockComment

spaceConsumerNewlines :: Lexer ()
spaceConsumerNewlines = L.space space1 lineComment blockComment

lineComment :: Lexer ()
lineComment  = L.skipLineComment "#"

blockComment :: Lexer ()
blockComment = empty

--
-- Token parsers
--

lexeme :: Lexer a -> Lexer (Located a)
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

lexToken :: Lexer (Located AmyToken)
lexToken = lexeme lexToken'

lexToken' :: Lexer AmyToken
lexToken' =
  choice
  [ try (either DoubleToken IntToken <$> lexNumber)
  , try (TextToken <$> lexText)
  , try (IdentToken . IdentName <$> lexIdentifier)
  , try (TyConToken . TyConName <$> lexTypeIdentifier)
  , try (TyVarToken . TyVarName <$> lexIdentifier)
  , try (DataConToken . DataConName <$> lexTypeIdentifier)
  , try (RowLabelToken . RowLabel <$> lexIdentifier)
  , try (symbol "if" *> pure IfToken)
  , try (symbol "then" *> pure ThenToken)
  , try (symbol "else" *> pure ElseToken)
  , try (symbol "case" *> pure CaseToken)
  , try (symbol "of" *> pure OfToken)
  , try (symbol "let" *> pure LetToken)
  , try (symbol "in" *> pure InToken)
  , try (symbol "|" *> pure PipeToken)
  , try (symbol "(" *> pure LParenToken)
  , try (symbol ")" *> pure RParenToken)
  , try (symbol "{" *> pure LBraceToken)
  , try (symbol "}" *> pure RBraceToken)
  , try (symbol "," *> pure CommaToken)
  , try (symbol "." *> pure DotToken)
  , try (symbol ":" *> pure ColonToken)
  , try (symbol "::" *> pure DoubleColonToken)
  , try (symbol ";" *> pure SemiColonToken)
  , try (symbol "=" *> pure EqualsToken)
  , try (symbol "->" *> pure RArrowToken)
  ]

-- TODO: Fix this for Doubles with a ".0". Fails on 1.0, thinks it is an Int
lexNumber :: Lexer (Either Double Int)
lexNumber = floatingOrInteger <$> L.scientific

lexText :: Lexer Text
lexText = fmap pack $ char '"' >> manyTill L.charLiteral (char '"')

lexIdentifier :: Lexer Text
lexIdentifier = try (p >>= check)
 where
  p = do
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

lexTypeIdentifier :: Lexer Text
lexTypeIdentifier = pack <$> ((:) <$> upperChar <*> many alphaNumChar) <?> "type identifier"

symbol :: Text -> Lexer Text
symbol sym = L.symbol spaceConsumer sym <?> unpack sym

--
-- Token parser helpers
--

testToken :: (MonadParsec e AmyTokens m) => (AmyToken -> Maybe a) -> m a
testToken f = token testToken' Nothing
 where
  mkTokens x = Tokens (x:|[])
  testToken' x =
    case f (locatedValue x) of
      Just x' -> Right x'
      Nothing -> Left (pure (mkTokens x), Set.empty)

matchToken :: (MonadParsec e AmyTokens m) => AmyToken -> m ()
matchToken t = testToken (\t' -> if t == t' then Just () else Nothing) <?> unpack (prettyToken t)

int :: (MonadParsec e AmyTokens m) => m Int
int = testToken (\case IntToken i -> Just i; _ -> Nothing) <?> "int literal"

double :: (MonadParsec e AmyTokens m) => m Double
double = testToken (\case DoubleToken i -> Just i; _ -> Nothing) <?> "double literal"

text :: (MonadParsec e AmyTokens m) => m Text
text = testToken (\case TextToken i -> Just i; _ -> Nothing) <?> "text literal"

ident :: (MonadParsec e AmyTokens m) => m IdentName
ident = testToken (\case IdentToken i -> Just i; _ -> Nothing)  <?> "identifer"

tyCon :: (MonadParsec e AmyTokens m) => m TyConName
tyCon = testToken (\case TyConToken i -> Just i; _ -> Nothing)  <?> "type name"

tyVar :: (MonadParsec e AmyTokens m) => m TyVarName
tyVar = testToken (\case TyVarToken i -> Just i; _ -> Nothing)  <?> "type variable"

dataCon :: (MonadParsec e AmyTokens m) => m DataConName
dataCon = testToken (\case DataConToken i -> Just i; _ -> Nothing)  <?> "data constructor"

if' :: (MonadParsec e AmyTokens m) => m ()
if' = matchToken IfToken

then' :: (MonadParsec e AmyTokens m) => m ()
then' = matchToken ThenToken

else' :: (MonadParsec e AmyTokens m) => m ()
else' = matchToken ElseToken

case' :: (MonadParsec e AmyTokens m) => m ()
case' = matchToken CaseToken

of' :: (MonadParsec e AmyTokens m) => m ()
of' = matchToken OfToken

let' :: (MonadParsec e AmyTokens m) => m ()
let' = matchToken LetToken

in' :: (MonadParsec e AmyTokens m) => m ()
in' = matchToken InToken

pipe :: (MonadParsec e AmyTokens m) => m ()
pipe = matchToken PipeToken

lParen :: (MonadParsec e AmyTokens m) => m ()
lParen = matchToken LParenToken

rParen :: (MonadParsec e AmyTokens m) => m ()
rParen = matchToken RParenToken

parens :: (MonadParsec e AmyTokens m) => m a -> m a
parens = between lParen rParen

lBrace :: (MonadParsec e AmyTokens m) => m ()
lBrace = matchToken LBraceToken

rBrace :: (MonadParsec e AmyTokens m) => m ()
rBrace = matchToken RBraceToken

comma :: (MonadParsec e AmyTokens m) => m ()
comma = matchToken CommaToken

dot :: (MonadParsec e AmyTokens m) => m ()
dot = matchToken DotToken

colon :: (MonadParsec e AmyTokens m) => m ()
colon = matchToken ColonToken

doubleColon :: (MonadParsec e AmyTokens m) => m ()
doubleColon = matchToken DoubleColonToken

semiColon :: (MonadParsec e AmyTokens m) => m ()
semiColon = matchToken SemiColonToken

equals :: (MonadParsec e AmyTokens m) => m ()
equals = matchToken EqualsToken

rArrow :: (MonadParsec e AmyTokens m) => m ()
rArrow = matchToken RArrowToken

--
-- Indentation helpers
--

noIndent :: (MonadParsec e AmyTokens m) => m a -> m a
noIndent = L.nonIndented (pure ())
