{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Amy.Syntax.Lexer
  ( -- * Lexer
    lexer
  , AmyTokens
  , AmyToken
  , prettyToken
  , prettyTokens

    -- * Parsers on AmyTokens
  , int
  , double
  , text
  , ident
  , tyCon
  , tyVar
  , dataCon
  , rowLabel
  , recordSelector
  , extern
  , if'
  , then'
  , else'
  , case'
  , of'
  , let'
  , in'
  , forall
  , pipe
  , lParen
  , rParen
  , parens
  , lBrace
  , rBrace
  , comma
  , dot
  , doubleColon
  , colon
  , semiColon
  , equals
  , rArrow

    -- * Indentation
  , noIndent
  ) where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.Scientific (floatingOrInteger)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
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
  | IdentToken !Text
  | TypeIdentToken !Text
  | RecordSelectorToken !RowLabel
  | ExternToken
  | IfToken
  | ThenToken
  | ElseToken
  | CaseToken
  | OfToken
  | LetToken
  | InToken
  | ForallToken
  | PipeToken
  | LParenToken
  | RParenToken
  | LBraceToken
  | RBraceToken
  | CommaToken
  | DotToken
  | DoubleColonToken
  | ColonToken
  | SemiColonToken
  | EqualsToken
  | RArrowToken
  deriving (Show, Eq, Ord)

prettyToken :: AmyToken -> Text
prettyToken (IntToken i) = pack (show i)
prettyToken (DoubleToken i) = pack (show i)
prettyToken (TextToken t) = "\"" <> t <> "\""
prettyToken (IdentToken name) = name
prettyToken (TypeIdentToken name) = name
prettyToken (RecordSelectorToken (RowLabel name)) = "." <> name
prettyToken ExternToken = "extern"
prettyToken IfToken = "if"
prettyToken ThenToken = "then"
prettyToken ElseToken = "else"
prettyToken CaseToken = "case"
prettyToken OfToken = "of"
prettyToken LetToken = "let"
prettyToken InToken = "in"
prettyToken ForallToken = "forall"
prettyToken PipeToken = "|"
prettyToken LParenToken = "("
prettyToken RParenToken = ")"
prettyToken LBraceToken = "{"
prettyToken RBraceToken = "}"
prettyToken CommaToken = ","
prettyToken DotToken = "."
prettyToken DoubleColonToken = "::"
prettyToken ColonToken = ":"
prettyToken SemiColonToken = ";"
prettyToken EqualsToken = "="
prettyToken RArrowToken = "->"

prettyTokens :: AmyTokens -> Text
prettyTokens (AmyTokens ts) = T.unwords $ prettyToken . locatedValue <$> ts

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
  advance1 Proxy _ _ (Located (SourceSpan fp _ _ l c) _) = SourcePos fp (mkPos l) (mkPos c)
  advanceN Proxy _ pos (AmyTokens []) = pos
  advanceN Proxy _ _ (AmyTokens ts) =
    let (Located (SourceSpan fp _ _ l c) _) = last ts
    in SourcePos fp (mkPos l) (mkPos c)
  take1_ (AmyTokens []) = Nothing
  take1_ (AmyTokens (t:ts)) = Just (t, AmyTokens ts)
  takeN_ n (AmyTokens s)
    | n <= 0    = Just (AmyTokens [], AmyTokens s)
    | null s    = Nothing
    | otherwise = Just ((\(t1, t2) -> (AmyTokens t1, AmyTokens t2)) $ splitAt n s)
  takeWhile_ f (AmyTokens ts) = (\(t1, t2) -> (AmyTokens t1, AmyTokens t2)) $ span f ts

instance ShowToken (Located AmyToken) where
  showTokens = unwords . fmap (unpack . prettyToken . locatedValue) . NE.toList

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
  [ try (string "extern" *> pure ExternToken)
  , try (string "if" *> pure IfToken)
  , try (string "then" *> pure ThenToken)
  , try (string "else" *> pure ElseToken)
  , try (string "case" *> pure CaseToken)
  , try (string "of" *> pure OfToken)
  , try (string "let" *> pure LetToken)
  , try (string "in" *> pure InToken)
  , try (string "forall" *> pure ForallToken)
  , try (string "|" *> pure PipeToken)
  , try (string "(" *> pure LParenToken)
  , try (string ")" *> pure RParenToken)
  , try (string "{" *> pure LBraceToken)
  , try (string "}" *> pure RBraceToken)
  , try (string "," *> pure CommaToken)
  , try (RecordSelectorToken . RowLabel <$> (char '.' >> lexIdentifier))
  , try (string "." *> pure DotToken)
  , try (string "::" *> pure DoubleColonToken)
  , try (string ":" *> pure ColonToken)
  , try (string ";" *> pure SemiColonToken)
  , try (string "=" *> pure EqualsToken)
  , try (string "->" *> pure RArrowToken)
  , try (either DoubleToken IntToken <$> lexNumber)
  , try (TextToken <$> lexText)
  , try (IdentToken <$> lexIdentifier)
  , try (TypeIdentToken <$> lexTypeIdentifier)
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

--
-- Token parser helpers
--

testToken :: (MonadParsec e AmyTokens m) => (AmyToken -> Maybe a) -> m (Located a)
testToken f = token testToken' Nothing
 where
  mkTokens x = Tokens (x:|[])
  testToken' x =
    case f (locatedValue x) of
      Just x' -> Right (Located (locatedSpan x) x')
      Nothing -> Left (pure (mkTokens x), Set.empty)

matchToken :: (MonadParsec e AmyTokens m) => AmyToken -> m SourceSpan
matchToken t = fmap locatedSpan (testToken (\t' -> if t == t' then Just () else Nothing)) <?> unpack (prettyToken t)

int :: (MonadParsec e AmyTokens m) => m (Located Int)
int = testToken (\case IntToken i -> Just i; _ -> Nothing) <?> "int literal"

double :: (MonadParsec e AmyTokens m) => m (Located Double)
double = testToken (\case DoubleToken i -> Just i; _ -> Nothing) <?> "double literal"

text :: (MonadParsec e AmyTokens m) => m (Located Text)
text = testToken (\case TextToken i -> Just i; _ -> Nothing) <?> "text literal"

ident :: (MonadParsec e AmyTokens m) => m (Located IdentName)
ident = fmap IdentName <$> testToken (\case IdentToken i -> Just i; _ -> Nothing) <?> "identifer"

tyCon :: (MonadParsec e AmyTokens m) => m (Located TyConName)
tyCon = fmap TyConName <$> testToken (\case TypeIdentToken i -> Just i; _ -> Nothing) <?> "type name"

tyVar :: (MonadParsec e AmyTokens m) => m (Located TyVarName)
tyVar = fmap TyVarName <$> testToken (\case IdentToken i -> Just i; _ -> Nothing) <?> "type variable"

dataCon :: (MonadParsec e AmyTokens m) => m (Located DataConName)
dataCon = fmap DataConName <$> testToken (\case TypeIdentToken i -> Just i; _ -> Nothing) <?> "data constructor"

rowLabel :: (MonadParsec e AmyTokens m) => m (Located RowLabel)
rowLabel = fmap RowLabel <$> testToken (\case IdentToken i -> Just i; _ -> Nothing) <?> "row label"

recordSelector :: (MonadParsec e AmyTokens m) => m (Located RowLabel)
recordSelector = testToken (\case RecordSelectorToken i -> Just i; _ -> Nothing) <?> "record selector"

extern :: (MonadParsec e AmyTokens m) => m SourceSpan
extern = matchToken ExternToken

if' :: (MonadParsec e AmyTokens m) => m SourceSpan
if' = matchToken IfToken

then' :: (MonadParsec e AmyTokens m) => m SourceSpan
then' = matchToken ThenToken

else' :: (MonadParsec e AmyTokens m) => m SourceSpan
else' = matchToken ElseToken

case' :: (MonadParsec e AmyTokens m) => m SourceSpan
case' = matchToken CaseToken

of' :: (MonadParsec e AmyTokens m) => m SourceSpan
of' = matchToken OfToken

let' :: (MonadParsec e AmyTokens m) => m SourceSpan
let' = matchToken LetToken

in' :: (MonadParsec e AmyTokens m) => m SourceSpan
in' = matchToken InToken

forall :: (MonadParsec e AmyTokens m) => m SourceSpan
forall = matchToken ForallToken

pipe :: (MonadParsec e AmyTokens m) => m SourceSpan
pipe = matchToken PipeToken

lParen :: (MonadParsec e AmyTokens m) => m SourceSpan
lParen = matchToken LParenToken

rParen :: (MonadParsec e AmyTokens m) => m SourceSpan
rParen = matchToken RParenToken

parens :: (MonadParsec e AmyTokens m) => m a -> m a
parens = between lParen rParen

lBrace :: (MonadParsec e AmyTokens m) => m SourceSpan
lBrace = matchToken LBraceToken

rBrace :: (MonadParsec e AmyTokens m) => m SourceSpan
rBrace = matchToken RBraceToken

comma :: (MonadParsec e AmyTokens m) => m SourceSpan
comma = matchToken CommaToken

dot :: (MonadParsec e AmyTokens m) => m SourceSpan
dot = matchToken DotToken

doubleColon :: (MonadParsec e AmyTokens m) => m SourceSpan
doubleColon = matchToken DoubleColonToken

colon :: (MonadParsec e AmyTokens m) => m SourceSpan
colon = matchToken ColonToken

semiColon :: (MonadParsec e AmyTokens m) => m SourceSpan
semiColon = matchToken SemiColonToken

equals :: (MonadParsec e AmyTokens m) => m SourceSpan
equals = matchToken EqualsToken

rArrow :: (MonadParsec e AmyTokens m) => m SourceSpan
rArrow = matchToken RArrowToken

--
-- Indentation helpers
--

noIndent :: (MonadParsec e AmyTokens m) => m a -> m a
noIndent = L.nonIndented (pure ())
