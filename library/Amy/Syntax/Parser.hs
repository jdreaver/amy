module Amy.Syntax.Parser
  ( parseModule

  , declaration
  , externDecl
  , bindingType
  , parseScheme
  , parseType
  , binding
  , expression
  , expression'
  , expressionParens
  , ifExpression
  , letExpression'
  , literal
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Amy.Syntax.AST
import Amy.Syntax.Lexer
import Amy.Type

type Parser = Parsec Void Text

parseModule :: Parser Module
parseModule = Module <$> do
  spaceConsumerNewlines
  noIndent (indentedBlock declaration) <* eof

declaration :: Parser Declaration
declaration =
  (DeclExtern <$> externDecl)
  <|> try (DeclBindingType <$> bindingType)
  <|> (DeclBinding <$> binding)

externDecl :: Parser Extern
externDecl = do
  extern
  name <- identifier
  doubleColon
  ty <- parseType
  pure
    Extern
    { externName = name
    , externType = ty
    }

bindingType :: Parser BindingType
bindingType = do
  name <- identifier
  doubleColon
  scheme <- parseScheme
  pure
    BindingType
    { bindingTypeName = name
    , bindingTypeScheme = scheme
    }

parseScheme :: Parser (Scheme (Located Text))
parseScheme = do
  vars <- optional parseSchemeVars
  ty <- parseType
  pure $ Forall (fromMaybe [] vars) ty

parseSchemeVars :: Parser [TVar]
parseSchemeVars = do
  forall
  vars <- many identifier
  dot
  pure (TVar . locatedValue <$> vars)

parseType :: Parser (Type (Located Text))
parseType = makeExprParser term table
 where
  tVar = (TyCon <$> typeIdentifier) <|> (TyVar . TVar . locatedValue <$> identifier)
  table = [[InfixR (TyArr <$ typeSeparatorArrow)]]
  term = parens parseType <|> tVar

binding :: Parser Binding
binding = do
  startingIndent <- L.indentLevel
  name <- identifier
  args <- many identifier
  equals
  spaceConsumerNewlines
  _ <- L.indentGuard spaceConsumerNewlines GT startingIndent
  body <- expression
  pure
    Binding
    { bindingName = name
    , bindingArgs = args
    , bindingBody = body
    }

expression :: Parser Expr
expression = do
  -- Parse a NonEmpty list of expressions separated by spaces.
  expressions <- lineFold expression'

  pure $
    case expressions of
      -- Just a simple expression
      expr :| [] -> expr
      -- We must have a function application
      f :| args ->
        EApp
        App
        { appFunction = f
        , appArgs = NE.fromList args
        }

-- | Parses any expression except function application. This is needed to avoid
-- left recursion. Without this distinction, f a b would be parsed as f (a b)
-- instead of (f a) b.
expression' :: Parser Expr
expression' =
  expressionParens
  <|> (ELit <$> literal)
  <|> (EIf <$> ifExpression)
  <|> (ELet <$> letExpression')
  <|> (EVar <$> variable)

expressionParens :: Parser Expr
expressionParens = EParens <$> parens expression

literal :: Parser (Located Literal)
literal =
  (fmap (either LiteralDouble LiteralInt) <$> number)
  <|> (fmap LiteralBool <$> bool)

variable :: Parser (Located Text)
variable = identifier

ifExpression :: Parser If
ifExpression = do
  if'
  predicate <- expression
  then'
  thenExpression <- expression
  else'
  elseExpression <- expression
  pure
    If
    { ifPredicate = predicate
    , ifThen = thenExpression
    , ifElse = elseExpression
    }

letExpression' :: Parser Let
letExpression' = do
  letIndentation <- L.indentLevel
  let'
  let
    parser =
      try (LetBinding <$> binding)
      <|> (LetBindingType <$> bindingType)
  bindings <- many $ do
    _ <- L.indentGuard spaceConsumerNewlines GT letIndentation
    parser <* spaceConsumerNewlines

  inIndentation <- L.indentLevel
  _ <- do
    -- TODO: What if the "in" and "let" are on the same line?
    _ <- L.indentGuard spaceConsumerNewlines EQ letIndentation
    in'
  expr <- do
    _ <- L.indentGuard spaceConsumerNewlines GT inIndentation
    expression
  pure
    Let
    { letBindings = bindings
    , letExpression = expr
    }
