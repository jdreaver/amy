module Amy.Parser.Parser
  ( parserAST

  , topLevel
  , externType
  , bindingType
  , binding
  , expression
  , expression'
  , expressionParens
  , ifExpression
  , literal
  ) where

import qualified Control.Applicative.Combinators.NonEmpty as CNE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

import Amy.AST
import Amy.Parser.Lexer

type Parser = Parsec Void Text

parserAST :: Parser (AST Text ())
parserAST = AST <$> do
  spaceConsumerNewlines
  noIndent (indentedBlock topLevel) <* eof

topLevel :: Parser (TopLevel Text ())
topLevel =
  try (TopLevelExternType <$> externType)
  <|> try (TopLevelBindingType <$> bindingType)
  <|> (TopLevelBindingValue <$> binding)

externType :: Parser (BindingType Text)
externType = do
  extern
  bindingType

bindingType :: Parser (BindingType Text)
bindingType = do
  bindingName <- identifier
  doubleColon
  typeNames <- typeIdentifier `CNE.sepBy1` typeSeparatorArrow
  pure
    BindingType
    { bindingTypeName = bindingName
    , bindingTypeTypeNames = typeNames
    }

binding :: Parser (BindingValue Text ())
binding = do
  bindingName <- identifier
  args <- many identifier
  equals
  expr <- expression
  pure
    BindingValue
    { bindingValueName = bindingName
    , bindingValueArgs = args
    , bindingValueType = ()
    , bindingValueBody = expr
    }

expression :: Parser (Expression Text ())
expression = do
  -- Parse a NonEmpty list of expressions separated by spaces.
  expressions <- lineFold expression'

  pure $
    case expressions of
      -- Just a simple expression
      expr :| [] -> expr
      -- We must have a function application
      f :| args ->
        ExpressionFunctionApplication
        FunctionApplication
        { functionApplicationFunction = f
        , functionApplicationArgs = NE.fromList args
        , functionApplicationReturnType = ()
        }


-- | Parses any expression except function application. This is needed to avoid
-- left recursion. Without this distinction, f a b would be parsed as f (a b)
-- instead of (f a) b.
expression' :: Parser (Expression Text ())
expression' =
  expressionParens
  <|> (ExpressionLiteral <$> literal)
  <|> (ExpressionIf <$> ifExpression)
  <|> try (ExpressionVariable <$> variable)

expressionParens :: Parser (Expression Text ())
expressionParens = ExpressionParens <$> between lparen rparen expression

literal :: Parser Literal
literal =
  try (LiteralDouble <$> double)
  <|> try (LiteralInt <$> integer)
  <|> (LiteralBool <$> bool)

variable :: Parser (Variable Text ())
variable = do
  name <- identifier
  pure
    Variable
    { variableName = name
    , variableType = ()
    }

ifExpression :: Parser (If Text ())
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
    , ifType = ()
    }
