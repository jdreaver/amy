module Rascal.Parser.Parser
  ( parserAST

  , externType
  , bindingType
  , binding
  , expression
  , expressionNotApplication
  , expressionParens
  , literal
  , functionApplication
  ) where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec

import Rascal.AST
import Rascal.Parser.Lexer

type Parser = Parsec Void Text
-- type ParserError = ParseError Void Char

parserAST :: Parser (AST Text ())
parserAST = AST <$> topLevel `sepBy` semicolon

topLevel :: Parser (TopLevel Text ())
topLevel =
  (TopLevelExternType <$> externType)
  <|> try (TopLevelBindingType <$> bindingType)
  <|> try (TopLevelBindingValue <$> binding)

externType :: Parser (BindingType Text)
externType = do
  extern
  bindingType

bindingType :: Parser (BindingType Text)
bindingType = do
  bindingName <- identifier
  doubleColon
  typeNames <- typeIdentifier `sepByNonEmpty` typeSeparatorArrow
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
expression =
  try (ExpressionFunctionApplication <$> functionApplication)
  <|> expressionNotApplication

-- | Parses any expression except function application. This is needed to avoid
-- left recursion. Without this distinction, f a b would be parsed as f (a b)
-- instead of (f a) b.
expressionNotApplication :: Parser (Expression Text ())
expressionNotApplication =
  expressionParens
  <|> (ExpressionLiteral <$> literal)
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

functionApplication :: Parser (FunctionApplication Text ())
functionApplication = do
  functionName <- identifier
  args <- someNonEmpty expressionNotApplication
  pure
    FunctionApplication
    { functionApplicationFunctionName = functionName
    , functionApplicationType = ()
    , functionApplicationArgs = args
    }
