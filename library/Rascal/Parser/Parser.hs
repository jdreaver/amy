module Rascal.Parser.Parser
  ( parserAST

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

import Rascal.Parser.Lexer
import Rascal.Parser.AST

type Parser = Parsec Void Text
-- type ParserError = ParseError Void Char

parserAST :: Parser ParserAST
parserAST = ParserAST <$> declaration `sepByNonEmpty` semicolon

declaration :: Parser ParserASTDeclaration
declaration =
  try (ParserASTBindingType <$> bindingType)
  <|> try (ParserASTBinding <$> binding)

bindingType :: Parser ParserBindingTypeDeclaration
bindingType = do
  bindingName <- identifier
  doubleColon
  typeNames <- typeIdentifier `sepByNonEmpty` typeSeparatorArrow
  pure
    ParserBindingTypeDeclaration
    { parserBindingTypeDeclarationName = bindingName
    , parserBindingTypeDeclarationTypeNames = typeNames
    }

binding :: Parser ParserBindingDeclaration
binding = do
  bindingName <- identifier
  args <- many identifier
  equals
  expr <- expression
  pure
    ParserBindingDeclaration
    { parserBindingDeclarationName = bindingName
    , parserBindingDeclarationArgs = args
    , parserBindingDeclarationBody = expr
    }

expression :: Parser ParserASTExpression
expression =
  try (ParserASTFunctionApplication <$> functionApplication)
  <|> expressionNotApplication

-- | Parses any expression except function application. This is needed to avoid
-- left recursion. Without this distinction, f a b would be parsed as f (a b)
-- instead of (f a) b.
expressionNotApplication :: Parser ParserASTExpression
expressionNotApplication =
  expressionParens
  <|> (ParserASTLiteral <$> literal)
  <|> try (ParserASTVariable <$> identifier)

expressionParens :: Parser ParserASTExpression
expressionParens = ParserASTExpressionParens <$> between lparen rparen expression

literal :: Parser Literal
literal =
  try (LiteralDouble <$> double)
  <|> (LiteralInt <$> integer)

functionApplication :: Parser ParserFunctionApplication
functionApplication = do
  functionName <- identifier
  args <- someNonEmpty expressionNotApplication
  pure
    ParserFunctionApplication
    { parserFunctionApplicationFunctionName = functionName
    , parserFunctionApplicationArgs = args
    }
