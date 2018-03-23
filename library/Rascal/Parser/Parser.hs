module Rascal.Parser.Parser
  ( parserAST

  , functionType
  , function
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
  try (ParserASTFunctionType <$> functionType)
  <|> try (ParserASTFunction <$> function)

functionType :: Parser ParserFunctionTypeDeclaration
functionType = do
  functionName <- identifier
  doubleColon
  typeNames <- typeIdentifier `sepByNonEmpty` typeSeparatorArrow
  pure
    ParserFunctionTypeDeclaration
    { parserFunctionTypeDeclarationName = functionName
    , parserFunctionTypeDeclarationTypeNames = typeNames
    }

function :: Parser ParserFunctionDeclaration
function = do
  functionName <- identifier
  args <- many identifier
  equals
  expr <- expression
  pure
    ParserFunctionDeclaration
    { parserFunctionDeclarationName = functionName
    , parserFunctionDeclarationArgs = args
    , parserFunctionDeclarationBody = expr
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

literal :: Parser ParserLiteral
literal =
  ParserLiteralInt <$> integer

functionApplication :: Parser ParserFunctionApplication
functionApplication = do
  functionName <- identifier
  args <- someNonEmpty expressionNotApplication
  pure
    ParserFunctionApplication
    { parserFunctionApplicationFunctionName = functionName
    , parserFunctionApplicationArgs = args
    }
