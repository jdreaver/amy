module Rascal.Parser.AST
  ( ParserAST(..)
  , ParserASTDeclaration(..)
  , ParserFunctionDeclaration(..)
  , ParserFunctionTypeDeclaration(..)
  , ParserASTExpression(..)
  , Literal(..)
  , ParserFunctionApplication(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

-- | AST produced by the parser.
newtype ParserAST = ParserAST (NonEmpty ParserASTDeclaration)
  deriving (Show, Eq)

data ParserASTDeclaration
  = ParserASTFunction ParserFunctionDeclaration
  | ParserASTFunctionType ParserFunctionTypeDeclaration
  deriving (Show, Eq)

data ParserFunctionDeclaration
  = ParserFunctionDeclaration
  { parserFunctionDeclarationName :: !Text
  , parserFunctionDeclarationArgs :: ![Text]
  , parserFunctionDeclarationBody :: !ParserASTExpression
  } deriving (Show, Eq)

data ParserFunctionTypeDeclaration
  = ParserFunctionTypeDeclaration
  { parserFunctionTypeDeclarationName :: !Text
  , parserFunctionTypeDeclarationTypeNames :: !(NonEmpty Text)
    -- ^ List of type names in order. For example "Int -> Int -> Double" would
    -- be ["Int", "Int", "Double"]
  } deriving (Show, Eq)

data ParserASTExpression
  = ParserASTLiteral Literal
  | ParserASTVariable Text
  | ParserASTFunctionApplication ParserFunctionApplication
  | ParserASTExpressionParens ParserASTExpression
  deriving (Show, Eq)

data Literal
  = LiteralInt !Int
  deriving (Show, Eq)

data ParserFunctionApplication
  = ParserFunctionApplication
  { parserFunctionApplicationFunctionName :: !Text
  , parserFunctionApplicationArgs :: !(NonEmpty ParserASTExpression)
  } deriving (Show, Eq)
