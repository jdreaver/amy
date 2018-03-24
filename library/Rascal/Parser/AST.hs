module Rascal.Parser.AST
  ( ParserAST(..)
  , ParserASTDeclaration(..)
  , ParserBindingDeclaration(..)
  , ParserBindingTypeDeclaration(..)
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
  = ParserASTBinding ParserBindingDeclaration
  | ParserASTBindingType ParserBindingTypeDeclaration
  deriving (Show, Eq)

data ParserBindingDeclaration
  = ParserBindingDeclaration
  { parserBindingDeclarationName :: !Text
  , parserBindingDeclarationArgs :: ![Text]
  , parserBindingDeclarationBody :: !ParserASTExpression
  } deriving (Show, Eq)

data ParserBindingTypeDeclaration
  = ParserBindingTypeDeclaration
  { parserBindingTypeDeclarationName :: !Text
  , parserBindingTypeDeclarationTypeNames :: !(NonEmpty Text)
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
  | LiteralDouble !Double
  deriving (Show, Eq)

data ParserFunctionApplication
  = ParserFunctionApplication
  { parserFunctionApplicationFunctionName :: !Text
  , parserFunctionApplicationArgs :: !(NonEmpty ParserASTExpression)
  } deriving (Show, Eq)
