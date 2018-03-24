module Rascal.Renamer.AST
  ( IdName(..)
  , NameId
  , RenamerAST(..)
  , RenamerASTDeclaration(..)
  , RenamerFunctionDeclaration(..)
  , RenamerASTExpression(..)
  , RenamerFunctionApplication(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Rascal.Parser.AST (Literal)

data IdName
  = IdName
  { idNameText :: !Text
  , idNameId :: !NameId
  } deriving (Show, Eq)

type NameId = Int

-- | AST produced by the renamer.
newtype RenamerAST = RenamerAST (NonEmpty RenamerASTDeclaration)
  deriving (Show, Eq)

data RenamerASTDeclaration
  = RenamerASTFunction !RenamerFunctionDeclaration
  deriving (Show, Eq)

data RenamerFunctionDeclaration
  = RenamerFunctionDeclaration
  { renamerFunctionDeclarationName :: !IdName
  , renamerFunctionDeclarationArgs :: ![IdName]
  , renamerFunctionDeclarationTypeNames :: !(NonEmpty IdName)
  , renamerFunctionDeclarationBody :: !RenamerASTExpression
  } deriving (Show, Eq)

data RenamerASTExpression
  = RenamerASTLiteral !Literal
  | RenamerASTVariable !IdName
  | RenamerASTFunctionApplication !RenamerFunctionApplication
  | RenamerASTExpressionParens !RenamerASTExpression
  deriving (Show, Eq)

data RenamerFunctionApplication
  = RenamerFunctionApplication
  { renamerFunctionApplicationFunctionName :: !IdName
  , renamerFunctionApplicationArgs :: !(NonEmpty RenamerASTExpression)
  } deriving (Show, Eq)
