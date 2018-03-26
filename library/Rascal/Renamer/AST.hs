module Rascal.Renamer.AST
  ( IdName(..)
  , IdNameProvenance(..)
  , NameId
  , RenamerAST(..)
  , RenamerASTDeclaration(..)
  , RenamerBindingDeclaration(..)
  , RenamerExternDeclaration(..)
  , RenamerASTExpression(..)
  , RenamerFunctionApplication(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Rascal.AST (Literal)

data IdName
  = IdName
  { idNameText :: !Text
  , idNameId :: !NameId
  , idNameProvenance :: !IdNameProvenance
  } deriving (Show, Eq)

data IdNameProvenance
  = LocalDefinition
  | TopLevelDefinition
  deriving (Show, Eq)

type NameId = Int

-- | AST produced by the renamer.
newtype RenamerAST = RenamerAST [RenamerASTDeclaration]
  deriving (Show, Eq)

data RenamerASTDeclaration
  = RenamerASTBinding !RenamerBindingDeclaration
  | RenamerASTExtern !RenamerExternDeclaration
  deriving (Show, Eq)

data RenamerBindingDeclaration
  = RenamerBindingDeclaration
  { renamerBindingDeclarationName :: !IdName
  , renamerBindingDeclarationArgs :: ![IdName]
  , renamerBindingDeclarationTypeNames :: !(NonEmpty IdName)
  , renamerBindingDeclarationBody :: !RenamerASTExpression
  } deriving (Show, Eq)

data RenamerExternDeclaration
  = RenamerExternDeclaration
  { renamerExternDeclarationName :: !IdName
  , renamerExternDeclarationTypeNames :: !(NonEmpty IdName)
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
