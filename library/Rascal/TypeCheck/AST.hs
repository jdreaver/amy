{-# LANGUAGE DeriveFunctor #-}

module Rascal.TypeCheck.AST
  ( NameId
  , Literal(..)
  , PrimitiveType(..)
  , FunctionType(..)
  , Typed(..)
  , TypeCheckAST(..)
  , TypeCheckASTDeclaration(..)
  , TypeCheckFunctionDeclaration(..)
  , TypeCheckASTExpression(..)
  , TypeCheckFunctionApplication(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

import Rascal.Parser.AST (Literal(..))
import Rascal.Renamer.AST (NameId, IdName)

data PrimitiveType
  = IntType
  deriving (Show, Eq)

data FunctionType
  = FunctionType
  { functionTypeArgTypes :: ![Typed IdName]
  , functionTypeReturnType :: !(Typed IdName)
  } deriving (Show, Eq)

-- | AST produced by the typeCheck.
newtype TypeCheckAST = TypeCheckAST [TypeCheckASTDeclaration]
  deriving (Show, Eq)

data Typed a
  = Typed
  { typedType :: !PrimitiveType
  , typedValue :: !a
  } deriving (Show, Eq, Functor)

data TypeCheckASTDeclaration
  = TypeCheckASTFunction !TypeCheckFunctionDeclaration
  deriving (Show, Eq)

data TypeCheckFunctionDeclaration
  = TypeCheckFunctionDeclaration
  { typeCheckFunctionDeclarationName :: !IdName
  , typeCheckFunctionDeclarationType :: !FunctionType
  , typeCheckFunctionDeclarationBody :: !(Typed TypeCheckASTExpression)
  } deriving (Show, Eq)

data TypeCheckASTExpression
  = TypeCheckASTLiteral !Literal
  | TypeCheckASTVariable !IdName
  | TypeCheckASTFunctionApplication !TypeCheckFunctionApplication
  | TypeCheckASTExpressionParens !TypeCheckASTExpression
  deriving (Show, Eq)

data TypeCheckFunctionApplication
  = TypeCheckFunctionApplication
  { typeCheckFunctionApplicationFunctionName :: !IdName
  , typeCheckFunctionApplicationArgs :: !(NonEmpty (Typed TypeCheckASTExpression))
  } deriving (Show, Eq)
