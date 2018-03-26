{-# LANGUAGE DeriveFunctor #-}

module Rascal.TypeCheck.AST
  ( NameId
  , IdName(..)
  , IdNameProvenance(..)
  , Literal(..)
  , Type(..)
  , PrimitiveType(..)
  , primitiveType
  , unPrimitiveType
  , FunctionType(..)
  , functionType
  , bindingReturnType
  , Typed(..)
  , TypeCheckAST(..)
  , TypeCheckASTDeclaration(..)
  , TypeCheckBindingDeclaration(..)
  , TypeCheckExternDeclaration(..)
  , TypeCheckASTExpression(..)
  , TypeCheckFunctionApplication(..)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Rascal.AST (Literal(..))
import Rascal.Renamer.AST

-- | A 'Type' is a 'NonEmpty' list of primitive types. A primitive type is just
-- a single @[PrimitiveType]@. A function is a 'Type' of length greater than
-- one, where the last type is the return type.
--
-- For example, a type Int is just [IntType]. A type Int -> Int is [IntType,
-- IntType].
newtype Type = Type { unType :: NonEmpty PrimitiveType }
  deriving (Show, Eq)

data PrimitiveType
  = IntType
  | DoubleType
  deriving (Show, Eq)

primitiveType :: Type -> Maybe PrimitiveType
primitiveType (Type ts) =
  if null (NE.tail ts)
  then Just $ NE.head ts
  else Nothing

unPrimitiveType :: PrimitiveType -> Type
unPrimitiveType pt = Type (pt :| [])

functionType :: Type -> Maybe FunctionType
functionType (Type ts) = do
  args <- NE.nonEmpty (NE.init ts)
  pure $ FunctionType args (NE.last ts)

bindingReturnType :: Type -> PrimitiveType
bindingReturnType (Type ts) = NE.last ts

data FunctionType
  = FunctionType
  { functionTypeArgTypes :: !(NonEmpty PrimitiveType)
  , functionTypeReturnType :: !PrimitiveType
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
  = TypeCheckASTBinding !TypeCheckBindingDeclaration
  | TypeCheckASTExtern !TypeCheckExternDeclaration
  deriving (Show, Eq)

data TypeCheckBindingDeclaration
  = TypeCheckBindingDeclaration
  { typeCheckBindingDeclarationName :: !IdName
    -- TODO: Have arguments be pairs of (IdName, PrimitiveType) so we
    -- statically know that there is one argument name per argument.
  , typeCheckBindingDeclarationArgs :: ![IdName]
  , typeCheckBindingDeclarationType :: !Type
  , typeCheckBindingDeclarationBody :: !(Typed TypeCheckASTExpression)
  } deriving (Show, Eq)

data TypeCheckExternDeclaration
  = TypeCheckExternDeclaration
  { typeCheckExternDeclarationName :: !IdName
  , typeCheckExternDeclarationType :: !Type
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
