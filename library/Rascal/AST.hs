module Rascal.AST
  ( AST(..)
  , TopLevel(..)
  , BindingValue(..)
  , BindingType(..)
  , Expression(..)
  , FunctionApplication(..)
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

-- | An 'AST' is simply a list of 'TopLevel' declarations.
newtype AST name = AST { unAST :: [TopLevel name] }
  deriving (Show, Eq)

-- | An 'TopLevel' value is anything that exists at the top level of a module.
data TopLevel name
  = TopLevelBindingValue !(BindingValue name)
  | TopLevelBindingType !(BindingType name)
  | TopLevelExternType !(BindingType name)
  deriving (Show, Eq)

-- | A 'BindingValue' is the value level part of a binding declaration. That
-- is, the part that isn't the type declaration. For example, in @f :: Int ->
-- Int@ and @f x = 1@, the binding value is the @f x = 1@ part.
data BindingValue name
  = BindingValue
  { bindingValueName :: !name
  , bindingValueArgs :: ![name]
  , bindingValueBody :: !(Expression name)
  } deriving (Show, Eq)

-- | A 'BindingType' is a type declaration for a 'BindingValue', like @f :: Int
-- -> Int@.
data BindingType name
  = BindingType
  { bindingTypeName :: !name
  , bindingTypeType :: !(NonEmpty name)
  } deriving (Show, Eq)

-- | An 'Expression' is any expression across the various ASTs. It is the
-- workhorse of AST types.
data Expression name
  = ExpressionLiteral !Literal
  | ExpressionVariable !name
  | ExpressionFunctionApplication (FunctionApplication name)
  | ExpressionParens (Expression name)
  deriving (Show, Eq)

data Variable name ty
  = Variable
  { variableName :: !name
  , variableType :: !ty
  } deriving (Show, Eq)

-- | A 'FunctionApplication' is a function followed by an expression per
-- argument, like @f x y@.
data FunctionApplication name
  = FunctionApplication
  { functionApplicationFunctionName :: !name
  , functionApplicationArgs :: !(NonEmpty (Expression name))
  } deriving (Show, Eq)

-- | A 'Literal' is any literal from the source code. This type is used in many
-- ASTs since there is no need for renaming or annotating types to a literal.
data Literal
  = LiteralInt !Int
  | LiteralDouble !Double
  deriving (Show, Eq)
