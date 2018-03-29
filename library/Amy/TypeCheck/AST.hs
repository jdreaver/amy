-- | Version of a renamer 'RModule' after type checking.

module Amy.TypeCheck.AST
  ( TModule(..)
  , TBinding(..)
  , TExtern(..)
  , TExpr(..)
  , TIf(..)
  , TLet(..)
  , TApp(..)
  , expressionType

    -- Re-export
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

import Amy.Literal
import Amy.Names
import Amy.Type

-- | A 'TModule' is an 'RModule' after renaming.
data TModule
  = TModule
  { tModuleBindings :: ![TBinding]
  , tModuleExterns :: ![TExtern]
  }
  deriving (Show, Eq)

-- | A binding after renaming. This is a combo of a 'Binding' and a
-- 'BindingType' after they've been paired together.
data TBinding
  = TBinding
  { tBindingName :: !ValueName
  , tBindingArgs :: ![(PrimitiveType, ValueName)]
  , tBindingReturnType :: !PrimitiveType
  , tBindingBody :: !TExpr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data TExtern
  = TExtern
  { tExternName :: !ValueName
  , tExternType :: !(NonEmpty PrimitiveType)
  } deriving (Show, Eq)

-- | A renamed 'Expr'
data TExpr
  = TELit !Literal
  | TEVar !(Typed ValueName)
  | TEIf !TIf
  | TELet !TLet
  | TEApp !TApp
  deriving (Show, Eq)

data TIf
  = TIf
  { tIfPredicate :: !TExpr
  , tIfThen :: !TExpr
  , tIfElse :: !TExpr
  } deriving (Show, Eq)

data TLet
  = TLet
  { tLetBindings :: ![TBinding]
  , tLetExpression :: !TExpr
  } deriving (Show, Eq)

data TApp
  = TApp
  { tAppFunction :: !TExpr
  , tAppArgs :: !(NonEmpty (PrimitiveType, TExpr))
  , tAppReturnType :: !PrimitiveType
  } deriving (Show, Eq)

expressionType :: TExpr -> Type
expressionType (TELit lit) = PrimitiveTy $ literalType lit
expressionType (TEVar (Typed ty _)) = ty
expressionType (TEIf if') = expressionType (tIfThen if') -- Checker ensure "then" and "else" types match
expressionType (TELet let') = expressionType (tLetExpression let')
expressionType (TEApp app) = PrimitiveTy $ tAppReturnType app
