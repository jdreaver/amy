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
import Amy.Prim
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
  , tBindingType :: !(Type PrimitiveType)
    -- ^ Type for whole function
  , tBindingArgs :: ![Typed PrimitiveType ValueName]
    -- ^ Argument names and types split out from 'tBindingType'
  , tBindingReturnType :: !(Type PrimitiveType)
    -- ^ Return type split out from 'tBindingType'
  , tBindingBody :: !TExpr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data TExtern
  = TExtern
  { tExternName :: !ValueName
  , tExternType :: !(Type PrimitiveType)
  } deriving (Show, Eq)

-- | A renamed 'Expr'
data TExpr
  = TELit !Literal
  | TEVar !(Typed PrimitiveType ValueName)
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
  , tAppArgs :: !(NonEmpty (Typed PrimitiveType TExpr))
  , tAppReturnType :: !PrimitiveType
  } deriving (Show, Eq)

expressionType :: TExpr -> Type PrimitiveType
expressionType (TELit lit) = TVar $ literalType lit
expressionType (TEVar (Typed ty _)) = ty
expressionType (TEIf if') = expressionType (tIfThen if') -- Checker ensure "then" and "else" types match
expressionType (TELet let') = expressionType (tLetExpression let')
expressionType (TEApp app) = TVar $ tAppReturnType app
