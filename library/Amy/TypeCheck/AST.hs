-- | Version of a renamer 'RModule' after type checking.

module Amy.TypeCheck.AST
  ( TModule(..)
  , TBinding(..)
  , TExtern(..)
  , TExpr(..)
  , TIf(..)
  , TLet(..)
  , TApp(..)

    -- Re-export
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

import Amy.Literal (Literal(..))
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
  , tBindingBody :: !(Typed TExpr)
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
  | TEVar !ValueName
  | TEIf !TIf
  | TELet !TLet
  | TEApp !TApp
  deriving (Show, Eq)

data TIf
  = TIf
  { tIfPredicate :: !(Typed TExpr)
  , tIfThen :: !(Typed TExpr)
  , tIfElse :: !(Typed TExpr)
  } deriving (Show, Eq)

data TLet
  = TLet
  { tLetBindings :: ![TBinding]
  , tLetExpression :: !(Typed TExpr)
  } deriving (Show, Eq)

data TApp
  = TApp
  { tAppFunction :: !(Typed TExpr)
  , tAppArgs :: !(NonEmpty (Typed TExpr))
  } deriving (Show, Eq)
