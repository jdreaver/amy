-- | Version of a parser 'Module' after renaming.

module Amy.Renamer.AST
  ( RModule(..)
  , RBinding(..)
  , RExtern(..)
  , RExpr(..)
  , RIf(..)
  , RLet(..)
  , RApp(..)

    -- Re-export
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

import Amy.Literal (Literal(..))
import Amy.Names
import Amy.Prim
import Amy.Syntax.Located
import Amy.Type

-- | An 'RModule' is a 'Module' after renaming.
data RModule
  = RModule
  { rModuleBindings :: ![RBinding]
  , rModuleExterns :: ![RExtern]
  }
  deriving (Show, Eq)

-- | A binding after renaming. This is a combo of a 'Binding' and a
-- 'BindingType' after they've been paired together.
data RBinding
  = RBinding
  { rBindingName :: !(Located Name)
  , rBindingType :: !(Maybe (Type (Located PrimitiveType)))
  , rBindingArgs :: ![Located Name]
  , rBindingBody :: !RExpr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data RExtern
  = RExtern
  { rExternName :: !(Located Name)
  , rExternType :: !(Type (Located PrimitiveType))
  } deriving (Show, Eq)

-- | A renamed 'Expr'
data RExpr
  = RELit !(Located Literal)
  | REVar !(Located Name)
  | REIf !RIf
  | RELet !RLet
  | REApp !RApp
  deriving (Show, Eq)

data RIf
  = RIf
  { rIfPredicate :: !RExpr
  , rIfThen :: !RExpr
  , rIfElse :: !RExpr
  } deriving (Show, Eq)

data RLet
  = RLet
  { rLetBindings :: ![RBinding]
  , rLetExpression :: !RExpr
  } deriving (Show, Eq)

-- | An 'App' after renaming.
data RApp
  = RApp
  { rAppFunction :: !RExpr
  , rAppArgs :: !(NonEmpty RExpr)
  } deriving (Show, Eq)
