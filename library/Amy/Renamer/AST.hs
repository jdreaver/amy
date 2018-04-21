-- | Version of a parser 'Module' after renaming.

module Amy.Renamer.AST
  ( RModule(..)
  , RBinding(..)
  , RExtern(..)
  , RExpr(..)
  , RIf(..)
  , RLet(..)
  , RApp(..)

  , RName(..)
  , RIdent(..)

    -- Re-export
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

import Amy.Literal (Literal(..))
import Amy.Prim
import Amy.Syntax.Located
import Data.Text (Text)
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
  { rBindingName :: !(Located RIdent)
  , rBindingType :: !(Maybe (Scheme (Located PrimitiveType)))
  , rBindingArgs :: ![Located RName]
  , rBindingBody :: !RExpr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data RExtern
  = RExtern
  { rExternName :: !(Located RIdent)
  , rExternType :: !(Type (Located PrimitiveType))
  } deriving (Show, Eq)

-- | A renamed 'Expr'
data RExpr
  = RELit !(Located Literal)
  | REVar !(Located RName)
  | REIf !RIf
  | RELet !RLet
  | REApp !RApp
  | REParens !RExpr
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

-- | An 'RName' is an identified name of something from the source code after
-- renaming.
data RName
  = RPrimitiveName !PrimitiveFunctionName
  | RIdentName !RIdent
  deriving (Show, Eq, Ord)

-- | An identifier from source code
data RIdent
  = RIdent
  { rIdentText :: !Text
  , rIdentId :: !Int
  , rIdentIsTopLevel :: !Bool
  } deriving (Show, Eq, Ord)
