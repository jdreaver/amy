-- | Version of a parser 'Module' after renaming.

module Amy.Renamer.AST
  ( RModule(..)
  , RBinding(..)
  , RExtern(..)
  , RExpr(..)
  , RIf(..)
  , RLet(..)
  , RApp(..)

  , RIdent(..)
  , RType(..)
  , RTypeName(..)
  , RScheme(..)

    -- Re-export
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

import Amy.Literal (Literal(..))
import Amy.Prim
import Amy.Syntax.Located
import Data.Text (Text)

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
  , rBindingType :: !(Maybe RScheme)
  , rBindingArgs :: ![Located RIdent]
  , rBindingBody :: !RExpr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data RExtern
  = RExtern
  { rExternName :: !(Located RIdent)
  , rExternType :: !RType
  } deriving (Show, Eq)

-- | A renamed 'Expr'
data RExpr
  = RELit !(Located Literal)
  | REVar !(Located RIdent)
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

-- | An identifier from source code
data RIdent
  = RIdent
  { rIdentText :: !Text
  , rIdentId :: !Int
  , rIdentPrimitiveName :: !(Maybe PrimitiveFunctionName)
  , rIdentIsTopLevel :: !Bool
  } deriving (Show, Eq, Ord)

data RType
  = RTyCon !RTypeName
  | RTyVar !RTypeName
  | RTyFun !RType !RType
  deriving (Show, Eq)

infixr 0 `RTyFun`

data RTypeName
  = RTypeName
  { rTypeNameText :: !Text
  , rTypeNameLocation :: !SourceSpan
  , rTypeNameId :: !Int
  , rTypeNamePrimitiveType :: !(Maybe PrimitiveType)
  } deriving (Show, Eq)

data RScheme
  = RForall ![RTypeName] RType
  deriving (Show, Eq)
