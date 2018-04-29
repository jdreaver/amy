-- | Version of a parser 'Module' after renaming.

module Amy.Renamer.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , Expr(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , Let(..)
  , App(..)

  , Ident(..)
  , Type(..)
  , TypeName(..)
  , Scheme(..)

    -- Re-export
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

import Amy.Literal (Literal(..))
import Amy.Prim
import Amy.Syntax.Located
import Data.Text (Text)

data Module
  = Module
  { moduleBindings :: ![Binding]
  , moduleExterns :: ![Extern]
  } deriving (Show, Eq)

-- | A binding after renaming. This is a combo of a 'Binding' and a
-- 'BindingType' after they've been paired together.
data Binding
  = Binding
  { bindingName :: !(Located Ident)
  , bindingType :: !(Maybe Scheme)
  , bindingArgs :: ![Located Ident]
  , bindingBody :: !Expr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data Extern
  = Extern
  { externName :: !(Located Ident)
  , externType :: !Type
  } deriving (Show, Eq)

-- | A renamed 'Expr'
data Expr
  = ELit !(Located Literal)
  | EVar !(Located Ident)
  | EIf !If
  | ECase !Case
  | ELet !Let
  | EApp !App
  | EParens !Expr
  deriving (Show, Eq)

data If
  = If
  { ifPredicate :: !Expr
  , ifThen :: !Expr
  , ifElse :: !Expr
  } deriving (Show, Eq)

data Case
  = Case
  { caseScrutinee :: !Expr
  , caseAlternatives :: !(NonEmpty Match)
  } deriving (Show, Eq)

data Match
  = Match
  { matchPattern :: !Pattern
  , matchBody :: !Expr
  } deriving (Show, Eq)

data Pattern
  = PatternLit !(Located Literal)
  | PatternVar !(Located Ident)
  deriving (Show, Eq)

data Let
  = Let
  { letBindings :: ![Binding]
  , letExpression :: !Expr
  } deriving (Show, Eq)

-- | An 'App' after enaming.
data App
  = App
  { appFunction :: !Expr
  , appArgs :: !(NonEmpty Expr)
  } deriving (Show, Eq)

-- | An identifier from source code
data Ident
  = Ident
  { identText :: !Text
  , identId :: !Int
  , identPrimitiveName :: !(Maybe PrimitiveFunctionName)
  } deriving (Show, Eq, Ord)

data Type
  = TyCon !TypeName
  | TyVar !TypeName
  | TyFun !Type !Type
  deriving (Show, Eq)

infixr 0 `TyFun`

data TypeName
  = TypeName
  { typeNameText :: !Text
  , typeNameLocation :: !SourceSpan
  , typeNameId :: !Int
  , typeNamePrimitiveType :: !(Maybe PrimitiveType)
  } deriving (Show, Eq)

data Scheme
  = Forall ![TypeName] Type
  deriving (Show, Eq)
