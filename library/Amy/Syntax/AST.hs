-- | AST for the frontend parser.

module Amy.Syntax.AST
  ( Module(..)
  , Declaration(..)
  , declBinding
  , declBindingType
  , declExtern
  , Binding(..)
  , BindingType(..)
  , Expr(..)
  , If(..)
  , Let(..)
  , letBinding
  , letBindingType
  , LetBinding(..)
  , App(..)

    -- Re-export
  , Literal(..)
  , Located(..)
  , SourceSpan(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Amy.Literal (Literal(..))
import Amy.Syntax.Located
import Amy.Type

-- | A 'Module' is simply a list of 'Declaration' values.
newtype Module = Module { unModule :: [Declaration] }
  deriving (Show, Eq)

data Declaration
  = DeclBinding !Binding
  | DeclBindingType !BindingType
  | DeclExtern !BindingType
  deriving (Show, Eq)

declBinding :: Declaration -> Maybe Binding
declBinding (DeclBinding x) = Just x
declBinding _ = Nothing

declBindingType :: Declaration -> Maybe BindingType
declBindingType (DeclBindingType x) = Just x
declBindingType _ = Nothing

declExtern :: Declaration -> Maybe BindingType
declExtern (DeclExtern x) = Just x
declExtern _ = Nothing

-- | A 'Binding' is a top-level definition of a binding, like @x = 1@ or @f x =
-- x@
data Binding
  = Binding
  { bindingName :: !(Located Text)
  , bindingArgs :: ![Located Text]
  , bindingBody :: !Expr
  } deriving (Show, Eq)

-- | A 'BindingType' is a top-level declaration of a 'Binding' type, like @x ::
-- Int@ or @f :: Int -> Int@
data BindingType
  = BindingType
  { bindingTypeName :: !(Located Text)
  , bindingTypeTypeNames :: !(Type (Located Text))
  } deriving (Show, Eq)

data Expr
  = ELit !(Located Literal)
  | EVar !(Located Text)
  | EIf !If
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

data Let
  = Let
  { letBindings :: ![LetBinding]
  , letExpression :: !Expr
  } deriving (Show, Eq)

data LetBinding
  = LetBinding !Binding
  | LetBindingType !BindingType
  deriving (Show, Eq)

letBinding :: LetBinding -> Maybe Binding
letBinding (LetBinding x) = Just x
letBinding _ = Nothing

letBindingType :: LetBinding -> Maybe BindingType
letBindingType (LetBindingType x) = Just x
letBindingType _ = Nothing

-- | Function application
data App
  = App
  { appFunction :: !Expr
  , appArgs :: !(NonEmpty Expr)
  } deriving (Show, Eq)
