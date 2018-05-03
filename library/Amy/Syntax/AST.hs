-- | AST for the frontend parser.

module Amy.Syntax.AST
  ( Module(..)
  , Declaration(..)
  , declBinding
  , declBindingType
  , declExtern
  , declType
  , Binding(..)
  , BindingType(..)
  , Extern(..)
  , TypeDeclaration(..)
  , DataConstructor(..)
  , Expr(..)
  , Var(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , ConstructorPattern(..)
  , Let(..)
  , letBinding
  , letBindingType
  , LetBinding(..)
  , App(..)
  , Type(..)
  , Scheme(..)

    -- Re-export
  , Literal(..)
  , Located(..)
  , SourceSpan(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Amy.Literal (Literal(..))
import Amy.Syntax.Located

-- | A 'Module' is simply a list of 'Declaration' values.
newtype Module = Module { unModule :: [Declaration] }
  deriving (Show, Eq)

data Declaration
  = DeclBinding !Binding
  | DeclBindingType !BindingType
  | DeclExtern !Extern
  | DeclType !TypeDeclaration
  deriving (Show, Eq)

declBinding :: Declaration -> Maybe Binding
declBinding (DeclBinding x) = Just x
declBinding _ = Nothing

declBindingType :: Declaration -> Maybe BindingType
declBindingType (DeclBindingType x) = Just x
declBindingType _ = Nothing

declExtern :: Declaration -> Maybe Extern
declExtern (DeclExtern x) = Just x
declExtern _ = Nothing

declType :: Declaration -> Maybe TypeDeclaration
declType (DeclType x) = Just x
declType _ = Nothing

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
  , bindingTypeScheme :: !Scheme
  } deriving (Show, Eq)

-- | A 'BindingType' is a top-level declaration of a 'Binding' type, like @x ::
-- Int@ or @f :: Int -> Int@
data Extern
  = Extern
  { externName :: !(Located Text)
  , externType :: !Type
  } deriving (Show, Eq)

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !(Located Text)
  , typeDeclarationConstructors :: ![DataConstructor]
  } deriving (Show, Eq)

data DataConstructor
  = DataConstructor
  { dataConstructorName :: !(Located Text)
  , dataConstructorArgument :: !(Maybe (Located Text))
  } deriving (Show, Eq)

data Expr
  = ELit !(Located Literal)
  | EVar !Var
  | EIf !If
  | ECase !Case
  | ELet !Let
  | EApp !App
  | EParens !Expr
  deriving (Show, Eq)

data Var
  = VVal !(Located Text)
  | VCons !(Located Text)
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
  | PatternVar !(Located Text)
  | PatternCons !ConstructorPattern
  | PatternParens !Pattern
  deriving (Show, Eq)

data ConstructorPattern
  = ConstructorPattern
  { constructorPatternConstructor :: !(Located Text)
  , constructorPatternArg :: !(Maybe Pattern)
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

data Type
  = TyCon !(Located Text)
  | TyVar !(Located Text)
  | TyFun !Type !Type
  deriving (Show, Eq)

infixr 0 `TyFun`

data Scheme
  = Forall ![Located Text] Type
  deriving (Show, Eq)
