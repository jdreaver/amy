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
  , TyConDefinition(..)
  , DataConDefinition(..)
  , Expr(..)
  , Row(..)
  , Var(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , Let(..)
  , letBinding
  , letBindingType
  , LetBinding(..)
  , App(..)
  , Type(..)
  , TyRow(..)
  , Scheme(..)

    -- Re-export
  , Literal(..)
  , Located(..)
  , SourceSpan(..)
  , module Amy.Names
  ) where

import Data.List.NonEmpty (NonEmpty)

import Amy.Literal (Literal(..))
import Amy.Names
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
  { bindingName :: !(Located IdentName)
  , bindingArgs :: ![Located IdentName]
  , bindingBody :: !Expr
  } deriving (Show, Eq)

-- | A 'BindingType' is a top-level declaration of a 'Binding' type, like @x ::
-- Int@ or @f :: Int -> Int@
data BindingType
  = BindingType
  { bindingTypeName :: !(Located IdentName)
  , bindingTypeScheme :: !Scheme
  } deriving (Show, Eq)

-- | A 'BindingType' is a top-level declaration of a 'Binding' type, like @x ::
-- Int@ or @f :: Int -> Int@
data Extern
  = Extern
  { externName :: !(Located IdentName)
  , externType :: !Type
  } deriving (Show, Eq)

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !TyConDefinition
  , typeDeclarationConstructors :: ![DataConDefinition]
  } deriving (Show, Eq)

data TyConDefinition
  = TyConDefinition
  { tyConDefinitionName :: !TyConName
  , tyConDefinitionArgs :: ![Located TyVarName]
  , tyConDefinitionLocation :: !SourceSpan
  } deriving (Show, Eq)

data DataConDefinition
  = DataConDefinition
  { dataConDefinitionName :: !(Located DataConName)
  , dataConDefinitionArgument :: !(Maybe Type)
  } deriving (Show, Eq)

data Expr
  = ELit !(Located Literal)
  | ERecord ![Row]
  | EVar !Var
  | EIf !If
  | ECase !Case
  | ELet !Let
  | EApp !App
  | EParens !Expr
  deriving (Show, Eq)

data Row
  = Row
  { rowLabel :: !(Located RowLabel)
  , rowValue :: !Expr
  } deriving (Show, Eq)

data Var
  = VVal !(Located IdentName)
  | VCons !(Located DataConName)
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
  = PLit !(Located Literal)
  | PVar !(Located IdentName)
  | PCons !PatCons
  | PParens !Pattern
  deriving (Show, Eq)

data PatCons
  = PatCons
  { patConsConstructor :: !(Located DataConName)
  , patConsArg :: !(Maybe Pattern)
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
  = TyCon !(Located TyConName)
  | TyVar !(Located TyVarName)
  | TyApp !(Located TyConName) !(NonEmpty Type)
  | TyRecord ![TyRow]
  | TyFun !Type !Type
  deriving (Show, Eq)

infixr 0 `TyFun`

data TyRow
  = TyRow
  { tyRowLabel :: !(Located RowLabel)
  , tyRowType :: !Type
  } deriving (Show, Eq)

data Scheme
  = Forall ![Located TyVarName] Type
  deriving (Show, Eq)
