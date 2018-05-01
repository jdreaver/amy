-- | Version of a parser 'Module' after renaming.

module Amy.Renamer.AST
  ( Module(..)
  , Binding(..)
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
  , App(..)

  , Ident(..)
  , ConstructorName(..)
  , fromPrimDataCon
  , Type(..)
  , TyConInfo(..)
  , fromPrimTyCon
  , TyVarInfo(..)
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
  , moduleTypeDeclarations :: ![TypeDeclaration]
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

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !TyConInfo
  , typeDeclarationConstructors :: ![DataConstructor]
  } deriving (Show, Eq)

data DataConstructor
  = DataConstructor
  { dataConstructorName :: !(Located ConstructorName)
  , dataConstructorArgument :: !(Maybe TyConInfo)
  } deriving (Show, Eq)

-- | A renamed 'Expr'
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
  = VVal !(Located Ident)
  | VCons !(Located ConstructorName)
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
  | PatternCons !ConstructorPattern
  deriving (Show, Eq)

data ConstructorPattern
  = ConstructorPattern
  { constructorPatternConstructor :: !(Located ConstructorName)
  , constructorPatternArg :: !(Maybe (Located Ident))
  } deriving (Show, Eq)

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

data Ident
  = Ident
  { identText :: !Text
  , identId :: !Int
  , identPrimitiveName :: !(Maybe PrimitiveFunctionName)
  } deriving (Show, Eq, Ord)

data ConstructorName
  = ConstructorName
  { constructorNameText :: !Text
  , constructorNameId :: !Int
  } deriving (Show, Eq, Ord)

fromPrimDataCon :: PrimDataCon -> ConstructorName
fromPrimDataCon (PrimDataCon name id') = ConstructorName name id'

data Type
  = TyCon !TyConInfo
  | TyVar !TyVarInfo
  | TyFun !Type !Type
  deriving (Show, Eq)

infixr 0 `TyFun`

data TyConInfo
  = TyConInfo
  { tyConInfoText :: !Text
  , tyConInfoLocation :: !(Maybe SourceSpan)
  , tyConInfoId :: !Int
  } deriving (Show, Eq)

fromPrimTyCon :: PrimTyCon -> TyConInfo
fromPrimTyCon (PrimTyCon name id') = TyConInfo name Nothing id'

data TyVarInfo
  = TyVarInfo
  { tyVarInfoName :: !Text
  , tyVarInfoId :: !Int
  , tyVarInfoLocation :: !SourceSpan
  } deriving (Show, Eq)

data Scheme
  = Forall ![TyVarInfo] Type
  deriving (Show, Eq)
