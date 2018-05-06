-- | Version of a parser 'Module' after renaming.

module Amy.Renamer.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , DataConstructor(..)
  , fromPrimDataCon
  , Expr(..)
  , Var(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , Let(..)
  , App(..)

  , Ident(..)
  , Type(..)
  , TyConInfo(..)
  , fromPrimTyCon
  , TyVarInfo(..)
  , Scheme(..)

    -- Re-export
  , Literal(..)
  , module Amy.ASTCommon
  ) where

import Data.List.NonEmpty (NonEmpty)

import Amy.ASTCommon
import Amy.Literal (Literal(..))
import Amy.Prim
import Amy.Syntax.Located
import Data.Text (Text)

data Module
  = Module
  { moduleBindings :: ![Binding]
  , moduleExterns :: ![Extern]
  , moduleTypeDeclarations :: ![TypeDeclaration]
    -- TODO: Instead of threading an Int through every AST Module, consider
    -- running the whole compiler in some monad that admits a fresh name
    -- supply. I'm not sure if that would make things easier at the expense of
    -- coupling.
  , moduleMaxId :: !Int
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
  { dataConstructorName :: !(Located Text)
  , dataConstructorId :: !Int
  , dataConstructorArgument :: !(Maybe TyConInfo)
  , dataConstructorType :: !TyConInfo
  , dataConstructorSpan :: !ConstructorSpan
  , dataConstructorIndex :: !ConstructorIndex
  } deriving (Show, Eq)

fromPrimDataCon :: PrimDataCon -> DataConstructor
fromPrimDataCon (PrimDataCon name id' ty span' index) =
  DataConstructor (Located (SourceSpan "" 1 1 1 1) name) id' Nothing (fromPrimTyCon ty) span' index

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
  | VCons !DataConstructor
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
  | PVar !(Located Ident)
  | PCons !PatCons
  | PParens !Pattern
  deriving (Show, Eq)

data PatCons
  = PatCons
  { patConsConstructor :: !DataConstructor
  , patConsArg :: !(Maybe Pattern)
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
  } deriving (Show, Eq, Ord)

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
