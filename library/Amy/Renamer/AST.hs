-- | Version of a parser 'Module' after renaming.

module Amy.Renamer.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , TyConDefinition(..)
  , tyConDefinitionToInfo
  , fromPrimTypeDef
  , DataConDefinition(..)
  , Expr(..)
  , Var(..)
  , DataCon(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , Let(..)
  , App(..)

  , Ident(..)
  , TypeTerm(..)
  , Type(..)
  , TyConInfo(..)
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
  { typeDeclarationTypeName :: !TyConDefinition
  , typeDeclarationConstructors :: ![DataConDefinition]
  } deriving (Show, Eq)

data TyConDefinition
  = TyConDefinition
  { tyConDefinitionName :: !Text
  , tyConDefinitionId :: !Int
  , tyConDefinitionArgs :: ![TyVarInfo]
  , tyConDefinitionLocation :: !(Maybe SourceSpan)
  } deriving (Show, Eq, Ord)

tyConDefinitionToInfo :: TyConDefinition -> TyConInfo
tyConDefinitionToInfo (TyConDefinition name' id' args span') = TyConInfo name' id' (TyVar <$> args) span'

fromPrimTyDef :: PrimTyCon -> TyConDefinition
fromPrimTyDef (PrimTyCon name id') = TyConDefinition name id' [] Nothing

fromPrimTypeDef :: PrimTypeDefinition -> TypeDeclaration
fromPrimTypeDef (PrimTypeDefinition tyCon dataCons) =
  TypeDeclaration (fromPrimTyDef tyCon) (fromPrimDataCon <$> dataCons)

data DataConDefinition
  = DataConDefinition
  { dataConDefinitionName :: !(Located Text)
  , dataConDefinitionId :: !Int
  , dataConDefinitionArgument :: !(Maybe TypeTerm)
  } deriving (Show, Eq)

fromPrimDataCon :: PrimDataCon -> DataConDefinition
fromPrimDataCon (PrimDataCon name id') =
  DataConDefinition (Located (SourceSpan "" 1 1 1 1) name) id' Nothing

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
  | VCons !DataCon
  deriving (Show, Eq)

data DataCon
  = DataCon
  { dataConName :: !(Located Text)
  , dataConId :: !Int
  } deriving (Show, Eq)

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
  { patConsConstructor :: !DataCon
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

data TypeTerm
  = TyCon !TyConInfo
  | TyVar !TyVarInfo
  | TyParens !TypeTerm
  deriving (Show, Eq, Ord)

data Type
  = TyTerm !TypeTerm
  | TyFun !Type !Type
  deriving (Show, Eq)

infixr 0 `TyFun`

data TyConInfo
  = TyConInfo
  { tyConInfoName :: !Text
  , tyConInfoId :: !Int
  , tyConInfoArgs :: ![TypeTerm]
  , tyConInfoLocation :: !(Maybe SourceSpan)
  } deriving (Show, Eq, Ord)

data TyVarInfo
  = TyVarInfo
  { tyVarInfoName :: !Text
  , tyVarInfoId :: !Int
  , tyVarInfoLocation :: !SourceSpan
  } deriving (Show, Eq, Ord)

data Scheme
  = Forall ![TyVarInfo] Type
  deriving (Show, Eq)
