{-# LANGUAGE DeriveFunctor #-}

-- | Version of a renamer 'RModule' after type checking.

module Amy.TypeCheck.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , TyConDefinition(..)
  , fromPrimTypeDef
  , DataConstructor(..)
  , fromPrimDataCon
  , DataConInfo(..)
  , Expr(..)
  , Var(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , Let(..)
  , App(..)
  , expressionType
  , patternType

  , Ident(..)
  , TypeTerm(..)
  , Type(..)
  , TyConInfo(..)
  , fromPrimTyCon
  , TyVarInfo(..)
  , TyVarGenerated(..)
  , typeKind
  , Scheme(..)
  , Typed(..)

    -- Re-export
  , Literal(..)
  , module Amy.ASTCommon
  , module Amy.Kind
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import Amy.ASTCommon
import Amy.Kind
import Amy.Literal
import Amy.Prim

data Module
  = Module
  { moduleBindings :: ![Binding]
  , moduleExterns :: ![Extern]
  , moduleTypeDeclarations :: ![TypeDeclaration]
  , moduleMaxId :: !Int
  } deriving (Show, Eq)

-- | A binding after renaming. This is a combo of a 'Binding' and a
-- 'BindingType' after they've been paired together.
data Binding
  = Binding
  { bindingName :: !Ident
  , bindingType :: !Scheme
    -- ^ Type for whole function
  , bindingArgs :: ![Typed Ident]
    -- ^ Argument names and types split out from 'bindingType'
  , bindingReturnType :: !Type
    -- ^ Return type split out from 'bindingType'
  , bindingBody :: !Expr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data Extern
  = Extern
  { externName :: !Ident
  , externType :: !Type
  } deriving (Show, Eq)

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !TyConDefinition
  , typeDeclarationConstructors :: ![DataConstructor]
  } deriving (Show, Eq, Ord)

data TyConDefinition
  = TyConDefinition
  { tyConDefinitionName :: !Text
  , tyConDefinitionId :: !Int
  , tyConDefinitionArgs :: ![TyVarInfo]
  , tyConDefinitionKind :: !Kind
  } deriving (Show, Eq, Ord)

tyConDefinitionToInfo :: TyConDefinition -> TyConInfo
tyConDefinitionToInfo tyDef@(TyConDefinition name' id' args _) = TyConInfo name' id' (TyVar <$> args) tyDef

fromPrimTyDef :: PrimTyCon -> TyConDefinition
fromPrimTyDef (PrimTyCon name id') = TyConDefinition name id' [] KStar

fromPrimTypeDef :: PrimTypeDefinition -> TypeDeclaration
fromPrimTypeDef (PrimTypeDefinition tyCon dataCons) =
  TypeDeclaration (fromPrimTyDef tyCon) (fromPrimDataCon <$> dataCons)

data DataConstructor
  = DataConstructor
  { dataConstructorName :: !Text
  , dataConstructorId :: !Int
  , dataConstructorArgument :: !(Maybe TypeTerm)
  , dataConstructorType :: !TyConInfo
  , dataConstructorSpan :: !ConstructorSpan
  , dataConstructorIndex :: !ConstructorIndex
  } deriving (Show, Eq, Ord)

fromPrimDataCon :: PrimDataCon -> DataConstructor
fromPrimDataCon (PrimDataCon name id' ty span' index) =
  DataConstructor name id' Nothing (fromPrimTyCon ty) span' index

data DataConInfo
  = DataConInfo
  { dataConInfoDefinition :: !TypeDeclaration
  , dataConInfoCons :: !DataConstructor
  } deriving (Show, Eq, Ord)

-- | A renamed 'Expr'
data Expr
  = ELit !Literal
  | EVar !Var
  | EIf !If
  | ECase !Case
  | ELet !Let
  | EApp !App
  | EParens !Expr
  deriving (Show, Eq)

data Var
  = VVal !(Typed Ident)
  | VCons !(Typed DataConInfo)
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
  = PLit !Literal
  | PVar !(Typed Ident)
  | PCons !PatCons
  | PParens !Pattern
  deriving (Show, Eq)

data PatCons
  = PatCons
  { patConsConstructor :: !DataConInfo
  , patConsArg :: !(Maybe Pattern)
  , patConsType :: !Type
  } deriving (Show, Eq)

data Let
  = Let
  { letBindings :: ![Binding]
  , letExpression :: !Expr
  } deriving (Show, Eq)

data App
  = App
  { appFunction :: !Expr
  , appArgs :: !(NonEmpty Expr)
  , appReturnType :: !Type
  } deriving (Show, Eq)

literalType' :: Literal -> Type
literalType' lit = TyTerm $ TyCon $ fromPrimTyCon $ literalType lit

expressionType :: Expr -> Type
expressionType (ELit lit) = literalType' lit
expressionType (EVar var) =
  case var of
    VVal (Typed ty _) -> ty
    VCons (Typed ty _) -> ty
expressionType (EIf if') = expressionType (ifThen if') -- Checker ensure "then" and "else" types match
expressionType (ECase (Case _ (Match _ expr :| _))) = expressionType expr
expressionType (ELet let') = expressionType (letExpression let')
expressionType (EApp app) = appReturnType app
expressionType (EParens expr) = expressionType expr

patternType :: Pattern -> Type
patternType (PLit lit) = literalType' lit
patternType (PVar (Typed ty _)) = ty
patternType (PCons (PatCons _ _ ty)) = ty
patternType (PParens pat) = patternType pat

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
  deriving (Show, Eq, Ord)

infixr 0 `TyFun`

data TyConInfo
  = TyConInfo
  { tyConInfoName :: !Text
  , tyConInfoId :: !Int
  , tyConInfoArgs :: ![TypeTerm]
  , tyConInfoDefinition :: !TyConDefinition
  } deriving (Show, Eq, Ord)

fromPrimTyCon :: PrimTyCon -> TyConInfo
fromPrimTyCon = tyConDefinitionToInfo . fromPrimTyDef

data TyVarInfo
  = TyVarInfo
  { tyVarInfoName :: !Text
  , tyVarInfoId :: !Int
  , tyVarInfoKind :: !Kind
  , tyVarInfoGenerated :: !TyVarGenerated
  } deriving (Show, Eq, Ord)

data TyVarGenerated
  = TyVarGenerated
  | TyVarNotGenerated
  deriving (Show, Eq, Ord)

typeKind :: Type -> Kind
typeKind (TyTerm term) = typeTermKind term
typeKind (TyFun _ _) = KStar

typeTermKind :: TypeTerm -> Kind
typeTermKind (TyCon info) =
  case tyConDefinitionKind (tyConInfoDefinition info) of
    KStar -> KStar
    KFun k _ -> k
typeTermKind (TyVar var) = tyVarInfoKind var
typeTermKind (TyParens t) = typeTermKind t

data Scheme
  = Forall ![TyVarInfo] Type
  deriving (Show, Eq)

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
