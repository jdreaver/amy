{-# LANGUAGE DeriveFunctor #-}

module Amy.Core.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , DataConstructor(..)
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

  , Ident(..)
  , Type(..)
  , TyConInfo(..)
  , TyVarInfo(..)
  , Scheme(..)
  , Typed(..)

    -- Re-export
  , Literal(..)
  , module Amy.ASTCommon
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import Amy.ASTCommon
import Amy.Literal
import Amy.Prim

data Module
  = Module
  { moduleBindings :: ![Binding]
  , moduleExterns :: ![Extern]
  , moduleTypeDeclarations :: ![TypeDeclaration]
  , moduleMaxId :: !Int
  } deriving (Show, Eq)

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

data Extern
  = Extern
  { externName :: !Ident
  , externType :: !Type
  } deriving (Show, Eq)

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !TyConInfo
  , typeDeclarationConstructors :: ![DataConstructor]
  } deriving (Show, Eq)

data DataConstructor
  = DataConstructor
  { dataConstructorName :: !Text
  , dataConstructorId :: !Int
  , dataConstructorArgument :: !(Maybe TyConInfo)
  , dataConstructorType :: !TyConInfo
  , dataConstructorSpan :: !ConstructorSpan
  , dataConstructorIndex :: !ConstructorIndex
  } deriving (Show, Eq)

data DataConInfo
  = DataConInfo
  { dataConInfoDefinition :: !TypeDeclaration
  , dataConInfoCons :: !DataConstructor
  } deriving (Show, Eq)

data Expr
  = ELit !Literal
  | EVar !Var
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
  deriving (Show, Eq)

data PatCons
  = PatCons
  { patConsConstructor :: !DataConInfo
  , patConsArg :: !(Maybe (Typed Ident))
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
literalType' lit = TyCon $ fromPrimTyCon $ literalType lit

expressionType :: Expr -> Type
expressionType (ELit lit) = literalType' lit
expressionType (EVar var) =
  case var of
    VVal (Typed ty _) -> ty
    VCons (Typed ty _) -> ty
expressionType (ECase (Case _ (Match _ expr :| _))) = expressionType expr
expressionType (ELet let') = expressionType (letExpression let')
expressionType (EApp app) = appReturnType app
expressionType (EParens expr) = expressionType expr

data Ident
  = Ident
  { identText :: !Text
  , identId :: !Int
  } deriving (Show, Eq, Ord)

data ConstructorName
  = ConstructorName
  { constructorNameText :: !Text
  , constructorNameId :: !Int
  } deriving (Show, Eq, Ord)

data Type
  = TyCon !TyConInfo
  | TyVar !TyVarInfo
  | TyFun !Type !Type
  deriving (Show, Eq, Ord)

infixr 0 `TyFun`

data TyConInfo
  = TyConInfo
  { tyConInfoText :: !Text
  , tyConInfoId :: !Int
  } deriving (Show, Eq, Ord)

fromPrimTyCon :: PrimTyCon -> TyConInfo
fromPrimTyCon (PrimTyCon name id') = TyConInfo name id'

data TyVarInfo
  = TyVarInfo
  { tyVarInfoName :: !Text
  , tyVarInfoId :: !Int
  } deriving (Show, Eq, Ord)

data Scheme
  = Forall ![TyVarInfo] Type
  deriving (Show, Eq)

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
