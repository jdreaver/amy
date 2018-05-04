{-# LANGUAGE DeriveFunctor #-}

module Amy.ANF.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , fromPrimTypeDefinition
  , DataConstructor(..)
  , Val(..)
  , Var(..)
  , Expr(..)
  , Let(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , App(..)

  , Ident(..)
  , ConstructorName(..)
  , fromPrimDataCon
  , Type(..)
  , TyConInfo(..)
  , fromPrimTyCon
  , TyVarInfo(..)
  , Scheme(..)
  , Typed(..)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import Amy.Literal
import Amy.Prim

data Module
  = Module
  { moduleBindings :: ![Binding]
  , moduleExterns :: ![Extern]
  , moduleTypeDeclarations :: ![TypeDeclaration]
  } deriving (Show, Eq)

data Binding
  = Binding
  { bindingName :: !Ident
  , bindingType :: !Scheme
  , bindingArgs :: ![Typed Ident]
  , bindingReturnType :: !Type
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

fromPrimTypeDefinition :: PrimTypeDefinition -> TypeDeclaration
fromPrimTypeDefinition (PrimTypeDefinition tyName cons) =
  TypeDeclaration (fromPrimTyCon tyName) ((\con -> DataConstructor (fromPrimDataCon con) Nothing) <$> cons)

data DataConstructor
  = DataConstructor
  { dataConstructorName :: !ConstructorName
  , dataConstructorArgument :: !(Maybe TyConInfo)
  } deriving (Show, Eq)

data Val
  = Var !Var
  | Lit !Literal
  deriving (Show, Eq)

data Var
  = VVal !(Typed Ident)
  | VCons !(Typed ConstructorName)
  deriving (Show, Eq)

data Expr
  = EVal !Val
  | ELet !Let
  | ECase !Case
  | EApp !(App Var)
  | EPrimOp !(App PrimitiveFunction)
  deriving (Show, Eq)

data Let
  = Let
  { letBindings :: ![Binding]
  , letExpression :: !Expr
  } deriving (Show, Eq)

data Case
  = Case
  { caseScrutinee :: !Val
  , caseAlternatives :: !(NonEmpty Match)
  , caseType :: !Type
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
  { patConsConstructor :: !(Typed ConstructorName)
  , patConsArg :: !(Maybe Pattern)
  , patConsType :: !Type
  } deriving (Show, Eq)

data App f
  = App
  { appFunction :: !f
  , appArgs :: ![Val]
  , appReturnType :: !Type
  } deriving (Show, Eq)

-- | An identifier from source code
data Ident
  = Ident
  { identText :: !Text
  , identId :: !Int
  , identIsTopLevel :: !Bool
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
