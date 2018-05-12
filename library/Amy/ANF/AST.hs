{-# LANGUAGE DeriveFunctor #-}

module Amy.ANF.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , DataConstructor(..)
  , DataConInfo(..)
  , Val(..)
  , Var(..)
  , Expr(..)
  , Let(..)
  , LetBinding(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , App(..)

  , Ident(..)
  , Type(..)
  , TyConInfo(..)
  , TyVarInfo(..)
  , Scheme(..)
  , Typed(..)

  , module Amy.ASTCommon
  ) where

import Data.Text (Text)

import Amy.ANF.TypeRep
import Amy.ASTCommon
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

data DataConstructor
  = DataConstructor
  { dataConstructorName :: !Text
  , dataConstructorId :: !Int
  , dataConstructorArgument :: !(Maybe TyConInfo)
  , dataConstructorType :: !TyConInfo
  , dataConstructorSpan :: !ConstructorSpan
  , dataConstructorIndex :: !ConstructorIndex
  } deriving (Show, Eq, Ord)

data DataConInfo
  = DataConInfo
  { dataConInfoDefinition :: !TypeDeclaration
  , dataConInfoCons :: !DataConstructor
  } deriving (Show, Eq)

data Val
  = Var !Var
  | Lit !Literal
  deriving (Show, Eq)

data Var
  = VVal !(Typed Ident)
  | VCons !(Typed DataConInfo)
  deriving (Show, Eq)

data Expr
  = EVal !Val
  | ELet !Let
  | ECase !Case
  | EApp !(App (Typed Ident))
  | ECons !(App (Typed DataConInfo))
  | EPrimOp !(App PrimitiveFunction)
  deriving (Show, Eq)

data Let
  = Let
  { letBindings :: ![LetBinding]
  , letExpression :: !Expr
  } deriving (Show, Eq)

data LetBinding
  = LetBinding
  { letBindingName :: !Ident
  , letBindingType :: !Scheme
  , letBindingBody :: !Expr
  } deriving (Show, Eq)

data Case
  = Case
  { caseScrutinee :: !Val
  , caseScrutineeBinding :: !(Typed Ident)
  , caseAlternatives :: ![Match]
  , caseDefault :: !(Maybe Expr)
  , caseType :: !Type
  } deriving (Show, Eq)

data Match
  = Match
  { matchPattern :: !Pattern
  , matchBody :: !Expr
  } deriving (Show, Eq)

data Pattern
  = PLit !Literal
  | PCons !PatCons
  deriving (Show, Eq)

data PatCons
  = PatCons
  { patConsConstructor :: !DataConInfo
  , patConsArg :: !(Maybe (Typed Ident))
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
  , tyConInfoTypeRep :: !TypeRep
  } deriving (Show, Eq, Ord)

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
