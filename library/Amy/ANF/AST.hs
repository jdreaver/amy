{-# LANGUAGE DeriveFunctor #-}

module Amy.ANF.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , Val(..)
  , valType
  , Expr(..)
  , Let(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , App(..)

  , Ident(..)
  , Type(..)
  , TypeName(..)
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
  { typeDeclarationTypeName :: !TypeName
  , typeDeclarationConstructorName :: !Ident
  , typeDeclarationArgument :: !TypeName
  } deriving (Show, Eq)

data Val
  = Var !(Typed Ident)
  | Lit !Literal
  deriving (Show, Eq)

valType :: Val -> Type
valType (Var (Typed ty _)) = ty
valType (Lit lit) =
  let primTy = literalType lit
  in TyCon $ TypeName (showPrimitiveType primTy) (primitiveTypeId primTy) (Just primTy)

data Expr
  = EVal !Val
  | ELet !Let
  | ECase !Case
  | EApp !(App (Typed Ident))
  | EPrimOp !(App PrimitiveFunctionName)
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
  = PatternLit !Literal
  | PatternVar !(Typed Ident)
  deriving (Show, Eq)

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
  , identPrimitiveName :: !(Maybe PrimitiveFunctionName)
  , identIsTopLevel :: !Bool
  } deriving (Show, Eq, Ord)

data Type
  = TyCon !TypeName
  | TyVar !TypeName
  | TyFun !Type !Type
  deriving (Show, Eq, Ord)

infixr 0 `TyFun`

data TypeName
  = TypeName
  { typeNameText :: !Text
  , typeNameId :: !Int
  , typeNamePrimitiveType :: !(Maybe PrimitiveType)
  } deriving (Show, Eq, Ord)

data Scheme
  = Forall ![TypeName] Type
  deriving (Show, Eq)

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
