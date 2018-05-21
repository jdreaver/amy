{-# LANGUAGE DeriveFunctor #-}

module Amy.ANF.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , DataConDefinition(..)
  , Val(..)
  , DataCon(..)
  , Expr(..)
  , LetVal(..)
  , LetValBinding(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , App(..)
  , ConApp(..)

  , Type(..)
  , Typed(..)

  , module Amy.ASTCommon
  , module Amy.Names
  ) where

import GHC.Word (Word32)
import Data.Map.Strict (Map)

import Amy.ASTCommon
import Amy.Literal
import Amy.Names
import Amy.Prim

data Module
  = Module
  { moduleBindings :: ![Binding]
  , moduleExterns :: ![Extern]
  , moduleTypeDeclarations :: ![TypeDeclaration]
  } deriving (Show, Eq)

data Binding
  = Binding
  { bindingName :: !IdentName
  , bindingArgs :: ![Typed IdentName]
  , bindingReturnType :: !Type
  , bindingBody :: !Expr
  } deriving (Show, Eq)

data Extern
  = Extern
  { externName :: !IdentName
  , externType :: !Type
  } deriving (Show, Eq)

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !TyConName
  , typeDeclarationType :: !Type
  , typeDeclarationConstructors :: ![DataConDefinition]
  } deriving (Show, Eq)

data DataConDefinition
  = DataConDefinition
  { dataConDefinitionName :: !DataConName
  , dataConDefinitionArgument :: !(Maybe Type)
  } deriving (Show, Eq, Ord)

data Val
  = Var !(Typed IdentName) !Bool -- Bool means isTopLevel
  | Lit !Literal
  | ConEnum !Word32 !DataCon
  deriving (Show, Eq)

data DataCon
  = DataCon
  { dataConName :: !DataConName
  , dataConType :: !Type
  , dataConIndex :: !ConstructorIndex
  } deriving (Show, Eq, Ord)

data Expr
  = EVal !Val
  | ERecord !(Typed (Map RowLabel Val))
  | ELetVal !LetVal
  | ECase !Case
  | EApp !(App (Typed IdentName))
  | EConApp !ConApp
  | EPrimOp !(App PrimitiveFunction)
  deriving (Show, Eq)

data LetVal
  = LetVal
  { letValBindings :: ![LetValBinding]
  , letValExpression :: !Expr
  } deriving (Show, Eq)

data LetValBinding
  = LetValBinding
  { letValBindingName :: !IdentName
  , letValBindingType :: !Type
  , letValBindingBody :: !Expr
  } deriving (Show, Eq)

data Case
  = Case
  { caseScrutinee :: !Val
  , caseScrutineeBinding :: !(Typed IdentName)
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
  { patConsConstructor :: !DataCon
  , patConsArg :: !(Maybe (Typed IdentName))
  , patConsType :: !Type
  } deriving (Show, Eq)

data App f
  = App
  { appFunction :: !f
  , appArgs :: ![Val]
  , appReturnType :: !Type
  } deriving (Show, Eq)

data ConApp
  = ConApp
  { conAppCon :: !DataCon
  , conAppArg :: !(Maybe Val)
  , conAppTaggedUnionName :: !TyConName
  , conAppTaggedUnionTagBits :: !Word32
  } deriving (Show, Eq)

data Type
  = PrimIntType
  | PrimDoubleType
  | PointerType !Type
  | OpaquePointerType
    -- ^ Used for polymorphic types
  | FuncType ![Type] !Type
  | EnumType !Word32
  | TaggedUnionType !TyConName !Word32
  | RecordType ![(RowLabel, Type)]
  deriving (Show, Eq, Ord)

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
