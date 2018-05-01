{-# LANGUAGE DeriveFunctor #-}

-- | Version of a renamer 'RModule' after type checking.

module Amy.TypeCheck.AST
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
  , expressionType
  , patternType

  , Ident(..)
  , ConstructorName(..)
  , Type(..)
  , TyConInfo(..)
  , TyVarInfo(..)
  , TyVarGenerated(..)
  , Scheme(..)
  , Typed(..)

    -- Re-export
  , Literal(..)
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
  { typeDeclarationTypeName :: !TyConInfo
  , typeDeclarationConstructors :: !(NonEmpty DataConstructor)
  } deriving (Show, Eq)

data DataConstructor
  = DataConstructor
  { dataConstructorName :: !ConstructorName
  , dataConstructorArgument :: !(Maybe TyConInfo)
  } deriving (Show, Eq)

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
  | VCons !(Typed ConstructorName)
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
  = PatternLit !Literal
  | PatternVar !(Typed Ident)
  | PatternCons !ConstructorPattern
  deriving (Show, Eq)

data ConstructorPattern
  = ConstructorPattern
  { constructorPatternConstructor :: !(Typed ConstructorName)
  , constructorPatternArg :: !(Maybe (Typed Ident))
  , constructorPatternReturnType :: !Type
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
literalType' lit =
  let primTy = literalType lit
  in TyCon $ TyConInfo (showPrimitiveType primTy) (primitiveTypeId primTy) (Just primTy)

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
patternType (PatternLit lit) = literalType' lit
patternType (PatternVar (Typed ty _)) = ty
patternType (PatternCons (ConstructorPattern _ _ retTy)) = retTy

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
  , tyConInfoPrimitiveType :: !(Maybe PrimitiveType)
  } deriving (Show, Eq, Ord)

data TyVarInfo
  = TyVarInfo
  { tyVarInfoName :: !Text
  , tyVarInfoId :: !Int
  , tyVarInfoGenerated :: !TyVarGenerated
  } deriving (Show, Eq, Ord)

data TyVarGenerated
  = TyVarGenerated
  | TyVarNotGenerated
  deriving (Show, Eq, Ord)

data Scheme
  = Forall ![TyVarInfo] Type
  deriving (Show, Eq)

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
