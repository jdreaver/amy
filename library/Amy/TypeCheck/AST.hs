{-# LANGUAGE DeriveFunctor #-}

-- | Version of a renamer 'RModule' after type checking.

module Amy.TypeCheck.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , TyConDefinition(..)
  , fromPrimTypeDef
  , DataConDefinition(..)
  , fromPrimDataCon
  , Expr(..)
  , Row(..)
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

  , Type(..)
  , TyVarInfo(..)
  , TyVarGenerated(..)
  , TyRow(..)
  , Scheme(..)
  , Typed(..)

    -- Re-export
  , Literal(..)
  , module Amy.ASTCommon
  , module Amy.Names
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import Amy.ASTCommon
import Amy.Literal
import Amy.Names
import Amy.Prim

data Module
  = Module
  { moduleBindings :: ![Binding]
  , moduleExterns :: ![Extern]
  , moduleTypeDeclarations :: ![TypeDeclaration]
    -- TODO: Instead of threading an Int through every AST Module, consider
    -- running the whole compiler in some monad that admits a fresh name
    -- supply. I'm not sure if that would make things easier at the expense of
  , moduleMaxId :: !Int
  } deriving (Show, Eq)

-- | A binding after renaming. This is a combo of a 'Binding' and a
-- 'BindingType' after they've been paired together.
data Binding
  = Binding
  { bindingName :: !IdentName
  , bindingType :: !Scheme
    -- ^ Type for whole function
  , bindingArgs :: ![Typed IdentName]
    -- ^ Argument names and types split out from 'bindingType'
  , bindingReturnType :: !Type
    -- ^ Return type split out from 'bindingType'
  , bindingBody :: !Expr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data Extern
  = Extern
  { externName :: !IdentName
  , externType :: !Type
  } deriving (Show, Eq)

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !TyConDefinition
  , typeDeclarationConstructors :: ![DataConDefinition]
  } deriving (Show, Eq, Ord)

data TyConDefinition
  = TyConDefinition
  { tyConDefinitionName :: !TyConName
  , tyConDefinitionArgs :: ![TyVarName]
  } deriving (Show, Eq, Ord)

fromPrimTyDef :: TyConName -> TyConDefinition
fromPrimTyDef name = TyConDefinition name []

fromPrimTypeDef :: PrimTypeDefinition -> TypeDeclaration
fromPrimTypeDef (PrimTypeDefinition tyCon dataCons) =
  TypeDeclaration (fromPrimTyDef tyCon) (fromPrimDataCon <$> dataCons)

data DataConDefinition
  = DataConDefinition
  { dataConDefinitionName :: !DataConName
  , dataConDefinitionArgument :: !(Maybe Type)
  } deriving (Show, Eq, Ord)

fromPrimDataCon :: DataConName -> DataConDefinition
fromPrimDataCon name = DataConDefinition name Nothing

-- | A renamed 'Expr'
data Expr
  = ELit !Literal
  | ERecord ![Row]
  | EVar !Var
  | EIf !If
  | ECase !Case
  | ELet !Let
  | EApp !App
  | EParens !Expr
  deriving (Show, Eq)

data Row
  = Row
  { rowLabel :: !RowLabel
  , rowValue :: !Expr
  } deriving (Show, Eq)

data Var
  = VVal !(Typed IdentName)
  | VCons !(Typed DataConName)
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
  | PVar !(Typed IdentName)
  | PCons !PatCons
  | PParens !Pattern
  deriving (Show, Eq)

data PatCons
  = PatCons
  { patConsConstructor :: !DataConName
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
literalType' lit = TyCon $ literalType lit

expressionType :: Expr -> Type
expressionType (ELit lit) = literalType' lit
expressionType (ERecord rows) = TyRecord $ mkTyRow <$> rows
 where
  mkTyRow (Row label expr) = TyRow label (expressionType expr)
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

data Type
  = TyCon !TyConName
  | TyVar !TyVarInfo
  | TyApp !TyConName !(NonEmpty Type)
  | TyRecord ![TyRow]
  | TyFun !Type !Type
  deriving (Show, Eq, Ord)

infixr 0 `TyFun`

data TyVarInfo
  = TyVarInfo
  { tyVarInfoName :: !TyVarName
  , tyVarInfoGenerated :: !TyVarGenerated
  } deriving (Show, Eq, Ord)

data TyVarGenerated
  = TyVarGenerated
  | TyVarNotGenerated
  deriving (Show, Eq, Ord)

data TyRow
  = TyRow
  { tyRowLabel :: !RowLabel
  , tyRowType :: !Type
  } deriving (Show, Eq, Ord)

data Scheme
  = Forall ![TyVarInfo] Type
  deriving (Show, Eq)

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
