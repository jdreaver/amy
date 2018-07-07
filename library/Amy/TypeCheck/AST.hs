module Amy.TypeCheck.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , TyConDefinition(..)
  , DataConDefinition(..)
  , Expr(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , Let(..)
  , Lambda(..)
  , App(..)
  , expressionType
  , matchType
  , patternType

    -- Re-export
  , Literal(..)
  , module Amy.ASTCommon
  , module Amy.Names
  , module Amy.Type
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)

import Amy.ASTCommon
import Amy.Literal
import Amy.Names
import Amy.Prim
import Amy.Type

data Module
  = Module
  { moduleBindings :: ![NonEmpty Binding]
  , moduleExterns :: ![Extern]
  , moduleTypeDeclarations :: ![TypeDeclaration]
  } deriving (Show, Eq)

-- | A binding after renaming. This is a combo of a 'Binding' and a
-- 'BindingType' after they've been paired together.
data Binding
  = Binding
  { bindingName :: !IdentName
  , bindingType :: !Type
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

data DataConDefinition
  = DataConDefinition
  { dataConDefinitionName :: !DataConName
  , dataConDefinitionArgument :: !(Maybe Type)
  } deriving (Show, Eq, Ord)

-- | A renamed 'Expr'
data Expr
  = ELit !Literal
  | ERecord !(Map RowLabel (Typed Expr))
  | ERecordSelect !Expr !RowLabel !Type
  | EVar !(Typed IdentName)
  | ECon !(Typed DataConName)
  | EIf !If
  | ECase !Case
  | ELet !Let
  | ELam !Lambda
  | EApp !App
  | EParens !Expr
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
  { letBindings :: ![NonEmpty Binding]
  , letExpression :: !Expr
  } deriving (Show, Eq)

data Lambda
  = Lambda
  { lambdaArgs :: !(NonEmpty (Typed IdentName))
  , lambdaBody :: !Expr
  , lambdaType :: !Type
  } deriving (Show, Eq)

data App
  = App
  { appFunction :: !Expr
  , appArg :: !Expr
  , appReturnType :: !Type
  } deriving (Show, Eq)

literalType' :: Literal -> Type
literalType' lit = TyCon $ literalType lit

expressionType :: Expr -> Type
expressionType (ELit lit) = literalType' lit
expressionType (ERecord rows) = TyRecord (typedType <$> rows) Nothing
expressionType (ERecordSelect _ _ ty) = ty
expressionType (EVar (Typed ty _)) = ty
expressionType (ECon (Typed ty _)) = ty
expressionType (EIf if') = expressionType (ifThen if') -- Checker ensure "then" and "else" types match
expressionType (ECase (Case _ (match :| _))) = matchType match
expressionType (ELet let') = expressionType (letExpression let')
expressionType (ELam (Lambda _ _ ty)) = ty
expressionType (EApp app) = appReturnType app
expressionType (EParens expr) = expressionType expr

matchType :: Match -> Type
matchType (Match _ expr) = expressionType expr

patternType :: Pattern -> Type
patternType (PLit lit) = literalType' lit
patternType (PVar (Typed ty _)) = ty
patternType (PCons (PatCons _ _ ty)) = ty
patternType (PParens pat) = patternType pat
