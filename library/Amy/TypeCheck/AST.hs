module Amy.TypeCheck.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
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
  , module Amy.Names
  , module Amy.Syntax.Located
  , module Amy.Type
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Amy.Literal
import Amy.Names
import Amy.Prim
import Amy.Syntax.Located
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
  { bindingName :: !(Located IdentName)
  , bindingType :: !Type
    -- ^ Type for whole function
  , bindingArgs :: ![Typed (Located IdentName)]
    -- ^ Argument names and types split out from 'bindingType'
  , bindingReturnType :: !Type
    -- ^ Return type split out from 'bindingType'
  , bindingBody :: !Expr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data Extern
  = Extern
  { externName :: !(Located IdentName)
  , externType :: !Type
  } deriving (Show, Eq)

-- | A renamed 'Expr'
data Expr
  = ELit !(Located Literal)
  | ERecord !(Map (Located RowLabel) (Typed Expr))
  | ERecordSelect !Expr !(Located RowLabel) !Type
  | EVar !(Typed (Located IdentName))
  | ECon !(Typed (Located DataConName))
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
  , ifSpan :: !SourceSpan
  } deriving (Show, Eq)

data Case
  = Case
  { caseScrutinee :: !Expr
  , caseAlternatives :: !(NonEmpty Match)
  , caseSpan :: !SourceSpan
  } deriving (Show, Eq)

data Match
  = Match
  { matchPattern :: !Pattern
  , matchBody :: !Expr
  } deriving (Show, Eq)

data Pattern
  = PLit !(Located Literal)
  | PVar !(Typed (Located IdentName))
  | PCons !PatCons
  | PParens !Pattern
  deriving (Show, Eq)

data PatCons
  = PatCons
  { patConsConstructor :: !(Located DataConName)
  , patConsArg :: !(Maybe Pattern)
  , patConsType :: !Type
  } deriving (Show, Eq)

data Let
  = Let
  { letBindings :: ![NonEmpty Binding]
  , letExpression :: !Expr
  , letSpan :: !SourceSpan
  } deriving (Show, Eq)

data Lambda
  = Lambda
  { lambdaArgs :: !(NonEmpty (Typed (Located IdentName)))
  , lambdaBody :: !Expr
  , lambdaSpan :: !SourceSpan
  , lambdaType :: !Type
  } deriving (Show, Eq)

data App
  = App
  { appFunction :: !Expr
  , appArg :: !Expr
  , appReturnType :: !Type
  } deriving (Show, Eq)

expressionType :: Expr -> Type
expressionType (ELit (Located _ lit)) = literalType lit
expressionType (ERecord rows) = TyRecord (Map.mapKeys (notLocated . locatedValue) $ typedType <$> rows) Nothing
expressionType (ERecordSelect _ _ ty) = ty
expressionType (EVar (Typed ty _)) = ty
expressionType (ECon (Typed ty _)) = ty
expressionType (EIf if') = expressionType (ifThen if') -- Checker ensure "then" and "else" types match
expressionType (ECase (Case _ (match :| _) _)) = matchType match
expressionType (ELet let') = expressionType (letExpression let')
expressionType (ELam (Lambda _ _ _ ty)) = ty
expressionType (EApp app) = appReturnType app
expressionType (EParens expr) = expressionType expr

matchType :: Match -> Type
matchType (Match _ expr) = expressionType expr

patternType :: Pattern -> Type
patternType (PLit (Located _ lit)) = literalType lit
patternType (PVar (Typed ty _)) = ty
patternType (PCons (PatCons _ _ ty)) = ty
patternType (PParens pat) = patternType pat
