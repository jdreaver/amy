module Amy.Syntax.AST
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

  , expressionSpan
  , matchSpan
  , patternSpan

  , expressionType
  , matchType
  , patternType

  -- Re-export
  , module Amy.Literal
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
  { moduleFile :: !FilePath
  , moduleTypeDeclarations :: ![TypeDeclaration]
  , moduleExterns :: ![Extern]
  , moduleBindings :: ![NonEmpty Binding]
  } deriving (Show, Eq)

data Binding
  = Binding
  { bindingName :: !(Located IdentName)
  , bindingType :: !Type
  , bindingArgs :: ![Typed (Located IdentName)]
  , bindingReturnType :: !Type
  , bindingBody :: !Expr
  } deriving (Show, Eq)

data Extern
  = Extern
  { externName :: !(Located IdentName)
  , externType :: !Type
  } deriving (Show, Eq)

data Expr
  = ELit !(Located Literal)
  | ERecord !SourceSpan !(Map (Located RowLabel) (Typed Expr))
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

expressionSpan :: Expr -> SourceSpan
expressionSpan (ELit (Located s _)) = s
expressionSpan (ERecord s _) = s
expressionSpan (ERecordSelect expr (Located end _) _) = mergeSpans (expressionSpan expr) end
expressionSpan (EVar (Typed _ (Located s _))) = s
expressionSpan (ECon (Typed _ (Located s _))) = s
expressionSpan (EIf (If _ _ _ s)) = s
expressionSpan (ECase (Case _ _ s)) = s
expressionSpan (ELet (Let _ _ s)) = s
expressionSpan (ELam (Lambda _ _ s _)) = s
expressionSpan (EApp (App e1 e2 _)) = mergeSpans (expressionSpan e1) (expressionSpan e2)
expressionSpan (EParens e) = expressionSpan e

matchSpan :: Match -> SourceSpan
matchSpan (Match pat expr) = mergeSpans (patternSpan pat) (expressionSpan expr)

patternSpan :: Pattern -> SourceSpan
patternSpan (PLit (Located s _)) = s
patternSpan (PVar (Typed _ (Located s _))) = s
patternSpan (PCons (PatCons (Located s _) mPat _)) =
  case mPat of
    Nothing -> s
    Just pat -> mergeSpans s (patternSpan pat)
patternSpan (PParens pat) = patternSpan pat

expressionType :: Expr -> Type
expressionType (ELit (Located _ lit)) = literalType lit
expressionType (ERecord _ rows) = TyRecord (Map.mapKeys (notLocated . locatedValue) $ typedType <$> rows) Nothing
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
