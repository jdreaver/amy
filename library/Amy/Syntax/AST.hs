-- | AST for the frontend parser.

module Amy.Syntax.AST
  ( Module(..)
  , Declaration(..)
  , declBinding
  , declExtern
  , declType
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
  , expressionSpan
  , matchSpan
  , patternSpan

    -- Re-export
  , Literal(..)
  , module Amy.Names
  , module Amy.Syntax.Located
  , module Amy.Type
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)

import Amy.Literal (Literal(..))
import Amy.Names
import Amy.Syntax.Located
import Amy.Type

-- | A 'Module' is simply a list of 'Declaration' values.
data Module
  = Module
  { moduleFile :: !FilePath
  , moduleDeclarations :: [Declaration]
  } deriving (Show, Eq)

data Declaration
  = DeclBinding !Binding
  | DeclExtern !Extern
  | DeclType !TypeDeclaration
  deriving (Show, Eq)

declBinding :: Declaration -> Maybe Binding
declBinding (DeclBinding x) = Just x
declBinding _ = Nothing

declExtern :: Declaration -> Maybe Extern
declExtern (DeclExtern x) = Just x
declExtern _ = Nothing

declType :: Declaration -> Maybe TypeDeclaration
declType (DeclType x) = Just x
declType _ = Nothing

data Binding
  = Binding
  { bindingName :: !(Located IdentName)
  , bindingType :: !Type
  , bindingArgs :: ![Located IdentName]
  , bindingReturnType :: !Type
  , bindingBody :: !Expr
  } deriving (Show, Eq)

-- | A 'BindingType' is a top-level declaration of a 'Binding' type, like @x ::
-- Int@ or @f :: Int -> Int@
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
  | EApp !Expr !Expr
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
  { letBindings :: ![Binding]
  , letExpression :: !Expr
  , letSpan :: !SourceSpan
  } deriving (Show, Eq)

data Lambda
  = Lambda
  { lambdaArgs :: !(NonEmpty (Located IdentName))
  , lambdaBody :: !Expr
  , lambdaSpan :: !SourceSpan
  , lambdaType :: !Type
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
expressionSpan (EApp e1 e2) = mergeSpans (expressionSpan e1) (expressionSpan e2)
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
