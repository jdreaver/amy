-- | AST for the frontend parser.

module Amy.Syntax.AST
  ( Module(..)
  , Declaration(..)
  , declBinding
  , declBindingType
  , declExtern
  , declType
  , Binding(..)
  , BindingType(..)
  , Extern(..)
  , TypeDeclaration(..)
  , TyConDefinition(..)
  , DataConDefinition(..)
  , Expr(..)
  , Var(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , Let(..)
  , letBinding
  , letBindingType
  , expressionSpan
  , matchSpan
  , patternSpan
  , LetBinding(..)
  , Type(..)

    -- Re-export
  , Literal(..)
  , Located(..)
  , SourceSpan(..)
  , module Amy.Names
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)

import Amy.Literal (Literal(..))
import Amy.Names
import Amy.Syntax.Located

-- | A 'Module' is simply a list of 'Declaration' values.
data Module
  = Module
  { moduleFile :: !FilePath
  , moduleDeclarations :: [Declaration]
  } deriving (Show, Eq)

data Declaration
  = DeclBinding !Binding
  | DeclBindingType !BindingType
  | DeclExtern !Extern
  | DeclType !TypeDeclaration
  deriving (Show, Eq)

declBinding :: Declaration -> Maybe Binding
declBinding (DeclBinding x) = Just x
declBinding _ = Nothing

declBindingType :: Declaration -> Maybe BindingType
declBindingType (DeclBindingType x) = Just x
declBindingType _ = Nothing

declExtern :: Declaration -> Maybe Extern
declExtern (DeclExtern x) = Just x
declExtern _ = Nothing

declType :: Declaration -> Maybe TypeDeclaration
declType (DeclType x) = Just x
declType _ = Nothing

-- | A 'Binding' is a top-level definition of a binding, like @x = 1@ or @f x =
-- x@
data Binding
  = Binding
  { bindingName :: !(Located IdentName)
  , bindingArgs :: ![Located IdentName]
  , bindingBody :: !Expr
  } deriving (Show, Eq)

-- | A 'BindingType' is a top-level declaration of a 'Binding' type, like @x ::
-- Int@ or @f :: Int -> Int@
data BindingType
  = BindingType
  { bindingTypeName :: !(Located IdentName)
  , bindingTypeType :: !Type
  } deriving (Show, Eq)

-- | A 'BindingType' is a top-level declaration of a 'Binding' type, like @x ::
-- Int@ or @f :: Int -> Int@
data Extern
  = Extern
  { externName :: !(Located IdentName)
  , externType :: !Type
  } deriving (Show, Eq)

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !TyConDefinition
  , typeDeclarationConstructors :: ![DataConDefinition]
  } deriving (Show, Eq)

data TyConDefinition
  = TyConDefinition
  { tyConDefinitionName :: !(Located TyConName)
  , tyConDefinitionArgs :: ![Located TyVarName]
  } deriving (Show, Eq)

data DataConDefinition
  = DataConDefinition
  { dataConDefinitionName :: !(Located DataConName)
  , dataConDefinitionArgument :: !(Maybe Type)
  } deriving (Show, Eq)

data Expr
  = ELit !(Located Literal)
  | ERecord !SourceSpan !(Map (Located RowLabel) Expr)
  | ERecordSelect !Expr !(Located RowLabel)
  | EVar !Var
  | EIf !If
  | ECase !Case
  | ELet !Let
  | EApp !Expr !Expr
  | EParens !Expr
  deriving (Show, Eq)

data Var
  = VVal !(Located IdentName)
  | VCons !(Located DataConName)
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
  | PVar !(Located IdentName)
  | PCons !PatCons
  | PParens !Pattern
  deriving (Show, Eq)

data PatCons
  = PatCons
  { patConsConstructor :: !(Located DataConName)
  , patConsArg :: !(Maybe Pattern)
  } deriving (Show, Eq)

data Let
  = Let
  { letBindings :: ![LetBinding]
  , letExpression :: !Expr
  , letSpan :: !SourceSpan
  } deriving (Show, Eq)

data LetBinding
  = LetBinding !Binding
  | LetBindingType !BindingType
  deriving (Show, Eq)

letBinding :: LetBinding -> Maybe Binding
letBinding (LetBinding x) = Just x
letBinding _ = Nothing

letBindingType :: LetBinding -> Maybe BindingType
letBindingType (LetBindingType x) = Just x
letBindingType _ = Nothing

expressionSpan :: Expr -> SourceSpan
expressionSpan (ELit (Located s _)) = s
expressionSpan (ERecord s _) = s
expressionSpan (ERecordSelect expr (Located end _)) = mergeSpans (expressionSpan expr) end
expressionSpan (EVar (VVal (Located s _))) = s
expressionSpan (EVar (VCons (Located s _))) = s
expressionSpan (EIf (If _ _ _ s)) = s
expressionSpan (ECase (Case _ _ s)) = s
expressionSpan (ELet (Let _ _ s)) = s
expressionSpan (EApp e1 e2) = mergeSpans (expressionSpan e1) (expressionSpan e2)
expressionSpan (EParens e) = expressionSpan e

matchSpan :: Match -> SourceSpan
matchSpan (Match pat expr) = mergeSpans (patternSpan pat) (expressionSpan expr)

patternSpan :: Pattern -> SourceSpan
patternSpan (PLit (Located s _)) = s
patternSpan (PVar (Located s _)) = s
patternSpan (PCons (PatCons (Located s _) mPat)) =
  case mPat of
    Nothing -> s
    Just pat -> mergeSpans s (patternSpan pat)
patternSpan (PParens pat) = patternSpan pat

data Type
  = TyCon !(Located TyConName)
  | TyVar !(Located TyVarName)
  | TyApp !Type !Type
  | TyRecord !(Map (Located RowLabel) Type) !(Maybe (Located TyVarName))
  | TyFun !Type !Type
  | TyForall !(NonEmpty (Located TyVarName)) !Type
  deriving (Show, Eq)

infixr 0 `TyFun`
