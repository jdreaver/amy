{-# LANGUAGE DeriveFunctor #-}

module Amy.Core.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , TyConDefinition(..)
  , fromPrimTypeDefinition
  , DataConDefinition(..)
  , Expr(..)
  , Var(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , Let(..)
  , App(..)
  , unfoldApp
  , expressionType

  , substExpr
  , Type(..)
  , unfoldTyApp
  , Scheme(..)
  , Typed(..)

    -- Re-export
  , Literal(..)
  , module Amy.ASTCommon
  , module Amy.Names
  ) where

import Data.List.NonEmpty (NonEmpty(..))
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
  , bindingType :: !Scheme
    -- ^ Type for whole function
  , bindingArgs :: ![Typed IdentName]
    -- ^ Argument names and types split out from 'bindingType'
  , bindingReturnType :: !Type
    -- ^ Return type split out from 'bindingType'
  , bindingBody :: !Expr
  } deriving (Show, Eq)

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

fromPrimTypeDefinition :: PrimTypeDefinition -> TypeDeclaration
fromPrimTypeDefinition (PrimTypeDefinition tyName cons) =
  TypeDeclaration (fromPrimTyDef tyName) (fromPrimDataCon <$> cons)

data DataConDefinition
  = DataConDefinition
  { dataConDefinitionName :: !DataConName
  , dataConDefinitionArgument :: !(Maybe Type)
  } deriving (Show, Eq, Ord)

fromPrimDataCon :: DataConName -> DataConDefinition
fromPrimDataCon name = DataConDefinition name Nothing

data Expr
  = ELit !Literal
  | ERecord !(Typed (Map RowLabel Expr))
  | ERecordSelect !Expr !RowLabel !Type
  | EVar !Var
  | ECase !Case
  | ELet !Let
  | EApp !App
  | EParens !Expr
  deriving (Show, Eq)

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
  , caseScrutineeBinding :: !(Typed IdentName)
  , caseAlternatives :: ![Match]
  , caseDefault :: !(Maybe Expr)
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
  { patConsConstructor :: !DataConName
  , patConsArg :: !(Maybe (Typed IdentName))
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
  , appArg :: !Expr
  , appReturnType :: !Type
  } deriving (Show, Eq)

unfoldApp :: App -> NonEmpty Expr
unfoldApp (App (EApp app@App{}) arg _) = unfoldApp app <> (arg :| [])
unfoldApp (App f arg _) = f :| [arg]

literalType' :: Literal -> Type
literalType' lit = TyCon $ literalType lit

expressionType :: Expr -> Type
expressionType (ELit lit) = literalType' lit
expressionType (ERecord (Typed ty _)) = ty
expressionType (ERecordSelect _ _ ty) = ty
expressionType (EVar var) =
  case var of
    VVal (Typed ty _) -> ty
    VCons (Typed ty _) -> ty
expressionType e@(ECase (Case _ _ matches defaultMatch)) =
  case (matches, defaultMatch) of
    (Match _ expr : _, _) -> expressionType expr
    ([], Just expr) -> expressionType expr
    _ -> error $ "Found empty case expression with no branches " ++ show e
expressionType (ELet let') = expressionType (letExpression let')
expressionType (EApp app) = appReturnType app
expressionType (EParens expr) = expressionType expr

substExpr :: Expr -> IdentName -> IdentName -> Expr
substExpr e@(ELit _) _ _ = e
substExpr (ERecord (Typed ty rows)) var newVar = ERecord $ Typed ty $ (\e -> substExpr e var newVar) <$> rows
substExpr (ERecordSelect expr label ty) var newVar =
  ERecordSelect (substExpr expr var newVar) label ty
substExpr (EVar v) var newVar =
  case v of
    VVal (Typed ty ident) -> EVar (VVal $ Typed ty $ replaceIdent ident var newVar)
    VCons _ -> EVar v
substExpr (ECase (Case scrut bind alts default')) var newVar =
  ECase
  ( Case
    (substExpr scrut var newVar)
    bind
    ((\m -> substMatch m var newVar) <$> alts)
    ((\e -> substExpr e var newVar) <$> default')
  )
substExpr (ELet (Let bindings body)) var newVar =
  ELet (Let ((\b -> substBinding b var newVar) <$> bindings) (substExpr body var newVar))
substExpr (EApp (App f arg ty)) var newVar =
  EApp (App (substExpr f var newVar) (substExpr arg var newVar) ty)
substExpr (EParens expr) var newVar = EParens (substExpr expr var newVar)

substBinding :: Binding -> IdentName -> IdentName -> Binding
substBinding binding var newVar = binding { bindingBody = substExpr (bindingBody binding) var newVar }

substMatch :: Match -> IdentName -> IdentName -> Match
substMatch match' var newVar = match' { matchBody = substExpr (matchBody match') var newVar }

replaceIdent :: IdentName -> IdentName -> IdentName -> IdentName
replaceIdent var oldVar newVar = if var == oldVar then newVar else var

data Type
  = TyCon !TyConName
  | TyVar !TyVarName
  | TyApp !Type !Type
  | TyRecord !(Map RowLabel Type) !(Maybe TyVarName)
  | TyFun !Type !Type
  deriving (Show, Eq, Ord)

infixr 0 `TyFun`

unfoldTyApp :: Type -> NonEmpty Type
unfoldTyApp (TyApp app@(TyApp _ _) arg) = unfoldTyApp app <> (arg :| [])
unfoldTyApp (TyApp f arg) = f :| [arg]
unfoldTyApp t = t :| []

data Scheme
  = Forall ![TyVarName] Type
  deriving (Show, Eq)

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
