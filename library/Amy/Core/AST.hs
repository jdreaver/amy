{-# LANGUAGE DeriveFunctor #-}

module Amy.Core.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , TyConDefinition(..)
  , tyConDefinitionToInfo
  , fromPrimTypeDefinition
  , DataConDefinition(..)
  , Expr(..)
  , Var(..)
  , DataCon(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , Let(..)
  , App(..)
  , expressionType

  , Ident(..)
  , substExpr
  , TypeTerm(..)
  , Type(..)
  , TyConInfo(..)
  , TyVarInfo(..)
  , Scheme(..)
  , Typed(..)

    -- Re-export
  , Literal(..)
  , module Amy.ASTCommon
  , module Amy.Kind
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import Amy.ASTCommon
import Amy.Kind
import Amy.Literal
import Amy.Prim

data Module
  = Module
  { moduleBindings :: ![Binding]
  , moduleExterns :: ![Extern]
  , moduleTypeDeclarations :: ![TypeDeclaration]
  , moduleMaxId :: !Int
  } deriving (Show, Eq)

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

data Extern
  = Extern
  { externName :: !Ident
  , externType :: !Type
  } deriving (Show, Eq)

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !TyConDefinition
  , typeDeclarationConstructors :: ![DataConDefinition]
  } deriving (Show, Eq, Ord)

data TyConDefinition
  = TyConDefinition
  { tyConDefinitionName :: !Text
  , tyConDefinitionId :: !Int
  , tyConDefinitionArgs :: ![TyVarInfo]
  , tyConDefinitionKind :: !Kind
  } deriving (Show, Eq, Ord)

tyConDefinitionToInfo :: TyConDefinition -> TyConInfo
tyConDefinitionToInfo (TyConDefinition name' id' args kind) = TyConInfo name' id' (TyVar <$> args) kind

fromPrimTyDef :: PrimTyCon -> TyConDefinition
fromPrimTyDef (PrimTyCon name id') = TyConDefinition name id' [] KStar

fromPrimTypeDefinition :: PrimTypeDefinition -> TypeDeclaration
fromPrimTypeDefinition (PrimTypeDefinition tyName cons) =
  TypeDeclaration (fromPrimTyDef tyName) (fromPrimDataCon <$> cons)

data DataConDefinition
  = DataConDefinition
  { dataConDefinitionName :: !Text
  , dataConDefinitionId :: !Int
  , dataConDefinitionArgument :: !(Maybe TypeTerm)
  } deriving (Show, Eq, Ord)

fromPrimDataCon :: PrimDataCon -> DataConDefinition
fromPrimDataCon (PrimDataCon name id') =
  DataConDefinition name id' Nothing

data Expr
  = ELit !Literal
  | EVar !Var
  | ECase !Case
  | ELet !Let
  | EApp !App
  | EParens !Expr
  deriving (Show, Eq)

data Var
  = VVal !(Typed Ident)
  | VCons !(Typed DataCon)
  deriving (Show, Eq)

data DataCon
  = DataCon
  { dataConName :: !Text
  , dataConId :: !Int
  } deriving (Show, Eq, Ord)

data If
  = If
  { ifPredicate :: !Expr
  , ifThen :: !Expr
  , ifElse :: !Expr
  } deriving (Show, Eq)

data Case
  = Case
  { caseScrutinee :: !Expr
  , caseScrutineeBinding :: !(Typed Ident)
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
  { patConsConstructor :: !DataCon
  , patConsArg :: !(Maybe (Typed Ident))
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
literalType' lit = TyTerm $ TyCon $ fromPrimTyCon $ literalType lit

expressionType :: Expr -> Type
expressionType (ELit lit) = literalType' lit
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

data Ident
  = Ident
  { identText :: !Text
  , identId :: !Int
  } deriving (Show, Eq, Ord)

substExpr :: Expr -> Ident -> Ident -> Expr
substExpr e@(ELit _) _ _ = e
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
substExpr (EApp (App f args ty)) var newVar =
  EApp (App (substExpr f var newVar) ((\arg -> substExpr arg var newVar) <$> args) ty)
substExpr (EParens expr) var newVar = EParens (substExpr expr var newVar)

substBinding :: Binding -> Ident -> Ident -> Binding
substBinding binding var newVar = binding { bindingBody = substExpr (bindingBody binding) var newVar }

substMatch :: Match -> Ident -> Ident -> Match
substMatch match' var newVar = match' { matchBody = substExpr (matchBody match') var newVar }

replaceIdent :: Ident -> Ident -> Ident -> Ident
replaceIdent var oldVar newVar = if var == oldVar then newVar else var

data ConstructorName
  = ConstructorName
  { constructorNameText :: !Text
  , constructorNameId :: !Int
  } deriving (Show, Eq, Ord)

data TypeTerm
  = TyCon !TyConInfo
  | TyVar !TyVarInfo
  deriving (Show, Eq, Ord)

data Type
  = TyTerm !TypeTerm
  | TyFun !Type !Type
  deriving (Show, Eq, Ord)

infixr 0 `TyFun`

data TyConInfo
  = TyConInfo
  { tyConInfoName :: !Text
  , tyConInfoId :: !Int
  , tyConInfoArgs :: ![TypeTerm]
  , tyConInfoKind :: !Kind
  } deriving (Show, Eq, Ord)

fromPrimTyCon :: PrimTyCon -> TyConInfo
fromPrimTyCon = tyConDefinitionToInfo . fromPrimTyDef

data TyVarInfo
  = TyVarInfo
  { tyVarInfoName :: !Text
  , tyVarInfoId :: !Int
  , tyVarInfoKind :: !Kind
  } deriving (Show, Eq, Ord)

data Scheme
  = Forall ![TyVarInfo] Type
  deriving (Show, Eq)

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
