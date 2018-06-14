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
  , traverseExprTopDown
  , traverseExprTopDownM

  , Type(..)
  , unfoldTyApp
  , Typed(..)

    -- Re-export
  , Literal(..)
  , module Amy.ASTCommon
  , module Amy.Names
  ) where

import Control.Monad.Identity (Identity(..), runIdentity)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)

import Amy.ASTCommon
import Amy.Literal
import Amy.Names
import Amy.Prim
import qualified Amy.Syntax.AST as S
import Amy.Syntax.Located

data Module
  = Module
  { moduleBindings :: ![NonEmpty Binding]
  , moduleExterns :: ![Extern]
  , moduleTypeDeclarations :: ![TypeDeclaration]
  } deriving (Show, Eq)

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

fromPrimTyDef :: S.TyConDefinition -> TyConDefinition
fromPrimTyDef (S.TyConDefinition (Located _ name) args) = TyConDefinition name (locatedValue <$> args)

fromPrimTypeDefinition :: S.TypeDeclaration -> TypeDeclaration
fromPrimTypeDefinition (S.TypeDeclaration tyConDef dataCons) =
  TypeDeclaration (fromPrimTyDef tyConDef) (fromPrimDataCon <$> dataCons)

data DataConDefinition
  = DataConDefinition
  { dataConDefinitionName :: !DataConName
  , dataConDefinitionArgument :: !(Maybe Type)
  } deriving (Show, Eq, Ord)

fromPrimDataCon :: S.DataConDefinition -> DataConDefinition
fromPrimDataCon (S.DataConDefinition (Located _ name) Nothing) = DataConDefinition name Nothing
fromPrimDataCon (S.DataConDefinition _ (Just _)) = error "Couldn't convert data con definiton type."

data Expr
  = ELit !Literal
  | ERecord !(Map RowLabel (Typed Expr))
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
  { letBindings :: ![NonEmpty Binding]
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
expressionType (ERecord rows) = TyRecord (typedType <$> rows) Nothing
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
substExpr expr oldVar newVar = traverseExprTopDown f expr
 where
  f (EVar (VVal (Typed ty var))) = EVar (VVal $ Typed ty $ if var == oldVar then newVar else var)
  f e = e

traverseExprTopDown :: (Expr -> Expr) -> Expr -> Expr
traverseExprTopDown f = runIdentity . traverseExprTopDownM (Identity . f)

traverseExprTopDownM :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
traverseExprTopDownM f expr = f' expr
 where
  f' e = f e >>= go
  go e@ELit{} = pure e
  go e@EVar{} = pure e
  go (ERecord rows) = ERecord <$> traverse (\(Typed ty e) -> Typed ty <$> f' e) rows
  go (ERecordSelect e label ty) = (\e' -> ERecordSelect e' label ty) <$> f' e
  go (ECase (Case scrut bind alts default')) = do
    scrut' <- f' scrut
    alts' <- traverse (\(Match pat e) -> Match pat <$> f' e) alts
    default'' <- traverse f' default'
    pure $ ECase $ Case scrut' bind alts' default''
  go (ELet (Let bindings e)) = do
    bindings' <- traverse (traverse (\(Binding name ty args ret body) -> Binding name ty args ret <$> f' body)) bindings
    e' <- f' e
    pure $ ELet $ Let bindings' e'
  go (EApp (App func arg ty)) = do
    func' <- f' func
    arg' <- f' arg
    pure $ EApp $ App func' arg' ty
  go (EParens e) = EParens <$> f' e

data Type
  = TyCon !TyConName
  | TyVar !TyVarName
  | TyApp !Type !Type
  | TyRecord !(Map RowLabel Type) !(Maybe Type)
  | TyFun !Type !Type
  | TyForall !(NonEmpty TyVarName) !Type
  deriving (Show, Eq, Ord)

infixr 0 `TyFun`

unfoldTyApp :: Type -> NonEmpty Type
unfoldTyApp (TyApp app@(TyApp _ _) arg) = unfoldTyApp app <> (arg :| [])
unfoldTyApp (TyApp f arg) = f :| [arg]
unfoldTyApp t = t :| []

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
