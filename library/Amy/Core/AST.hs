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
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , PatCons(..)
  , Let(..)
  , Lambda(..)
  , App(..)
  , unfoldApp
  , foldApp
  , expressionType
  , substExpr
  , traverseExprTopDown
  , traverseExprTopDownM
  , traverseExpr
  , traverseExprM
  , freeBindingVars
  , freeExprVars

  , Type(..)
  , unfoldTyApp
  , unfoldTyFun
  , Typed(..)

    -- Re-export
  , Literal(..)
  , module Amy.ASTCommon
  , module Amy.Names
  ) where

import Control.Monad.Identity (Identity(..), runIdentity)
import Data.List (foldl', tails)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set

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
  | EVar !(Typed IdentName)
  | ECon !(Typed DataConName)
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
  { letBindings :: !(NonEmpty Binding)
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

-- | Unfold an 'App' node into function and args.
unfoldApp :: App -> NonEmpty Expr
unfoldApp (App (EApp app@App{}) arg _) = unfoldApp app <> (arg :| [])
unfoldApp (App f arg _) = f :| [arg]

-- | Combine function and arg expressions into an 'App' node.
foldApp :: Expr -> [Expr] -> Expr
foldApp func args =
  let
    tys = unfoldTyFun $ expressionType func
    appTys = NE.drop 1 tys
    varsAndTys = zip args $ tails appTys
  in foldl' mkApp func varsAndTys
 where
  mkApp :: Expr -> (Expr, [Type]) -> Expr
  mkApp e (arg, tys') = EApp $ App e arg (foldr1 TyFun tys')

literalType' :: Literal -> Type
literalType' lit = TyCon $ literalType lit

expressionType :: Expr -> Type
expressionType (ELit lit) = literalType' lit
expressionType (ERecord rows) = TyRecord (typedType <$> rows) Nothing
expressionType (ERecordSelect _ _ ty) = ty
expressionType (EVar (Typed ty _)) = ty
expressionType (ECon (Typed ty _)) = ty
expressionType e@(ECase (Case _ _ matches defaultMatch)) =
  case (matches, defaultMatch) of
    (Match _ expr : _, _) -> expressionType expr
    ([], Just expr) -> expressionType expr
    _ -> error $ "Found empty case expression with no branches " ++ show e
expressionType (ELet let') = expressionType (letExpression let')
expressionType (ELam (Lambda _ _ ty)) = ty
expressionType (EApp app) = appReturnType app
expressionType (EParens expr) = expressionType expr

substExpr :: Expr -> IdentName -> IdentName -> Expr
substExpr expr oldVar newVar = traverseExprTopDown f expr
 where
  f (EVar (Typed ty var)) = EVar (Typed ty $ if var == oldVar then newVar else var)
  f e = e

traverseExprTopDown :: (Expr -> Expr) -> Expr -> Expr
traverseExprTopDown f = runIdentity . traverseExprTopDownM (Identity . f)

traverseExprTopDownM :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
traverseExprTopDownM f expr = f' expr
 where
  -- TODO: The definition of f' is incorrect if the AST changes. If f returns a
  -- different node than was passed in (because it changed), it will get sent
  -- to "go", but we don't run f on that output node.
  f' e = f e >>= go
  go e@ELit{} = pure e
  go e@EVar{} = pure e
  go e@ECon{} = pure e
  go (ERecord rows) = ERecord <$> traverse (\(Typed ty e) -> Typed ty <$> f' e) rows
  go (ERecordSelect e label ty) = (\e' -> ERecordSelect e' label ty) <$> f' e
  go (ECase (Case scrut bind alts default')) = do
    scrut' <- f' scrut
    alts' <- traverse (\(Match pat e) -> Match pat <$> f' e) alts
    default'' <- traverse f' default'
    pure $ ECase $ Case scrut' bind alts' default''
  go (ELet (Let bindings e)) = do
    bindings' <- traverse (\(Binding name ty args ret body) -> Binding name ty args ret <$> f' body) bindings
    e' <- f' e
    pure $ ELet $ Let bindings' e'
  go (ELam (Lambda args body ty)) = do
    body' <- f' body
    pure $ ELam $ Lambda args body' ty
  go (EApp (App func arg ty)) = do
    func' <- f' func
    arg' <- f' arg
    pure $ EApp $ App func' arg' ty
  go (EParens e) = EParens <$> f' e

-- | Pure version of 'traverseExprM'.
traverseExpr :: (Expr -> Expr) -> Expr -> Expr
traverseExpr f = runIdentity . traverseExprM (Identity . f)

-- | Single step of a traversal through an @'Expr'@.
--
-- This function doesn't traverse the entire expression. It applies a function
-- to all the immediate sub expressions of a single node. This is most useful
-- when paired with another mutually recursive function (@f@) that singles out
-- the nodes it cares about, and leaves this function to traverse the ones it
-- doesn't.
--
traverseExprM :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
traverseExprM f = go
 where
  go e@ELit{} = pure e
  go e@EVar{} = pure e
  go e@ECon{} = pure e
  go (ERecord rows) = ERecord <$> traverse (\(Typed ty e) -> Typed ty <$> f e) rows
  go (ERecordSelect e label ty) = (\e' -> ERecordSelect e' label ty) <$> f e
  go (ECase (Case scrut bind alts default')) = do
    scrut' <- f scrut
    alts' <- traverse (\(Match pat e) -> Match pat <$> f e) alts
    default'' <- traverse f default'
    pure $ ECase $ Case scrut' bind alts' default''
  go (ELet (Let bindings e)) = do
    bindings' <- traverse (\(Binding name ty args ret body) -> Binding name ty args ret <$> f body) bindings
    e' <- f e
    pure $ ELet $ Let bindings' e'
  go (ELam (Lambda args body ty)) = do
    body' <- f body
    pure $ ELam $ Lambda args body' ty
  go (EApp (App func arg ty)) = do
    func' <- f func
    arg' <- f arg
    pure $ EApp $ App func' arg' ty
  go (EParens e) = EParens <$> f e

freeBindingVars :: Binding -> Set (Typed IdentName)
freeBindingVars (Binding name ty args _ body) =
  freeExprVars body `Set.difference` Set.fromList (Typed ty name : args)

freeExprVars :: Expr -> Set (Typed IdentName)
freeExprVars ELit{} = Set.empty
freeExprVars (ERecord rows) = Set.unions $ freeExprVars . typedValue <$> Map.elems rows
freeExprVars (ERecordSelect expr _ _) = freeExprVars expr
freeExprVars (EVar var) = Set.singleton var
freeExprVars ECon{} = Set.empty
freeExprVars (ECase (Case scrutinee bind matches default')) =
  let
    scrutVars = freeExprVars scrutinee
    matchVars = freeMatchVars <$> matches
    defaultVars = maybeToList $ freeExprVars <$> default'
  in Set.unions (scrutVars : matchVars ++ defaultVars) `Set.difference` Set.singleton bind
 where
  freeMatchVars (Match pat expr) = freeExprVars expr `Set.difference` patternVars pat
  patternVars PLit{} = Set.empty
  patternVars (PCons (PatCons _ mPat _)) = maybe Set.empty Set.singleton mPat
freeExprVars (ELet (Let bindings expr)) =
  Set.unions (freeExprVars expr : (freeBindingVars <$> NE.toList bindings))
freeExprVars (ELam (Lambda args body _)) =
  freeExprVars body `Set.difference` Set.fromList (NE.toList args)
freeExprVars (EApp (App f arg _)) = freeExprVars f `Set.union` freeExprVars arg
freeExprVars (EParens expr) = freeExprVars expr

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

unfoldTyFun :: Type -> NonEmpty Type
unfoldTyFun (TyForall _ t) = unfoldTyFun t
unfoldTyFun (t1 `TyFun` t2) = NE.cons t1 (unfoldTyFun t2)
unfoldTyFun ty = ty :| []

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
