{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test implementation of Complete and Easy Bidirectional Typechecking for
-- Higher-Rank Polymorphism (Dunfield/Krishnaswami 2013)

module Amy.TypeChecking.Bidirectional
  ( inferBinding
  , inferExpr
  , checkExpr
  , Type(..)
  , TVar(..)
  , TEVar(..)
  , Expr(..)
  , Binding(..)
  , Var(..)
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.List (lookup)
import Data.Maybe (isJust, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Traversable (traverse)
import GHC.Exts (IsList, IsString)
import qualified GHC.Exts as GHC

--
-- Types
--

data Type
  = TyUnit
  | TyVar TVar
  | TyEVar TEVar
  | TyFun Type Type
  | TyForall TVar Type
  deriving (Show, Eq, Ord)

newtype TVar = TVar { unTVar :: Text }
  deriving (Show, Eq, Ord, IsString)

newtype TEVar = TEVar { unTEVar :: Text }
  deriving (Show, Eq, Ord, IsString)

freeTEVars :: Type -> Set TEVar
freeTEVars TyUnit = Set.empty
freeTEVars (TyVar _) = Set.empty
freeTEVars (TyEVar v) = Set.singleton v
freeTEVars (TyFun a b) = freeTEVars a <> freeTEVars b
freeTEVars (TyForall _ t) = freeTEVars t

--
-- Expr
--

data Expr
  = EUnit
  | EVar Var
  | EAnn Expr Type
  | ELam Var Expr
  | EApp Expr Expr
  deriving (Show, Eq, Ord)

newtype Var = Var { unVar :: Text }
  deriving (Show, Eq, Ord, IsString)

data Binding = Binding Var [Var] Expr

--
-- Context
--

data ContextMember
  = ContextVar TVar
  | ContextAssump Var Type
  | ContextEVar TEVar
  | ContextSolved TEVar Type
  | ContextMarker TEVar
  deriving (Show, Eq, Ord)

-- TODO: Could this just be a stack (reversed List) instead of a Seq?
newtype Context = Context (Seq ContextMember)
  deriving (Show, Eq, Ord, Semigroup)

instance IsList Context where
  type Item Context = ContextMember
  fromList = Context . GHC.fromList
  toList (Context c) = GHC.toList c

(|>) :: Context -> ContextMember -> Context
(Context context) |> member = Context (context Seq.|> member)

contextElem :: ContextMember -> Context -> Bool
contextElem member (Context context) = member `elem` context

contextSubst :: Context -> Type -> Type
contextSubst _ TyUnit = TyUnit
contextSubst _ t@(TyVar _) = t
contextSubst ctx t@(TyEVar v) = maybe t (contextSubst ctx) (contextSolution ctx v)
contextSubst ctx (TyFun a b) = TyFun (contextSubst ctx a) (contextSubst ctx b)
contextSubst ctx (TyForall v t) = TyForall v (contextSubst ctx t)

-- | Γ = Γ0[Θ] means Γ has the form (ΓL, Θ,ΓR)
contextHole :: ContextMember -> Context -> Maybe (Context, Context)
contextHole member (Context context) = (\(cl, cr) -> (Context cl, Context (Seq.drop 1 cr))) . flip Seq.splitAt context <$> mIndex
 where
  mIndex = Seq.elemIndexR member context

-- | Γ = Γ0 [Θ1][Θ2] means Γ has the form (ΓL, Θ1, ΓM, Θ2, ΓR)
contextTwoHoles :: ContextMember -> ContextMember -> Context -> Maybe (Context, Context, Context)
contextTwoHoles member1 member2 context = do
  (contextL, context') <- contextHole member1 context
  (contextM, contextR) <- contextHole member2 context'
  pure (contextL, contextM, contextR)

contextAssumption :: Context -> Var -> Maybe Type
contextAssumption (Context context) var = lookup var assumpPairs
 where
  assumpPairs = mapMaybe assumpPair $ toList context
  assumpPair (ContextAssump var' ty) = Just (var', ty)
  assumpPair _ = Nothing

contextSolution :: Context -> TEVar -> Maybe Type
contextSolution (Context context) var = lookup var solutionPairs
 where
  solutionPairs = mapMaybe solutionPair $ toList context
  solutionPair (ContextSolved var' ty) = Just (var', ty)
  solutionPair _ = Nothing

contextUntil :: ContextMember -> Context -> Context
contextUntil member (Context context) = Context $ Seq.takeWhileL (/= member) context

typeWellFormed :: Context -> Type -> Either String ()
typeWellFormed _ TyUnit = return ()
typeWellFormed context (TyVar v) = unless (ContextVar v `contextElem` context) $ Left $ "unbound type variable " ++ show v
typeWellFormed context (TyEVar v) = unless (ContextEVar v `contextElem` context || hasSolution) $ Left $ "unbound existential variable " ++ show v
  where hasSolution = isJust (contextSolution context v)
typeWellFormed context (TyFun x y) = typeWellFormed context x >> typeWellFormed context y
typeWellFormed context (TyForall v t) = typeWellFormed (context |> ContextVar v) t

--
-- Monad
--

newtype Checker a = Checker (ExceptT String (State Int) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadError String)

runChecker :: Checker a -> Either String a
runChecker (Checker action) = evalState (runExceptT action) 0

freshId :: Checker Int
freshId = modify' (+1) >> get

freshTEVar :: Checker TEVar
freshTEVar = TEVar . ("a" <>) . pack . show <$> freshId

--
-- Instantiate
--

instantiate :: TVar -> Type -> Type -> Type
instantiate _ _ TyUnit = TyUnit
instantiate v s t@(TyVar v')
  | v == v' = s
  | otherwise = t
instantiate _ _ t@(TyEVar _) = t
instantiate v s (TyFun a b) = TyFun (instantiate v s a) (instantiate v s b)
instantiate v s (TyForall a t) = TyForall a (instantiate v s t)

instantiateLeft :: Context -> TEVar -> Type -> Checker Context
instantiateLeft context a (TyEVar b) = instantiateReach context a b
instantiateLeft context a (TyFun t1 t2) = do
  (context', a1, a2) <- instantiateTyFunContext context a
  context'' <- instantiateRight context' t1 a1
  instantiateLeft context'' a2 (contextSubst context'' t2)
instantiateLeft context a (TyForall b t) = do
  context' <- instantiateLeft (context |> ContextVar b) a t
  case contextHole (ContextEVar a) context' of
    Nothing -> throwError $ "Something weird happened and the TEVar isn't in the context anymore " ++ show (a, context')
    Just (contextL, _) -> pure contextL
-- Catch-all for all monotypes
instantiateLeft context a t = instantiateMonoType context a t

instantiateRight :: Context -> Type -> TEVar -> Checker Context
instantiateRight context (TyEVar b) a = instantiateReach context a b
instantiateRight context (TyFun t1 t2) a = do
  (context', a1, a2) <- instantiateTyFunContext context a
  context'' <- instantiateLeft context' a1 t1
  instantiateRight context'' (contextSubst context'' t2) a2
instantiateRight context (TyForall b t) a = do
  b' <- freshTEVar
  context' <- instantiateRight (context |> ContextMarker b' |> ContextEVar b') (instantiate b (TyEVar b') t) a
  case contextHole (ContextMarker b') context' of
    Nothing -> throwError $ "Something weird happened and the TEVar isn't in the context anymore " ++ show (a, context')
    Just (contextL, _) -> pure contextL
-- Catch-all for all monotypes
instantiateRight context t a = instantiateMonoType context a t

instantiateReach :: Context -> TEVar -> TEVar -> Checker Context
instantiateReach context a b =
  catchError (instantiateMonoType context a (TyEVar b)) $ \_ ->
    case contextTwoHoles (ContextEVar a) (ContextEVar b) context of
      Nothing -> throwError $ "Couldn't instantiate reach rule, failed on contextTwoHoles " ++ show (a, b, context)
      Just (contextL, contextM, contextR) -> pure $ contextL |> ContextEVar a <> contextM |> ContextSolved b (TyEVar a) <> contextR

instantiateTyFunContext :: Context -> TEVar -> Checker (Context, TEVar, TEVar)
instantiateTyFunContext context a =
  case contextHole (ContextEVar a) context of
    Nothing -> throwError $ "Couldn't instantiate TyFun, failed on contextHole " ++ show (a, context)
    Just (contextL, contextR) -> do
      a1 <- freshTEVar
      a2 <- freshTEVar
      let context' = contextL |> ContextEVar a2 |> ContextEVar a1 |> ContextSolved a (TyFun (TyEVar a1) (TyEVar a2)) <> contextR
      pure (context', a1, a2)

instantiateMonoType :: Context -> TEVar -> Type -> Checker Context
instantiateMonoType context a t =
  case contextHole (ContextEVar a) context of
    Nothing -> throwError $ "Couldn't instantiate mono type, failed on contextHole " ++ show (a, context)
    Just (contextL, contextR) -> do
      liftEither $ typeWellFormed contextL t
      pure $ contextL |> ContextSolved a t <> contextR

--
-- Subtyping
--

subType :: Context -> Type -> Type -> Checker Context
subType context TyUnit TyUnit = pure context
subType context (TyVar a) (TyVar b) | a == b = pure context
subType context (TyEVar a) (TyEVar b) | a == b = pure context
subType context (TyFun t1 t2) (TyFun t1' t2') = do
  context' <- subType context t1' t1
  subType context' (contextSubst context' t2) (contextSubst context' t2')
subType context (TyForall a t1) t2 = do
  a' <- freshTEVar
  context' <- subType (context |> ContextMarker a' |> ContextEVar a') (instantiate a (TyEVar a') t1) t2
  pure $ contextUntil (ContextMarker a') context'
subType context t1 (TyForall a t2) =
  contextUntil (ContextVar a) <$> subType (context |> ContextVar a) t1 t2
subType context (TyEVar a) t = occursCheck a t >> instantiateLeft context a t
subType context t (TyEVar a) = occursCheck a t >> instantiateRight context t a
subType context t1 t2 = throwError $ "subType mismatch " ++ show (context, t1, t2)

occursCheck :: TEVar -> Type -> Checker ()
occursCheck a t =
  if a `elem` freeTEVars t
  then throwError $ "Infinite type " ++ show (a, t)
  else pure ()

--
-- Checking
--

checkExpr :: Context -> Expr -> Type -> Checker Context
checkExpr context EUnit TyUnit = pure context
checkExpr context e (TyForall a t) =
  contextUntil (ContextVar a) <$> checkExpr (context |> ContextVar a) e t
checkExpr context (ELam x e) (TyFun t1 t2) =
  contextUntil (ContextAssump x t1) <$> checkExpr (context |> ContextAssump x t1) e t2
checkExpr context e t = do
  (t', context') <- inferExpr context e
  subType context' (contextSubst context' t') (contextSubst context' t)

--
-- Infer
--

inferBinding :: Context -> Binding -> Checker (Type, Context)
inferBinding context (Binding _ args expr) = do
  argsAndVars <- traverse (\a -> (a,) <$> freshTEVar) args
  exprVar <- freshTEVar
  let
    allVars = (snd <$> argsAndVars) ++ [exprVar]
    argAssumps = uncurry ContextAssump . fmap TyEVar <$> argsAndVars
  marker <- ContextMarker <$> freshTEVar
  let
    contextExtension = Context $ Seq.fromList $ (ContextEVar <$> allVars) ++ argAssumps ++ [marker]
  context' <- checkExpr (context <> contextExtension) expr (TyEVar exprVar)
  let ty = contextSubst context' $ foldr1 TyFun $ TyEVar <$> allVars
  pure (ty, contextUntil marker context')

inferExpr :: Context -> Expr -> Checker (Type, Context)
inferExpr context EUnit = pure (TyUnit, context)
inferExpr context (EVar x) = maybe (throwError $ "Unbound variable " ++ show x) (\t -> pure (t, context)) $ contextAssumption context x
inferExpr context (EAnn e t) = do
  liftEither (typeWellFormed context t)
  context' <- checkExpr context e t
  pure (contextSubst context' t, context')
inferExpr context (ELam x e) = do
  a <- freshTEVar
  b <- freshTEVar
  context' <- checkExpr (context |> ContextEVar a |> ContextEVar b |> ContextAssump x (TyEVar a)) e (TyEVar b)
  let ty = contextSubst context' (TyEVar a `TyFun` TyEVar b)
  pure (ty, contextUntil (ContextAssump x (TyEVar a)) context')
inferExpr context (EApp f e) = do
  (tf, context') <- inferExpr context f
  inferApp context' (contextSubst context' tf) e

inferApp :: Context -> Type -> Expr -> Checker (Type, Context)
inferApp context (TyForall a t) e = do
  a' <- freshTEVar
  inferApp (context |> ContextEVar a') (instantiate a (TyEVar a') t) e
inferApp context (TyEVar a) e = do
  (context', a1, a2) <- instantiateTyFunContext context a
  context'' <- checkExpr context' e (TyEVar a1)
  let ty = contextSubst context'' (TyEVar a2)
  pure (ty, context'')
inferApp context (TyFun t1 t2) e = do
  context' <- checkExpr context e t1
  let ty = contextSubst context' t2
  pure (ty, context')
inferApp context t e = throwError $ "Cannot inferApp for " ++ show (context, t, e)

--
-- Top Level
--

-- checkExpr :: Expr -> Type -> Either String ()
-- checkExpr expr ty = void $ runChecker (checkExpr (Context Seq.empty) expr ty)

-- inferExpr :: Expr -> Either String Type
-- inferExpr expr = fst <$> runChecker (inferExpr (Context Seq.empty) expr)
