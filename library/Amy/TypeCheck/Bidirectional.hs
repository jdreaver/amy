{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test implementation of Complete and Easy Bidirectional Typechecking for
-- Higher-Rank Polymorphism (Dunfield/Krishnaswami 2013)

module Amy.TypeChecking.Bidirectional
  ( inferBinding
  , checkBinding
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
import Data.List (foldl', lookup)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
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

substituteTEVar :: TEVar -> Type -> Type -> Type
substituteTEVar _ _ TyUnit = TyUnit
substituteTEVar v s t@(TyEVar v')
  | v == v' = s
  | otherwise = t
substituteTEVar _ _ t@(TyVar _) = t
substituteTEVar v s (TyFun a b) = TyFun (substituteTEVar v s a) (substituteTEVar v s b)
substituteTEVar v s (TyForall a t) = TyForall a (substituteTEVar v s t)

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

contextUnsolved :: Context -> [TEVar]
contextUnsolved (Context context) = mapMaybe getEVar $ toList context
 where
  getEVar (ContextEVar var) = Just var
  getEVar _ = Nothing

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

newtype Checker a = Checker (ExceptT String (State CheckState) a)
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError String)

data CheckState
  = CheckState
  { latestId :: !Int
  , stateContext :: !Context
  } deriving (Show, Eq)

runChecker :: Checker a -> Either String a
runChecker (Checker action) = evalState (runExceptT action) (CheckState 0 (Context Seq.empty))

freshId :: Checker Int
freshId = modify' (\s -> s { latestId = latestId s + 1 }) >> gets latestId

freshTEVar :: Checker TEVar
freshTEVar = TEVar . ("a" <>) . pack . show <$> freshId

getContext :: Checker Context
getContext = gets stateContext

putContext :: Context -> Checker ()
putContext context = modify' (\s -> s { stateContext = context })

currentContextSubst :: Type -> Checker Type
currentContextSubst t = do
  context <- getContext
  pure $ contextSubst context t

modifyContext :: (Context -> Context) -> Checker ()
modifyContext f = modify' (\s -> s { stateContext = f (stateContext s) })

withContextUntil :: ContextMember -> Checker a -> Checker a
withContextUntil member action = do
  modifyContext $ \context -> context |> member
  result <- action
  modifyContext $ contextUntil member
  pure result

findTEVarHole :: TEVar -> Checker (Context, Context)
findTEVarHole var = do
  context <- getContext
  maybe
    (throwError $ "Couldn't find existential variable in context " ++ show (var, context))
    pure
    (contextHole (ContextEVar var) context)

findMarkerHole :: TEVar -> Checker (Context, Context)
findMarkerHole var = do
  context <- getContext
  pure $
    fromMaybe
    (error $ "Couldn't find marker in context " ++ show (var, context))
    (contextHole (ContextMarker var) context)

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

instantiateLeft :: TEVar -> Type -> Checker ()
instantiateLeft a (TyEVar b) = instantiateReach a b
instantiateLeft a (TyFun t1 t2) = do
  (a1, a2) <- instantiateTyFunContext a
  instantiateRight t1 a1
  context' <- getContext
  instantiateLeft a2 (contextSubst context' t2)
instantiateLeft a (TyForall b t) =
  withContextUntil (ContextVar b) $
    instantiateLeft a t
-- Catch-all for all monotypes
instantiateLeft a t = instantiateMonoType a t

instantiateRight :: Type -> TEVar -> Checker ()
instantiateRight (TyEVar b) a = instantiateReach a b
instantiateRight (TyFun t1 t2) a = do
  (a1, a2) <- instantiateTyFunContext a
  instantiateLeft a1 t1
  t2' <- currentContextSubst t2
  instantiateRight t2' a2
instantiateRight (TyForall b t) a = do
  b' <- freshTEVar
  withContextUntil (ContextMarker b') $ do
    modifyContext $ \context -> context |> ContextEVar b'
    instantiateRight (instantiate b (TyEVar b') t) a
-- Catch-all for all monotypes
instantiateRight t a = instantiateMonoType a t

instantiateReach :: TEVar -> TEVar -> Checker ()
instantiateReach a b =
  catchError (instantiateMonoType a (TyEVar b)) $ \_ -> do
    (contextL, contextR) <- findTEVarHole b
    putContext $ contextL |> ContextSolved b (TyEVar a) <> contextR

instantiateTyFunContext :: TEVar -> Checker (TEVar, TEVar)
instantiateTyFunContext a = do
  (contextL, contextR) <- findTEVarHole a
  a1 <- freshTEVar
  a2 <- freshTEVar
  putContext $ contextL |> ContextEVar a2 |> ContextEVar a1 |> ContextSolved a (TyFun (TyEVar a1) (TyEVar a2)) <> contextR
  pure (a1, a2)

instantiateMonoType :: TEVar -> Type -> Checker ()
instantiateMonoType a t = do
  (contextL, contextR) <- findTEVarHole a
  liftEither $ typeWellFormed contextL t
  putContext $ contextL |> ContextSolved a t <> contextR

--
-- Subtyping
--

subType :: Type -> Type -> Checker ()
subType TyUnit TyUnit = pure ()
subType (TyVar a) (TyVar b) | a == b = pure ()
subType (TyEVar a) (TyEVar b) | a == b = pure ()
subType (TyFun t1 t2) (TyFun t1' t2') = do
  subType t1' t1
  t2Sub <- currentContextSubst t2
  t2Sub' <- currentContextSubst t2'
  subType t2Sub t2Sub'
subType (TyForall a t1) t2 = do
  a' <- freshTEVar
  withContextUntil (ContextMarker a') $ do
    modifyContext $ \context -> context |> ContextEVar a'
    subType (instantiate a (TyEVar a') t1) t2
subType t1 (TyForall a t2) =
  withContextUntil (ContextVar a) $
    subType t1 t2
subType (TyEVar a) t = occursCheck a t >> instantiateLeft a t
subType t (TyEVar a) = occursCheck a t >> instantiateRight t a
subType t1 t2 = throwError $ "subType mismatch " ++ show (t1, t2)

occursCheck :: TEVar -> Type -> Checker ()
occursCheck a t =
  if a `elem` freeTEVars t
  then throwError $ "Infinite type " ++ show (a, t)
  else pure ()

--
-- Checking
--

checkBinding :: Binding -> Type -> Checker ()
checkBinding binding (TyForall a t) =
  withContextUntil (ContextVar a) $
    checkBinding binding t
checkBinding binding t = do
  t' <- inferBinding binding
  tSub <- currentContextSubst t
  subType t' tSub

checkExpr :: Expr -> Type -> Checker ()
checkExpr EUnit TyUnit = pure ()
checkExpr e (TyForall a t) =
  withContextUntil (ContextVar a) $
    checkExpr e t
checkExpr (ELam x e) (TyFun t1 t2) =
  withContextUntil (ContextAssump x t1) $
    checkExpr e t2
checkExpr e t = do
  t' <- inferExpr e
  tSub <- currentContextSubst t
  subType t' tSub

--
-- Infer
--

inferBinding :: Binding -> Checker Type
inferBinding (Binding _ args expr) = do
  argsAndVars <- traverse (\a -> (a,) <$> freshTEVar) args
  exprVar <- freshTEVar
  let
    allVars = (snd <$> argsAndVars) ++ [exprVar]
    argAssumps = uncurry ContextAssump . fmap TyEVar <$> argsAndVars
  marker <- freshTEVar
  modifyContext (<> Context (Seq.fromList $ ContextMarker marker : (ContextEVar <$> allVars) ++ argAssumps))
  checkExpr expr (TyEVar exprVar)

  -- Generalize (the Hindley-Milner extension in the paper)
  (contextL, contextR) <- findMarkerHole marker
  let
    unsolvedEVars = contextUnsolved contextR
    tyVars = TVar . unTEVar <$> unsolvedEVars  -- Would probably use letters and a substitution here
    ty = contextSubst contextR $ foldr1 TyFun $ TyEVar <$> allVars
    ty' = foldl' (\t (TEVar var) -> substituteTEVar (TEVar var) (TyVar $ TVar var) t) ty unsolvedEVars
    tyForall = foldr TyForall ty' tyVars
  putContext contextL
  pure tyForall

inferExpr :: Expr -> Checker Type
inferExpr EUnit = pure TyUnit
inferExpr (EVar x) = do
  context <- getContext
  maybe
    (throwError $ "Unbound variable " ++ show x)
    pure
    (contextAssumption context x)
inferExpr (EAnn e t) = do
  context <- getContext
  liftEither (typeWellFormed context t)
  checkExpr e t
  currentContextSubst t
inferExpr (ELam x e) = do
  a <- freshTEVar
  b <- freshTEVar
  modifyContext $ \context -> context |> ContextEVar a |> ContextEVar b
  withContextUntil (ContextAssump x (TyEVar a)) $ do
    checkExpr e (TyEVar b)
    currentContextSubst (TyEVar a `TyFun` TyEVar b)
inferExpr (EApp f e) = do
  tf <- inferExpr f
  tfSub <- currentContextSubst tf
  inferApp tfSub e

inferApp :: Type -> Expr -> Checker Type
inferApp (TyForall a t) e = do
  a' <- freshTEVar
  modifyContext $ \context -> context |> ContextEVar a'
  inferApp (instantiate a (TyEVar a') t) e
inferApp (TyEVar a) e = do
  (a1, a2) <- instantiateTyFunContext a
  checkExpr e (TyEVar a1)
  currentContextSubst (TyEVar a2)
inferApp (TyFun t1 t2) e = do
  checkExpr e t1
  currentContextSubst t2
inferApp t e = throwError $ "Cannot inferApp for " ++ show (t, e)

-- Tests
-- putStrLn $ groom $ runChecker $ modifyContext (|> ContextEVar "a") >> instantiateRight (TyForall "b" $ TyVar "b" `TyFun` TyVar "b") "a" >> getContext
-- putStrLn $ groom $ runChecker $ inferBinding $ Binding "id" ["x"] (EVar "x")
-- putStrLn $ groom $ runChecker $ inferBinding $ Binding "id" ["x", "f"] (EApp (EVar "f") (EVar "x"))
-- putStrLn $ groom $ runChecker $ checkBinding (Binding "id" ["x"] (EVar "x")) (TyForall "a" $ TyVar "a" `TyFun` TyVar "a")
