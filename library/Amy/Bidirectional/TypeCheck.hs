module Amy.Bidirectional.TypeCheck
  (
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable (for_, toList)
import Data.List (foldl', lookup)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Traversable (for)

import Amy.Bidirectional.AST as T
import Amy.Bidirectional.Monad
import Amy.Bidirectional.Subtyping
import Amy.Renamer.AST as R
import Amy.Syntax.Located

--
-- Checking
--

-- checkBinding :: Binding -> Type -> Checker ()
-- checkBinding binding (TyForall a t) =
--   withContextUntil (ContextVar a) $
--     checkBinding binding t
-- checkBinding binding t = do
--   t' <- inferBinding binding
--   tSub <- currentContextSubst t
--   subType t' tSub

checkExpr :: R.Expr -> T.Type -> Checker T.Expr
--checkExpr EUnit TyUnit = pure ()
checkExpr e (TyForall as t) =
  withContextUntilNE (ContextVar <$> as) $
    checkExpr e t
-- checkExpr (ELam x e) (TyFun t1 t2) =
--   withNewNameScope $ do
--     addTypeToScope x t1
--     withContextUntil (ContextScopeMarker x) $
--       checkExpr e t2
checkExpr e t = do
  e' <- inferExpr e
  tSub <- currentContextSubst t
  subtype t tSub
  pure e'

--
-- Infer
--

-- inferBindingGroup :: [Binding] -> Checker [(Binding, Type)]
-- inferBindingGroup bindings = do
--   -- Add all binding group types to context
--   for_ bindings $ \(Binding name _ _) -> do
--     ty <- freshTEVar
--     addTypeToScope name (TyEVar ty)
--     modifyContext (|> ContextEVar ty)

--   -- Infer each binding individually
--   for bindings $ \binding@(Binding name _ _) -> do
--     ty <- inferBinding binding
--     -- Update type of binding name in State
--     -- TODO: Proper binding dependency analysis so this is done in the proper
--     -- order
--     addTypeToScope name ty
--     pure (binding, ty)

-- inferBinding :: Binding -> Checker Type
-- inferBinding (Binding _ args expr) = withNewNameScope $ do
--   argVars <- for args $ \arg -> do
--     ty <- freshTEVar
--     addTypeToScope arg (TyEVar ty)
--     pure ty
--   exprVar <- freshTEVar
--   let
--     allVars = argVars ++ [exprVar]
--   marker <- freshTEVar
--   modifyContext (<> Context (Seq.fromList $ ContextMarker marker : (ContextEVar <$> allVars)))
--   checkExpr expr (TyEVar exprVar)

--   -- Generalize (the Hindley-Milner extension in the paper)
--   (contextL, contextR) <- findMarkerHole marker
--   let
--     unsolvedEVars = contextUnsolved contextR
--     mkTVar = TVar . ("a" <>) . pack . show . unTEVar
--     tyVars = mkTVar <$> unsolvedEVars  -- Would probably use letters and a substitution here
--     ty = contextSubst contextR $ foldr1 TyFun $ TyEVar <$> allVars
--     ty' = foldl' (\t evar -> substituteTEVar evar (TyVar $ mkTVar evar) t) ty unsolvedEVars
--     tyForall = foldr TyForall ty' tyVars
--   putContext contextL
--   pure tyForall

inferExpr :: R.Expr -> Checker T.Expr
inferExpr (R.ELit (Located _ lit)) = pure $ T.ELit lit
inferExpr (R.EVar var) =
  case var of
    R.VVal (Located _ valVar) -> do
      t <- lookupValueType valVar
      pure $ T.EVar $ T.VVal (T.Typed t valVar)
    -- R.VCons (Located _ con) -> do
    --   t <- instantiate =<< dataConstructorScheme con
    --   pure (T.EVar $ T.VCons (T.Typed t con), [])

-- inferExpr (EAnn e t) = do
--   context <- getContext
--   liftEither (typeWellFormed context t)
--   checkExpr e t
--   currentContextSubst t
-- inferExpr (ELam x e) = do
--   a <- freshTEVar
--   b <- freshTEVar
--   modifyContext $ \context -> context |> ContextEVar a |> ContextEVar b
--   withNewNameScope $ do
--     addTypeToScope x (TyEVar a)
--     withContextUntil (ContextScopeMarker x) $ do
--       checkExpr e (TyEVar b)
--       currentContextSubst (TyEVar a `TyFun` TyEVar b)
inferExpr (R.EApp f e) = do
  f' <- inferExpr f
  tfSub <- currentContextSubst (expressionType f')
  (e', retTy) <- inferApp tfSub e
  pure (T.EApp $ T.App f' e' retTy)

inferApp :: T.Type -> R.Expr -> Checker (T.Expr, T.Type)
inferApp (TyForall as t) e = do
  as' <- traverse (const freshTEVar) as
  modifyContext $ \context -> context <> (Context $ Seq.fromList $ NE.toList $ ContextEVar <$> as')
  let t' = foldl' (\ty (a, a') -> instantiate a (TyExistVar a') ty) t $ NE.zip as as'
  inferApp t' e
inferApp (TyExistVar a) e = do
  (a1, a2) <- instantiateTyFunContext a
  e' <- checkExpr e (TyExistVar a1)
  t <- currentContextSubst (TyExistVar a2)
  pure (e', t)
inferApp (T.TyFun t1 t2) e = do
  e' <- checkExpr e t1
  t <- currentContextSubst t2
  pure (e', t)
inferApp t e = throwError $ "Cannot inferApp for " ++ show (t, e)

--
-- Converting types
--

convertScheme :: R.Scheme -> T.Type
convertScheme (R.Forall vars ty) = T.TyForall (NE.fromList $ convertTyVarInfo <$> vars) (convertType ty)

convertType :: R.Type -> T.Type
convertType (R.TyCon (Located _ con)) = T.TyCon con
convertType (R.TyVar var) = T.TyVar (convertTyVarInfo var)
convertType (R.TyApp f arg) = T.TyApp (convertType f) (convertType arg)
convertType (R.TyRecord rows mVar) =
  T.TyRecord
    (Map.mapKeys locatedValue $ convertType <$> rows)
    (locatedValue <$> mVar)
convertType (R.TyFun ty1 ty2) = T.TyFun (convertType ty1) (convertType ty2)

convertTyConDefinition :: R.TyConDefinition -> T.TyConDefinition
convertTyConDefinition (R.TyConDefinition name' args _) = T.TyConDefinition name' (locatedValue <$> args)

convertTyVarInfo :: Located TyVarName -> T.TyVarName
convertTyVarInfo (Located _ name') = name'
