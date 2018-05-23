{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.TypeCheck.Substitution
  ( Subst(..)
  , emptySubst
  , singletonSubst
  , composeSubst
  , substituteScheme
  , substituteType
  , substituteBinding
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Amy.TypeCheck.AST as T

newtype Subst = Subst (Map T.TyVarInfo T.Type)
  deriving (Eq, Show, Semigroup, Monoid)

emptySubst :: Subst
emptySubst = Subst Map.empty

singletonSubst :: T.TyVarInfo -> T.Type -> Subst
singletonSubst a t = Subst $ Map.singleton a t

composeSubst :: Subst -> Subst -> Subst
(Subst s1) `composeSubst` (Subst s2) = Subst $ Map.map (substituteType (Subst s1)) s2 `Map.union` s1

substituteScheme :: Subst -> T.Scheme -> T.Scheme
substituteScheme (Subst subst) (T.Forall vars ty) = T.Forall vars $ substituteType s' ty
 where
  s' = Subst $ foldr Map.delete subst vars

substituteType :: Subst -> T.Type -> T.Type
substituteType _ (T.TyCon con) = T.TyCon con
substituteType (Subst subst) t@(T.TyVar var) = Map.findWithDefault t var subst
substituteType subst (T.TyApp f arg) = T.TyApp (substituteType subst f) (substituteType subst arg)
substituteType subst@(Subst subst') record@(T.TyRecord rows mVar) =
  let rows' = substituteType subst <$> rows
  in case mVar >>= flip Map.lookup subst' of
    Nothing -> T.TyRecord rows' mVar
    Just (T.TyVar var) -> T.TyRecord rows' (Just var)
    Just record'@(T.TyRecord newRows mVar') ->
      -- Ensure no overlap in rows. If there was overlap then unification
      -- shouldn't have allowed it.
      if null $ Set.intersection (Map.keysSet rows') (Map.keysSet newRows)
      then T.TyRecord (Map.union rows' newRows) mVar'
      else error $ "Found duplicate keys in record substitution: " ++ show (record, record')
    Just t -> error $ "Invalid substitution for record. Did a kind check fail? " ++ show (record, t)
substituteType subst (t1 `T.TyFun` t2) = substituteType subst t1 `T.TyFun` substituteType subst t2

substituteTyped :: Subst -> T.Typed a -> T.Typed a
substituteTyped subst (T.Typed ty x) = T.Typed (substituteType subst ty) x

substituteBinding :: Subst -> T.Binding -> T.Binding
substituteBinding subst (T.Binding name ty args retTy body) =
  T.Binding
  { T.bindingName = name
  , T.bindingType = substituteScheme subst ty
  , T.bindingArgs = substituteTyped subst <$> args
  , T.bindingReturnType = substituteType subst retTy
  , T.bindingBody = substituteExpr subst body
  }

substituteExpr :: Subst -> T.Expr -> T.Expr
substituteExpr _ lit@T.ELit{} = lit
substituteExpr subst (T.ERecord (Typed ty rows)) = T.ERecord $ Typed (substituteType subst ty) (substituteExpr subst <$> rows)
substituteExpr subst (T.ERecordSelect expr label ty) =
  T.ERecordSelect (substituteExpr subst expr) label (substituteType subst ty)
substituteExpr subst (T.EVar var) =
  T.EVar $
    case var of
      T.VVal var' -> T.VVal $ substituteTyped subst var'
      T.VCons (T.Typed ty con) -> T.VCons (T.Typed (substituteType subst ty) con)
substituteExpr subst (T.EIf (T.If pred' then' else')) =
  T.EIf (T.If (substituteExpr subst pred') (substituteExpr subst then') (substituteExpr subst else'))
substituteExpr subst (T.ECase (T.Case scrutinee matches)) =
  T.ECase (T.Case (substituteExpr subst scrutinee) (substituteMatch subst <$> matches))
substituteExpr subst (T.ELet (T.Let bindings expr)) =
  T.ELet (T.Let (substituteBinding subst <$> bindings) (substituteExpr subst expr))
substituteExpr subst (T.EApp (T.App f arg returnType)) =
  T.EApp (T.App (substituteExpr subst f) (substituteExpr subst arg) (substituteType subst returnType))
substituteExpr subst (T.EParens expr) = T.EParens (substituteExpr subst expr)

substituteMatch :: Subst -> T.Match -> T.Match
substituteMatch subst (T.Match pat body) =
  T.Match (substitutePattern subst pat) (substituteExpr subst body)

substitutePattern :: Subst -> T.Pattern -> T.Pattern
substitutePattern _ pat@(T.PLit _) = pat
substitutePattern subst (T.PVar var) = T.PVar $ substituteTyped subst var
substitutePattern subst (T.PCons (T.PatCons con mArg retTy)) =
  let
    mArg' = substitutePattern subst <$> mArg
    retTy' = substituteType subst retTy
  in T.PCons (T.PatCons con mArg' retTy')
substitutePattern subst (T.PParens pat) = T.PParens (substitutePattern subst pat)
