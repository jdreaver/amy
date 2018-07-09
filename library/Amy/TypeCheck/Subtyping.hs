{-# LANGUAGE TupleSections #-}

module Amy.TypeCheck.Subtyping
  ( subtype
  , instantiate
  , articulateTyFunExist
  , freeTEVars
  ) where

import Data.Foldable (for_)
import Data.List (foldl', nub)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Sequence as Seq

import Amy.Errors
import Amy.TypeCheck.AST
import Amy.TypeCheck.Monad

--
-- Subtyping
--

subtype :: Type -> Type -> Checker ()
subtype (TyCon (MaybeLocated _ a)) (TyCon (MaybeLocated _ b)) | a == b = pure ()
subtype (TyVar (MaybeLocated _ a)) (TyVar (MaybeLocated _ b)) | a == b = pure ()
subtype (TyExistVar a) (TyExistVar b) | a == b = pure ()
subtype (TyApp t1 t2) (TyApp t1' t2') = subtypeMany [t1, t2] [t1', t2']
subtype (TyFun t1 t2) (TyFun t1' t2') = subtypeMany [t1, t2] [t1', t2']
subtype (TyForall as t1) t2 =
  withNewContextScope $ do
    as' <- traverse (const freshTyExistVar) as
    let t1' = foldl' (\t (MaybeLocated _ a, a') -> instantiate a (TyExistVar a') t) t1 $ NE.zip as as'
    subtype t1' t2
subtype t1 (TyForall as t2) =
  withContextUntilNE (ContextVar . maybeLocatedValue <$> as) $
    subtype t1 t2
subtype (TyExistVar a) t = occursCheck a t >> instantiateLeft a t
subtype t (TyExistVar a) = occursCheck a t >> instantiateRight t a
subtype t1@(TyRecord rows1 mTail1) t2@(TyRecord rows2 mTail2) = do
  -- TODO: This logic was copy/pasted and modified from old unification code.
  -- This probably needs to be audited to make sure it is actually checking for
  -- subtyping in the correct order (that is, t1 is a subtype of t2) and we
  -- aren't accidentally switching things around.
  let
    rows1' = Map.mapKeys maybeLocatedValue rows1
    rows2' = Map.mapKeys maybeLocatedValue rows2
    commonFields = Map.intersectionWith (,) rows1' rows2'
    justFields1 = Map.difference rows1' rows2'
    justFields2 = Map.difference rows2' rows1'
    -- We only need fresh type variables for record tails if there are
    -- differences between the records.
    maybeFreshTail distinctRows x =
      if Map.null distinctRows
      then pure x
      else TyExistVar <$> freshTyExistVar
    subtypeRecordWithVar rows mTail mUnifyVar = do
      recordTy <- currentContextSubst $ TyRecord (Map.mapKeys notLocated rows) mTail
      mTail' <- traverse currentContextSubst mTail
      mUnifyVar' <- traverse currentContextSubst mUnifyVar
      case (Map.null rows, mTail', mUnifyVar') of
        -- Rows are empty and unify variable is empty. This corresponds to
        -- unifying an empty row with an empty row.
        (True, Nothing, Nothing) -> pure ()
        -- Distinct rows are empty but there are type variables so unify them
        (True, Just tail', Just unifyVar) -> subtype tail' unifyVar
        -- Rows are empty and we are unifying with the empty row.
        (True, Just tail', Nothing) -> subtype tail' (TyRecord Map.empty Nothing)
        -- Base case, unify the record with the unify var
        (_, _, Just unifyVar) -> subtype recordTy unifyVar
        (_, _, _) -> throwAmyError $ UnificationFail t1 t2

  -- Unify common fields
  uncurry subtypeMany $ unzip $ snd <$> Map.toAscList commonFields
  -- Unify fields unique first record with second variable.
  mTail1' <- traverse (maybeFreshTail justFields2) mTail1
  subtypeRecordWithVar justFields1 mTail1' mTail2
  -- Vice versa, unifying rows from second record with first variable.
  mTail2' <- traverse (maybeFreshTail justFields1) mTail2
  subtypeRecordWithVar justFields2 mTail2' mTail1
subtype t1 t2 = throwAmyError $ UnificationFail t1 t2

subtypeMany :: [Type] -> [Type] -> Checker ()
subtypeMany [] [] = pure ()
subtypeMany (t1 : ts1) (t2 : ts2) = do
  subtype t1 t2
  ts1' <- traverse currentContextSubst ts1
  ts2' <- traverse currentContextSubst ts2
  subtypeMany ts1' ts2'
subtypeMany t1 t2 = error $ "subtypeMany lists different length " ++ show (t1, t2)

occursCheck :: TyExistVarName -> Type -> Checker ()
occursCheck a t =
  if a `elem` freeTEVars t
  then throwAmyError $ InfiniteType a t
  else pure ()

-- N.B. We use a list + nub here so we can easily preserve the order in which
-- the vars were found, versus using something like a Set.
freeTEVars :: Type -> [TyExistVarName]
freeTEVars = nub . freeTEVars'

freeTEVars' :: Type -> [TyExistVarName]
freeTEVars' (TyCon _) = []
freeTEVars' (TyVar _) = []
freeTEVars' (TyExistVar v) = [v]
freeTEVars' (TyApp a b) = freeTEVars a ++ freeTEVars b
freeTEVars' (TyRecord rows mTail) = concat $ (freeTEVars <$> Map.elems rows) ++ maybeToList (freeTEVars <$> mTail)
freeTEVars' (TyFun a b) = freeTEVars a ++ freeTEVars b
freeTEVars' (TyForall _ t) = freeTEVars t

--
-- Instantiate
--

instantiate :: TyVarName -> Type -> Type -> Type
instantiate v s = go
 where
  go t@(TyVar (MaybeLocated _ v'))
   | v == v' = s
   | otherwise = t
  go t = traverseType go t

instantiateLeft :: TyExistVarName -> Type -> Checker ()
instantiateLeft a (TyFun t1 t2) = do
  (a1, a2) <- articulateTyFunExist a
  instantiateRight t1 a1
  t2' <- currentContextSubst t2
  instantiateLeft a2 t2'
instantiateLeft a (TyApp t1 t2) = do
  (a1, a2) <- articulateTyAppExist a
  instantiateLeft a1 t1
  t2' <- currentContextSubst t2
  instantiateLeft a2 t2'
instantiateLeft a (TyForall bs t) =
  withContextUntilNE (ContextVar . maybeLocatedValue <$> bs) $
    instantiateLeft a t
instantiateLeft a (TyRecord rows mTail) = do
  tysAndVars <- articulateRecord a rows mTail
  for_ tysAndVars $ \(ty, rowVar) -> do
    ty' <- currentContextSubst ty
    instantiateLeft rowVar ty'
-- Catch-all for all monotypes and reach
instantiateLeft a t = instantiateMonoType a t

instantiateRight :: Type -> TyExistVarName -> Checker ()
instantiateRight (TyFun t1 t2) a = do
  (a1, a2) <- articulateTyFunExist a
  instantiateLeft a1 t1
  t2' <- currentContextSubst t2
  instantiateRight t2' a2
instantiateRight (TyApp t1 t2) a = do
  (a1, a2) <- articulateTyAppExist a
  instantiateRight t1 a1
  t2' <- currentContextSubst t2
  instantiateRight t2' a2
instantiateRight (TyForall bs t) a =
  withNewContextScope $ do
    bs' <- traverse (const freshTyExistVar) bs
    let t' = foldl' (\ty (MaybeLocated _ b, b') -> instantiate b (TyExistVar b') ty) t $ NE.zip bs bs'
    instantiateRight t' a
instantiateRight (TyRecord rows mTail) a = do
  tysAndVars <- articulateRecord a rows mTail
  for_ tysAndVars $ \(ty, rowVar) -> do
    ty' <- currentContextSubst ty
    instantiateRight ty' rowVar
-- Catch-all for all monotypes and reach
instantiateRight t a = instantiateMonoType a t

articulateTyFunExist :: TyExistVarName -> Checker (TyExistVarName, TyExistVarName)
articulateTyFunExist = articulateTyAppExist' TyFun

articulateTyAppExist :: TyExistVarName -> Checker (TyExistVarName, TyExistVarName)
articulateTyAppExist = articulateTyAppExist' TyApp

articulateTyAppExist' :: (Type -> Type -> Type) -> TyExistVarName -> Checker (TyExistVarName, TyExistVarName)
articulateTyAppExist' f a = do
  (contextL, contextR) <- findTEVarHole a
  a1 <- freshTyExistVarNoContext
  a2 <- freshTyExistVarNoContext
  putContext $ contextL |> ContextEVar a2 |> ContextEVar a1 |> ContextSolved a (f (TyExistVar a1) (TyExistVar a2)) <> contextR
  pure (a1, a2)

articulateRecord :: TyExistVarName -> Map (MaybeLocated RowLabel) Type -> Maybe Type -> Checker [(Type, TyExistVarName)]
articulateRecord a rows mTail = do
  (contextL, contextR) <- findTEVarHole a
  rowsAndVars <- traverse (\t -> (t,) <$> freshTyExistVarNoContext) rows
  mTailAndVar <- traverse (\t -> (t,) <$> freshTyExistVarNoContext) mTail
  let
    rowVars = snd <$> rowsAndVars
    mTailVar = snd <$> mTailAndVar
    newContextVars = Context $ Seq.fromList $ ContextEVar <$> Map.elems rowVars ++ maybeToList mTailVar
    rows' = TyExistVar <$> rowVars
  putContext $ contextL <> newContextVars |> ContextSolved a (TyRecord rows' (TyExistVar <$> mTailVar)) <> contextR

  pure $ Map.elems rowsAndVars ++ maybeToList mTailAndVar

instantiateMonoType :: TyExistVarName -> Type -> Checker ()
instantiateMonoType a t = do
  (contextL, contextR) <- findTEVarHole a
  case (t, typeWellFormed contextL t) of
    (_, Nothing) -> putContext $ contextL |> ContextSolved a t <> contextR
    -- Special reach rule for existentials
    (TyExistVar b, Just _) -> instantiateReach a b
    (_, Just err) -> throwAmyError err

instantiateReach :: TyExistVarName -> TyExistVarName -> Checker ()
instantiateReach a b = do
  (contextL, contextR) <- findTEVarHole b
  putContext $ contextL |> ContextSolved b (TyExistVar a) <> contextR
