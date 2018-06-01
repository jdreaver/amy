module Amy.Bidirectional.Subtyping
  ( subtype
  , instantiate
  , instantiateTyFunContext
  ) where

import Control.Monad.Except
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Amy.Bidirectional.AST
import Amy.Bidirectional.Monad

--
-- Subtyping
--

subtype :: Type -> Type -> Checker ()
subtype (TyCon a) (TyCon b) | a == b = pure ()
subtype (TyVar a) (TyVar b) | a == b = pure ()
subtype (TyExistVar a) (TyExistVar b) | a == b = pure ()
subtype (TyApp t1 t2) (TyApp t1' t2') = subtypeMany [t1, t1'] [t2, t2']
subtype (TyFun t1 t2) (TyFun t1' t2') = subtypeMany [t1, t1'] [t2, t2']
subtype (TyForall as t1) t2 = do
  as' <- traverse (const freshTEVar) as
  withContextUntilNE (ContextMarker <$> as') $ do
    modifyContext $ \context -> context <> (Context $ Seq.fromList $ NE.toList $ ContextEVar <$> as')
    let t1' = foldl' (\t (a, a') -> instantiate a (TyExistVar a') t) t1 $ NE.zip as as'
    subtype t1' t2
subtype t1 (TyForall as t2) =
  withContextUntilNE (ContextVar <$> as) $
    subtype t1 t2
subtype (TyExistVar a) t = occursCheck a t >> instantiateLeft a t
subtype t (TyExistVar a) = occursCheck a t >> instantiateRight t a
subtype t1@(TyRecord rows1 mTail1) t2@(TyRecord rows2 mTail2) = do
  -- TODO: This logic was copy/pasted and modified from old unification code.
  -- This probably needs to be audited to make sure it is actually checking for
  -- subtyping in the correct order (that is, t1 is a subtype of t2) and we
  -- aren't accidentally switching things around.
  let
    commonFields = Map.intersectionWith (,) rows1 rows2
    justFields1 = Map.difference rows1 rows2
    justFields2 = Map.difference rows2 rows1
    -- We only need fresh type variables for record tails if there are
    -- differences between the records.
    maybeFreshTail distinctRows x =
      if Map.null distinctRows
      then pure x
      else TyExistVar <$> freshTEVar
    subtypeRecordWithVar rows mTail mUnifyVar = do
      recordTy <- currentContextSubst $ TyRecord rows mTail
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
        --(_, _, _) -> throwError $ UnificationFail t1 t2
        (_, _, _) -> throwError $ "subtype mismatch record " ++ show (t1, t2)

  -- Unify common fields
  uncurry subtypeMany $ unzip $ snd <$> Map.toAscList commonFields
  -- Unify fields unique first record with second variable.
  mTail1' <- traverse (maybeFreshTail justFields2) mTail1
  subtypeRecordWithVar justFields1 mTail1' mTail2
  -- Vice versa, unifying rows from second record with first variable.
  mTail2' <- traverse (maybeFreshTail justFields1) mTail2
  subtypeRecordWithVar justFields2 mTail2' mTail1
subtype t1 t2 = throwError $ "subtype mismatch " ++ show (t1, t2)

-- TODO: Fix this subtype case for TyRecord
-- runChecker $ modifyContext (|> ContextEVar (TyExistVarName 1)) >> subtype (TyRecord [("x", TyCon "Int"), ("y", TyCon "Bool")] Nothing) (TyRecord [("x", TyCon "Int")] (Just $ TyExistVar (TyExistVarName 1)))
--
-- Note that the following works with the old unification code
-- runInference (TyEnv Map.empty Map.empty Map.empty Map.empty 0) $ unifies (T.TyRecord [("x", T.TyCon "Int")] (Just $ T.TyVarInfo "a" TyVarGenerated)) (T.TyRecord [("x", T.TyCon "Int"), ("y", T.TyCon "Bool")] Nothing)

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
  then throwError $ "Infinite type " ++ show (a, t)
  else pure ()

freeTEVars :: Type -> Set TyExistVarName
freeTEVars (TyCon _) = Set.empty
freeTEVars (TyVar _) = Set.empty
freeTEVars (TyExistVar v) = Set.singleton v
freeTEVars (TyApp a b) = freeTEVars a <> freeTEVars b
freeTEVars (TyRecord rows _) = Set.unions $ freeTEVars <$> Map.elems rows
freeTEVars (TyFun a b) = freeTEVars a <> freeTEVars b
freeTEVars (TyForall _ t) = freeTEVars t

--
-- Instantiate
--

instantiate :: TyVarName -> Type -> Type -> Type
instantiate _ _ t@(TyCon _) = t
instantiate v s t@(TyVar v')
  | v == v' = s
  | otherwise = t
instantiate _ _ t@(TyExistVar _) = t
instantiate v s (TyApp a b) = TyApp (instantiate v s a) (instantiate v s b)
instantiate v s (TyRecord rows mVar) = TyRecord (instantiate v s <$> rows) mVar -- TODO: What to do with mVar?
instantiate v s (TyFun a b) = TyFun (instantiate v s a) (instantiate v s b)
instantiate v s (TyForall a t) = TyForall a (instantiate v s t)

instantiateLeft :: TyExistVarName -> Type -> Checker ()
instantiateLeft a (TyExistVar b) = instantiateReach a b
instantiateLeft a (TyFun t1 t2) = do
  (a1, a2) <- instantiateTyFunContext a
  instantiateRight t1 a1
  context' <- getContext
  instantiateLeft a2 (contextSubst context' t2)
instantiateLeft a (TyForall bs t) =
  withContextUntilNE (ContextVar <$> bs) $
    instantiateLeft a t
-- Catch-all for all monotypes
instantiateLeft a t = instantiateMonoType a t

instantiateRight :: Type -> TyExistVarName -> Checker ()
instantiateRight (TyExistVar b) a = instantiateReach a b
instantiateRight (TyFun t1 t2) a = do
  (a1, a2) <- instantiateTyFunContext a
  instantiateLeft a1 t1
  t2' <- currentContextSubst t2
  instantiateRight t2' a2
instantiateRight (TyForall bs t) a = do
  bs' <- traverse (const freshTEVar) bs
  withContextUntilNE (ContextMarker <$> bs') $ do
    modifyContext $ \context -> context <> (Context $ Seq.fromList $ NE.toList $ ContextEVar <$> bs')
    let t' = foldl' (\ty (b, b') -> instantiate b (TyExistVar b') ty) t $ NE.zip bs bs'
    instantiateRight t' a
-- Catch-all for all monotypes
instantiateRight t a = instantiateMonoType a t

instantiateReach :: TyExistVarName -> TyExistVarName -> Checker ()
instantiateReach a b =
  catchError (instantiateMonoType a (TyExistVar b)) $ \_ -> do
    (contextL, contextR) <- findTEVarHole b
    putContext $ contextL |> ContextSolved b (TyExistVar a) <> contextR

instantiateTyFunContext :: TyExistVarName -> Checker (TyExistVarName, TyExistVarName)
instantiateTyFunContext a = do
  (contextL, contextR) <- findTEVarHole a
  a1 <- freshTEVar
  a2 <- freshTEVar
  putContext $ contextL |> ContextEVar a2 |> ContextEVar a1 |> ContextSolved a (TyFun (TyExistVar a1) (TyExistVar a2)) <> contextR
  pure (a1, a2)

instantiateMonoType :: TyExistVarName -> Type -> Checker ()
instantiateMonoType a t = do
  (contextL, contextR) <- findTEVarHole a
  liftEither $ typeWellFormed contextL t
  putContext $ contextL |> ContextSolved a t <> contextR
