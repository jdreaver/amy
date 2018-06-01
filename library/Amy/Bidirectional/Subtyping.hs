module Amy.Bidirectional.Subtyping
  ( subtype
  , instantiate
  , articulateTyFunExist
  ) where

import Control.Monad.Except
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Amy.Bidirectional.AST
import Amy.Bidirectional.Monad
import Amy.Errors

--
-- Subtyping
--

subtype :: Type -> Type -> Checker ()
subtype (TyCon a) (TyCon b) | a == b = pure ()
subtype (TyVar a) (TyVar b) | a == b = pure ()
subtype (TyExistVar a) (TyExistVar b) | a == b = pure ()
subtype (TyApp t1 t2) (TyApp t1' t2') = subtypeMany [t1, t2] [t1', t2']
subtype (TyFun t1 t2) (TyFun t1' t2') = subtypeMany [t1, t2] [t1', t2']
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
      else do
        var <- freshTEVar
        modifyContext (|> ContextEVar var)
        pure $ TyExistVar var
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
        (_, _, _) -> throwError $ OtherTodoError $ "UnificationFail " ++ show (t1, t2)

  -- Unify common fields
  uncurry subtypeMany $ unzip $ snd <$> Map.toAscList commonFields
  -- Unify fields unique first record with second variable.
  mTail1' <- traverse (maybeFreshTail justFields2) mTail1
  subtypeRecordWithVar justFields1 mTail1' mTail2
  -- Vice versa, unifying rows from second record with first variable.
  mTail2' <- traverse (maybeFreshTail justFields1) mTail2
  subtypeRecordWithVar justFields2 mTail2' mTail1
subtype t1 t2 = throwError $ OtherTodoError $ "UnificationFail " ++ show (t1, t2)

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
  then throwError $ OtherTodoError $ "Infinite type " ++ show (a, t)
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
instantiate v s (TyRecord rows mTail) = TyRecord (instantiate v s <$> rows) (instantiate v s <$> mTail)
instantiate v s (TyFun a b) = TyFun (instantiate v s a) (instantiate v s b)
instantiate v s (TyForall a t) = TyForall a (instantiate v s t)

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
  withContextUntilNE (ContextVar <$> bs) $
    instantiateLeft a t
instantiateLeft a (TyRecord rows (Just tailVar)) = do
  tailVar' <- articulateRecord a rows
  instantiateLeft tailVar' tailVar
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
instantiateRight (TyForall bs t) a = do
  bs' <- traverse (const freshTEVar) bs
  withContextUntilNE (ContextMarker <$> bs') $ do
    modifyContext $ \context -> context <> (Context $ Seq.fromList $ NE.toList $ ContextEVar <$> bs')
    let t' = foldl' (\ty (b, b') -> instantiate b (TyExistVar b') ty) t $ NE.zip bs bs'
    instantiateRight t' a
instantiateRight (TyRecord rows (Just tailVar)) a = do
  tailVar' <- articulateRecord a rows
  instantiateRight tailVar tailVar'
-- Catch-all for all monotypes and reach
instantiateRight t a = instantiateMonoType a t

articulateTyFunExist :: TyExistVarName -> Checker (TyExistVarName, TyExistVarName)
articulateTyFunExist = articulateTyAppExist' TyFun

articulateTyAppExist :: TyExistVarName -> Checker (TyExistVarName, TyExistVarName)
articulateTyAppExist = articulateTyAppExist' TyApp

articulateTyAppExist' :: (Type -> Type -> Type) -> TyExistVarName -> Checker (TyExistVarName, TyExistVarName)
articulateTyAppExist' f a = do
  (contextL, contextR) <- findTEVarHole a
  a1 <- freshTEVar
  a2 <- freshTEVar
  putContext $ contextL |> ContextEVar a2 |> ContextEVar a1 |> ContextSolved a (f (TyExistVar a1) (TyExistVar a2)) <> contextR
  pure (a1, a2)

articulateRecord :: TyExistVarName -> Map RowLabel Type -> Checker TyExistVarName
articulateRecord a rows = do
  (contextL, contextR) <- findTEVarHole a
  tailVar' <- freshTEVar
  putContext $ contextL |> ContextEVar tailVar' |> ContextSolved a (TyRecord rows (Just $ TyExistVar tailVar')) <> contextR
  pure tailVar'

instantiateMonoType :: TyExistVarName -> Type -> Checker ()
instantiateMonoType a t = do
  (contextL, contextR) <- findTEVarHole a
  case (t, typeWellFormed contextL t) of
    (_, True) -> putContext $ contextL |> ContextSolved a t <> contextR
    -- Special reach rule for existentials
    (TyExistVar b, False) -> instantiateReach a b
    _ -> error $ "Type not well-formed " ++ show (a, t, contextL, contextR)

instantiateReach :: TyExistVarName -> TyExistVarName -> Checker ()
instantiateReach a b = do
  (contextL, contextR) <- findTEVarHole b
  putContext $ contextL |> ContextSolved b (TyExistVar a) <> contextR
