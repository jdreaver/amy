{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.Bidirectional.Monad
  ( -- * Context
    Context(..)
  , ContextMember(..)
  , (|>)
  , contextSubst
  , contextHole
  , contextUntil
  , contextUnsolved
  , typeWellFormed

    -- * Monad
  , Checker
  , runChecker
  , freshTEVar
  , getContext
  , putContext
  , modifyContext
  , currentContextSubst
  , withContextUntil
  , withContextUntilNE
  , findTEVarHole
  , findMarkerHole
  , withNewValueTypeScope
  , addValueTypeToScope
  , lookupValueType
  , lookupDataConType
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.List (lookup)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Amy.Bidirectional.AST
import Amy.Errors

--
-- Context
--

data ContextMember
  = ContextVar TyVarName
  | ContextEVar TyExistVarName
  | ContextSolved TyExistVarName Type
  | ContextMarker TyExistVarName
    -- We store context assumptions in a Map for efficiency, but a lot of the
    -- typing judgements use the assumptions for scoping. We add this
    -- ContextScopeMarker to take the place of that.

    -- TODO: Should we just use ContextAssump like in the paper for names in
    -- the current binding group or expression, and only fall back to the Map
    -- for globally known names? Diverging from the paper is a bit scary since
    -- there are lots of subtle implications for the scoping rules.
  | ContextScopeMarker IdentName
  deriving (Show, Eq, Ord)

-- TODO: Could this just be a stack (reversed List) instead of a Seq? I don't
-- think that would be more efficient when dealing with holes.
newtype Context = Context (Seq ContextMember)
  deriving (Show, Eq, Ord, Semigroup)

(|>) :: Context -> ContextMember -> Context
(Context context) |> member = Context (context Seq.|> member)

contextElem :: ContextMember -> Context -> Bool
contextElem member (Context context) = member `elem` context

contextSubst :: Context -> Type -> Type
contextSubst _ t@(TyCon _) = t
contextSubst _ t@(TyVar _) = t
contextSubst ctx t@(TyExistVar v) = maybe t (contextSubst ctx) (contextSolution ctx v)
contextSubst ctx (TyApp a b) = TyApp (contextSubst ctx a) (contextSubst ctx b)
contextSubst ctx record@(TyRecord rows mTail) =
  let rows' = contextSubst ctx <$> rows
  in case contextSubst ctx <$> mTail of
    Nothing -> TyRecord rows' mTail
    Just record'@(TyRecord newRows mTail') ->
      -- Ensure no overlap in rows. If there was overlap then unification
      -- shouldn't have allowed i
      if null $ Set.intersection (Map.keysSet rows') (Map.keysSet newRows)
      then TyRecord (Map.union rows' newRows) mTail'
      else error $ "Found duplicate keys in record substitution: " ++ show (record, record')
    Just t -> TyRecord rows' (Just t)
contextSubst ctx (TyFun a b) = TyFun (contextSubst ctx a) (contextSubst ctx b)
contextSubst ctx (TyForall vs t) = TyForall vs (contextSubst ctx t)

-- | Γ = Γ0[Θ] means Γ has the form (ΓL, Θ,ΓR)
contextHole :: ContextMember -> Context -> Maybe (Context, Context)
contextHole member (Context context) = (\(cl, cr) -> (Context cl, Context (Seq.drop 1 cr))) . flip Seq.splitAt context <$> mIndex
 where
  mIndex = Seq.elemIndexR member context

contextSolution :: Context -> TyExistVarName -> Maybe Type
contextSolution (Context context) var = lookup var solutionPairs
 where
  solutionPairs = mapMaybe solutionPair $ toList context
  solutionPair (ContextSolved var' ty) = Just (var', ty)
  solutionPair _ = Nothing

contextUnsolved :: Context -> [TyExistVarName]
contextUnsolved (Context context) = mapMaybe getEVar $ toList context
 where
  getEVar (ContextEVar var) = Just var
  getEVar _ = Nothing

contextUntil :: ContextMember -> Context -> Context
contextUntil member (Context context) = Context $ Seq.takeWhileL (/= member) context

typeWellFormed :: Context -> Type -> Bool
typeWellFormed _ (TyCon _) = True
typeWellFormed context (TyVar v) = ContextVar v `contextElem` context
typeWellFormed context (TyExistVar v) = ContextEVar v `contextElem` context || hasSolution
  where hasSolution = isJust (contextSolution context v)
typeWellFormed context (TyApp x y) = typeWellFormed context x && typeWellFormed context y
typeWellFormed context (TyFun x y) = typeWellFormed context x && typeWellFormed context y
typeWellFormed context (TyRecord rows mTy) = all (typeWellFormed context) rows && maybe True (typeWellFormed context) mTy
typeWellFormed context (TyForall vs t) = typeWellFormed (context <> Context (Seq.fromList $ NE.toList $ ContextVar <$> vs)) t

--
-- Monad
--

-- TODO: Use Validation instead of ExceptT

newtype Checker a = Checker (ExceptT Error (State CheckState) a)
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError Error)

data CheckState
  = CheckState
  { latestId :: !Int
  , stateContext :: !Context
  , valueTypes :: !(Map IdentName Type)
  , dataConstructorTypes :: !(Map DataConName Type)
  } deriving (Show, Eq)

runChecker
  :: [(IdentName, Type)]
  -> [(DataConName, Type)]
  -> Checker a -> Either Error a
runChecker identTypes dataConTypes (Checker action) = evalState (runExceptT action) checkState
 where
  checkState =
    CheckState
    { latestId = 0
    , stateContext = Context Seq.empty
    , valueTypes = Map.fromList identTypes
    , dataConstructorTypes = Map.fromList dataConTypes
    }

freshId :: Checker Int
freshId = modify' (\s -> s { latestId = latestId s + 1 }) >> gets latestId

freshTEVar :: Checker TyExistVarName
freshTEVar = TyExistVarName <$> freshId

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

withContextUntilNE :: NonEmpty ContextMember -> Checker a -> Checker a
withContextUntilNE members action = do
  modifyContext $ \context -> context <> (Context $ Seq.fromList $ NE.toList members)
  result <- action
  modifyContext $ contextUntil (NE.head members)
  pure result

findTEVarHole :: TyExistVarName -> Checker (Context, Context)
findTEVarHole var = do
  context <- getContext
  pure $
    fromMaybe
    (error $ "Couldn't find existential variable in context " ++ show (var, context))
    (contextHole (ContextEVar var) context)

findMarkerHole :: TyExistVarName -> Checker (Context, Context)
findMarkerHole var = do
  context <- getContext
  pure $
    fromMaybe
    (error $ "Couldn't find marker in context " ++ show (var, context))
    (contextHole (ContextMarker var) context)

withNewValueTypeScope :: Checker a -> Checker a
withNewValueTypeScope action = do
  orig <- gets valueTypes
  result <- action
  modify' $ \s -> s { valueTypes = orig }
  pure result

addValueTypeToScope :: IdentName -> Type -> Checker ()
addValueTypeToScope name ty = modify' $ \s -> s { valueTypes = Map.insert name ty (valueTypes s) }

lookupValueType :: IdentName -> Checker Type
lookupValueType name = do
  mTy <- Map.lookup name <$> gets valueTypes
  maybe (throwError $ UnboundVariable name) pure mTy

lookupDataConType :: DataConName -> Checker Type
lookupDataConType con =
  fromMaybe (error $ "No type definition for " ++ show con)
    . Map.lookup con
    <$> gets dataConstructorTypes
