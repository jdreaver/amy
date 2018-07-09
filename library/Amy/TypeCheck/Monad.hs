{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Amy.TypeCheck.Monad
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
  , freshId
  , freshTyExistVarNoContext
  , freshTyExistVar
  , freshTyExistMarkerVar
  , withSourceSpan
  , throwAmyError
  , getContext
  , putContext
  , modifyContext
  , currentContextSubst
  , withContextUntil
  , withContextUntilNE
  , withNewContextScope
  , findTEVarHole
  , findMarkerHole
  , withNewLexicalScope
  , addValueTypeToScope
  , lookupValueType
  , lookupDataConType
  , addUnknownTyVarKindToScope
  , lookupTyVarKind
  , addTyConKindToScope
  , addUnknownTyConKindToScope
  , lookupTyConKind
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Bifunctor (first)
import Data.Foldable (asum, for_, toList)
import Data.List (lookup, sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Amy.Errors
import Amy.Kind
import Amy.Syntax.AST

--
-- Context
--

data ContextMember
  = ContextVar TyVarName
  | ContextEVar TyExistVarName
  | ContextSolved TyExistVarName Type
  | ContextMarker TyExistVarName
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
contextSubst ctx = go
 where
  go t@(TyExistVar v) = maybe t go (contextSolution ctx v)
  go record@(TyRecord rows mTail) =
    let rows' = go <$> rows
    in case go <$> mTail of
      Nothing -> TyRecord rows' mTail
      Just record'@(TyRecord newRows mTail') ->
        -- Ensure no overlap in rows. If there was overlap then unification
        -- shouldn't have allowed i
        if null $ Set.intersection (Map.keysSet rows') (Map.keysSet newRows)
        then TyRecord (Map.union rows' newRows) mTail'
        else error $ "Found duplicate keys in record substitution: " ++ show (record, record')
      Just t -> TyRecord rows' (Just t)
  go t = traverseType go t

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

typeWellFormed :: Context -> Type -> Maybe ErrorMessage
typeWellFormed _ TyUnknown = error "Encountered TyUnknown in typeWellFormed"
typeWellFormed _ (TyCon _) = Nothing
typeWellFormed context (TyVar (MaybeLocated _ v)) = do
  guard $ not $ ContextVar v `contextElem` context
  Just $ UnknownTypeVariable v
typeWellFormed context (TyExistVar v) = do
  let hasSolution = isJust (contextSolution context v)
  guard $ not $ ContextEVar v `contextElem` context || hasSolution
  -- N.B. This is an internal error because this should never be the user's
  -- fault.
  Just $ error $ "Unknown TyExistVar " ++ show v
typeWellFormed context (TyApp x y) = typeWellFormed context x >> typeWellFormed context y
typeWellFormed context (TyFun x y) = typeWellFormed context x >> typeWellFormed context y
typeWellFormed context (TyRecord rows mTy) = asum (typeWellFormed context <$> rows) >> (mTy >>= typeWellFormed context)
typeWellFormed context (TyForall vs t) = typeWellFormed (context <> Context (Seq.fromList $ NE.toList $ ContextVar . maybeLocatedValue <$> vs)) t

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
  , tyVarKinds :: !(Map TyVarName Kind)
  , tyConKinds :: !(Map TyConName Kind)
  , sourceSpan :: !SourceSpan
  } deriving (Show, Eq)

runChecker
  :: [(IdentName, Type)]
  -> [(Located DataConName, Type)]
  -> FilePath
  -> Checker a -> Either Error a
runChecker identTypes dataConTypes fp (Checker action) = do
  -- Check for duplicate data con names
  for_ groupedConNames $ \nameGroup ->
    case NE.tail nameGroup of
      [] -> Right ()
      (Located span' name : _) -> Left $ Error (DuplicateDataConstructor name) span'

  -- Run action
  evalState (runExceptT action) checkState
 where
  dataConNames = fst <$> dataConTypes
  groupedConNames = NE.groupAllWith locatedValue . sort $ dataConNames
  pos = SourcePos fp pos1 pos1
  checkState =
    CheckState
    { latestId = 0
    , stateContext = Context Seq.empty
    , valueTypes = Map.fromList identTypes
    , dataConstructorTypes = Map.fromList (first locatedValue <$> dataConTypes)
    , tyVarKinds = Map.empty
    , tyConKinds = Map.empty
    , sourceSpan = SourceSpan pos pos
    }

freshId :: Checker Int
freshId = modify' (\s -> s { latestId = latestId s + 1 }) >> gets latestId

freshTyExistVarNoContext :: Checker TyExistVarName
freshTyExistVarNoContext = TyExistVarName <$> freshId

freshTyExistVar :: Checker TyExistVarName
freshTyExistVar = do
  var <- freshTyExistVarNoContext
  modifyContext (|> ContextEVar var)
  pure var

freshTyExistMarkerVar :: Checker TyExistVarName
freshTyExistMarkerVar = do
  var <- freshTyExistVarNoContext
  modifyContext (|> ContextMarker var)
  pure var

withSourceSpan :: SourceSpan -> Checker a -> Checker a
withSourceSpan span' action = do
  orig <- gets sourceSpan
  modify' $ \s -> s { sourceSpan = span' }
  result <- action
  modify' $ \s -> s { sourceSpan = orig }
  pure result

throwAmyError :: ErrorMessage -> Checker a
throwAmyError message = do
  span' <- gets sourceSpan
  throwError $ Error message span'

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
  modifyContext $ contextUntil $ NE.head members
  pure result

withNewContextScope :: Checker a -> Checker a
withNewContextScope action = do
  marker <- freshTyExistMarkerVar
  result <- action
  modifyContext $ contextUntil (ContextMarker marker)
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

withNewLexicalScope :: Checker a -> Checker a
withNewLexicalScope action = do
  orig <- get
  result <- action
  modify' $ \s ->
    s
    { valueTypes = valueTypes orig
    , tyConKinds = tyConKinds orig
    , tyVarKinds = tyVarKinds orig
    }
  pure result

insertMapDuplicateError
  :: (Ord k)
  => (CheckState -> Map k v)
  -> (CheckState -> Map k v -> CheckState)
  -> k
  -> v
  -> (k -> ErrorMessage)
  -> Checker ()
insertMapDuplicateError getMap putMap name value mkError = do
  theMap <- gets getMap
  if Map.member name theMap
    then throwAmyError $ mkError name
    else modify' $ \s -> putMap s $ Map.insert name value (getMap s)

addValueTypeToScope :: Located IdentName -> Type -> Checker ()
addValueTypeToScope (Located span' name) ty =
  withSourceSpan span' $
    insertMapDuplicateError valueTypes (\s m -> s { valueTypes = m }) name ty VariableShadowed

lookupValueType :: Located IdentName -> Checker Type
lookupValueType (Located span' name) = do
  mTy <- Map.lookup name <$> gets valueTypes
  maybe (throwError $ Error (UnknownVariable name) span') pure mTy

lookupDataConType :: Located DataConName -> Checker Type
lookupDataConType (Located span' con) = do
  mTy <- Map.lookup con <$> gets dataConstructorTypes
  maybe (throwError $ Error (UnknownDataCon con) span') pure mTy

addUnknownTyVarKindToScope :: TyVarName -> Checker Int
addUnknownTyVarKindToScope name = do
  i <- freshId
  modify' (\s -> s { tyVarKinds = Map.insert name (KUnknown i) (tyVarKinds s) })
  pure i

lookupTyVarKind :: TyVarName -> Checker Kind
lookupTyVarKind name = do
  mKind <- Map.lookup name <$> gets tyVarKinds
  maybe (throwAmyError $ UnknownTypeVariable name) pure mKind

addTyConKindToScope :: Located TyConName -> Kind -> Checker ()
addTyConKindToScope (Located span' name) kind =
  withSourceSpan span' $
    insertMapDuplicateError tyConKinds (\s m -> s { tyConKinds = m }) name kind DuplicateTypeConstructor

addTyConKindToScope' :: TyConName -> Kind -> Checker ()
addTyConKindToScope' name kind = insertMapDuplicateError tyConKinds (\s m -> s { tyConKinds = m }) name kind DuplicateTypeConstructor

addUnknownTyConKindToScope :: TyConName -> Checker Int
addUnknownTyConKindToScope name = do
  i <- freshId
  addTyConKindToScope' name $ KUnknown i
  pure i

lookupTyConKind :: TyConName -> Checker Kind
lookupTyConKind name = do
  mKind <- Map.lookup name <$> gets tyConKinds
  maybe (throwAmyError $ UnknownTypeConstructor name) pure mKind
