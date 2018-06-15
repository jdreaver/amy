{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lambda lifting core transformation.

module Amy.Core.LambdaLift
  ( lambdaLifting
  ) where

import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Data.List (foldl', tails)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)

import Amy.Core.AST

--
-- Top Level
--

lambdaLifting :: Module -> Module
lambdaLifting mod' =
  let
    bindings = moduleBindings mod'
    bindingsAndLifted = fmap (runLift . liftBindingBindings) <$> bindings
    bindings' = fmap fst <$> bindingsAndLifted
    -- TODO: This violates top-level binding group dependencies. We are just
    -- shoving all the lifted bindings at the end. Do we even need top-level
    -- binding group dependencies? Should we nuke them and just use a list?
    lifted :: [NonEmpty Binding]
    lifted = mapMaybe NE.nonEmpty $ fmap (fmap liftedBinding) $ concatMap NE.toList $ fmap snd <$> bindingsAndLifted
  in mod' { moduleBindings = bindings' ++ lifted }

--
-- Monad
--

newtype Lift a = Lift (State LiftState a)
  deriving (Functor, Applicative, Monad, MonadState LiftState)

runLift :: Lift a -> (a, [LiftedBinding])
runLift (Lift action) =
  let (result, liftState) = runState action (LiftState 0 Set.empty Map.empty)
  in (result, Map.elems $ liftedBindings liftState)

data LiftState
  = LiftState
  { lastId :: !Int
  , boundVariables :: !(Set (Typed IdentName))
  , liftedBindings :: !(Map IdentName LiftedBinding)
    -- ^ Map from old IdentName to new lifted function
  } deriving (Show, Eq)

data LiftedBinding
  = LiftedBinding
  { liftedBindingNewArgs :: ![Typed IdentName]
  , liftedBindingBinding :: !Binding
  } deriving (Show, Eq)

-- | Get the 'Binding' for a lifted binding
liftedBinding :: LiftedBinding -> Binding
liftedBinding lifted@(LiftedBinding newArgs (Binding name _ oldArgs retTy body)) =
  let ty = liftedBindingType lifted
  in Binding name ty (newArgs ++ oldArgs) retTy body

liftedBindingType :: LiftedBinding -> Type
liftedBindingType (LiftedBinding newArgs (Binding _ oldTy _ _ _)) =
  foldr1 TyFun $ (typedType <$> newArgs) ++ [oldTy]

freshId :: Lift Int
freshId = do
  modify' (\s -> s { lastId = 1 + lastId s })
  gets lastId

withNewScope :: Lift a -> Lift a
withNewScope action = do
  orig <- get
  result <- action
  modify' $ \s ->
    s
    { boundVariables = boundVariables orig
    }
  pure result

bindVariable :: Typed IdentName -> Lift ()
bindVariable var = modify' $ \s -> s { boundVariables = var `Set.insert` boundVariables s }

liftFunction :: [Typed IdentName] -> Binding -> Lift ()
liftFunction newArgs binding = do
  let oldIdent@(IdentName oldName) = bindingName binding
  newName <- IdentName . ((oldName <> "_$") <>) . pack . show <$> freshId
  let lifted = LiftedBinding newArgs binding { bindingName = newName }
  modify' $ \s -> s { liftedBindings = Map.insert oldIdent lifted (liftedBindings s) }

lookupVar :: IdentName -> Lift (Maybe LiftedBinding)
lookupVar var = Map.lookup var <$> gets liftedBindings

--
-- Lambda Lifting
--

liftBindingBindings :: Binding -> Lift Binding
liftBindingBindings (Binding name ty args retTy body) = withNewScope $ do
  bindVariable $ Typed ty name
  traverse_ bindVariable args
  body' <- liftExprBindings body
  pure $ Binding name ty args retTy body'

liftExprBindings :: Expr -> Lift Expr
liftExprBindings = go
 where
  go e@ELit{} = pure e
  go e@EVar{} = pure e
  go (ERecord rows) = ERecord <$> traverse (\(Typed ty e) -> Typed ty <$> go e) rows
  go (ERecordSelect e label ty) = (\e' -> ERecordSelect e' label ty) <$> go e
  go (EApp (App func arg ty)) = do
    func' <- go func
    arg' <- go arg
    pure $ EApp $ App func' arg' ty
  go (EParens e) = EParens <$> go e
  go (ECase (Case scrut bind alts default')) = do
    scrut' <- go scrut
    alts' <- traverse (\(Match pat e) -> Match pat <$> go e) alts
    default'' <- traverse go default'
    pure $ ECase $ Case scrut' bind alts' default''
  -- Non-recursive binding group
  go (ELet (Let (binding :| []) body)) = withNewScope $ do
    bindVariable $ Typed (bindingType binding) (bindingName binding)
    case bindingArgs binding of
      [] -> do
        binding' <- goBinding binding
        body' <- go body
        pure $ ELet $ Let (binding' :| []) body'
      _ -> do
        -- Lift the function and just return the body
        boundVars <- gets boundVariables
        let
          freeVars = freeBindingVars binding
          closeVars = freeVars `Set.intersection` boundVars
        liftFunction (Set.toList closeVars) binding
        -- TODO: Run lifted binding replacement on the lifted binding itself,
        -- in case it is recursive!
        body' <- replaceLiftedBindings body
        go body'
  -- Recurive binding group
  go (ELet (Let bindings _)) = error $ "Can't lambda lift recursive binding groups yet " ++ show (NE.toList $ bindingName <$> bindings)

  goBinding binding = do
    body' <- go (bindingBody binding)
    pure $ binding { bindingBody = body' }

replaceLiftedBindings :: Expr -> Lift Expr
replaceLiftedBindings = traverseExprTopDownM f
 where
  f e@(EVar (VVal (Typed _ var))) = do
    mLifted <- lookupVar var
    case mLifted of
      Nothing -> pure e
      Just lifted -> pure $ makeLiftedAppNode lifted
  f e = pure e

-- | Insert an App node for a lifted binding.
--
-- For example, if the old function is @f z = e@, and the lifted function has
-- new arguments @f_new x y z = e@, then we replace @f@ with
--
-- @
--    (f_new x) y
-- @
--
-- If the old type was @tz -> r@, then the new app types are:
--
-- @
--    (f_new :: ty -> tz -> r) y :: tz -> r
-- @
--
-- Note that we don't have to worry about @z@. Presumably whatever is being
-- bound as the @z@ argument is already present.
makeLiftedAppNode :: LiftedBinding -> Expr
makeLiftedAppNode lifted@(LiftedBinding newArgs binding) =
  let
    -- Compute the type for each app node. This is a bit tricky.
    tys = (typedType <$> newArgs) ++ [bindingType binding]
    appTys = drop 1 tys
    f = Typed (liftedBindingType lifted) (bindingName binding)
    varsAndTys = zip newArgs $ tails appTys
  in foldl' mkApp (EVar $ VVal f) varsAndTys
 where
  mkApp :: Expr -> (Typed IdentName, [Type]) -> Expr
  mkApp e (var', tys') = EApp $ App e (EVar (VVal var')) (foldr1 TyFun tys')