{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lambda lifting core transformation.

module Amy.Core.LambdaLift
  ( lambdaLifting
  ) where

import Control.Monad.State.Strict
import Data.List (foldl', tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
    liftBinding binding = do
      body <- undefined
      pure $ binding { bindingBody = body }
    bindings' = runLift $ traverse (traverse liftBinding) bindings
  in undefined

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
  , boundVariables :: !(Set IdentName)
  , liftedBindings :: !(Map IdentName LiftedBinding)
    -- ^ Map from old IdentName to new lifted function
  } deriving (Show, Eq)

data LiftedBinding
  = LiftedBinding
  { liftedBindingName :: !IdentName
  , liftedBindingNewArgs :: ![Typed IdentName]
  , liftedBindingBinding :: !Binding
  } deriving (Show, Eq)

-- | Get the 'Binding' for a lifted binding
liftedBinding :: LiftedBinding -> Binding
liftedBinding lifted@(LiftedBinding name newArgs (Binding oldName oldTy oldArgs retTy body)) =
  let
    ty = liftedBindingType lifted
    body' = substExpr body oldName name
  in Binding name ty (newArgs ++ oldArgs) retTy body'

liftedBindingType :: LiftedBinding -> Type
liftedBindingType (LiftedBinding _ newArgs (Binding _ oldTy _ _ _)) =
  foldr1 TyFun $ (typedType <$> newArgs) ++ [oldTy]

freshId :: Lift Int
freshId = do
  modify' (\s -> s { lastId = 1 + lastId s })
  gets lastId

bindVariable :: IdentName -> Lift ()
bindVariable var = modify' $ \s -> s { boundVariables = var `Set.insert` boundVariables s }

liftFunction :: IdentName -> [Typed IdentName] -> Binding -> Lift ()
liftFunction oldIdent@(IdentName oldName) newArgs binding = do
  newName <- IdentName . ((oldName <> "_$") <>) . pack . show <$> freshId
  let lifted = LiftedBinding newName newArgs binding
  modify' $ \s -> s { liftedBindings = Map.insert oldIdent lifted (liftedBindings s) }

lookupVar :: IdentName -> Lift (Maybe LiftedBinding)
lookupVar var = Map.lookup var <$> gets liftedBindings

--
-- Lambda Lifting
--

liftExprBindings :: Expr -> Lift Expr
liftExprBindings = traverseExprTopDownM f
 where
  f (ELet (Let bindings expr)) = do
    undefined
  f e = pure e

replaceLiftedBindings :: Expr -> Lift Expr
replaceLiftedBindings = traverseExprTopDownM f
 where
  f (EVar (VVal typed@(Typed _ var))) = do
    mLifted <- lookupVar var
    case mLifted of
      Nothing -> pure $ EVar (VVal typed)
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
makeLiftedAppNode lifted@(LiftedBinding name newArgs oldBinding) =
  let
    -- Compute the type for each app node. This is a bit tricky.
    tys = (typedType <$> newArgs) ++ [bindingType oldBinding]
    appTys = drop 1 tys
    f = Typed (liftedBindingType lifted) name
    varsAndTys = zip newArgs $ tails appTys
  in foldl' mkApp (EVar $ VVal f) varsAndTys
 where
  mkApp :: Expr -> (Typed IdentName, [Type]) -> Expr
  mkApp e (var', tys') = EApp $ App e (EVar (VVal var')) (foldr1 TyFun tys')


--
-- Testing
--

testLift :: LiftedBinding
testLift =
  LiftedBinding
  "f_new"
  [Typed (TyCon "FX") "x", Typed (TyCon "FY") "y"]
  $ Binding
    "f"
    (TyCon "FZ" `TyFun` TyCon "R")
    [Typed (TyCon "FZ") "z"]
    (TyCon "R")
    (ELit (LiteralInt 1))
