{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lambda lifting core transformation.

module Amy.Core.LambdaLift
  (
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
-- Monad
--

newtype Lift a = Lift (State LiftState a)
  deriving (Functor, Applicative, Monad, MonadState LiftState)

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

liftExpr :: Expr -> Lift Expr
liftExpr e@ELit{} = pure e
liftExpr (EVar val@(VVal (Typed _ var))) = do
  mLifted <- lookupVar var
  case mLifted of
    Nothing -> pure $ EVar val
    Just lifted -> pure $ makeLiftedAppNode lifted

replaceLiftedFunction :: Typed IdentName -> Lift Expr
replaceLiftedFunction typed@(Typed _ var) = do
  mLifted <- lookupVar var
  case mLifted of
    Nothing -> pure $ EVar (VVal typed)
    Just lifted -> pure $ makeLiftedAppNode lifted

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
