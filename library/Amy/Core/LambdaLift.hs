{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lambda lifting Core transformation.
--
-- This is needed to target backends that don't have nested functions, like
-- LLVM. It is a pre-requisite to ANF conversion.
--
-- The algorithm is a slightly simpler version of the one from "Lambda Lifting:
-- Transforming Programs into Recursive Equations (Johnsson 1985)".
--
-- This pass also eta expands partially-applied data constructors and primitive
-- operations before lifting them.

module Amy.Core.LambdaLift
  ( lambdaLifting
  ) where

import Control.Monad.State.Strict
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Traversable (for)

import Amy.Core.AST
import Amy.Prim
import Amy.Utils.SolveSetEquations

--
-- Top Level
--

lambdaLifting :: Module -> Module
lambdaLifting mod' =
  let
    bindings = moduleBindings mod'
    (bindings', lifted) = runLift $ traverse (traverse liftBindingBindings) bindings
    -- TODO: This violates top-level binding group dependencies. We are just
    -- shoving all the lifted bindings at the end. Do we even need top-level
    -- binding group dependencies? Should we nuke them and just use a list?
    lifted' :: [NonEmpty Binding]
    lifted' = (:| []) . liftedBinding <$> lifted
  in mod' { moduleBindings = bindings' ++ lifted' }

--
-- Monad
--

newtype Lift a = Lift (State LiftState a)
  deriving (Functor, Applicative, Monad, MonadState LiftState)

runLift :: Lift a -> (a, [LiftedBinding])
runLift (Lift action) =
  let (result, liftState) = runState action (LiftState 0 Set.empty [])
  in (result, reverse $ liftedBindings liftState)

data LiftState
  = LiftState
  { lastId :: !Int
  , boundVariables :: !(Set (Typed IdentName))
  , liftedBindings :: ![LiftedBinding]
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

mkLiftedFunction :: [Typed IdentName] -> Binding -> Lift (IdentName, LiftedBinding)
mkLiftedFunction newArgs binding = do
  let oldIdent@(IdentName oldName) = bindingName binding
  newName <- IdentName . ((oldName <> "_$") <>) . pack . show <$> freshId
  let lifted = LiftedBinding newArgs binding { bindingName = newName }
  pure (oldIdent, lifted)

storeLifted :: LiftedBinding -> Lift ()
storeLifted lifted = do
  binding' <- liftBindingBindings $ liftedBindingBinding lifted
  let lifted' = lifted { liftedBindingBinding = binding' }
  modify' $ \s -> s { liftedBindings = lifted' : liftedBindings s }

--
-- Lambda Lifting
--

liftBindingBindings :: Binding -> Lift Binding
liftBindingBindings (Binding name ty args retTy body) = withNewScope $ do
  -- Bind binding name and args to context
  bindVariable $ Typed ty name
  traverse_ bindVariable args

  -- Perform lambda lifting in the body
  body' <- liftExprBindings body
  pure $ Binding name ty args retTy body'

liftExprBindings :: Expr -> Lift Expr
liftExprBindings = go
 where
  -- Turn lambdas into let expression so we can use the same code
  go (ELam (Lambda args body ty)) = do
    name <- IdentName . ("lambda" <>) . pack . show <$> freshId
    let
      binding = Binding name ty (NE.toList args) (expressionType body) body
      var = EVar $ VVal $ Typed ty name
    go $ ELet $ Let (binding :| []) var

  -- Lift let expressions
  go (ELet (Let bindings body)) = withNewScope $ do
    -- Bind binding names to scope
    for_ bindings $ \binding -> bindVariable $ Typed (bindingType binding) (bindingName binding)
    boundVars <- gets boundVariables

    -- Compute variables we need to close for each binding
    let
      allBindingNamesAndTys = Set.fromList . NE.toList $ (\b -> Typed (bindingType b) (bindingName b)) <$> bindings
      computeCloseVars binding = (freeBindingVars binding `Set.intersection` boundVars) `Set.difference` allBindingNamesAndTys
      bindingsAndCloseVars = fmap (\b -> (b, computeCloseVars b)) bindings

    -- Solve set equations
    let
      allBindingNames = Set.fromList $ typedValue <$> Set.toList allBindingNamesAndTys
      mkEq (binding, vars) = SetEquation (bindingName binding) vars (bindingName binding `Set.delete` allBindingNames)
      equations = mkEq <$> bindingsAndCloseVars
      solutions = solveSetEquations (NE.toList equations)

    -- Re-associate bindings with solutions
    let
      lookupNewVars binding =
        fromMaybe (error $ "Panic! Lost solution for binding " ++ show (bindingName binding))
        $ lookup (bindingName binding) solutions
      bindingsAndSolutions = (\b -> (b, lookupNewVars b)) <$> bindings

    -- Perform lifting
    let
      hasArgs = not . null . bindingArgs
      (bindingsToLift, unlifted) = NE.partition (hasArgs . fst) bindingsAndSolutions
    lifted <- traverse (\(binding, vars) -> mkLiftedFunction (Set.toList vars) binding) bindingsToLift

    -- Replace variables in body and return

    -- TODO: Performing this replacement immediately is less efficient than if
    -- we saved the variables needing replacing and did it later in the
    -- traversal. We could store these replacements + extra vars in the State
    -- and add them as-needed when computing free variables later on. See the
    -- Johnsson paper for details.
    --
    -- This method is much simpler though, and I hesitate to make it more
    -- efficient unless we can profile and find an example for which this
    -- function dominates compilation time.
    let
      replaceBody binding =
        let
          bindBody = bindingBody binding
          bindBody' = replaceLiftedBindings lifted bindBody
        in binding { bindingBody = bindBody' }
      replaceLifted (LiftedBinding args binding) = LiftedBinding args (replaceBody binding)
      lifted' = replaceLifted . snd <$> lifted
      unlifted' = replaceBody . fst <$> unlifted
      body' = replaceLiftedBindings lifted body
    traverse_ storeLifted lifted'

    case NE.nonEmpty unlifted' of
      Nothing -> go body'
      Just unliftedNE -> do
        unliftedNE' <- traverse goBinding unliftedNE
        body'' <- go body'
        pure $ ELet $ Let unliftedNE' body''

  -- Try to eta expand primops and data constructors
  go (EVar var) = do
    expr <- maybeEtaExpandVar var 0
    case expr of
      EVar{} -> pure expr
      _ -> go expr
  go (EApp app) = do
    let func :| args = unfoldApp app
    func' <-
      case func of
        EVar var -> maybeEtaExpandVar var (length args)
        _ -> go func
    func'' <-
      case func' of
        EVar{} -> pure func'
        _ -> go func'
    args' <- traverse go args
    case foldApp func'' args' of
      -- TODO: Fix this kludge where foldApp has a polymorphic return type when
      -- it should be monomorphized.
      EApp app' -> pure $ EApp app' { appReturnType = appReturnType app }
      e -> pure e

  go e = traverseExprM liftExprBindings e

  goBinding binding = do
    body' <- go (bindingBody binding)
    pure $ binding { bindingBody = body' }

replaceLiftedBindings :: [(IdentName, LiftedBinding)] -> Expr -> Expr
replaceLiftedBindings lifted = traverseExprTopDown f
 where
  f e@(EVar (VVal (Typed _ var))) = maybe e makeLiftedAppNode (lookup var lifted)
  f e = e

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
--    ((f_new :: tx -> ty -> tz -> r) x :: ty -> tz -> r) y :: tz -> r
-- @
--
-- Note that we don't have to worry about @z@. Presumably whatever is being
-- bound as the @z@ argument is already present.
makeLiftedAppNode :: LiftedBinding -> Expr
makeLiftedAppNode lifted@(LiftedBinding newArgs binding) =
  let f = Typed (liftedBindingType lifted) (bindingName binding)
  in foldApp (EVar $ VVal f) (EVar . VVal <$> newArgs)

-- | Eta expands primops and data constructors.
--
-- The @Int@ argument is the number of arguments already applied.
--
maybeEtaExpandVar :: Var -> Int -> Lift Expr
maybeEtaExpandVar var@(VVal (Typed _ func)) numArgsApplied =
  etaExpand var
  . maybe [] (fmap TyCon . drop numArgsApplied . NE.init . primitiveFunctionType)
  $ Map.lookup func primitiveFunctionsByName
maybeEtaExpandVar var@(VCons (Typed ty _)) numArgsApplied = do
  let
    allArgTys = NE.init $ unfoldTyApp ty
    argTys = drop numArgsApplied allArgTys
  etaExpand var argTys

-- | Eta expands the given expression to be applied to N extra args.
--
-- Example in pseudo-code: @etaExpand f [Int, Double]@ would give @(\(x :: Int)
-- (y :: Double) -> f x y)@
--
etaExpand :: Var -> [Type] -> Lift Expr
etaExpand func argTypes =
  case NE.nonEmpty argTypes of
    Nothing -> pure $ EVar func
    Just argTypesNE -> do
      args <- for argTypesNE $ \argType ->
        Typed argType . IdentName . ("_x" <>) . pack . show <$> freshId
      let
        app = foldApp (EVar func) (EVar . VVal <$> NE.toList args)
        lambdaTy = foldr1 TyFun $ argTypes ++ [expressionType app]
      pure $ ELam $ Lambda args app lambdaTy
