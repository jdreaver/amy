{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- | Collect constraints from the AST

module Amy.TypeCheck.Inference.Constraints
  ( Inference
  , runInference
  , TypeError(..)
  , freshTypeVariable
  , letters
  , inferBindings
  , inferBinding
  , Constraint(..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)

import Amy.Literal
import Amy.Names
import Amy.Prim
import Amy.Renamer.AST
import Amy.Syntax.Located
import Amy.Type

--
-- Inference Monad
--

-- | Holds a set of monomorphic variables in a 'ReaderT' and a 'State' 'Int'
-- counter for producing type variables.
newtype Inference a = Inference (ReaderT (Set TVar) (StateT Int (Except TypeError)) a)
  deriving (Functor, Applicative, Monad, MonadReader (Set TVar), MonadState Int, MonadError TypeError)

runInference :: Inference a -> Either TypeError a
runInference (Inference action) = runExcept $ evalStateT (runReaderT action Set.empty) 0

-- TODO: Separate monads for constraint collection and unification.

-- TODO: Don't use Except, use Validation

-- TODO: Move these into the main Error type
-- TODO: Include source spans in these errors
data TypeError
  = UnificationFail !(Type PrimitiveType) !(Type PrimitiveType)
  | InfiniteType TVar !(Type PrimitiveType)
  | UnboundVariable ValueName
  | Ambigious [Constraint]
  deriving (Show, Eq)

-- | Generate a fresh type variable
freshTypeVariable :: Inference TVar
freshTypeVariable = do
  modify' (+ 1)
  TVar . (letters !!) <$> get

-- TODO: Don't use letters for type variables, just use integers. Then at the
-- end of inference we can turn all the type variables into letters so the user
-- gets nice letters to see.
letters :: [Text]
letters = [1..] >>= fmap pack . flip replicateM ['a'..'z']

-- | Add a monomorphic variable to the monomorphic set and run a computation
withAddedMonomorphicVars :: [TVar] -> Inference a -> Inference a
withAddedMonomorphicVars xs = local (Set.union (Set.fromList xs))

data InferenceResult
  = InferenceResult
  { inferenceResultAssumptions :: !AssumptionSet
  , inferenceResultConstraints :: ![Constraint]
  , inferenceResultType :: !(Type PrimitiveType)
  } deriving (Show, Eq)

--
-- Inference Functions
--

inferBindings :: [RBinding] -> Inference [([Constraint], Type PrimitiveType)]
inferBindings bindings = do
  bindingsInference <- traverse inferBinding bindings
  let
    bindingNames = locatedValue . rBindingName <$> bindings
    allAssumptions = concatAssumptions $ inferenceResultAssumptions <$> bindingsInference
    unbounds = Set.fromList (assumptionKeys allAssumptions) `Set.difference` Set.fromList bindingNames
  unless (Set.null unbounds) $
    throwError $ UnboundVariable $ Set.findMin unbounds
  pure $ (\(InferenceResult _ cs t) -> (cs, t)) <$> bindingsInference

inferBinding :: RBinding -> Inference InferenceResult
inferBinding (RBinding _ _ args body) = do
  -- Instantiate a fresh type variable for every argument
  argsAndTyVars <- traverse (\arg -> (arg,) <$> freshTypeVariable) args

  -- Infer the type of the expression after extending the monomorphic set with
  -- the type variables for the arguments.
  let
    tyVars = snd <$> argsAndTyVars
  (InferenceResult asBody consBody tyBody) <- withAddedMonomorphicVars tyVars $ inferExpr body

  -- Create equality constraints for each argument by looking up the argument
  -- in the assumptions from the body.
  let
    argConstraint (Located _ argName, argTyVar) =
      (\t -> EqConstraint t (TyVar argTyVar)) <$> lookupAssumption argName asBody
    argConstraints = concatMap argConstraint argsAndTyVars

  pure
    InferenceResult
      -- Remove the arguments from the assumption set because they are now
      -- represented in the constraints with fresh type variables.
    { inferenceResultAssumptions = foldl' removeAssumption asBody (locatedValue <$> args)
    , inferenceResultConstraints = consBody ++ argConstraints
    , inferenceResultType = typeFromNonEmpty (NE.fromList $ (TyVar <$> tyVars) ++ [tyBody])
    }

-- | Collect constraints for an expression.
inferExpr :: RExpr -> Inference InferenceResult
inferExpr (RELit (Located _ (LiteralInt _))) = pure $ InferenceResult emptyAssumptionSet [] (TyCon IntType)
inferExpr (RELit (Located _ (LiteralDouble _))) = pure $ InferenceResult emptyAssumptionSet [] (TyCon DoubleType)
inferExpr (RELit (Located _ (LiteralBool _))) = pure $ InferenceResult emptyAssumptionSet [] (TyCon BoolType)
inferExpr (REVar (Located _ name)) = do
  -- For a Var, generate a fresh type variable and add it to the assumption set
  tyVar <- TyVar <$> freshTypeVariable
  pure $ InferenceResult (singletonAssumption name tyVar) [] tyVar
inferExpr (REIf (RIf pred' then' else')) = do
  -- If statements are simple. Merge all the assumptions/constraints from each
  -- sub expression. Then add a constraint saying the predicate must be a Bool,
  -- and that the then and else branches have equal types.
  (InferenceResult predAssumptions predConstraints predTy) <- inferExpr pred'
  (InferenceResult thenAssumptions thenConstraints thenTy) <- inferExpr then'
  (InferenceResult elseAssumptions elseConstraints elseTy) <- inferExpr else'
  pure
    InferenceResult
    { inferenceResultAssumptions = predAssumptions `mergeAssumptions` thenAssumptions `mergeAssumptions` elseAssumptions
    , inferenceResultConstraints =
        predConstraints ++ thenConstraints ++ elseConstraints ++
        [EqConstraint predTy (TyCon BoolType), EqConstraint thenTy elseTy]
    , inferenceResultType = thenTy
    }
inferExpr (RELet (RLet bindings expression)) = do
  bindingResults <- traverse inferBinding bindings
  (InferenceResult expressionAssumptions expressionConstraints expressionTy) <- inferExpr expression
  monomorphicSet <- ask
  let
    bindingNames = locatedValue . rBindingName <$> bindings
    bindingAssumptions = inferenceResultAssumptions <$> bindingResults
    bindingConstraints = inferenceResultConstraints <$> bindingResults
    bindingTypes = inferenceResultType <$> bindingResults

    -- Remove binding names from expression assumption
    expressionAssumption = foldl' removeAssumption expressionAssumptions bindingNames

    -- For each binding, add an implicit instance constraint associating the
    -- binding with the inferred result type, along with the current
    -- monomorphic set.
    newConstraints = flip concatMap (zip bindingNames bindingTypes) $
      \(bindingName, bindingType) ->
        (\t -> ImplicitInstanceConstraint t monomorphicSet bindingType)
        <$> lookupAssumption bindingName expressionAssumptions
  pure
    InferenceResult
    { inferenceResultAssumptions = concatAssumptions bindingAssumptions `mergeAssumptions` expressionAssumption
    , inferenceResultConstraints = concat bindingConstraints ++ expressionConstraints ++ newConstraints
    , inferenceResultType = expressionTy
    }
inferExpr (REApp (RApp func args)) = do
  -- For an App, we first collect constraints for the function and the
  -- arguments. Then, we instantiate a fresh type variable. The assumption sets
  -- and constraint sets are merged, and the additional constraint that the
  -- function is a TyApp from the args to the fresh type variable is added.
  (InferenceResult asFunc consFunc tyFunc) <- inferExpr func
  argResults <- NE.toList <$> traverse inferExpr args
  let
    argAssumptions = inferenceResultAssumptions <$> argResults
    argConstraints = inferenceResultConstraints <$> argResults
    argTypes = inferenceResultType <$> argResults
  tyVar <- TyVar <$> freshTypeVariable
  let
    newConstraint = EqConstraint tyFunc (typeFromNonEmpty $ NE.fromList (argTypes ++ [tyVar]))
  pure
    InferenceResult
    { inferenceResultAssumptions = concatAssumptions argAssumptions `mergeAssumptions` asFunc
    , inferenceResultConstraints = consFunc ++ concat argConstraints ++ [newConstraint]
    , inferenceResultType = tyVar
    }

--
-- Constraints
--

-- | A 'Constraint' places a restriction on what type is assigned to a
-- variable. Constraints are collected and then solved after collection.
data Constraint
  = EqConstraint !(Type PrimitiveType) !(Type PrimitiveType)
    -- ^ Indicates types should be unified
  | ImplicitInstanceConstraint !(Type PrimitiveType) !(Set TVar) !(Type PrimitiveType)
    -- ^ The first type should be an explicit instance of the generalized
    -- scheme from the second type. The monomorphized type variables are
    -- carried around to compute this generalized type.
  | ExplicitInstanceConstraint !(Type PrimitiveType) !(Scheme PrimitiveType)
    -- ^ The type must be a generic instance of the given scheme
  deriving (Show, Eq)

--
-- AssumptionSet
--

-- | An Assumption is an assignment of a type variable to a free variable in an
-- expression. An Assumption is created during the inference of a Var in an
-- expression, and assumptions are bubbled up the AST during bottom-up
-- constraint collection. There can be more than on assumption for a given
-- variable.
newtype AssumptionSet = AssumptionSet { unAssumptionSet :: Map ValueName [Type PrimitiveType] }
  deriving (Show, Eq)

emptyAssumptionSet :: AssumptionSet
emptyAssumptionSet = AssumptionSet Map.empty

-- | Remove an assumption from the assumption set. This is done when we
-- encounter a binding for a variable, like a lambda or let expression
-- variable. The assumption gets "converted" into a constraint.
removeAssumption :: AssumptionSet -> ValueName -> AssumptionSet
removeAssumption (AssumptionSet xs) name = AssumptionSet (Map.delete name xs)

lookupAssumption :: ValueName -> AssumptionSet -> [Type PrimitiveType]
lookupAssumption name (AssumptionSet xs) = Map.findWithDefault [] name xs

concatAssumptions :: [AssumptionSet] -> AssumptionSet
concatAssumptions = foldl' mergeAssumptions emptyAssumptionSet

mergeAssumptions :: AssumptionSet -> AssumptionSet -> AssumptionSet
mergeAssumptions (AssumptionSet a) (AssumptionSet b) = AssumptionSet (Map.unionWith (++) a b)

singletonAssumption :: ValueName -> Type PrimitiveType -> AssumptionSet
singletonAssumption x y = AssumptionSet (Map.singleton x [y])

assumptionKeys :: AssumptionSet -> [ValueName]
assumptionKeys (AssumptionSet xs) = Map.keys xs
