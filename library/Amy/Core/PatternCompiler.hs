{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Pattern match compiler, inspired by the great book Implementation of
-- Functional Programming Languages.

module Amy.Core.PatternCompiler
  ( match
  , VarSubst(..)
  , InputPattern(..)
  , Con(..)
  , Arity
  , Span
  , Equation
  , CaseExpr(..)
  , Clause(..)
  ) where

import Control.Monad.Reader
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text (pack)

import Amy.Core.AST (Ident(..))
import Amy.Core.Monad
import Amy.Literal

-- | A 'InputPattern' is either a constructor or a variable.
data InputPattern con
  = PCon !(Con con) ![InputPattern con]
    -- ^ Constructor patterns are nested. We have the constructor tag and a
    -- list of pattern arguments.
  | PVar !Ident
    -- ^ Variables are catch-all patterns that match anything and binds the
    -- result to the variable.
  deriving (Show, Eq)

-- | A 'Con'structor is used to construct data types and destructure them in
-- patterns.
data Con con
  = Con !con !Arity !Span
    -- ^ "Normal" constructor
  | ConLit !Literal
    -- ^ Special constructor for a literal. Literals have 0 arity and infinite
    -- span.
  deriving (Show, Eq, Ord)

-- | 'Arity' is the number of arguments a constructor takes. A constructor with
-- no arguments has arity 0, and a constructor with N arguments has arity N.
type Arity = Int

conArity :: Con con -> Arity
conArity (Con _ arity _) = arity
conArity (ConLit _) = 0

-- | The 'Span' of a constructor is the total number of constructors in the
-- constructor's type. For example, in @data Bool = False | True@, each
-- constructor @False@ and @True@ have span 2 since there are two constructors.
type Span = Int

conSpan :: Con con -> Maybe Span
conSpan (Con _ _ span') = Just span'
conSpan (ConLit _) = Nothing

-- | An 'Equation' is a list of patterns and an expression.
type Equation expr con = ([InputPattern con], expr)

-- | A 'CaseExpr' is the output of the match algorithm
data CaseExpr expr con
  = Case !Ident ![Clause expr con] !(Maybe (CaseExpr expr con))
    -- ^ Corresponds to a case in Core
  | Expr !expr
    -- ^ An input expression
  | Error
    -- ^ Indicates a pattern matching failure error
  deriving (Show, Eq)

-- | A case expression contains a list of 'Clauses' that match against a
-- constructor.
data Clause expr con =
  Clause
    !(Con con) -- ^ Constructor for the clause
    ![Ident] -- ^ Variables to bind the constructor arguments to
    !(CaseExpr expr con) -- ^ Expression for the clause
  deriving (Show, Eq)

-- | Monad to run the algorithm in.
newtype Match expr a = Match (ReaderT (VarSubst expr) Desugar a)
  deriving (Functor, Applicative, Monad, MonadReader (VarSubst expr))

-- | A 'VarSubst' is a function to replace variables in an expression.
newtype VarSubst expr = VarSubst (expr -> Ident -> Ident -> expr)

substituteVariable :: expr -> Ident -> Ident -> Match expr expr
substituteVariable expr var newVar = do
  VarSubst f <- ask
  pure $ f expr var newVar

runMatch :: VarSubst expr -> Match expr a -> Desugar a
runMatch subst (Match action) = runReaderT action subst

freshVar :: Match expr Ident
freshVar = do
  id' <- Match $ lift freshId
  pure $ Ident ("_u" <> pack (show id')) id'

-- | Main function for this algorithm. Takes equations and produces a
-- 'CaseExpr'.
match
  :: (Show expr, Ord con)
  => VarSubst expr
  -> [Ident]
  -> [Equation expr con]
  -> Desugar (CaseExpr expr con)
match subst vars eqs = runMatch subst $ match' vars eqs Error

match'
  :: (Show expr, Ord con)
  => [Ident]
  -> [Equation expr con]
  -> CaseExpr expr con
  -> Match expr (CaseExpr expr con)
match' [] eqs def = pure $ foldr applyFatbar def (Expr . snd <$> eqs)
match' vars eqs def = foldM (matchGroup vars) def $ reverse $ groupEquations eqs

-- | The book uses a special infix operator called FATBAR. This function
-- applies some FATBAR optimizations.
applyFatbar
  :: CaseExpr expr con
  -> CaseExpr expr con
  -> CaseExpr expr con
applyFatbar Case{} _ = error "Can't guarantee case doesn't fail yet "
applyFatbar e@(Expr _) _ = e
applyFatbar Error _ = Error
-- applyFatbar e Fail = e
-- applyFatbar Fail e = e

--
-- Equation grouping
--

-- | There exist simple rules for the match function when a set of equations
-- starts with either all variables or all constructors. However, in the
-- general case constructors and variables are mixed. If we group a list of
-- equations by their type, then we can treat each group as a simpler case of
-- all variables or all constructors.
groupEquations :: [Equation expr con] -> [GroupedEquations expr con]
groupEquations = concatGroupedEquations . fmap equationType

data GroupedEquations expr con
  = VarEquations ![VarEquation expr con]
  | ConEquations ![ConEquation expr con]
  deriving (Show, Eq)

data VarEquation expr con = VarEquation !Ident !(Equation expr con)
  deriving (Show, Eq)

data ConEquation expr con = ConEquation !(Con con) ![InputPattern con] !(Equation expr con)
  deriving (Show, Eq)

equationType :: Equation expr con -> GroupedEquations expr con
equationType ([], _) = error "Encountered empty equations in equationType"
equationType (PVar var:ps, expr) = VarEquations [VarEquation var (ps, expr)]
equationType (PCon con ps:ps', expr) = ConEquations [ConEquation con ps' (ps, expr)]

concatGroupedEquations :: [GroupedEquations expr con] -> [GroupedEquations expr con]
concatGroupedEquations [] = []
concatGroupedEquations [x] = [x]
concatGroupedEquations (x:y:xs) =
  case (x, y) of
    (VarEquations es1, VarEquations es2) -> concatGroupedEquations $ VarEquations (es1 ++ es2) : xs
    (ConEquations es1, ConEquations es2) -> concatGroupedEquations $ ConEquations (es1 ++ es2) : xs
    (_, _) -> x : concatGroupedEquations (y:xs)

--
-- Matching
--

-- | Now that we have grouped our equations, we can treat each equation as a
-- simple case of all variables or all constructors.
matchGroup
  :: (Show expr, Ord con)
  => [Ident]
  -> CaseExpr expr con
  -> GroupedEquations expr con
  -> Match expr (CaseExpr expr con)
matchGroup vars def (VarEquations eqs) = matchVar vars eqs def
matchGroup vars def (ConEquations eqs) = matchCon vars eqs def

-- | The variable case is simple. Eat up the first match variable and replace
-- the pattern variable in the equation with the match variable.
matchVar
  :: (Show expr, Ord con)
  => [Ident]
  -> [VarEquation expr con]
  -> CaseExpr expr con
  -> Match expr (CaseExpr expr con)
matchVar [] _ _ = error "matchVars called with empty variables"
matchVar (u:us) eqs def = do
  let mkNewEquation (VarEquation var (ps, eq)) = (ps,) <$>  substituteVariable eq var u
  eqs' <- traverse mkNewEquation eqs
  match' us eqs' def

-- | When matching constructors, we first group all the equations by
-- constructor. Then we make a new 'Clause' for each constructor and embed them
-- in a 'Case' expression.
matchCon
  :: (Show expr, Ord con)
  => [Ident]
  -> [ConEquation expr con]
  -> CaseExpr expr con
  -> Match expr (CaseExpr expr con)
matchCon [] _ _ = error "matchCon called with empty variables"
matchCon _ [] _ = error "matchCon called with no equations"
matchCon (u:us) eqs@(ConEquation con _ _ : _) def = do
  let
    grouped = groupByConstructor eqs
    -- Check for inexhaustive match.
    default' =
      case conSpan con of
        Just span' ->
          if length grouped < span'
          then Just def
          else Nothing
        Nothing -> Just def
  eqs' <- traverse (matchClause us def) grouped
  pure $ Case u eqs' default'

-- | Groups constructors equations by 'Con'.
groupByConstructor
  :: (Ord con)
  => [ConEquation expr con]
  -> [NonEmpty (ConEquation expr con)]
groupByConstructor eqs =
  let
    -- N.B. We record the original order of the constructors as they come in.
    -- We then sort all of the equations by order of their constructor before
    -- grouping. That way when we group them, they don't come out in whatever
    -- random order "Ord con" gives us.
    consAndEqs = (\(i, eq@(ConEquation con _ _)) -> (con, (i, eq))) <$> zip [(0 :: Int)..] eqs
    originalOrder = Map.fromList $ reverse $ (\(con, (i, _)) -> (con, i)) <$> consAndEqs
    grouped = NE.groupWith fst $ sortOn (\(con, _) -> originalOrder Map.! con) consAndEqs
  in fmap (snd . snd) <$> grouped

matchClause
  :: (Show expr, Ord con)
  => [Ident]
  -> CaseExpr expr con
  -> NonEmpty (ConEquation expr con)
  -> Match expr (Clause expr con)
matchClause us def eqs = do
  let (ConEquation con _ _) = NE.head eqs
  us' <- traverse (const freshVar) [1..conArity con]
  let mkNewEquation (ConEquation _ pats' (pats, expr)) = (pats' ++ pats, expr)
  eqs' <- match' (us' ++ us) (NE.toList $ mkNewEquation <$> eqs) def
  pure $ Clause con us' eqs'
