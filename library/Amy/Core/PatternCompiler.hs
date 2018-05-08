{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Pattern match compiler, inspired by the great book Implementation of
-- Functional Programming Languages.

module Amy.Core.PatternCompiler
  ( match
  , VarSubst(..)
  , MakeVar(..)
  , Pattern(..)
  , Con(..)
  , Arity
  , Span
  , Equation
  , CaseExpr(..)
  , Clause(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack)

import Amy.Literal

-- | A 'Pattern' is either a constructor or a variable.
data Pattern con var
  = PCon !(Con con) ![Pattern con var]
    -- ^ Constructor patterns are nested. We have the constructor tag and a
    -- list of pattern arguments.
  | PVar !var
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
type Equation expr con var = ([Pattern con var], expr)

-- | A 'CaseExpr' is the output of the match algorithm
data CaseExpr expr con var
  = Case !var ![Clause expr con var] !(Maybe (CaseExpr expr con var))
    -- ^ Corresponds to a case in Core
  | Expr !expr
    -- ^ An input expression
  | Error
    -- ^ Indicates a pattern matching failure error
  deriving (Show, Eq)

-- | A case expression contains a list of 'Clauses' that match against a
-- constructor.
data Clause expr con var =
  Clause
    !(Con con) -- ^ Constructor for the clause
    ![var] -- ^ Variables to bind the constructor arguments to
    !(CaseExpr expr con var) -- ^ Expression for the clause
  deriving (Show, Eq)

-- | Monad to run the algorithm in.
newtype Match expr var a = Match (ReaderT (MatchData expr var) (State Int) a)
  deriving (Functor, Applicative, Monad, MonadReader (MatchData expr var), MonadState Int)

data MatchData expr var = MatchData (VarSubst expr var) (MakeVar var)

-- | A 'VarSubst' is a function to replace variables in an expression.
newtype VarSubst expr var = VarSubst (expr -> var -> var -> expr)

-- | Describes how to make a new variable given an 'Int' ID and a 'Text' name.
newtype MakeVar var = MakeVar (Int -> Text -> var)

substituteVariable :: expr -> var -> var -> Match expr var expr
substituteVariable expr var newVar = do
  MatchData (VarSubst f) _ <- ask
  pure $ f expr var newVar

runMatch :: MatchData expr var -> Int -> Match expr var a -> a
runMatch matchData i (Match action) = evalState (runReaderT action matchData) i

freshVar :: Match expr var var
freshVar = do
  modify' (+ 1)
  id' <- get
  MatchData _ (MakeVar mkVar) <- ask
  pure $ mkVar id' ("_u" <> pack (show id'))

-- | Main function for this algorithm. Takes equations and produces a
-- 'CaseExpr'.
match
  :: (Show expr, Ord con)
  => VarSubst expr var
  -> MakeVar var
  -> [var]
  -> [Equation expr con var]
  -> CaseExpr expr con var
match subst mkVar vars eqs = runMatch (MatchData subst mkVar) 0 $ match' vars eqs Error

match'
  :: (Show expr, Ord con)
  => [var]
  -> [Equation expr con var]
  -> CaseExpr expr con var
  -> Match expr var (CaseExpr expr con var)
match' [] eqs def = pure $ foldr applyFatbar def (Expr . snd <$> eqs)
match' vars eqs def = foldM (matchGroup vars) def $ reverse $ groupEquations eqs

-- | The book uses a special infix operator called FATBAR. This function
-- applies some FATBAR optimizations.
applyFatbar
  :: CaseExpr expr con var
  -> CaseExpr expr con var
  -> CaseExpr expr con var
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
groupEquations :: [Equation expr con var] -> [GroupedEquations expr con var]
groupEquations = concatGroupedEquations . fmap equationType

data GroupedEquations expr con var
  = VarEquations ![VarEquation expr con var]
  | ConEquations ![ConEquation expr con var]
  deriving (Show, Eq)

data VarEquation expr con var = VarEquation !var !(Equation expr con var)
  deriving (Show, Eq)

data ConEquation expr con var = ConEquation !(Con con) ![Pattern con var] !(Equation expr con var)
  deriving (Show, Eq)

equationType :: Equation expr con var -> GroupedEquations expr con var
equationType ([], _) = error "Encountered empty equations in equationType"
equationType (PVar var:ps, expr) = VarEquations [VarEquation var (ps, expr)]
equationType (PCon con ps:ps', expr) = ConEquations [ConEquation con ps' (ps, expr)]

concatGroupedEquations :: [GroupedEquations expr con var] -> [GroupedEquations expr con var]
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
  => [var]
  -> CaseExpr expr con var
  -> GroupedEquations expr con var
  -> Match expr var (CaseExpr expr con var)
matchGroup vars def (VarEquations eqs) = matchVar vars eqs def
matchGroup vars def (ConEquations eqs) = matchCon vars eqs def

-- | The variable case is simple. Eat up the first match variable and replace
-- the pattern variable in the equation with the match variable.
matchVar
  :: (Show expr, Ord con)
  => [var]
  -> [VarEquation expr con var]
  -> CaseExpr expr con var
  -> Match expr var (CaseExpr expr con var)
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
  => [var]
  -> [ConEquation expr con var]
  -> CaseExpr expr con var
  -> Match expr var (CaseExpr expr con var)
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

groupByConstructor
  :: (Ord con)
  => [ConEquation expr con var]
  -> [NonEmpty (ConEquation expr con var)]
groupByConstructor = fmap (fmap snd) . NE.groupWith fst . sortOn fst . fmap (\eq@(ConEquation con _ _) -> (con, eq))

matchClause
  :: (Show expr, Ord con)
  => [var]
  -> CaseExpr expr con var
  -> NonEmpty (ConEquation expr con var)
  -> Match expr var (Clause expr con var)
matchClause us def eqs = do
  let (ConEquation con _ _) = NE.head eqs
  us' <- traverse (const freshVar) [1..conArity con]
  let mkNewEquation (ConEquation _ pats' (pats, expr)) = (pats' ++ pats, expr)
  eqs' <- match' (us' ++ us) (NE.toList $ mkNewEquation <$> eqs) def
  pure $ Clause con us' eqs'
