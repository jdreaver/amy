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

data Pattern con var
  = PCon !(Con con) ![Pattern con var]
  | PVar !var
  deriving (Show, Eq)

data Con con
  = Con !con !Arity !Span
  | ConLit !Literal
  deriving (Show, Eq, Ord)

conArity :: Con con -> Arity
conArity (Con _ arity _) = arity
conArity (ConLit _) = 0

conSpan :: Con con -> Maybe Span
conSpan (Con _ _ span') = Just span'
conSpan (ConLit _) = Nothing

type Arity = Int
type Span = Int

type Equation expr con var = ([Pattern con var], expr)

data CaseExpr expr con var
  = Case !var ![Clause expr con var] !(Maybe (CaseExpr expr con var))
  | Expr !expr
  | Fail
  | Error
  deriving (Show, Eq)

data Clause expr con var = Clause !(Con con) ![var] !(CaseExpr expr con var)
  deriving (Show, Eq)

newtype Match expr var a = Match (ReaderT (MatchData expr var) (State Int) a)
  deriving (Functor, Applicative, Monad, MonadReader (MatchData expr var), MonadState Int)

data MatchData expr var = MatchData (VarSubst expr var) (MakeVar var)

newtype VarSubst expr var = VarSubst (expr -> var -> var -> expr)
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
match' vars eqs def = foldrM (matchGroup vars) def $ groupEquations eqs

-- TODO: Refactor the algorithm so we can use foldM instead of foldrM
foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ d [] = return d
foldrM f d (x:xs) = f x =<< foldrM f d xs

applyFatbar
  :: CaseExpr expr con var
  -> CaseExpr expr con var
  -> CaseExpr expr con var
applyFatbar e Fail = e
applyFatbar Fail e = e
applyFatbar Case{} _ = error "Can't guarantee case doesn't fail yet "
applyFatbar e@(Expr _) _ = e
applyFatbar Error _ = Error

--
-- Equation grouping
--

data GroupedEquations expr con var
  = EmptyEquations ![expr]
  | VarEquations ![VarEquation expr con var]
  | ConEquations ![ConEquation expr con var]
  deriving (Show, Eq)

data VarEquation expr con var = VarEquation !var !(Equation expr con var)
  deriving (Show, Eq)

data ConEquation expr con var = ConEquation !(Con con) ![Pattern con var] !(Equation expr con var)
  deriving (Show, Eq)

groupEquations :: [Equation expr con var] -> [GroupedEquations expr con var]
groupEquations = concatGroupedEquations . fmap equationType

equationType :: Equation expr con var -> GroupedEquations expr con var
equationType ([], expr) = EmptyEquations [expr]
equationType (PVar var:ps, expr) = VarEquations [VarEquation var (ps, expr)]
equationType (PCon con ps:ps', expr) = ConEquations [ConEquation con ps' (ps, expr)]

concatGroupedEquations :: [GroupedEquations expr con var] -> [GroupedEquations expr con var]
concatGroupedEquations [] = []
concatGroupedEquations [x] = [x]
concatGroupedEquations (x:y:xs) =
  case (x, y) of
    (EmptyEquations es1, EmptyEquations es2) -> concatGroupedEquations $ EmptyEquations (es1 ++ es2) : xs
    (VarEquations es1, VarEquations es2) -> concatGroupedEquations $ VarEquations (es1 ++ es2) : xs
    (ConEquations es1, ConEquations es2) -> concatGroupedEquations $ ConEquations (es1 ++ es2) : xs
    (_, _) -> x : concatGroupedEquations (y:xs)

--
-- Matching
--

matchGroup
  :: (Show expr, Ord con)
  => [var]
  -> GroupedEquations expr con var
  -> CaseExpr expr con var
  -> Match expr var (CaseExpr expr con var)
matchGroup _ (EmptyEquations _) = error "Encountered empty equations in matchGroup"
matchGroup vars (VarEquations eqs) = matchVar vars eqs
matchGroup vars (ConEquations eqs) = matchCon vars eqs

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
