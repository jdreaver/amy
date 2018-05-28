{-# LANGUAGE OverloadedStrings #-}

-- | Pattern match compiler, inspired by the great book Implementation of
-- Functional Programming Languages.

module Amy.Core.PatternCompiler
  ( match
  , InputPattern(..)
  , Con(..)
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

import Amy.Core.AST
import Amy.Core.Monad

-- | A 'InputPattern' is either a constructor or a variable.
data InputPattern
  = PCon !Con ![InputPattern]
    -- ^ Constructor patterns are nested. We have the constructor tag and a
    -- list of pattern arguments.
  | PVar !(Typed IdentName)
    -- ^ Variables are catch-all patterns that match anything and binds the
    -- result to the variable.
  deriving (Show, Eq)

-- | A 'Con'structor is used to construct data types and destructure them in
-- patterns.
data Con
  = Con !DataConName ![Type] !Span
    -- ^ "Normal" constructor
  | ConLit !Literal
    -- ^ Special constructor for a literal. Literals have 0 arity and infinite
    -- span.
  deriving (Show, Eq, Ord)

conArgTys :: Con -> [Type]
conArgTys (Con _ argTys _) = argTys
conArgTys (ConLit _) = []

-- | The 'Span' of a constructor is the total number of constructors in the
-- constructor's type. For example, in @data Bool = False | True@, each
-- constructor @False@ and @True@ have span 2 since there are two constructors.
type Span = Int

conSpan :: Con -> Maybe Span
conSpan (Con _ _ span') = Just span'
conSpan (ConLit _) = Nothing

-- | An 'Equation' is a list of patterns and an expression.
type Equation = ([InputPattern], Expr)

-- | A 'CaseExpr' is the output of the match algorithm
data CaseExpr
  = CaseExpr !(Typed IdentName) ![Clause] !(Maybe CaseExpr)
    -- ^ Corresponds to a case in Core
  | Expr !Expr
    -- ^ An input expression
  | Error
    -- ^ Indicates a pattern matching failure error
  deriving (Show, Eq)

-- | A case expression contains a list of 'Clauses' that match against a
-- constructor.
data Clause =
  Clause
    !Con -- Constructor for the clause
    ![Typed IdentName] -- Variables to bind the constructor arguments to
    !CaseExpr -- Expression for the clause
  deriving (Show, Eq)

freshVar :: Desugar IdentName
freshVar = do
  id' <- freshId
  -- TODO: Give these a special name to ensure the name doesn't conflict with
  -- user-defined type variables. Prefix with "$"?
  pure $ IdentName ("_u" <> pack (show id'))

-- | Main function for this algorithm. Takes equations and produces a
-- 'CaseExpr'.
match
  :: [Typed IdentName]
  -> [Equation]
  -> Desugar CaseExpr
match vars eqs = match' vars eqs Error

match'
  :: [Typed IdentName]
  -> [Equation]
  -> CaseExpr
  -> Desugar CaseExpr
match' [] eqs def = pure $ foldr applyFatbar def (Expr . snd <$> eqs)
match' vars eqs def = foldM (matchGroup vars) def $ reverse $ groupEquations eqs

-- | The book uses a special infix operator called FATBAR. This function
-- applies some FATBAR optimizations.
applyFatbar :: CaseExpr -> CaseExpr -> CaseExpr
applyFatbar CaseExpr{} _ = error "Can't guarantee case doesn't fail yet "
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
groupEquations :: [Equation] -> [GroupedEquations]
groupEquations = concatGroupedEquations . fmap equationType

data GroupedEquations
  = VarEquations ![VarEquation]
  | ConEquations ![ConEquation]
  deriving (Show, Eq)

data VarEquation = VarEquation !(Typed IdentName) !Equation
  deriving (Show, Eq)

data ConEquation = ConEquation !Con ![InputPattern] !Equation
  deriving (Show, Eq)

equationType :: Equation -> GroupedEquations
equationType ([], _) = error "Encountered empty equations in equationType"
equationType (PVar var:ps, expr) = VarEquations [VarEquation var (ps, expr)]
equationType (PCon con ps:ps', expr) = ConEquations [ConEquation con ps' (ps, expr)]

concatGroupedEquations :: [GroupedEquations] -> [GroupedEquations]
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
  :: [Typed IdentName]
  -> CaseExpr
  -> GroupedEquations
  -> Desugar CaseExpr
matchGroup vars def (VarEquations eqs) = matchVar vars eqs def
matchGroup vars def (ConEquations eqs) = matchCon vars eqs def

-- | The variable case is simple. Eat up the first match variable and replace
-- the pattern variable in the equation with the match variable.
matchVar
  :: [Typed IdentName]
  -> [VarEquation]
  -> CaseExpr
  -> Desugar CaseExpr
matchVar [] _ _ = error "matchVars called with empty variables"
matchVar (u:us) eqs def = do
  let
    mkNewEquation (VarEquation (Typed _ var) (ps, eq)) = (ps, substExpr eq var (typedValue u))
    eqs' = mkNewEquation <$> eqs
  match' us eqs' def

-- | When matching constructors, we first group all the equations by
-- constructor. Then we make a new 'Clause' for each constructor and embed them
-- in a 'Case' expression.
matchCon
  :: [Typed IdentName]
  -> [ConEquation]
  -> CaseExpr
  -> Desugar CaseExpr
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
  pure $ CaseExpr u eqs' default'

-- | Groups constructors equations by 'Con'.
groupByConstructor
  :: [ConEquation]
  -> [NonEmpty ConEquation]
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
  :: [Typed IdentName]
  -> CaseExpr
  -> NonEmpty ConEquation
  -> Desugar Clause
matchClause us def eqs = do
  let (ConEquation con _ _) = NE.head eqs
  us' <- traverse (\ty -> Typed ty <$> freshVar) $ conArgTys con
  let mkNewEquation (ConEquation _ pats' (pats, expr)) = (pats' ++ pats, expr)
  eqs' <- match' (us' ++ us) (NE.toList $ mkNewEquation <$> eqs) def
  pure $ Clause con us' eqs'
