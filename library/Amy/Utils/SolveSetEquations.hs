{-# LANGUAGE TupleSections #-}

-- | Solve set equations based on unions.
--
-- This is algorithm is needed for lambda lifting a la "Lambda Lifting:
-- Transforming Programs into Recursive Equations (Johnsson 1985)"
--
module Amy.Utils.SolveSetEquations
  ( SetEquation(..)
  , solveSetEquations
  ) where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Description of a set equation.
--
-- A @'SetEquation' eq a@ is of the form:
--
-- @
--     eq = Set a ∪ Set eq
-- @
--
-- For example, here are three equations:
--
-- @
--     X = {a, b} ∪ Y ∪ Z
--     Y = {c} ∪ X
--     Z = {d} ∪ Y
-- @
--
data SetEquation eq a
  = SetEquation
  { setEquationName :: !eq
  , setEquationVars :: !(Set a)
  , setEquationOthers :: !(Set eq)
  } deriving (Show, Eq)

-- | Solve a set of @'SetEquation'@s via repeated substitution.
solveSetEquations :: (Eq eq, Ord eq, Ord a) => [SetEquation eq a] -> [(eq, Set a)]
solveSetEquations = solveSetEquations' []

solveSetEquations' :: (Eq eq, Ord eq, Ord a) => [(eq, Set a)] -> [SetEquation eq a] -> [(eq, Set a)]
solveSetEquations' solutions [] = reverse solutions
solveSetEquations' solutions (SetEquation name vars others : eqs) =
  let
    (newVars, newOthers) = unzip $ lookupEquation solutions eqs <$> Set.toList others
    vars' = vars `Set.union` Set.unions newVars
    others' = Set.unions newOthers `Set.difference` (name `Set.insert` others)
  in
    if Set.null others'
    then solveSetEquations' ((name, vars') : solutions) eqs
    else solveSetEquations' solutions (eqs ++ [SetEquation name vars' others'])

lookupEquation :: (Eq eq) => [(eq, Set a)] -> [SetEquation eq a] -> eq -> (Set a, Set eq)
lookupEquation solved unsolved name =
  fromMaybe (error "Internal error: Couldn't find set equation!")
  $ lookupSolvedEquation name solved <|> lookupUnsolvedEquation name unsolved

lookupSolvedEquation :: (Eq eq) => eq -> [(eq, Set a)] -> Maybe (Set a, Set eq)
lookupSolvedEquation name = fmap (, Set.empty) . lookup name

lookupUnsolvedEquation :: (Eq eq) => eq -> [SetEquation eq a] -> Maybe (Set a, Set eq)
lookupUnsolvedEquation name =
  fmap (\(SetEquation _ vars others) -> (vars, others))
  . find ((== name) . setEquationName)
