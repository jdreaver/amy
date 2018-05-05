module Amy.Core.DesugarPatterns
  ( GroupedMatches(..)
  , groupMatches
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))

import Amy.TypeCheck.AST as T

data GroupedMatches
  = GroupedMatches
  { groupedMatchesLiterals :: ![(Literal, T.Expr)]
    -- TODO: This map doesn't preserve the order of the constructors. Bad!
  , groupedMatchesConstructors :: !(Map T.ConstructorName (NonEmpty (Maybe T.Pattern, T.Expr)))
  , groupedMatchesDefault :: !(Maybe (T.Typed T.Ident, T.Expr))
  } deriving (Show, Eq)

groupedMatches :: GroupedMatches
groupedMatches =
  GroupedMatches
  { groupedMatchesLiterals = []
  , groupedMatchesConstructors = Map.empty
  , groupedMatchesDefault = Nothing
  }

addConstructorMatch
  :: T.ConstructorName
  -> Maybe T.Pattern
  -> T.Expr
  -> GroupedMatches
  -> GroupedMatches
addConstructorMatch consName mArg body grouped =
  grouped
  { groupedMatchesConstructors =
    Map.insertWith
      (flip (<>))
      consName
      ((mArg, body) :| [])
      (groupedMatchesConstructors grouped)
  }

groupMatches :: [T.Match] -> GroupedMatches
groupMatches = flip groupMatches' groupedMatches

groupMatches' :: [T.Match] -> GroupedMatches -> GroupedMatches
groupMatches' [] grouped = grouped
groupMatches' (T.Match pat body : rest) grouped =
  case pat of
    T.PLit lit ->
      groupMatches' rest
      $ grouped { groupedMatchesLiterals = groupedMatchesLiterals grouped ++ [(lit, body)] }
    T.PVar var ->
      -- N.B. Terminate here because we found a catch-all pattern
      -- TODO: Warn if there are multiple catch-all patterns
      grouped { groupedMatchesDefault = Just (var, body) }
    T.PCons (T.PatCons (Typed _ consName) mArg _) ->
      groupMatches' rest
      $ addConstructorMatch consName mArg body grouped
    T.PParens pat' -> groupMatches' (T.Match pat' body : rest) grouped
