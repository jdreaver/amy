module Amy.Core.DesugarPatterns
  ( groupMatches
  ) where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)

import Amy.Core.AST as C
import Amy.TypeCheck.AST as T

data GroupedMatches
  = GroupedMatches
  { groupedMatchesLiterals :: ![LitMatchGroup]
  , groupedMatchesConstructors :: ![ConsMatchGroup]
  , groupedMatchesDefault :: !(Maybe DefaultMatch)
  } deriving (Show, Eq)

data LitMatchGroup
  = LitMatchGroup
  { litMatchGroupLiteral :: !Literal
  , litMatchGroupBody :: !T.Expr
  } deriving (Show, Eq)

data ConsMatchGroup
  = ConsMatchGroup
  { consMatchGroupConstructor :: !(T.Typed T.ConstructorName)
  , consMatchGroupArgs :: ![(Maybe T.Pattern, T.Expr)]
  } deriving (Show, Eq)

data DefaultMatch
  = DefaultMatch
  { defaultMatchIdent :: !(T.Typed T.Ident)
  , defaultMatchBody :: !T.Expr
  } deriving (Show, Eq)

-- | Groups 'T.Match'es so we can desugar any nested patterns.
groupMatches :: [T.Match] -> GroupedMatches
groupMatches matches =
  let
    -- Extract the different types of matches we can have
    mDefaultMatch = findDefaultMatch matches
    litMatches = mapMaybe litMatch matches
    consMatches = mapMaybe consMatch matches

    -- Group constructor matches by constructor name
    groupedByCons :: [NonEmpty (T.Typed T.ConstructorName, (Maybe T.Pattern, T.Expr))]
    groupedByCons = NE.group $ sortOn fst consMatches

    -- For each constructor, make a ConsMatchGroup
    mkConsMatchGroup :: NonEmpty (T.Typed T.ConstructorName, (Maybe T.Pattern, T.Expr)) -> ConsMatchGroup
    mkConsMatchGroup matches' =
      let
        consName = fst . NE.head $ matches'
        args = NE.toList $ snd <$> matches'
      in ConsMatchGroup consName args
  in
    GroupedMatches
    { groupedMatchesLiterals = litMatches
    , groupedMatchesConstructors = mkConsMatchGroup <$> groupedByCons
    , groupedMatchesDefault = mDefaultMatch
    }

findDefaultMatch :: [T.Match] -> Maybe DefaultMatch
findDefaultMatch matches =
  let
    defaultMatches = mapMaybe defaultMatch matches
  in
    case defaultMatches of
      [] -> Nothing
      [x] -> Just x
      -- TODO: Make this a warning somehow instead of an error.
      _ -> error $ "Found multiple default matches in " ++ show matches

-- | A default match is a match for a simple variable or a wildcard.
defaultMatch :: T.Match -> Maybe DefaultMatch
defaultMatch (T.Match pat body) =
  case pat of
    T.PVar ident -> Just (DefaultMatch ident body)
    _ -> Nothing

litMatch :: T.Match -> Maybe LitMatchGroup
litMatch (T.Match pat body) =
  case pat of
    T.PLit lit -> Just $ LitMatchGroup lit body
    _ -> Nothing

consMatch :: T.Match -> Maybe (T.Typed T.ConstructorName, (Maybe T.Pattern, T.Expr))
consMatch (T.Match pat body) =
  case pat of
    T.PCons (T.PatCons consName mArg _) -> Just (consName, (mArg, body))
    _ -> Nothing
