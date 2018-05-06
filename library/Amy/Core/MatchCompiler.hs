{-# LANGUAGE OverloadedStrings #-}

-- | Implementation of "ML pattern match compilation and partial evaluation" by
-- Peter Sestoft

module Amy.Core.MatchCompiler
  ( Con(..)
  , Arity
  , Span
  , Pat(..)
  , Match
  , Decision(..)
  , Access(..)
  , compileMatch

    -- Exported for testing
  , compileMatch'
  , PrelimDecision(..)
  ) where

import Data.Text (Text)

import Amy.Literal

--
-- Constructor
--

-- | A 'Con' represents a generic pattern constructor.
data Con
  = Con !Text !Span !Arity
    -- ^ A non-literal constructor with a name, span, and arity.
  | ConLit !Literal
    -- ^ A constructor made from a literal. This case is separate because
    -- int/double literals have 0 arity and "infinite" span (represented by
    -- 'Nothing').
  deriving (Show, Eq)

-- | 'Arity' is the number of arguments a constructor takes. A constructor with
-- no arguments has arity 0, and a constructor with N arguments has arity N.
type Arity = Int

-- | The 'Span' of a constructor is the total number of constructors in the
-- constructor's type. For example, in @data Bool = False | True@, each
-- constructor @False@ and @True@ have span 2 since there are two constructors.
type Span = Int

-- | Returns a 'Con's arity or 0 for a 'ConLit'
conArity :: Con -> Arity
conArity (Con _ arity _) = arity
conArity (ConLit _) = 0

-- | The span of a 'Con'. Returns 'Just' for a constructor with finite span and
-- 'Nothing' for a constructor with infinite span.
conSpan :: Con -> Maybe Span
conSpan (Con _ _ span') = Just span'
conSpan (ConLit _) = Nothing

--
-- Patterns and Matches
--

-- | A 'Pat'tern is either a variable or a constructor applied to more
-- patterns.
data Pat
  = PVar !Text
  | PCon !Con ![Pat]
  deriving (Show, Eq)

type MatchRule a = (Pat, a)
type Match a = [MatchRule a]

--
-- Term Description
--

-- | A 'TermDesc'ription describes information we know about a given term.
data TermDesc
  = Pos Con [TermDesc]
    -- | A 'Pos'itive description describes a term we know has a constructor
    -- 'Con', and with a 'TermDescr' for each argument.
  | Neg [Con]
    -- | A 'Neg'ative description describes a term who's constructor is none of
    -- the given constructors.
  deriving (Show, Eq)

--
-- Main Algorithm
--

-- | A 'Decision' is a tree-like structure describing how to match an object.
data Decision a
  = Failure
    -- ^ Leaf node when matching has failed
  | Success a
    -- ^ Leaf node when matching has succeeded
  | Switch Access [(Con, Decision a)] (Decision a)
  deriving (Show, Eq)

-- | A 'PrelimDecision' is like 'Decision' except it is made of simple 'IfEq'
-- branches. It gets compiled to a 'Decision' by turning consecutive
data PrelimDecision a
  = Failure'
    -- ^ Leaf node when matching has failed
  | Success' a
    -- ^ Leaf node when matching has succeeded
  | IfEq Access Con (PrelimDecision a) (PrelimDecision a)
    -- ^ Tests if the given subterm 'Access' of the object is equal to 'Con'.
    -- If it does, then follow the first 'PrelimDecision', and otherwise follow
    -- the second.
  deriving (Show, Eq)

-- | Main pattern compilation algorithm. This algorithm takes a list of match
-- rules and produces a tree-like 'Decision' structure describing statically
-- how to match a term object against the match rules.
--
-- The mutually recursive functions 'compileFail', 'compileSucceed', and
-- 'match' are the meat of the algorithm.
compileMatch :: Match a -> Decision a
compileMatch = switchify . compileMatch'

compileMatch' :: Match a -> PrelimDecision a
compileMatch' = compileFail (Neg [])

-- | A 'MatchState' holds the context and workd stack for the current sub-term.
data MatchState
  = MatchState
  { matchStateContext :: (Con, [TermDesc])
    -- ^ The Context of successive 'MatchState's is a path of constructors from
    -- the current sub-term up to the root, along with term descriptions of the
    -- arguments to those constructors.
    --
    -- The argument term descriptions are stored in reverse order (like a
    -- stack), and the constructor can be partially applied. When the
    -- constructor is fully applied, we convert it to a 'Pos'.
  , matchStateWorkStack :: [(Pat, Access, TermDesc)] -- Make this a data type?
    -- ^ The work stack is the stack of hypotheses still left to be checked.
  } deriving (Show, Eq)

-- TODO: Document this
compileFail :: TermDesc -> Match a -> PrelimDecision a
compileFail _ [] = Failure'
compileFail dsc ((pat1, rhs1):rulesrest) = match pat1 Obj dsc [] rhs1 rulesrest

-- TODO: Document this
compileSucceed :: [MatchState] -> a -> Match a -> PrelimDecision a
compileSucceed [] rhs _ = Success' rhs
compileSucceed (MatchState ctx work : staterest) rhs rules =
  case work of
    [] -> compileSucceed (normContext ctx staterest) rhs rules
    ((pat, obj, dsc):workr) ->
      match pat obj dsc (MatchState ctx workr : staterest) rhs rules

-- TODO: Document this
match :: Pat -> Access -> TermDesc -> [MatchState] -> a -> Match a -> PrelimDecision a
match (PVar _) _ dsc state rhs rules = compileSucceed (augmentContext state dsc) rhs rules
match (PCon pcon pargs) obj dsc state rhs rules =
  let
    mapArity f = f <$> [0..(conArity pcon - 1)]

    getdargs =
      case dsc of
        (Neg _) -> mapArity (const $ Neg [])
        (Pos _ dargs) -> dargs

    getoargs = mapArity (\i -> Sel (i+1) obj)

    newState = MatchState (pcon, []) (zip3Error pargs getoargs getdargs)
    succeed' = compileSucceed (newState : state) rhs rules

    compileFail' newdsc = compileFail (builddsc state newdsc) rules
  in
    case staticmatch pcon dsc of
      Yes -> succeed'
      No -> compileFail' dsc
      Maybe' negcons -> IfEq obj pcon succeed' $ compileFail' $ Neg negcons

-- | When matching a term succeeds, we augment the context by filling in the
-- hole of the last argument of the local-most constructor with the term
-- description.
augmentContext :: [MatchState] -> TermDesc -> [MatchState]
augmentContext [] _ = []
augmentContext (MatchState (con, args) work : rest) dsc = MatchState (con, dsc:args) work : rest

-- | When all arguments for a constructor have been found, a positive term
-- description is constructed and added to the context.
normContext :: (Con, [TermDesc]) -> [MatchState] -> [MatchState]
normContext _ [] = []
normContext (con, args) state = augmentContext state (Pos con (reverse args))

-- TODO: Document and understand this better. I just know it is used when
-- matching a sub-term fails.
builddsc :: [MatchState] -> TermDesc -> TermDesc
builddsc [] dsc = dsc
builddsc (MatchState (con, args) work : rest) dsc =
  let
    dargs = (\(_, _, z) -> z) <$> work
    conArgs = reverse args ++ (dsc : dargs)
    newDesc =
      if length conArgs /= conArity con
      then error ("Assertion failed in builddsc. Arity/argument mismatch " ++ show (con, conArgs))
      else Pos con conArgs
  in builddsc rest newDesc

-- | An 'Access' path is either the full object or a path to select sub-terms
-- of the object.
data Access
  = Obj
    -- ^ Full object
  | Sel !Int !Access
    -- ^ Select the i-th subterm of the given 'Access' path.
  deriving (Show, Eq)

data StaticMatchResult
  = Yes
  | No
  | Maybe' [Con]
    -- ^ The '[Con]' is the new list of negative constructors.
  deriving (Show, Eq)

-- | Attempt to match a constructor against the term description of a pattern.
staticmatch :: Con -> TermDesc -> StaticMatchResult
staticmatch pcon (Pos con _) =
  if con == pcon
    -- We positively know the object constructor is @con@ and @pcon@ matches
    then Yes
    -- We positively know the object constructor is @con@ and @pcon@ doesn't
    -- match
    else No
staticmatch pcon (Neg negcons)
  -- We know that the object constructor is not one of @negcons@, but the given
  -- constructor is one of those constructors.
  | pcon `elem` negcons = No
  -- We know that the given constructor is not one of @negcons@ (from the
  -- previous guard) and there are no possibilities left because of the span.
  -- Note this never applies to constructors with infinite span like number
  -- literals.
  | conSpan pcon == Just (length negcons + 1) = Yes
  -- We know
  | otherwise = Maybe' (pcon:negcons)

-- | Convert a 'PrelimDecision' into a 'Decision' by converting all consecutive
-- 'IfEq' nodes into 'Switch' nodes.
switchify :: PrelimDecision a -> Decision a
switchify Failure' = Failure
switchify (Success' x) = Success x
switchify (IfEq acc con successDec failDec) =
  let
    (cases, defaultCase) = collectSwitch acc failDec [(con, switchify successDec)]
  in Switch acc cases defaultCase

-- | Collect consecutive 'IfEq' nodes into a list of 'Switch' decisions as long
-- as they are acting on the same 'Access' path.
collectSwitch :: Access -> PrelimDecision a -> [(Con, Decision a)] -> ([(Con, Decision a)], Decision a)
collectSwitch acc failDec otherDecs =
  case failDec of
    IfEq acc' con' successDec' failDec' ->
      if acc == acc'
        then collectSwitch acc failDec' (otherDecs ++ [(con', switchify successDec')])
        else (otherDecs, switchify failDec)
    _ -> (otherDecs, switchify failDec)

--
-- Utils
--

zip3Error :: (Show a, Show b, Show c) => [a] -> [b] -> [c] -> [(a, b, c)]
zip3Error [] [] [] = []
zip3Error (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3Error xs ys zs
zip3Error xs ys zs =
  error $ "Assertion failed in zip3Error. Lists have different length. Leftovers: " ++ show (xs, ys, zs)
