{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Implementation of "ML pattern match compilation and partial evaluation" by
-- Peter Sestoft

module Amy.Core.MatchCompiler
  ( Con(..)
  , Arity
  , Span
  , Pat(..)
  , Match
  , Decision'(..)
  , Access(..)
  , compileMatch
  , switchify
  ) where

import Data.Text (Text)

import Amy.Literal

type Arity = Int
type Span = Int

data Con
  = Con !Text !Span !Arity
  | ConLit !Literal
  deriving (Show, Eq)

conArity :: Con -> Arity
conArity (Con _ arity _) = arity
conArity (ConLit _) = 0

conSpan :: Con -> Maybe Span
conSpan (Con _ _ span') = Just span'
conSpan (ConLit _) = Nothing

data Pat
  = PVar !Text
  | PCon !Con ![Pat]
  deriving (Show, Eq)

type Match a = [(Pat, a)]

data TermD
  = Pos Con [TermD]
  | Neg [Con]
  deriving (Show, Eq)

addneg :: TermD -> Con -> TermD
addneg (Neg nonset) con = Neg (con:nonset)
addneg p@(Pos _ _) _ = error $ "Encountered Pos in addneg " ++ show p

augment :: [(Con, [TermD])] -> TermD -> [(Con, [TermD])]
augment [] _ = []
augment ((con, args):rest) dsc = (con, dsc:args):rest

norm :: [(Con, [TermD])] -> [(Con, [TermD])]
norm [] = error "Unexpected empty list in norm"
norm ((con, args):rest) = augment rest (Pos con (reverse args))

builddsc :: [(Con, [TermD])] -> TermD -> [([Pat], [Access], [TermD])] -> TermD
builddsc [] dsc [] = dsc
builddsc ((con, args):rest) dsc ((_, _, dargs):work) =
  builddsc rest (Pos con (reverse args ++ (dsc : dargs))) work
builddsc a b c = error $ "Encountered problem in builddsc " ++ show (a, b, c)

data Access
  = Obj
  | Sel !Int !Access
  deriving (Show, Eq)

data Decision a
  = Failure
  | Success a
  | IfEq Access Con (Decision a) (Decision a)
  deriving (Show, Eq)

compileMatch :: forall a. Match a -> Decision a
compileMatch allmrules =
  let
    fail' :: TermD -> Match a -> Decision a
    fail' _ [] = Failure
    fail' dsc ((pat1, rhs1):rulesrest) = match pat1 Obj dsc [] [] rhs1 rulesrest

    succeed :: [(Con, [TermD])] -> [([Pat], [Access], [TermD])] -> a -> [(Pat, a)] -> Decision a
    succeed _ [] rhs _ = Success rhs
    succeed ctx (work1:workr) rhs rules =
      case work1 of
        ([], [], []) -> succeed (norm ctx) workr rhs rules
        (pat1:patr, obj1:objr, dsc1:dscr) ->
          match pat1 obj1 dsc1 ctx ((patr, objr, dscr):workr) rhs rules
        (x, y, z) -> error $ "succeed found work args of different length " ++ show (x, y, z)

    match :: Pat -> Access -> TermD -> [(Con, [TermD])] -> [([Pat], [Access], [TermD])] -> a -> [(Pat, a)] -> Decision a
    match (PVar _) _ dsc ctx work rhs rules = succeed (augment ctx dsc) work rhs rules
    match (PCon pcon pargs) obj dsc ctx work rhs rules =
      let
        args f = tabulate (conArity pcon) f

        getdargs (Neg _) = args (const $ Neg [])
        getdargs (Pos _ dargs) = dargs

        getoargs = args (\i -> Sel (i+1) obj)

        succeed' = succeed ((pcon, []):ctx) ((pargs, getoargs, getdargs dsc):work) rhs rules

        fail'' newdsc = fail' (builddsc ctx newdsc work) rules
      in
        case staticmatch pcon dsc of
          Yes -> succeed'
          No -> fail'' dsc
          Maybe' -> IfEq obj pcon succeed' (fail'' (addneg dsc pcon))

  in fail' (Neg []) allmrules

tabulate :: Int -> (Int -> a) -> [a]
tabulate i f = f <$> [0..(i-1)]

data StaticMatchResult
  = Yes
  | No
  | Maybe'
  deriving (Show, Eq)

staticmatch :: Con -> TermD -> StaticMatchResult
staticmatch pcon (Pos con _) =
  if con == pcon then Yes else No
staticmatch pcon (Neg negcons)
  | pcon `elem` negcons = No
  | pcon `notElem` negcons && conSpan pcon == Just (length negcons + 1) = Yes
  | otherwise = Maybe'

data Decision' a
  = Failure'
  | Success' a
  | Switch Access [(Con, Decision' a)] (Decision' a)
  deriving (Show, Eq)

switchify :: Decision a -> Decision' a
switchify Failure = Failure'
switchify (Success x) = Success' x
switchify (IfEq acc con successDec failDec) =
  let
    (cases, defaultCase) = collectSwitch acc failDec [(con, switchify successDec)]
  in Switch acc cases defaultCase

collectSwitch :: Access -> Decision a -> [(Con, Decision' a)] -> ([(Con, Decision' a)], Decision' a)
collectSwitch acc failDec otherDecs =
  case failDec of
    IfEq acc' con' successDec' failDec' ->
      if acc == acc'
        then collectSwitch acc failDec' (otherDecs ++ [(con', switchify successDec')])
        else (otherDecs, switchify failDec)
    _ -> (otherDecs, switchify failDec)
