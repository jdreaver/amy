{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Implementation of "ML pattern match compilation and partial evaluation" by
-- Peter Sestoft

module Amy.Core.MatchCompiler
  ( compile
  ) where

import Data.Text (Text)

data Con
  = Con
  { conName :: !Text
  , conArity :: !Int
  , conSpan :: !Int
  } deriving (Show, Eq)

data Pat
  = PVar !Text
  | PCon !Con ![Pat]
  deriving (Show, Eq)

nullC :: Con
nullC = Con { conName = "Null", conArity = 0, conSpan = 3 }

leafC :: Con
leafC = Con { conName = "Leaf", conArity = 1, conSpan = 3 }

nodeC :: Con
nodeC = Con { conName = "Node", conArity = 3, conSpan = 3 }

true :: Con
true = Con { conName = "True", conArity = 0, conSpan = 2}

false :: Con
false = Con { conName = "False", conArity = 0, conSpan = 2}

trueP :: Pat
trueP = PCon true []

falseP :: Pat
falseP = PCon false []

tupP :: [Pat] -> Pat
tupP args = PCon Con { conName = "", conArity = length args, conSpan = 1 } args

-- data Color = Red | Blue | Green

redC, blueC, greenC :: Con
redC = Con "Red" 0 3
blueC = Con "Blue" 0 3
greenC = Con "Green" 0 3

redP, blueP, greenP :: Pat
redP = PCon redC []
blueP = PCon blueC []
greenP = PCon greenC []

type Match a = [(Pat, a)]

mainNaive :: Pat -> Match a -> Maybe a
mainNaive origobj allmrules =
  let
    fail' :: Match a -> Maybe a
    fail' [] = Nothing
    fail' ((pat1, rhs1) : rulesrest) = match pat1 origobj [] rhs1 rulesrest

    succeed :: [([Pat], [Pat])] -> a -> Match a -> Maybe a
    succeed [] rhs _ = Just rhs
    succeed (work1:workr) rhs rules =
      case work1 of
        ([], []) -> succeed workr rhs rules
        (pat1:patr, obj1:objr) -> match pat1 obj1 ((patr,objr):workr) rhs rules
        (x, y) -> error $ "succeed found args of different length " ++ show (x, y)

    match :: Pat -> Pat -> [([Pat], [Pat])] -> a -> Match a -> Maybe a
    match (PVar _) _ work rhs rules = succeed work rhs rules
    match (PCon pcon pargs) (PCon ocon oargs) work rhs rules =
      if ocon == pcon
        then succeed ((pargs, oargs):work) rhs rules
        else fail' rules
    match _ (PVar _) _ _ _ = error "match found a PVar as an object"

  in fail' allmrules

--
--
--

data TermD
  = Pos Con [TermD]
  | Neg [Con]
  deriving (Show, Eq)

addneg (Neg nonset) con = Neg (con:nonset)

augment :: [(Con, [TermD])] -> TermD -> [(Con, [TermD])]
augment [] dsc = []
augment ((con, args):rest) dsc = (con, dsc:args):rest

norm :: [(Con, [TermD])] -> [(Con, [TermD])]
norm ((con, args):rest) = augment rest (Pos con (reverse args))

builddsc :: [(Con, [TermD])] -> TermD -> [(a, b, [TermD])] -> TermD
builddsc [] dsc [] = dsc
builddsc ((con, args):rest) dsc ((_, _, dargs):work) =
  builddsc rest (Pos con (reverse args ++ (dsc : dargs))) work

data Access
  = Obj
  | Sel !Int !Access
  deriving (Show, Eq)

data Decision a
  = Failure
  | Success a
  | IfEq Access Con (Decision a) (Decision a)
  deriving (Show, Eq)

compile :: forall a. Match a -> Decision a
compile allmrules =
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
  | pcon `notElem` negcons && conSpan pcon == (length negcons + 1) = Yes
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

--
-- Lam
--

varC, lamC, appC, letC :: Con
varC = Con "Var" 1 4
lamC = Con "Lam" 2 4
appC = Con "App" 2 4
letC = Con "Let" 3 4

lamMatch :: Match Int
lamMatch =
  [ (PCon varC [PVar "x"], 111)
  , (PCon lamC [PVar "x", PCon varC [PVar "y"]], 222)
  , (PCon lamC [PVar "x", PCon lamC [PVar "y", PVar "z"]], 333)
  , (PCon lamC [PVar "x", PCon appC [PVar "y", PVar "z"]], 444)
  , (PCon appC [PCon lamC [PVar "x", PVar "y"], PVar "z"], 555)
  --, (PCon appC [PCon appC [PCon lamC [PVar "x", PCon lamC [PVar "y", PVar "z"]], PVar "v"], PVar "w"], 0)
  , (PCon appC [PCon appC [PVar "x", PVar "y"], PVar "z"], 666)
  , (PCon letC [PVar "x", PCon letC [PVar "y", PVar "z", PVar "v"], PVar "w"], 777)
  , (PCon lamC [PVar "x", PCon letC [PVar "y", PVar "z", PVar "v"]], 888)
  , (PCon letC [PVar "x", PVar "y", PCon appC [PVar "z", PVar "v"]], 999)
  , (PCon appC [PCon appC [PCon lamC [PVar "x", PCon lamC [PVar "y", PVar "z"]], PVar "v"], PVar "w"], 1010)
  ]
