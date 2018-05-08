{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.PatternCompilerSpec
  ( spec
  ) where

import Test.Hspec

import Amy.Core.PatternCompiler
import Amy.Literal

-- Bool type
trueC, falseC :: Con
trueC = Con "True" 0 2
falseC = Con "False" 0 2

trueP :: Pattern
trueP = PCon trueC []

falseP :: Pattern
falseP = PCon falseC []

-- List type
nilC, consC :: Con
nilC = Con "Nil" 0 2
consC = Con "Cons" 2 2

nilP :: Pattern
nilP = PCon nilC []

consP :: Pattern -> Pattern -> Pattern
consP x y = PCon consC [x, y]

-- Newtype
newtypeC :: Con
newtypeC = Con "MyNewtype" 1 1

newtypeP :: Pattern -> Pattern
newtypeP pat = PCon newtypeC [pat]

-- Literal
intC :: Int -> Con
intC = ConLit . LiteralInt

intP :: Int -> Pattern
intP i = PCon (intC i) []

-- mappairs example from book
mappairsEquations :: [Equation Int]
mappairsEquations =
  [ ([PVar "f", nilP, PVar "ys"], 1)
  , ([PVar "f", consP (PVar "x") (PVar "xs"), nilP], 2)
  , ([PVar "f", consP (PVar "x") (PVar "xs"), consP (PVar "y") (PVar "ys")], 3)
  ]

mappairsExpect :: CaseExpr Int
mappairsExpect =
  Case "x2"
  [ Clause consC ["_u1", "_u2"]
     (Case "_u1"
        [ Clause consC ["_u3", "_u4"] (Expr 3)
        , Clause nilC [] (Expr 2)
        ]
        Nothing
     )
  , Clause nilC [] (Expr 1)
  ]
  Nothing

-- Example from Sestoft paper
varC, lamC, appC, letC :: Con
varC = Con "Var" 1 4
lamC = Con "Lam" 2 4
appC = Con "App" 2 4
letC = Con "Let" 3 4

lamEquations :: [Equation Int]
lamEquations =
  [ ([PCon varC [PVar "x"]], 111)
  , ([PCon lamC [PVar "x", PCon varC [PVar "y"]]], 222)
  , ([PCon lamC [PVar "x", PCon lamC [PVar "y", PVar "z"]]], 333)
  , ([PCon lamC [PVar "x", PCon appC [PVar "y", PVar "z"]]], 444)
  , ([PCon appC [PCon lamC [PVar "x", PVar "y"], PVar "z"]], 555)
  --, (PCon appC [PCon appC [PCon lamC [PVar "x", PCon lamC [PVar "y", PVar "z"]], PVar "v"], PVar "w"], 0)
  , ([PCon appC [PCon appC [PVar "x", PVar "y"], PVar "z"]], 666)
  , ([PCon letC [PVar "x", PCon letC [PVar "y", PVar "z", PVar "v"], PVar "w"]], 777)
  , ([PCon lamC [PVar "x", PCon letC [PVar "y", PVar "z", PVar "v"]]], 888)
  , ([PCon letC [PVar "x", PVar "y", PCon appC [PVar "z", PVar "v"]]], 999)
  , ([PCon appC [PCon appC [PCon lamC [PVar "x", PCon lamC [PVar "y", PVar "z"]], PVar "v"], PVar "w"]], 1010)
  ]

-- case lam of
--   Var x -> 111
--   Lam x (Var y) -> 222
--   Lam x (Lam y z) -> 333
--   Lam x (App y z) -> 444
--   App (Lam x y) z -> 555
--   App (App x y) z -> 666
--   Let x (Let y z v) w -> 777
--   Lam x (Let y z v) -> 888
--   Let x y (App z v) -> 999
--   App (App (Lam x (Lam y z) v) w) -> 1010

-- Should desugar to:
-- case lam of
--   Var x -> 111
--   Lam v1 v2 ->
--      case v2 of
--        Var y -> 222
--        Lam y z -> 333
--        App y z -> 444
--        Let y z f -> 888
--   App v3 v4 ->
--     case v3 of
--       Lam x y -> 555
--       App x y -> 666
--       _ -> fail
--   Let v5 v6 v7 ->
--     case v6 of
--       Let y z v -> 777
--       _ ->
--         case v7 of
--           App z v -> 999
--           _ -> fail

lamExpected :: CaseExpr Int
lamExpected =
  Case "x"
  [Clause appC ["_u1", "_u2"]
     (Case "_u1"
        [Clause appC ["_u3", "_u4"] (Expr 666),
         Clause lamC ["_u9", "_u10"] (Expr 555)]
        (Just Error)),
   Clause lamC ["_u11", "_u12"]
     (Case "_u12"
        [Clause appC ["_u13", "_u14"] (Expr 444),
         Clause lamC ["_u15", "_u16"] (Expr 333),
         Clause letC ["_u17", "_u18", "_u19"] (Expr 888),
         Clause varC ["_u20"] (Expr 222)]
        Nothing),
   Clause letC ["_u21", "_u22", "_u23"]
     (Case "_u22"
        [Clause letC ["_u26", "_u27", "_u28"] (Expr 777)]
        (Just
           (Case "_u23" [Clause appC ["_u24", "_u25"] (Expr 999)]
              (Just Error)))),
   Clause varC ["_u29"] (Expr 111)]
  Nothing

spec :: Spec
spec = do

  describe "compileMatch" $ do

    it "handles a simple variable case" $ do
      match ["x"] [([PVar "x"], 'a')] `shouldBe` Expr 'a'

    it "handles a single constructor case with a variable" $ do
      match ["x"] [([newtypeP (PVar "y")], 'a')]
        `shouldBe`
        Case "x" [Clause newtypeC ["_u1"] (Expr 'a')] Nothing

    it "handles a single constructor case with a literal and variable" $ do
      let
        equations =
          [ ([newtypeP (intP 1)], 'a')
          , ([newtypeP (PVar "y")], 'b')
          ]
        expected =
          Case "x"
          [ Clause (Con "MyNewtype" 1 1) ["_u1"]
            ( Case "_u1"
              [ Clause (ConLit (LiteralInt 1)) [] (Expr 'a')
              ]
              (Just (Expr 'b'))
            )
          ]
          Nothing
      match ["x"] equations `shouldBe` expected

    it "handles a simple true/false case" $ do
      let
        equations =
          [ ([trueP], 'a')
          , ([falseP], 'b')
          ]
        expected =
          Case "x"
          [ Clause falseC [] (Expr 'b')
          , Clause trueC [] (Expr 'a')
          ]
          Nothing
      match ["x"] equations `shouldBe` expected

    it "handles a pair of bools case" $ do
      let
        equations =
          [ ([trueP, trueP], '1')
          , ([falseP, falseP], '2')
          , ([trueP, falseP] , '3')
          , ([falseP, trueP], '4')
          ]
        expected =
          Case "x"
          [ Clause falseC []
            ( Case "y"
              [ Clause falseC [] (Expr '2')
              , Clause trueC [] (Expr '4')
              ]
              Nothing
            )
          , Clause trueC []
            ( Case "y"
              [ Clause falseC [] (Expr '3')
              , Clause trueC [] (Expr '1')
              ]
              Nothing
            )
          ]
          Nothing
      match ["x", "y"] equations `shouldBe` expected

    it "handles the mappairs example" $ do
      match ["x1", "x2", "x3"] mappairsEquations `shouldBe` mappairsExpect

    it "handles the example from the Setsoft paper" $ do
      match ["x"] lamEquations `shouldBe` lamExpected
