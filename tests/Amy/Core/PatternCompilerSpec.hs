{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.PatternCompilerSpec
  ( spec
  ) where

import Test.Hspec

import Amy.Core.PatternCompiler

-- Bool type
trueC, falseC :: Con
trueC = Con "True" 0
falseC = Con "False" 0

trueCI, falseCI :: ConInfo
trueCI = ConInfo trueC [falseC, trueC]
falseCI = ConInfo falseC [falseC, trueC]

trueP :: Pattern
trueP = PCon trueCI []

falseP :: Pattern
falseP = PCon falseCI []

-- List type
nilC, consC :: Con
nilC = Con "Nil" 0
consC = Con "Cons" 2

nilCI, consCI :: ConInfo
nilCI = ConInfo nilC [nilC, consC]
consCI = ConInfo consC [nilC, consC]

nilP :: Pattern
nilP = PCon nilCI []

consP :: Pattern -> Pattern -> Pattern
consP x y = PCon consCI [x, y]

-- Newtype
newtypeC :: Con
newtypeC = Con "MyNewtype" 1

newtypeCI :: ConInfo
newtypeCI = ConInfo newtypeC [newtypeC]

newtypeP :: Pattern -> Pattern
newtypeP pat = PCon newtypeCI [pat]

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
        ])
  , Clause nilC [] (Expr 1)
  ]

-- Example from Sestoft paper
varC, lamC, appC, letC :: Con
varC = Con "Var" 1
lamC = Con "Lam" 2
appC = Con "App" 2
letC = Con "Let" 3

varCI, lamCI, appCI, letCI :: ConInfo
varCI = ConInfo varC [varC, lamC, appC, letC]
lamCI = ConInfo lamC [varC, lamC, appC, letC]
appCI = ConInfo appC [varC, lamC, appC, letC]
letCI = ConInfo letC [varC, lamC, appC, letC]

lamEquations :: [Equation Int]
lamEquations =
  [ ([PCon varCI [PVar "x"]], 111)
  , ([PCon lamCI [PVar "x", PCon varCI [PVar "y"]]], 222)
  , ([PCon lamCI [PVar "x", PCon lamCI [PVar "y", PVar "z"]]], 333)
  , ([PCon lamCI [PVar "x", PCon appCI [PVar "y", PVar "z"]]], 444)
  , ([PCon appCI [PCon lamCI [PVar "x", PVar "y"], PVar "z"]], 555)
  --, (PCon appC [PCon appC [PCon lamC [PVar "x", PCon lamC [PVar "y", PVar "z"]], PVar "v"], PVar "w"], 0)
  , ([PCon appCI [PCon appCI [PVar "x", PVar "y"], PVar "z"]], 666)
  , ([PCon letCI [PVar "x", PCon letCI [PVar "y", PVar "z", PVar "v"], PVar "w"]], 777)
  , ([PCon lamCI [PVar "x", PCon letCI [PVar "y", PVar "z", PVar "v"]]], 888)
  , ([PCon letCI [PVar "x", PVar "y", PCon appCI [PVar "z", PVar "v"]]], 999)
  , ([PCon appCI [PCon appCI [PCon lamCI [PVar "x", PCon lamCI [PVar "y", PVar "z"]], PVar "v"], PVar "w"]], 1010)
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
  [Clause (Con "App" 2) ["_u1", "_u2"]
     (Case "_u1"
        [Clause (Con "App" 2) ["_u3", "_u4"]
           (Case "_u4"
              [Clause (Con "App" 2) ["_u5", "_u6"] (Expr 666),
               Clause (Con "Lam" 2) ["_u7", "_u8"]
                 (Case "_u2"
                    [Clause (Con "App" 2) ["_u9", "_u10"] (Expr 666),
                     Clause (Con "Lam" 2) ["_u11", "_u12"] (Expr 1010),
                     Clause (Con "Let" 3) ["_u13", "_u14", "_u15"] (Expr 666),
                     Clause (Con "Var" 1) ["_u16"] (Expr 666)]),
               Clause (Con "Let" 3) ["_u17", "_u18", "_u19"] (Expr 666),
               Clause (Con "Var" 1) ["_u20"] (Expr 666)]),
         Clause (Con "Lam" 2) ["_u21", "_u22"] (Expr 555),
         Clause (Con "Let" 3) ["_u23", "_u24", "_u25"] Error,
         Clause (Con "Var" 1) ["_u26"] Error]),
   Clause (Con "Lam" 2) ["_u27", "_u28"]
     (Case "_u28"
        [Clause (Con "App" 2) ["_u29", "_u30"] (Expr 444),
         Clause (Con "Lam" 2) ["_u31", "_u32"] (Expr 333),
         Clause (Con "Let" 3) ["_u33", "_u34", "_u35"] (Expr 888),
         Clause (Con "Var" 1) ["_u36"] (Expr 222)]),
   Clause (Con "Let" 3) ["_u37", "_u38", "_u39"]
     (Case "_u39"
        [Clause (Con "App" 2) ["_u48", "_u49"] (Expr 999),
         Clause (Con "Lam" 2) ["_u50", "_u51"]
           (Case "_u38"
              [Clause (Con "App" 2) ["_u40", "_u41"] Error,
               Clause (Con "Lam" 2) ["_u42", "_u43"] Error,
               Clause (Con "Let" 3) ["_u44", "_u45", "_u46"] (Expr 777),
               Clause (Con "Var" 1) ["_u47"] Error]),
         Clause (Con "Let" 3) ["_u52", "_u53", "_u54"]
           (Case "_u38"
              [Clause (Con "App" 2) ["_u40", "_u41"] Error,
               Clause (Con "Lam" 2) ["_u42", "_u43"] Error,
               Clause (Con "Let" 3) ["_u44", "_u45", "_u46"] (Expr 777),
               Clause (Con "Var" 1) ["_u47"] Error]),
         Clause (Con "Var" 1) ["_u55"]
           (Case "_u38"
              [Clause (Con "App" 2) ["_u40", "_u41"] Error,
               Clause (Con "Lam" 2) ["_u42", "_u43"] Error,
               Clause (Con "Let" 3) ["_u44", "_u45", "_u46"] (Expr 777),
               Clause (Con "Var" 1) ["_u47"] Error])]),
   Clause (Con "Var" 1) ["_u56"] (Expr 111)]

spec :: Spec
spec = do

  describe "compileMatch" $ do

    it "handles a simple variable case" $ do
      match ["x"] [([PVar "x"], 'a')] Error `shouldBe` Expr 'a'

    it "handles a single constructor case with a variable" $ do
      match ["x"] [([newtypeP (PVar "y")], 'a')] Error
        `shouldBe`
        Case "x" [Clause (Con "MyNewtype" 1) ["_u1"] (Expr 'a')]

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
      match ["x"] equations Error `shouldBe` expected

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
            )
          , Clause trueC []
            ( Case "y"
              [ Clause falseC [] (Expr '3')
              , Clause trueC [] (Expr '1')
              ]
            )
          ]
      match ["x", "y"] equations Error `shouldBe` expected

    it "handles the mappairs example" $ do
      match ["x1", "x2", "x3"] mappairsEquations Error `shouldBe` mappairsExpect

    it "handles the example from the Setsoft paper" $ do
      match ["x"] lamEquations Error `shouldBe` lamExpected
