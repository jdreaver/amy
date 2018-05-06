{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.MatchCompilerSpec
  ( spec
  ) where

import Data.Text (Text)
import Test.Hspec

import Amy.Core.MatchCompiler
import Amy.Literal

trueC, falseC :: Con Text
trueC = Con "True" 0 2
falseC = Con "False" 0 2

trueP :: Pat Text
trueP = PCon trueC []

falseP :: Pat Text
falseP = PCon falseC []

tupC :: Int -> Con Text
tupC arity = Con "" arity 1

tupP :: [Pat Text] -> Pat Text
tupP args = PCon (tupC (length args)) args

litP :: Literal -> Pat Text
litP lit = PCon (ConLit lit) []

newtypeC :: Con Text
newtypeC = Con "MyNewtype" 1 1

newtypeP :: Pat Text -> Pat Text
newtypeP pat = PCon newtypeC [pat]

-- data Color = Red | Blue | Green

-- redC, blueC, greenC :: Con
-- redC = Con "Red" 0 3
-- blueC = Con "Blue" 0 3
-- greenC = Con "Green" 0 3

-- redP, blueP, greenP :: Pat
-- redP = PCon redC []
-- blueP = PCon blueC []
-- greenP = PCon greenC []

varC, lamC, appC, letC :: Con Text
varC = Con "Var" 1 4
lamC = Con "Lam" 2 4
appC = Con "App" 2 4
letC = Con "Let" 3 4

lamMatch :: Match Text Int
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

-- case x of
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
-- case x of
--   Var x -> 111
--   Lam v1 v2 ->
--      case v2 of
--        Var y -> 222
--        Lam y z -> 333
--        App y z -> 444
--        _ -> 888
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

expectedLamCompile :: Decision Text Int
expectedLamCompile =
  Switch Obj
  [ (varC, Success 111)
  , ( lamC
    , Switch (Sel 1 Obj lamC)
      [ (varC, Success 222)
      , (lamC, Success 333)
      , (appC, Success 444)
      ]
      (Success 888)
    )
  , ( appC
    , Switch (Sel 0 Obj appC)
      [ (lamC, Success 555)
      , (appC, Success 666)
      ]
      Failure
    )
  ]
  (Switch (Sel 1 Obj letC)
     [ (letC, Success 777)
     ]
     (Switch (Sel 2 Obj letC)
        [ (appC, Success 999)
        ]
        Failure
     )
  )

spec :: Spec
spec = do

  describe "compileMatch" $ do

    it "handles a simple variable case" $ do
      compileMatch [(PVar "x" :: Pat Text, 'a')] `shouldBe` Success 'a'

    it "handles a single constructor case with a variable" $ do
      compileMatch [(newtypeP (PVar "x"), 'a')] `shouldBe` Success 'a'

    it "handles a single constructor case with a literal" $ do
      compileMatch [(newtypeP (PCon (ConLit (LiteralInt 1)) []), 'a')]
        `shouldBe`
        Switch (Sel 0 Obj newtypeC)
        [ (ConLit (LiteralInt 1), Success 'a')
        ]
        Failure

    it "handles a single constructor case with a literal and variable" $ do
      compileMatch
        [ (newtypeP (PCon (ConLit (LiteralInt 1)) []), 'a')
        , (newtypeP (PVar "x"), 'b')
        ]
        `shouldBe`
        Switch (Sel 0 Obj newtypeC)
        [ (ConLit (LiteralInt 1), Success 'a')
        ]
        (Success 'b')

    it "handles a simple true/false case" $ do
      let
        match =
          [ (trueP, 'a')
          , (falseP, 'b')
          ]
        expected =
          Switch Obj
          [(trueC, Success 'a')]
          (Success 'b')
      compileMatch match `shouldBe` expected

    it "handles a tuple true/false case" $ do
      let
        match =
          [ (tupP [trueP, trueP], '1')
          , (tupP [falseP, falseP], '2')
          , (tupP [trueP, falseP] , '3')
          , (tupP [falseP, trueP], '4')
          ]
        expected =
          Switch (Sel 0 Obj (tupC 2))
          [ ( trueC
            , Switch (Sel 1 Obj (tupC 2))
              [ (trueC, Success '1')
              ]
              (Success '3')
            )
          ]
          ( Switch (Sel 1 Obj (tupC 2))
            [ (falseC, Success '2')
            ]
            (Success '4')
          )
      compileMatch match `shouldBe` expected

    it "handles literals" $ do
      let
        match =
          [ (litP (LiteralInt 1), 'a')
          , (litP (LiteralInt 2), 'b')
          , (PVar "x", 'c')
          , (litP (LiteralInt 4), 'd') -- Redundant
          ]
        expected =
          Switch Obj
          [ (ConLit (LiteralInt 1), Success 'a')
          , (ConLit (LiteralInt 2), Success 'b')
          ]
          (Success 'c')
      compileMatch match `shouldBe` expected

    it "handles the example from the Sestoft paper" $ do
      compileMatch lamMatch `shouldBe` expectedLamCompile
