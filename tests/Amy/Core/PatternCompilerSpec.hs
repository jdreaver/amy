{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.PatternCompilerSpec
  ( spec
  ) where

import Test.Hspec

import Amy.Core.PatternCompiler

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
        [ Clause consC ["_u1", "_u2"] (Expr 3)
        , Clause nilC [] (Expr 2)
        ])
  , Clause nilC [] (Expr 1)
  ]

spec :: Spec
spec = do

  describe "compileMatch" $ do

    it "handles a simple variable case" $ do
      match ["x"] [([PVar "x"], 1)] Error `shouldBe` Expr (1 :: Int)

    it "handles the mappairs example" $ do
      match ["x1", "x2", "x3"] mappairsEquations Error `shouldBe` mappairsExpect
