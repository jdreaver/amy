{-# LANGUAGE OverloadedLists #-}

module Amy.Utils.SolveSetEquationsSpec
  ( spec
  ) where

import Data.Set (Set)
import Test.Hspec

import Amy.Utils.SolveSetEquations

spec :: Spec
spec = do
  describe "solveSetEquations" $ do
    it "handles no equations" $
      solveSetEquations [] `shouldBe` ([] :: [(String, Set String)])

    it "handles a single equation" $
      solveSetEquations [SetEquation "1" ["a", "b"] []] `shouldBe` [("1", ["a", "b"])]

    it "handles multiple equation" $
      solveSetEquations
        [ SetEquation "1" ["a", "b"] []
        , SetEquation "2" ["b", "c"] ["1"]
        ]
        `shouldBe`
        [ ("1", ["a", "b"])
        , ("2", ["a", "b", "c"])
        ]

    it "handles multiple recursive equations" $
      solveSetEquations
        [ SetEquation "1" ["a", "b"] ["2"]
        , SetEquation "2" ["b", "c"] ["1"]
        ]
        `shouldBe`
        [ ("1", ["a", "b", "c"])
        , ("2", ["a", "b", "c"])
        ]

    it "handles a lot of recursive equations" $
      solveSetEquations
        [ SetEquation "1" ["a", "b"] ["2", "3"]
        , SetEquation "2" ["b", "c"] ["1", "3"]
        , SetEquation "3" ["d"] ["1", "2"]
        ]
        `shouldBe`
        [ ("1", ["a", "b", "c", "d"])
        , ("2", ["a", "b", "c", "d"])
        , ("3", ["a", "b", "c", "d"])
        ]
