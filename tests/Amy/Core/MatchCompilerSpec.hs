{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.MatchCompilerSpec
  ( spec
  ) where

import Test.Hspec

import Amy.Core.MatchCompiler

-- nullC :: Con
-- nullC = Con { conName = "Null", conArity = 0, conSpan = 3 }

-- leafC :: Con
-- leafC = Con { conName = "Leaf", conArity = 1, conSpan = 3 }

-- nodeC :: Con
-- nodeC = Con { conName = "Node", conArity = 3, conSpan = 3 }

trueC :: Con
trueC = Con { conName = "True", conArity = 0, conSpan = 2}

falseC :: Con
falseC = Con { conName = "False", conArity = 0, conSpan = 2}

trueP :: Pat
trueP = PCon trueC []

falseP :: Pat
falseP = PCon falseC []

-- tupP :: [Pat] -> Pat
-- tupP args = PCon Con { conName = "", conArity = length args, conSpan = 1 } args

-- data Color = Red | Blue | Green

-- redC, blueC, greenC :: Con
-- redC = Con "Red" 0 3
-- blueC = Con "Blue" 0 3
-- greenC = Con "Green" 0 3

-- redP, blueP, greenP :: Pat
-- redP = PCon redC []
-- blueP = PCon blueC []
-- greenP = PCon greenC []

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

expectedLamCompile :: Decision' Int
expectedLamCompile =
  Switch Obj
  [ (varC, Success' 111)
  , ( lamC
    , Switch (Sel 2 Obj)
      [ (varC, Success' 222)
      , (lamC, Success' 333)
      , (appC, Success' 444)
      ]
      (Success' 888)
    )
  , ( appC
    , Switch (Sel 1 Obj)
      [ (lamC, Success' 555)
      , (appC, Success' 666)
      ]
      Failure'
    )
  ]
  (Switch (Sel 2 Obj)
     [ (letC, Success' 777)
     ]
     (Switch (Sel 3 Obj)
        [ (appC, Success' 999)
        ]
        Failure'
     )
  )

spec :: Spec
spec = do

  describe "compileMatch" $ do

    it "handles a simple true/false case" $ do
      let
        match =
          [ (trueP, 'a')
          , (falseP, 'b')
          ]
        expected =
          Switch Obj
          [(trueC, Success' 'a')]
          (Success' 'b')
      switchify (compileMatch match) `shouldBe` expected

    it "handles the example from the Sestoft paper" $ do
      switchify (compileMatch lamMatch) `shouldBe` expectedLamCompile
