{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.PatternCompilerSpec
  ( spec
  ) where

import Data.Text (Text, pack)
import Test.Hspec

import Amy.Core.AST (Ident(..))
import Amy.Core.Monad
import Amy.Core.PatternCompiler
import Amy.Literal

-- Utils
ignoreSubst :: VarSubst expr
ignoreSubst = VarSubst $ \x' _ _ -> x'

listSubst :: VarSubst [Ident]
listSubst = VarSubst $ \xs var newVar -> (\v' -> if v' == var then newVar else v') <$> xs

mkId :: Int -> Ident
mkId i = Ident ("_u" <> pack (show i)) i

x, y, z, v, w, c :: Ident
x = Ident "x" 0
y = Ident "y" 1
z = Ident "z" 2
v = Ident "v" 3
w = Ident "w" 4
c = Ident "c" 0

match'
  :: (Show expr, Ord con)
  => VarSubst expr
  -> [Ident]
  -> [Equation expr con]
  -> CaseExpr expr con
match' subst vars eqs = runDesugar 0 $ match subst vars eqs

-- Bool type
trueC, falseC :: Con Text
trueC = Con "True" 0 2
falseC = Con "False" 0 2

trueP :: InputPattern Text
trueP = PCon trueC []

falseP :: InputPattern Text
falseP = PCon falseC []

-- List type
nilC, consC :: Con Text
nilC = Con "Nil" 0 2
consC = Con "Cons" 2 2

nilP :: InputPattern Text
nilP = PCon nilC []

consP :: InputPattern Text -> InputPattern Text -> InputPattern Text
consP x' y' = PCon consC [x', y']

-- Newtype
newtypeC :: Con Text
newtypeC = Con "MyNewtype" 1 1

newtypeP :: InputPattern Text -> InputPattern Text
newtypeP pat = PCon newtypeC [pat]

-- Literal
intC :: Int -> Con Text
intC = ConLit . LiteralInt

intP :: Int -> InputPattern Text
intP i = PCon (intC i) []

-- mappairs example from book
mappairsEquations :: [Equation Int Text]
mappairsEquations =
  [ ([PVar (Ident "f" 0), nilP, PVar (Ident "ys" 1)], 1)
  , ([PVar (Ident "f" 0), consP (PVar x) (PVar (Ident "xs" 2)), nilP], 2)
  , ([PVar (Ident "f" 0), consP (PVar x) (PVar (Ident "xs" 2)), consP (PVar y) (PVar (Ident "ys" 4))], 3)
  ]

mappairsExpect :: CaseExpr Int Text
mappairsExpect =
  Case y
  [ Clause nilC [] (Expr 1)
  , Clause consC [mkId 1, mkId 2]
     (Case (mkId 1)
        [ Clause nilC [] (Expr 2)
        , Clause consC [mkId 3, mkId 4] (Expr 3)
        ]
        Nothing
     )
  ]
  Nothing

-- Example from Sestoft paper
varC, lamC, appC, letC :: Con Text
varC = Con "Var" 1 4
lamC = Con "Lam" 2 4
appC = Con "App" 2 4
letC = Con "Let" 3 4

lamEquations :: [Equation [Ident] Text]
lamEquations =
  [ ([PCon varC [PVar x]], [x, Ident "111" 111])
  , ([PCon lamC [PVar x, PCon varC [PVar y]]], [Ident "222" 222])
  , ([PCon lamC [PVar x, PCon lamC [PVar y, PVar z]]], [Ident "333" 333])
  , ([PCon lamC [PVar x, PCon appC [PVar y, PVar z]]], [Ident "444" 444])
  , ([PCon appC [PCon lamC [PVar x, PVar y], PVar z]], [x, y, z, Ident "555" 555])
  --, ([PCon appC [PCon appC [PCon lamC [PVar x, PCon lamC [PVar y, PVar z]], PVar v], PVar w]], [Ident "000" 000])
  , ([PCon appC [PCon appC [PVar x, PVar y], PVar z]], [Ident "666" 666])
  , ([PCon letC [PVar x, PCon letC [PVar y, PVar z, PVar v], PVar w]], [Ident "777" 777])
  , ([PCon lamC [PVar x, PCon letC [PVar y, PVar z, PVar v]]], [Ident "888" 888])
  , ([PCon letC [PVar x, PVar y, PCon appC [PVar z, PVar v]]], [x, y, z, v, Ident "999" 999])
  , ([PCon appC [PCon appC [PCon lamC [PVar x, PCon lamC [PVar y, PVar z]], PVar v], PVar w]], [Ident "1010" 1010])
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

lamExpected :: CaseExpr [Ident] Text
lamExpected =
  Case c
  [Clause (Con "Var" 1 4) [mkId 1] (Expr [mkId 1, Ident "111" 111]),
   Clause (Con "Lam" 2 4) [mkId 2, mkId 3]
     (Case (mkId 3)
        [Clause (Con "Var" 1 4) [mkId 4] (Expr [Ident "222" 222]),
         Clause (Con "Lam" 2 4) [mkId 5, mkId 6] (Expr [Ident "333" 333]),
         Clause (Con "App" 2 4) [mkId 7, mkId 8] (Expr [Ident "444" 444]),
         Clause (Con "Let" 3 4) [mkId 9, mkId 10, mkId 11] (Expr [Ident "888" 888])]
        Nothing),
   Clause (Con "App" 2 4) [mkId 12, mkId 13]
     (Case (mkId 12)
        [Clause (Con "Lam" 2 4) [mkId 14, mkId 15] (Expr [mkId 15, mkId 13, mkId 14, Ident "555" 555]),
         Clause (Con "App" 2 4) [mkId 16, mkId 17] (Expr [Ident "666" 666])]
        (Just Error)),
   Clause (Con "Let" 3 4) [mkId 22, mkId 23, mkId 24]
     (Case (mkId 23)
        [Clause (Con "Let" 3 4) [mkId 27, mkId 28, mkId 29] (Expr [Ident "777" 777])]
        (Just
           (Case (mkId 24)
              [Clause (Con "App" 2 4) [mkId 25, mkId 26]
                 (Expr [mkId 22, mkId 23, mkId 25, mkId 26, Ident "999" 999])]
              (Just Error))))]
  Nothing

spec :: Spec
spec = do

  describe "compileMatch" $ do

    it "handles a simple variable case" $ do
      match' ignoreSubst [x] [([PVar x], 'a')] `shouldBe` (Expr 'a' :: CaseExpr Char Text)

    it "handles a single constructor case with a variable" $ do
      match' ignoreSubst [x] [([newtypeP (PVar y)], 'a')]
        `shouldBe`
        Case x [Clause newtypeC [mkId 1] (Expr 'a')] Nothing

    it "handles a single constructor case with a literal and variable" $ do
      let
        equations =
          [ ([newtypeP (intP 1)], [x])
          , ([newtypeP (PVar y)], [x, y])
          ]
        expected =
          Case (x)
          [ Clause newtypeC [mkId 1]
            ( Case (mkId 1)
              [ Clause (intC 1) [] (Expr [x])
              ]
              (Just (Expr [x, mkId 1]))
            )
          ]
          Nothing
      match' listSubst [x] equations `shouldBe` expected

    it "handles a simple true/false case" $ do
      let
        equations =
          [ ([trueP], 'a')
          , ([falseP], 'b')
          ]
        expected =
          Case x
          [ Clause trueC [] (Expr 'a')
          , Clause falseC [] (Expr 'b')
          ]
          Nothing
      match' ignoreSubst [x] equations `shouldBe` expected

    it "handles redundant branches" $ do
      let
        equations =
          [ ([intP 1], 'a')
          , ([intP 2], 'b')
          , ([intP 2], 'c')
          , ([intP 1], 'd')
          , ([intP 2], 'e')
          , ([intP 3], 'f')
          ]
        expected =
          Case x
          [ Clause (intC 1) [] (Expr 'a')
          , Clause (intC 2) [] (Expr 'b')
          , Clause (intC 3) [] (Expr 'f')
          ]
          (Just Error)
      match' ignoreSubst [x] equations `shouldBe` expected

    it "handles a pair of bools case" $ do
      let
        equations =
          [ ([trueP, trueP], '1')
          , ([falseP, falseP], '2')
          , ([trueP, falseP] , '3')
          , ([falseP, trueP], '4')
          ]
        expected =
          Case x
          [ Clause trueC []
            ( Case y
              [ Clause trueC [] (Expr '1')
              , Clause falseC [] (Expr '3')
              ]
              Nothing
            )
          , Clause falseC []
            ( Case y
              [ Clause falseC [] (Expr '2')
              , Clause trueC [] (Expr '4')
              ]
              Nothing
            )
          ]
          Nothing
      match' ignoreSubst [x, y] equations `shouldBe` expected

    it "handles the mappairs example" $ do
      match' ignoreSubst [x, y, z] mappairsEquations `shouldBe` mappairsExpect

    it "handles the example from the Setsoft paper" $ do
      match' listSubst [Ident "c" 0] lamEquations `shouldBe` lamExpected
