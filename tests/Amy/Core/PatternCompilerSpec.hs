{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.PatternCompilerSpec
  ( spec
  ) where

import Data.Text (Text, pack)
import qualified Data.List.NonEmpty as NE
import Test.Hspec

import Amy.Core.AST
import Amy.Core.Monad
import Amy.Core.PatternCompiler

-- Utils
mkId :: Int -> Typed Ident
mkId i = mkIdent $ Ident ("_u" <> pack (show i)) i

mkIdent :: Ident -> Typed Ident
mkIdent = Typed boolTy

x, y, z, v, w, c, f, xs, ys :: Typed Ident
x = Typed boolTy $ Ident "x" 0
y = Typed boolTy $ Ident "y" 1
z = Typed boolTy $ Ident "z" 2
v = Typed boolTy $ Ident "v" 3
w = Typed boolTy $ Ident "w" 4
c = Typed boolTy $ Ident "c" 0
f = Typed boolTy $ Ident "f" 0
xs = Typed boolTy $ Ident "xs" 10
ys = Typed boolTy $ Ident "ys" 11

match'
  :: (Ord con)
  => [Typed Ident]
  -> [Equation con]
  -> CaseExpr con
match' vars eqs = runDesugar 0 $ match vars eqs

boolTy :: Type
boolTy = TyTerm $ TyCon $ TyConInfo "Bool" 0 [] (TyConDefinition "Bool" 0 [] KStar)

mkVal :: Typed Ident -> Expr
mkVal x' = EVar $ VVal x'

mkExpr :: [Typed Ident] -> Expr
mkExpr [] = error "empty list"
mkExpr [x'] = mkVal x'
mkExpr (x':y':xs') = EApp (App (mkVal x') (NE.fromList $ mkVal <$> (y':xs')) boolTy)

-- Bool type
trueC, falseC :: Con Text
trueC = Con "True" [] 2
falseC = Con "False" [] 2

trueP :: InputPattern Text
trueP = PCon trueC []

falseP :: InputPattern Text
falseP = PCon falseC []

-- List type
nilC, consC :: Con Text
nilC = Con "Nil" [] 2
consC = Con "Cons" [boolTy, boolTy] 2

nilP :: InputPattern Text
nilP = PCon nilC []

consP :: InputPattern Text -> InputPattern Text -> InputPattern Text
consP x' y' = PCon consC [x', y']

-- Newtype
newtypeC :: Con Text
newtypeC = Con "MyNewtype" [boolTy] 1

newtypeP :: InputPattern Text -> InputPattern Text
newtypeP pat = PCon newtypeC [pat]

-- Literal
intC :: Int -> Con Text
intC = ConLit . LiteralInt

intP :: Int -> InputPattern Text
intP i = PCon (intC i) []

-- mappairs example from book
mappairsEquations :: [Equation Text]
mappairsEquations =
  [ ([PVar f, nilP, PVar ys], mkVal x)
  , ([PVar f, consP (PVar x) (PVar xs), nilP], mkVal y)
  , ([PVar f, consP (PVar x) (PVar xs), consP (PVar y) (PVar ys)], mkVal z)
  ]

mappairsExpect :: CaseExpr Text
mappairsExpect =
  CaseExpr y
  [ Clause nilC [] (Expr $ mkVal x)
  , Clause consC [mkId 1, mkId 2]
     (CaseExpr (mkId 1)
        [ Clause nilC [] (Expr $ mkVal y)
        , Clause consC [mkId 3, mkId 4] (Expr $ mkVal z)
        ]
        Nothing
     )
  ]
  Nothing

-- Example from Sestoft paper
varC, lamC, appC, letC :: Con Text
varC = Con "Var" [boolTy] 4
lamC = Con "Lam" [boolTy, boolTy] 4
appC = Con "App" [boolTy, boolTy] 4
letC = Con "Let" [boolTy, boolTy, boolTy] 4

lamId :: Int -> Typed Ident
lamId i = mkIdent $ Ident (pack $ show i) i

lamEquations :: [Equation Text]
lamEquations =
  [ ([PCon varC [PVar x]], mkExpr [x, lamId 111])
  , ([PCon lamC [PVar x, PCon varC [PVar y]]], mkExpr [lamId 222])
  , ([PCon lamC [PVar x, PCon lamC [PVar y, PVar z]]], mkExpr [lamId 333])
  , ([PCon lamC [PVar x, PCon appC [PVar y, PVar z]]], mkExpr [lamId 444])
  , ([PCon appC [PCon lamC [PVar x, PVar y], PVar z]], mkExpr [x, y, z, lamId 555])
  --, ([PCon appC [PCon appC [PCon lamC [PVar x, PCon lamC [PVar y, PVar z]], PVar v], PVar w]], mkExpr [lamId 000])
  , ([PCon appC [PCon appC [PVar x, PVar y], PVar z]], mkExpr [lamId 666])
  , ([PCon letC [PVar x, PCon letC [PVar y, PVar z, PVar v], PVar w]], mkExpr [lamId 777])
  , ([PCon lamC [PVar x, PCon letC [PVar y, PVar z, PVar v]]], mkExpr [lamId 888])
  , ([PCon letC [PVar x, PVar y, PCon appC [PVar z, PVar v]]], mkExpr [x, y, z, v, lamId 999])
  , ([PCon appC [PCon appC [PCon lamC [PVar x, PCon lamC [PVar y, PVar z]], PVar v], PVar w]], mkExpr [lamId 1010])
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

lamExpected :: CaseExpr Text
lamExpected =
  CaseExpr c
  [Clause varC [mkId 1] (Expr $ mkExpr [mkId 1, lamId 111]),
   Clause lamC [mkId 2, mkId 3]
     (CaseExpr (mkId 3)
        [Clause varC [mkId 4] (Expr $ mkExpr [lamId 222]),
         Clause lamC [mkId 5, mkId 6] (Expr $ mkExpr [lamId 333]),
         Clause appC [mkId 7, mkId 8] (Expr $ mkExpr [lamId 444]),
         Clause letC [mkId 9, mkId 10, mkId 11] (Expr $ mkExpr [lamId 888])]
        Nothing),
   Clause appC [mkId 12, mkId 13]
     (CaseExpr (mkId 12)
        [Clause lamC [mkId 14, mkId 15] (Expr $ mkExpr [mkId 15, mkId 13, mkId 14, lamId 555]),
         Clause appC [mkId 16, mkId 17] (Expr $ mkExpr [lamId 666])]
        (Just Error)),
   Clause letC [mkId 22, mkId 23, mkId 24]
     (CaseExpr (mkId 23)
        [Clause letC [mkId 27, mkId 28, mkId 29] (Expr $ mkExpr [lamId 777])]
        (Just
           (CaseExpr (mkId 24)
              [Clause appC [mkId 25, mkId 26]
                 (Expr $ mkExpr [mkId 22, mkId 23, mkId 25, mkId 26, lamId 999])]
              (Just Error))))]
  Nothing

spec :: Spec
spec = do

  describe "compileMatch" $ do

    it "handles a simple variable case" $ do
      match' [x] [([PVar x], mkVal x)] `shouldBe` (Expr (mkVal x) :: CaseExpr Text)

    it "handles a single constructor case with a variable" $ do
      match' [x] [([newtypeP (PVar y)], mkVal x)]
        `shouldBe`
        CaseExpr x [Clause newtypeC [mkId 1] (Expr (mkVal x))] Nothing

    it "handles a single constructor case with a literal and variable" $ do
      let
        equations =
          [ ([newtypeP (intP 1)], mkVal x)
          , ([newtypeP (PVar y)], mkExpr [x, y])
          ]
        expected =
          CaseExpr x
          [ Clause newtypeC [mkId 1]
            ( CaseExpr (mkId 1)
              [ Clause (intC 1) [] (Expr $ mkVal x)
              ]
              (Just (Expr $ mkExpr [x, mkId 1]))
            )
          ]
          Nothing
      match' [x] equations `shouldBe` expected

    it "handles a simple true/false case" $ do
      let
        equations =
          [ ([trueP], mkVal x)
          , ([falseP], mkVal y)
          ]
        expected =
          CaseExpr x
          [ Clause trueC [] (Expr $ mkVal x)
          , Clause falseC [] (Expr $ mkVal y)
          ]
          Nothing
      match' [x] equations `shouldBe` expected

    it "handles redundant branches" $ do
      let
        equations =
          [ ([intP 1], mkVal x)
          , ([intP 2], mkVal y)
          , ([intP 2], mkVal z)
          , ([intP 1], mkVal v)
          , ([intP 2], mkVal w)
          , ([intP 3], mkVal c)
          ]
        expected =
          CaseExpr x
          [ Clause (intC 1) [] (Expr $ mkVal x)
          , Clause (intC 2) [] (Expr $ mkVal y)
          , Clause (intC 3) [] (Expr $ mkVal c)
          ]
          (Just Error)
      match' [x] equations `shouldBe` expected

    it "handles a pair of bools case" $ do
      let
        equations =
          [ ([trueP, trueP], mkVal x)
          , ([falseP, falseP], mkVal y)
          , ([trueP, falseP] , mkVal z)
          , ([falseP, trueP], mkVal w)
          ]
        expected =
          CaseExpr x
          [ Clause trueC []
            ( CaseExpr y
              [ Clause trueC [] (Expr $ mkVal x)
              , Clause falseC [] (Expr $ mkVal z)
              ]
              Nothing
            )
          , Clause falseC []
            ( CaseExpr y
              [ Clause falseC [] (Expr $ mkVal y)
              , Clause trueC [] (Expr $ mkVal w)
              ]
              Nothing
            )
          ]
          Nothing
      match' [x, y] equations `shouldBe` expected

    it "handles the mappairs example" $ do
      match' [x, y, z] mappairsEquations `shouldBe` mappairsExpect

    it "handles the example from the Setsoft paper" $ do
      match' [c] lamEquations `shouldBe` lamExpected
