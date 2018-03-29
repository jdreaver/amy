{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Amy.Parser.ParserSpec
  ( spec
  ) where

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Shakespeare.Text (st)

import Amy.Parser.AST
import Amy.Parser.Parser

spec :: Spec
spec = do

  describe "parseModule" $ do
    it "parses a small module" $ do
      parse parseModule "" sampleModule
        `shouldParse`
        Module
        [ DeclBindingType
          BindingType
          { bindingTypeName = "f"
          , bindingTypeTypeNames = ["Int", "Double"]
          }
        , DeclBinding
          Binding
          { bindingName = "f"
          , bindingArgs = ["x"]
          , bindingBody =
            ELit (LiteralInt 1)
          }
        , DeclBindingType
          BindingType
          { bindingTypeName = "main"
          , bindingTypeTypeNames = ["Int"]
          }
        , DeclBinding
          Binding
          { bindingName = "main"
          , bindingArgs = []
          , bindingBody =
            ELet
              Let
              { letBindings =
                [ LetBindingType
                  BindingType
                  { bindingTypeName = "x"
                  , bindingTypeTypeNames = ["Int"]
                  }
                , LetBinding
                  Binding
                  { bindingName = "x"
                  , bindingArgs = []
                  , bindingBody = ELit (LiteralInt 1)
                  }
                ]
              , letExpression =
                EApp (
                  App
                  (EVar "f")
                  [ EVar "x"
                  ]
                )
              }
          }
        ]

    it "rejects indented top-level declarations" $ do
      parse parseModule "" "  f :: Int"
        `shouldFailWith` FancyError [SourcePos "" (mkPos 1) (mkPos 3)] [ErrorIndentation EQ (mkPos 1) (mkPos 3)]

  describe "expression" $ do
    it "parses complex expressions" $ do
      parse expression "" "f (g x) 1" `shouldParse`
        EApp (
          App
          (EVar "f")
          [ EParens $ EApp $ App (EVar "g") [EVar "x"]
          , ELit (LiteralInt 1)
          ]
        )

    it "parses multi-line expression" $ do
      parse expression "" "f 1\n 2 3" `shouldParse`
        EApp (
          App
          (EVar "f")
          [ ELit (LiteralInt 1)
          , ELit (LiteralInt 2)
          , ELit (LiteralInt 3)
          ]
        )

  describe "externType" $ do
    it "parses extern declaration" $ do
      parse externType "" "extern f :: Int" `shouldParse` BindingType "f" ["Int"]
      parse externType "" "extern f :: Int -> Double" `shouldParse` BindingType "f" ["Int", "Double"]

  describe "bindingType" $ do
    it "parses binding types" $ do
      parse bindingType "" "f :: Int" `shouldParse` BindingType "f" ["Int"]
      parse bindingType "" "f :: Int -> Double" `shouldParse` BindingType "f" ["Int", "Double"]

  describe "expressionParens" $ do
    it "parses expressions in parens" $ do
      parse expressionParens "" "(x)" `shouldParse` EParens (EVar "x")
      parse expressionParens "" "(f x)"
        `shouldParse`
        EParens (EApp (App (EVar "f") [EVar "x"]))

  describe "ifExpression" $ do
    it "parses if expressions" $ do
      parse ifExpression "" "if True then 1 else 2"
        `shouldParse`
        If
          (ELit (LiteralBool True))
          (ELit (LiteralInt 1))
          (ELit (LiteralInt 2))
      parse ifExpression "" "if f x then f y else g 2"
        `shouldParse`
        If
          (EApp $ App (EVar "f") [EVar "x"])
          (EApp $ App (EVar "f") [EVar "y"])
          (EApp $ App (EVar "g") [ELit (LiteralInt 2)])

  describe "literal" $ do
    it "can discriminate between integer and double" $ do
      parse literal "" "1" `shouldParse` LiteralInt 1
      parse literal "" "1." `shouldParse` LiteralInt 1
      parse literal "" "1.5" `shouldParse` LiteralDouble 1.5

    it "can parse bools" $ do
      parse literal "" "True" `shouldParse` LiteralBool True
      parse literal "" "False" `shouldParse` LiteralBool False

sampleModule :: Text
sampleModule = [st|
f :: Int -> Double
f x = 1

main :: Int
main =
  let
    x :: Int
    x = 1
  in
    f x

|]
