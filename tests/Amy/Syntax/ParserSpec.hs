{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Amy.Syntax.ParserSpec
  ( spec
  ) where

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Shakespeare.Text (st)

import Amy.Syntax.AST
import Amy.Syntax.Parser
import Amy.Type

spec :: Spec
spec = do

  describe "parseModule" $ do
    it "parses a small module" $ do
      parse parseModule "" sampleModule
        `shouldParse`
        Module
        [ DeclBindingType
          BindingType
          { bindingTypeName = Located (SourceSpan "" 2 1 2 1) "f"
          , bindingTypeTypeNames =
            TArr
              (TVar (Located (SourceSpan "" 2 6 2 8) "Int"))
              (TVar (Located (SourceSpan "" 2 13 2 18) "Double"))
          }
        , DeclBinding
          Binding
          { bindingName = Located (SourceSpan "" 3 1 3 1) "f"
          , bindingArgs = [Located (SourceSpan "" 3 3 3 3) "x"]
          , bindingBody =
            ELit (Located (SourceSpan "" 3 7 3 7) (LiteralInt 1))
          }
        , DeclBindingType
          BindingType
          { bindingTypeName = Located (SourceSpan "" 5 1 5 4) "main"
          , bindingTypeTypeNames = TVar (Located (SourceSpan "" 5 9 5 11) "Int")
          }
        , DeclBinding
          Binding
          { bindingName = Located (SourceSpan "" 6 1 6 4) "main"
          , bindingArgs = []
          , bindingBody =
            ELet
              Let
              { letBindings =
                [ LetBindingType
                  BindingType
                  { bindingTypeName = Located (SourceSpan "" 8 5 8 5) "x"
                  , bindingTypeTypeNames = TVar (Located (SourceSpan "" 8 10 8 12) "Int")
                  }
                , LetBinding
                  Binding
                  { bindingName = Located (SourceSpan "" 9 5 9 5) "x"
                  , bindingArgs = []
                  , bindingBody = ELit (Located (SourceSpan "" 9 9 9 9) (LiteralInt 1))
                  }
                ]
              , letExpression =
                EApp (
                  App
                  (EVar (Located (SourceSpan "" 11 5 11 5) "f"))
                  [ EVar (Located (SourceSpan "" 11 7 11 7) "x")
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
          (EVar (Located (SourceSpan "" 1 1 1 1) "f"))
          [ EParens $
              EApp $
                App
                (EVar (Located (SourceSpan "" 1 4 1 4) "g"))
                [EVar (Located (SourceSpan "" 1 6 1 6) "x")]
          , ELit (Located (SourceSpan "" 1 9 1 9) (LiteralInt 1))
          ]
        )

    it "parses multi-line expression" $ do
      parse expression "" "f 1\n 2 3" `shouldParse`
        EApp (
          App
          (EVar (Located (SourceSpan "" 1 1 1 1) "f"))
          [ ELit (Located (SourceSpan "" 1 3 1 3) (LiteralInt 1))
          , ELit (Located (SourceSpan "" 2 2 2 2) (LiteralInt 2))
          , ELit (Located (SourceSpan "" 2 4 2 4) (LiteralInt 3))
          ]
        )

  describe "externType" $ do
    it "parses extern declaration" $ do
      parse externType "" "extern f :: Int"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 8 1 8) "f")
          (TVar (Located (SourceSpan "" 1 13 1 15) "Int"))
      parse externType "" "extern f :: Int -> Double"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 8 1 8) "f")
          (TArr
             (TVar (Located (SourceSpan "" 1 13 1 15) "Int"))
             (TVar (Located (SourceSpan "" 1 20 1 25) "Double"))
          )

  describe "bindingType" $ do
    it "parses binding types" $ do
      parse bindingType "" "f :: Int"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          (TVar (Located (SourceSpan "" 1 6 1 8) "Int"))
      parse bindingType "" "f :: Int -> Double"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          (TArr
             (TVar (Located (SourceSpan "" 1 6 1 8) "Int"))
             (TVar (Located (SourceSpan "" 1 13 1 18) "Double"))
          )

  describe "parseType" $ do
    it "handles simple types" $ do
      parse parseType "" "A" `shouldParse` TVar (Located (SourceSpan "" 1 1 1 1) "A")
      parse parseType "" "A -> B"
        `shouldParse`
        TArr
          (TVar (Located (SourceSpan "" 1 1 1 1) "A"))
          (TVar (Located (SourceSpan "" 1 6 1 6) "B"))
      parse parseType "" "A -> B -> C"
        `shouldParse`
        TArr
          (TVar (Located (SourceSpan "" 1 1 1 1) "A"))
          (TArr
             (TVar (Located (SourceSpan "" 1 6 1 6) "B"))
             (TVar (Located (SourceSpan "" 1 11 1 11) "C"))
          )

    it "handles parens" $ do
      parse parseType "" "(A)" `shouldParse` TVar (Located (SourceSpan "" 1 2 1 2) "A")
      parse parseType "" "((X))" `shouldParse` TVar (Located (SourceSpan "" 1 3 1 3) "X")
      parse parseType "" "((A)) -> ((B))"
        `shouldParse`
        TArr
          (TVar (Located (SourceSpan "" 1 3 1 3) "A"))
          (TVar (Located (SourceSpan "" 1 12 1 12) "B"))
      parse parseType "" "(A -> B) -> C"
        `shouldParse`
        TArr
          (TArr
            (TVar (Located (SourceSpan "" 1 2 1 2) "A"))
            (TVar (Located (SourceSpan "" 1 7 1 7) "B"))
          )
          (TVar (Located (SourceSpan "" 1 13 1 13) "C"))
      parse parseType "" "A -> (B -> C) -> D"
        `shouldParse`
        TArr
          (TVar (Located (SourceSpan "" 1 1 1 1) "A"))
          (TArr
            (TArr
              (TVar (Located (SourceSpan "" 1 7 1 7) "B"))
              (TVar (Located (SourceSpan "" 1 12 1 12) "C")))
            (TVar (Located (SourceSpan "" 1 18 1 18) "D"))
          )

    it "should fail gracefully without infinite loops" $ do
      parse parseType "" `shouldFailOn` ""
      parse parseType "" `shouldFailOn` "()"
      parse parseType "" `shouldFailOn` "(())"
      parse parseType "" `shouldFailOn` "A ->"

  describe "expressionParens" $ do
    it "parses expressions in parens" $ do
      parse expressionParens "" "(x)" `shouldParse` EParens (EVar (Located (SourceSpan "" 1 2 1 2) "x"))
      parse expressionParens "" "(f x)"
        `shouldParse`
        EParens
          (EApp
             (App
               (EVar (Located (SourceSpan "" 1 2 1 2) "f"))
               [EVar (Located (SourceSpan "" 1 4 1 4) "x")])
          )

  describe "ifExpression" $ do
    it "parses if expressions" $ do
      parse ifExpression "" "if True then 1 else 2"
        `shouldParse`
        If
          (ELit (Located (SourceSpan "" 1 4 1 7) (LiteralBool True)))
          (ELit (Located (SourceSpan "" 1 14 1 14) (LiteralInt 1)))
          (ELit (Located (SourceSpan "" 1 21 1 21) (LiteralInt 2)))
      parse ifExpression "" "if f x then f y else g 2"
        `shouldParse`
        If
          (EApp $ App (EVar (Located (SourceSpan "" 1 4 1 4) "f")) [EVar (Located (SourceSpan "" 1 6 1 6) "x")])
          (EApp $ App (EVar (Located (SourceSpan "" 1 13 1 13) "f")) [EVar (Located (SourceSpan "" 1 15 1 15) "y")])
          (EApp $ App (EVar (Located (SourceSpan "" 1 22 1 22) "g")) [ELit (Located (SourceSpan "" 1 24 1 24) (LiteralInt 2))])

  describe "literal" $ do
    it "can discriminate between integer and double" $ do
      parse literal "" "1" `shouldParse` Located (SourceSpan "" 1 1 1 1) (LiteralInt 1)
      -- TODO: Trailing decimals?
      -- parse (literal <* eof) "" "2." `shouldParse` Located (SourceSpan "" 1 1 1 2) (LiteralInt 2)
      parse literal "" "1.5" `shouldParse` Located (SourceSpan "" 1 1 1 3) (LiteralDouble 1.5)

    it "can parse bools" $ do
      parse literal "" "True" `shouldParse` Located (SourceSpan "" 1 1 1 4) (LiteralBool True)
      parse literal "" "False" `shouldParse` Located (SourceSpan "" 1 1 1 5) (LiteralBool False)

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