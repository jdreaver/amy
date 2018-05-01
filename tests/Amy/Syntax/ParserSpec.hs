{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Amy.Syntax.ParserSpec
  ( spec
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Shakespeare.Text (st)

import Amy.Syntax.AST
import Amy.Syntax.Monad
import Amy.Syntax.Parser

parse' :: AmyParser a -> Text -> Either (ParseError Char Void) a
parse' parser = parse (runAmyParser parser) ""

spec :: Spec
spec = do

  describe "parseModule" $ do
    it "parses a small module" $ do
      parse' parseModule sampleModule
        `shouldParse`
        Module
        [ DeclBindingType
          BindingType
          { bindingTypeName = Located (SourceSpan "" 2 1 2 1) "f"
          , bindingTypeScheme =
            Forall [] $
            TyCon (Located (SourceSpan "" 2 6 2 8) "Int")
            `TyFun`
            TyCon (Located (SourceSpan "" 2 13 2 18) "Double")
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
          , bindingTypeScheme = Forall [] $ TyCon (Located (SourceSpan "" 5 9 5 11) "Int")
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
                  , bindingTypeScheme = Forall [] $ TyCon (Located (SourceSpan "" 8 10 8 12) "Int")
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
                  (EVar (VVal $ Located (SourceSpan "" 11 5 11 5) "f"))
                  [ EVar (VVal $ Located (SourceSpan "" 11 7 11 7) "x")
                  ]
                )
              }
          }
        ]

    it "rejects indented top-level declarations" $ do
      parse' parseModule "  f :: Int"
        `shouldFailWith` FancyError [SourcePos "" (mkPos 1) (mkPos 3)] [ErrorIndentation EQ (mkPos 1) (mkPos 3)]

  describe "expression" $ do
    it "parses complex expressions" $ do
      parse' expression "f (g x) 1" `shouldParse`
        EApp (
          App
          (EVar (VVal $ Located (SourceSpan "" 1 1 1 1) "f"))
          [ EParens $
              EApp $
                App
                (EVar (VVal $ Located (SourceSpan "" 1 4 1 4) "g"))
                [EVar (VVal $ Located (SourceSpan "" 1 6 1 6) "x")]
          , ELit (Located (SourceSpan "" 1 9 1 9) (LiteralInt 1))
          ]
        )

    it "parses multi-line expression" $ do
      parse' expression "f 1\n 2 3" `shouldParse`
        EApp (
          App
          (EVar (VVal $ Located (SourceSpan "" 1 1 1 1) "f"))
          [ ELit (Located (SourceSpan "" 1 3 1 3) (LiteralInt 1))
          , ELit (Located (SourceSpan "" 2 2 2 2) (LiteralInt 2))
          , ELit (Located (SourceSpan "" 2 4 2 4) (LiteralInt 3))
          ]
        )

  describe "externDecl" $ do
    it "parses extern declaration" $ do
      parse' externDecl "extern f :: Int"
        `shouldParse`
        Extern
          (Located (SourceSpan "" 1 8 1 8) "f")
          (TyCon (Located (SourceSpan "" 1 13 1 15) "Int"))
      parse' externDecl "extern f :: Int -> Double"
        `shouldParse`
        Extern
          (Located (SourceSpan "" 1 8 1 8) "f")
          ( TyCon (Located (SourceSpan "" 1 13 1 15) "Int")
            `TyFun`
            TyCon (Located (SourceSpan "" 1 20 1 25) "Double")
          )

  describe "bindingType" $ do
    it "parses binding types" $ do
      parse' bindingType "f :: Int"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          (Forall [] $ TyCon (Located (SourceSpan "" 1 6 1 8) "Int"))
      parse' bindingType "f :: Int -> Double"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          ( Forall [] $
            TyCon (Located (SourceSpan "" 1 6 1 8) "Int")
            `TyFun`
            TyCon (Located (SourceSpan "" 1 13 1 18) "Double")
          )

    it "parses polymorphic types" $ do
      parse' bindingType "f :: forall a. a"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          (Forall [Located (SourceSpan "" 1 13 1 13) "a"] $ TyVar (Located (SourceSpan "" 1 16 1 16) "a"))
      parse' bindingType "f :: forall a b. a -> b -> a"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          ( Forall
              [ Located (SourceSpan "" 1 13 1 13) "a"
              , Located (SourceSpan "" 1 15 1 15) "b"] $
            TyVar (Located (SourceSpan "" 1 18 1 18) "a")
            `TyFun`
            TyVar (Located (SourceSpan "" 1 23 1 23) "b")
            `TyFun`
            TyVar (Located (SourceSpan "" 1 28 1 28) "a")
          )

  describe "parseType" $ do
    it "handles simple types" $ do
      parse' parseType "A" `shouldParse` TyCon (Located (SourceSpan "" 1 1 1 1) "A")
      parse' parseType "A -> B"
        `shouldParse` (
          TyCon (Located (SourceSpan "" 1 1 1 1) "A")
          `TyFun`
          TyCon (Located (SourceSpan "" 1 6 1 6) "B")
        )
      parse' parseType "A -> B -> C"
        `shouldParse` (
          TyCon (Located (SourceSpan "" 1 1 1 1) "A")
          `TyFun`
          TyCon (Located (SourceSpan "" 1 6 1 6) "B")
          `TyFun`
          TyCon (Located (SourceSpan "" 1 11 1 11) "C")
        )

    it "handles parens" $ do
      parse' parseType "(A)" `shouldParse` TyCon (Located (SourceSpan "" 1 2 1 2) "A")
      parse' parseType "((X))" `shouldParse` TyCon (Located (SourceSpan "" 1 3 1 3) "X")
      parse' parseType "((A)) -> ((B))"
        `shouldParse` (
          TyCon (Located (SourceSpan "" 1 3 1 3) "A")
          `TyFun`
          TyCon (Located (SourceSpan "" 1 12 1 12) "B")
        )
      parse' parseType "(A -> B) -> C"
        `shouldParse` (
          ( TyCon (Located (SourceSpan "" 1 2 1 2) "A")
            `TyFun`
            TyCon (Located (SourceSpan "" 1 7 1 7) "B")
          )
          `TyFun`
          TyCon (Located (SourceSpan "" 1 13 1 13) "C")
        )
      parse' parseType "A -> (B -> C) -> D"
        `shouldParse` (
          TyCon (Located (SourceSpan "" 1 1 1 1) "A")
          `TyFun`
          ( TyCon (Located (SourceSpan "" 1 7 1 7) "B")
            `TyFun`
             TyCon (Located (SourceSpan "" 1 12 1 12) "C")
          )
          `TyFun`
          TyCon (Located (SourceSpan "" 1 18 1 18) "D")
        )

    it "should fail gracefully without infinite loops" $ do
      parse' parseType `shouldFailOn` ""
      parse' parseType `shouldFailOn` "()"
      parse' parseType `shouldFailOn` "(())"
      parse' parseType `shouldFailOn` "A ->"

  describe "expressionParens" $ do
    it "parses expressions in parens" $ do
      parse' expressionParens "(x)" `shouldParse` EParens (EVar (VVal $ Located (SourceSpan "" 1 2 1 2) "x"))
      parse' expressionParens "(f x)"
        `shouldParse`
        EParens
          (EApp
             (App
               (EVar (VVal $ Located (SourceSpan "" 1 2 1 2) "f"))
               [EVar (VVal $ Located (SourceSpan "" 1 4 1 4) "x")])
          )

  describe "ifExpression" $ do
    it "parses if expressions" $ do
      parse' ifExpression "if True then 1 else 2"
        `shouldParse`
        If
          (EVar (VCons $ Located (SourceSpan "" 1 4 1 7) "True"))
          (ELit (Located (SourceSpan "" 1 14 1 14) (LiteralInt 1)))
          (ELit (Located (SourceSpan "" 1 21 1 21) (LiteralInt 2)))
      parse' ifExpression "if f x then f y else g 2"
        `shouldParse`
        If
          (EApp $ App (EVar (VVal $ Located (SourceSpan "" 1 4 1 4) "f")) [EVar (VVal $ Located (SourceSpan "" 1 6 1 6) "x")])
          (EApp $ App (EVar (VVal $ Located (SourceSpan "" 1 13 1 13) "f")) [EVar (VVal $ Located (SourceSpan "" 1 15 1 15) "y")])
          (EApp $ App (EVar (VVal $ Located (SourceSpan "" 1 22 1 22) "g")) [ELit (Located (SourceSpan "" 1 24 1 24) (LiteralInt 2))])

  describe "literal" $ do
    it "can discriminate between integer and double" $ do
      parse' literal "1" `shouldParse` Located (SourceSpan "" 1 1 1 1) (LiteralInt 1)
      -- TODO: Trailing decimals?
      -- parse (literal <* eof) "" "2." `shouldParse` Located (SourceSpan "" 1 1 1 2) (LiteralInt 2)
      parse' literal "1.5" `shouldParse` Located (SourceSpan "" 1 1 1 3) (LiteralDouble 1.5)

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
