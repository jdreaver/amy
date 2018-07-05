{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Syntax.ParserSpec
  ( spec
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Amy.Syntax.AST
import Amy.Syntax.Lexer
import Amy.Syntax.Monad
import Amy.Syntax.Parser

parse' :: AmyParser a -> Text -> Either (ParseError (Located AmyToken) Void) a
parse' parser input =
  let (Right tokens') = lexer "" input
  in parse (runAmyParser parser) "" tokens'

spec :: Spec
spec = do

  describe "externDecl" $ do
    it "parses extern declaration" $ do
      parse' externDecl "extern f :: Int"
        `shouldParse`
        Extern
          (Located (SourceSpan "" 1 8 1 8) "f")
          (TyCon $ Located (SourceSpan "" 1 13 1 15) "Int")
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
          (TyCon (Located (SourceSpan "" 1 6 1 8) "Int"))
      parse' bindingType "f :: Int -> Double"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          ( TyCon (Located (SourceSpan "" 1 6 1 8) "Int")
            `TyFun`
            TyCon (Located (SourceSpan "" 1 13 1 18) "Double")
          )

    it "parses polymorphic types" $ do
      parse' bindingType "f :: forall a. a"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          (TyForall [Located (SourceSpan "" 1 13 1 13) "a"] $ TyVar (Located (SourceSpan "" 1 16 1 16) "a"))
      parse' bindingType "f :: forall a b. a -> b -> a"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          ( TyForall
              [ Located (SourceSpan "" 1 13 1 13) "a"
              , Located (SourceSpan "" 1 15 1 15) "b"] $
            TyVar (Located (SourceSpan "" 1 18 1 18) "a")
            `TyFun`
            TyVar ( Located (SourceSpan "" 1 23 1 23) "b")
            `TyFun`
            TyVar ( Located (SourceSpan "" 1 28 1 28) "a")
          )

  describe "parseType" $ do
    it "handles simple terms" $ do
      parse' parseType "A" `shouldParse` TyCon (Located (SourceSpan "" 1 1 1 1) "A")
      parse' parseType "a" `shouldParse` TyVar (Located (SourceSpan "" 1 1 1 1) "a")

    it "handles terms with args" $ do
      parse' parseType "A a" `shouldParse`
        TyApp (TyCon $ Located (SourceSpan "" 1 1 1 1) "A") (TyVar (Located (SourceSpan "" 1 3 1 3) "a"))

    it "tightly binds constructor applications" $ do
      parse' parseType "A B C" `shouldParse`
        TyApp
          ( TyApp
            (TyCon $ Located (SourceSpan "" 1 1 1 1) "A")
            (TyCon (Located (SourceSpan "" 1 3 1 3) "B"))
          )
          (TyCon (Located (SourceSpan "" 1 5 1 5) "C"))

    it "handles terms with args and parens" $ do
      parse' parseType "A (B b) a" `shouldParse`
        TyApp
        ( TyApp
          (TyCon $ Located (SourceSpan "" 1 1 1 1) "A")
          ( TyApp
            (TyCon $ Located (SourceSpan "" 1 4 1 4) "B")
            (TyVar (Located (SourceSpan "" 1 6 1 6) "b"))
          )
        )
        (TyVar (Located (SourceSpan "" 1 9 1 9) "a"))

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

    it "handles parens with functions" $ do
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
      parse' expressionParens "(x)" `shouldParse` EParens (EVar (Located (SourceSpan "" 1 2 1 2) "x"))
      parse' expressionParens "(f x)"
        `shouldParse`
        EParens
          (EApp
            (EVar (Located (SourceSpan "" 1 2 1 2) "f"))
            (EVar (Located (SourceSpan "" 1 4 1 4) "x"))
          )

  describe "ifExpression" $ do
    it "parses if expressions" $ do
      parse' ifExpression "if True then 1 else 2"
        `shouldParse`
        If
          (ECon (Located (SourceSpan "" 1 4 1 7) "True"))
          (ELit (Located (SourceSpan "" 1 14 1 14) (LiteralInt 1)))
          (ELit (Located (SourceSpan "" 1 21 1 21) (LiteralInt 2)))
          (SourceSpan "" 1 1 1 21)
      parse' ifExpression "if f x then f y else g 2"
        `shouldParse`
        If
          (EApp (EVar (Located (SourceSpan "" 1 4 1 4) "f")) (EVar (Located (SourceSpan "" 1 6 1 6) "x")))
          (EApp (EVar (Located (SourceSpan "" 1 13 1 13) "f")) (EVar (Located (SourceSpan "" 1 15 1 15) "y")))
          (EApp (EVar (Located (SourceSpan "" 1 22 1 22) "g")) (ELit (Located (SourceSpan "" 1 24 1 24) (LiteralInt 2))))
          (SourceSpan "" 1 1 1 24)

  describe "literal" $ do
    it "can discriminate between integer and double" $ do
      parse' literal "1" `shouldParse` Located (SourceSpan "" 1 1 1 1) (LiteralInt 1)
      -- TODO: Trailing decimals?
      -- parse (literal <* eof) "" "2." `shouldParse` Located (SourceSpan "" 1 1 1 2) (LiteralInt 2)
      parse' literal "1.5" `shouldParse` Located (SourceSpan "" 1 1 1 3) (LiteralDouble 1.5)
