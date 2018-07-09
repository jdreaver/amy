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
import Amy.Syntax.Located
import Amy.Syntax.Monad
import Amy.Syntax.Parser

parse' :: AmyParser a -> Text -> Either (ParseError (Located AmyToken) Void) a
parse' parser input =
  let (Right tokens') = lexer "" input
  in parse (runAmyParser parser) "" tokens'

mkSpan :: Int -> Int -> Int -> Int -> SourceSpan
mkSpan startLine startCol endLine endCol = SourceSpan (mkSourcePos "" startLine startCol) (mkSourcePos "" endLine endCol)

spec :: Spec
spec = do

  describe "externDecl" $ do
    it "parses extern declaration" $ do
      parse' externDecl "extern f :: Int"
        `shouldParse`
        Extern
          (Located (mkSpan 1 8 1 9) "f")
          (LocatedType (mkSpan 1 13 1 16) (TyCon "Int"))
      parse' externDecl "extern f :: Int -> Double"
        `shouldParse`
        Extern
          (Located (mkSpan 1 8 1 9) "f")
          ( LocatedType (mkSpan 1 13 1 16) (TyCon "Int")
            `TyFun`
            LocatedType (mkSpan 1 20 1 26) (TyCon "Double")
          )

  describe "bindingType" $ do
    it "parses binding types" $ do
      parse' bindingType "f :: Int"
        `shouldParse`
        BindingType
          (Located (mkSpan 1 1 1 2) "f")
          (LocatedType (mkSpan 1 6 1 9) (TyCon "Int"))
      parse' bindingType "f :: Int -> Double"
        `shouldParse`
        BindingType
          (Located (mkSpan 1 1 1 2) "f")
          ( LocatedType (mkSpan 1 6 1 9) (TyCon "Int")
            `TyFun`
            LocatedType (mkSpan 1 13 1 19) (TyCon "Double")
          )

    it "parses polymorphic types" $ do
      parse' bindingType "f :: forall a. a"
        `shouldParse`
        BindingType
          (Located (mkSpan 1 1 1 2) "f")
          (TyForall ["a"] $ LocatedType (mkSpan 1 16 1 17) (TyVar "a"))
      parse' bindingType "f :: forall a b. a -> b -> a"
        `shouldParse`
        BindingType
          (Located (mkSpan 1 1 1 2) "f")
          ( TyForall ["a", "b"] $
              LocatedType (mkSpan 1 18 1 19) (TyVar "a")
              `TyFun`
              LocatedType (mkSpan 1 23 1 24) (TyVar "b")
              `TyFun`
              LocatedType (mkSpan 1 28 1 29) (TyVar "a")
          )

  describe "parseType" $ do
    it "handles simple terms" $ do
      parse' parseType "A" `shouldParse` LocatedType (mkSpan 1 1 1 2) (TyCon "A")
      parse' parseType "a" `shouldParse` LocatedType (mkSpan 1 1 1 2) (TyVar "a")

    it "handles terms with args" $ do
      parse' parseType "A a" `shouldParse`
        (LocatedType (mkSpan 1 1 1 2) (TyCon "A") `TyApp` LocatedType (mkSpan 1 3 1 4) (TyVar "a"))

    it "tightly binds constructor applications" $ do
      parse' parseType "A B C" `shouldParse`
        TyApp
          ( TyApp
            (LocatedType (mkSpan 1 1 1 2) (TyCon "A"))
            (LocatedType (mkSpan 1 3 1 4) (TyCon "B"))
          )
          (LocatedType (mkSpan 1 5 1 6) (TyCon "C"))

    it "handles terms with args and parens" $ do
      parse' parseType "A (B b) a" `shouldParse`
        TyApp
        ( TyApp
          (LocatedType (mkSpan 1 1 1 2) (TyCon "A"))
          ( TyApp
            (LocatedType (mkSpan 1 4 1 5) (TyCon "B"))
            (LocatedType (mkSpan 1 6 1 7) (TyVar "b"))
          )
        )
        (LocatedType (mkSpan 1 9 1 10) (TyVar "a"))

  describe "parseType" $ do
    it "handles simple types" $ do
      parse' parseType "A" `shouldParse` LocatedType (mkSpan 1 1 1 2) (TyCon "A")
      parse' parseType "A -> B"
        `shouldParse` (
          LocatedType (mkSpan 1 1 1 2) (TyCon "A")
          `TyFun`
          LocatedType (mkSpan 1 6 1 7) (TyCon "B")
        )
      parse' parseType "A -> B -> C"
        `shouldParse` (
          LocatedType (mkSpan 1 1 1 2) (TyCon "A")
          `TyFun`
          LocatedType (mkSpan 1 6 1 7) (TyCon "B")
          `TyFun`
          LocatedType (mkSpan 1 11 1 12) (TyCon "C")
        )

    it "handles parens" $ do
      parse' parseType "(A)" `shouldParse` LocatedType (mkSpan 1 2 1 3) (TyCon "A")
      parse' parseType "((X))" `shouldParse` LocatedType (mkSpan 1 3 1 4) (TyCon "X")

    it "handles parens with functions" $ do
      parse' parseType "((A)) -> ((B))"
        `shouldParse` (
          LocatedType (mkSpan 1 3 1 4) (TyCon "A")
          `TyFun`
          LocatedType (mkSpan 1 12 1 13) (TyCon "B")
        )
      parse' parseType "(A -> B) -> C"
        `shouldParse` (
          ( LocatedType (mkSpan 1 2 1 3) (TyCon "A")
            `TyFun`
            LocatedType (mkSpan 1 7 1 8) (TyCon "B")
          )
          `TyFun`
          LocatedType (mkSpan 1 13 1 14) (TyCon "C")
        )
      parse' parseType "A -> (B -> C) -> D"
        `shouldParse` (
          LocatedType (mkSpan 1 1 1 2) (TyCon "A")
          `TyFun`
          ( LocatedType (mkSpan 1 7 1 8) (TyCon "B")
            `TyFun`
             LocatedType (mkSpan 1 12 1 13) (TyCon "C")
          )
          `TyFun`
          LocatedType (mkSpan 1 18 1 19) (TyCon "D")
        )

    it "should fail gracefully without infinite loops" $ do
      parse' parseType `shouldFailOn` ""
      parse' parseType `shouldFailOn` "()"
      parse' parseType `shouldFailOn` "(())"
      parse' parseType `shouldFailOn` "A ->"

  describe "expressionParens" $ do
    it "parses expressions in parens" $ do
      parse' expressionParens "(x)" `shouldParse` EParens (EVar (Located (mkSpan 1 2 1 3) "x"))
      parse' expressionParens "(f x)"
        `shouldParse`
        EParens
          (EApp
            (EVar (Located (mkSpan 1 2 1 3) "f"))
            (EVar (Located (mkSpan 1 4 1 5) "x"))
          )

  describe "ifExpression" $ do
    it "parses if expressions" $ do
      parse' ifExpression "if True then 1 else 2"
        `shouldParse`
        If
          (ECon (Located (mkSpan 1 4 1 8) "True"))
          (ELit (Located (mkSpan 1 14 1 15) (LiteralInt 1)))
          (ELit (Located (mkSpan 1 21 1 22) (LiteralInt 2)))
          (mkSpan 1 1 1 22)
      parse' ifExpression "if f x then f y else g 2"
        `shouldParse`
        If
          (EApp (EVar (Located (mkSpan 1 4 1 5) "f")) (EVar (Located (mkSpan 1 6 1 7) "x")))
          (EApp (EVar (Located (mkSpan 1 13 1 14) "f")) (EVar (Located (mkSpan 1 15 1 16) "y")))
          (EApp (EVar (Located (mkSpan 1 22 1 23) "g")) (ELit (Located (mkSpan 1 24 1 25) (LiteralInt 2))))
          (mkSpan 1 1 1 25)

  describe "literal" $ do
    it "can discriminate between integer and double" $ do
      parse' literal "1" `shouldParse` Located (mkSpan 1 1 1 2) (LiteralInt 1)
      -- TODO: Trailing decimals?
      -- parse (literal <* eof) "" "2." `shouldParse` Located (mkSpan 1 1 1 2) (LiteralInt 2)
      parse' literal "1.5" `shouldParse` Located (mkSpan 1 1 1 4) (LiteralDouble 1.5)
