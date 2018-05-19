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
import Amy.Syntax.Monad
import Amy.Syntax.Parser

parse' :: AmyParser a -> Text -> Either (ParseError Char Void) a
parse' parser = parse (runAmyParser parser) ""

spec :: Spec
spec = do

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
          (TyTerm $ TyCon (TyConInfo "Int" [] (SourceSpan "" 1 13 1 15)))
      parse' externDecl "extern f :: Int -> Double"
        `shouldParse`
        Extern
          (Located (SourceSpan "" 1 8 1 8) "f")
          ( TyTerm (TyCon (TyConInfo "Int" [] (SourceSpan "" 1 13 1 15)))
            `TyFun`
            TyTerm (TyCon (TyConInfo "Double" [] (SourceSpan "" 1 20 1 25)))
          )

  describe "bindingType" $ do
    it "parses binding types" $ do
      parse' bindingType "f :: Int"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          (Forall [] $ TyTerm $ TyCon (TyConInfo "Int" [] (SourceSpan "" 1 6 1 8)))
      parse' bindingType "f :: Int -> Double"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          ( Forall [] $
            TyTerm (TyCon (TyConInfo "Int" [] (SourceSpan "" 1 6 1 8)))
            `TyFun`
            TyTerm (TyCon (TyConInfo "Double" [] (SourceSpan "" 1 13 1 18)))
          )

    it "parses polymorphic types" $ do
      parse' bindingType "f :: forall a. a"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          (Forall [Located (SourceSpan "" 1 13 1 13) "a"] $ TyTerm (TyVar (Located (SourceSpan "" 1 16 1 16) "a")))
      parse' bindingType "f :: forall a b. a -> b -> a"
        `shouldParse`
        BindingType
          (Located (SourceSpan "" 1 1 1 1) "f")
          ( Forall
              [ Located (SourceSpan "" 1 13 1 13) "a"
              , Located (SourceSpan "" 1 15 1 15) "b"] $
            TyTerm (TyVar (Located (SourceSpan "" 1 18 1 18) "a"))
            `TyFun`
            TyTerm (TyVar ( Located (SourceSpan "" 1 23 1 23) "b"))
            `TyFun`
            TyTerm (TyVar ( Located (SourceSpan "" 1 28 1 28) "a"))
          )

  describe "typeTerm" $ do
    it "handles simple terms" $ do
      parse' typeTerm "A" `shouldParse` TyCon (TyConInfo "A" [] (SourceSpan "" 1 1 1 1))
      parse' typeTerm "a" `shouldParse` TyVar (Located (SourceSpan "" 1 1 1 1) "a")

    it "handles terms with args" $ do
      parse' typeTerm "A a" `shouldParse`
        TyCon (TyConInfo "A" [TyVar (Located (SourceSpan "" 1 3 1 3) "a")] (SourceSpan "" 1 1 1 1))

    it "tightly binds constructor applications" $ do
      parse' typeTerm "A B C" `shouldParse`
        TyCon (
          TyConInfo "A"
          [ TyCon (TyConInfo "B" [] (SourceSpan "" 1 3 1 3))
          , TyCon (TyConInfo "C" [] (SourceSpan "" 1 5 1 5))
          ]
          (SourceSpan "" 1 1 1 1)
        )

    it "handles terms with args and parens" $ do
      parse' typeTerm "A (B b) a" `shouldParse`
        TyCon (
          TyConInfo "A"
          [ TyCon (
              TyConInfo "B"
              [ TyVar (Located (SourceSpan "" 1 6 1 6) "b")
              ]
              (SourceSpan "" 1 4 1 4)
            )
          , TyVar (Located (SourceSpan "" 1 9 1 9) "a")
          ]
          (SourceSpan "" 1 1 1 1)
        )

  describe "parseType" $ do
    it "handles simple types" $ do
      parse' parseType "A" `shouldParse` TyTerm (TyCon (TyConInfo "A" [] (SourceSpan "" 1 1 1 1)))
      parse' parseType "A -> B"
        `shouldParse` (
          TyTerm (TyCon (TyConInfo "A" [] (SourceSpan "" 1 1 1 1)))
          `TyFun`
          TyTerm (TyCon (TyConInfo "B" [] (SourceSpan "" 1 6 1 6)))
        )
      parse' parseType "A -> B -> C"
        `shouldParse` (
          TyTerm (TyCon (TyConInfo "A" [] (SourceSpan "" 1 1 1 1)))
          `TyFun`
          TyTerm (TyCon (TyConInfo "B" [] (SourceSpan "" 1 6 1 6)))
          `TyFun`
          TyTerm (TyCon (TyConInfo "C" [] (SourceSpan "" 1 11 1 11)))
        )

    it "handles parens" $ do
      parse' parseType "(A)" `shouldParse` TyTerm (TyCon (TyConInfo "A" [] (SourceSpan "" 1 2 1 2)))
      parse' parseType "((X))" `shouldParse` TyTerm (TyCon (TyConInfo "X" [] (SourceSpan "" 1 3 1 3)))

    it "handles parens with functions" $ do
      parse' parseType "((A)) -> ((B))"
        `shouldParse` (
          TyTerm (TyCon (TyConInfo "A" [] (SourceSpan "" 1 3 1 3)))
          `TyFun`
          TyTerm (TyCon (TyConInfo "B" [] (SourceSpan "" 1 12 1 12)))
        )
      parse' parseType "(A -> B) -> C"
        `shouldParse` (
          ( TyTerm (TyCon (TyConInfo "A" [] (SourceSpan "" 1 2 1 2)))
            `TyFun`
            TyTerm (TyCon (TyConInfo "B" [] (SourceSpan "" 1 7 1 7)))
          )
          `TyFun`
          TyTerm (TyCon (TyConInfo "C" [] (SourceSpan "" 1 13 1 13)))
        )
      parse' parseType "A -> (B -> C) -> D"
        `shouldParse` (
          TyTerm (TyCon (TyConInfo "A" [] (SourceSpan "" 1 1 1 1)))
          `TyFun`
          ( TyTerm (TyCon (TyConInfo "B" [] (SourceSpan "" 1 7 1 7)))
            `TyFun`
             TyTerm (TyCon (TyConInfo "C" [] (SourceSpan "" 1 12 1 12)))
          )
          `TyFun`
          TyTerm (TyCon (TyConInfo "D" [] (SourceSpan "" 1 18 1 18)))
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
