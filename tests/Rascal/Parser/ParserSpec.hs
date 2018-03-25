{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Rascal.Parser.ParserSpec
  ( spec
  ) where

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Shakespeare.Text (st)

import Rascal.Parser.AST
import Rascal.Parser.Parser

spec :: Spec
spec = do

  describe "parserAST" $ do
    it "parses a small module" $ do
      parse parserAST "" sampleModule
        `shouldParse`
        ParserAST
        [ ParserASTBindingType
          ParserBindingTypeDeclaration
          { parserBindingTypeDeclarationName = "f"
          , parserBindingTypeDeclarationTypeNames = ["Int", "Double"]
          }
        , ParserASTBinding
          ParserBindingDeclaration
          { parserBindingDeclarationName = "f"
          , parserBindingDeclarationArgs = ["x"]
          , parserBindingDeclarationBody =
            ParserASTLiteral (LiteralInt 1)
          }
        ]

  describe "expression" $ do
    it "parses complex expressions" $ do
      parse expression "" "f (g x) 1" `shouldParse`
        ParserASTFunctionApplication (
          ParserFunctionApplication
          "f"
          [ ParserASTExpressionParens (
             ParserASTFunctionApplication $
             ParserFunctionApplication "g" [ParserASTVariable "x"]
            )
          , ParserASTLiteral (LiteralInt 1)
          ]
        )

  describe "externType" $ do
    it "parses extern declaration" $ do
      parse externType "" "extern f :: Int" `shouldParse` ParserBindingTypeDeclaration "f" ["Int"]
      parse externType "" "extern f :: Int -> Double" `shouldParse` ParserBindingTypeDeclaration "f" ["Int", "Double"]

  describe "bindingType" $ do
    it "parses binding types" $ do
      parse bindingType "" "f :: Int" `shouldParse` ParserBindingTypeDeclaration "f" ["Int"]
      parse bindingType "" "f :: Int -> Double" `shouldParse` ParserBindingTypeDeclaration "f" ["Int", "Double"]

  describe "expressionParens" $ do
    it "parses expressions in parens" $ do
      parse expressionParens "" "(x)" `shouldParse` ParserASTExpressionParens (ParserASTVariable "x")
      parse expressionParens "" "(f x)"
        `shouldParse`
        ParserASTExpressionParens (ParserASTFunctionApplication $ ParserFunctionApplication "f" [ParserASTVariable "x"])

  describe "functionApplication" $ do
    it "parses an application" $ do
      parse functionApplication "" "f x" `shouldParse` ParserFunctionApplication "f" [ParserASTVariable "x"]
      parse functionApplication "" "f x 1"
        `shouldParse`
        ParserFunctionApplication "f" [ParserASTVariable "x", ParserASTLiteral (LiteralInt 1)]

  describe "literal" $ do
    it "can discriminate between integer and double" $ do
      parse literal "" "1" `shouldParse` LiteralInt 1
      parse literal "" "1." `shouldParse` LiteralInt 1
      parse literal "" "1.5" `shouldParse` LiteralDouble 1.5

sampleModule :: Text
sampleModule = [st|f :: Int -> Double;
f x = 1
|]
