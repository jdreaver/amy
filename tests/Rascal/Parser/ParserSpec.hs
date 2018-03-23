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
        [ ParserASTFunctionType
          ParserFunctionTypeDeclaration
          { parserFunctionTypeDeclarationName = "f"
          , parserFunctionTypeDeclarationTypeNames = ["Int", "Double"]
          }
        , ParserASTFunction
          ParserFunctionDeclaration
          { parserFunctionDeclarationName = "f"
          , parserFunctionDeclarationArgs = ["x"]
          , parserFunctionDeclarationBody =
            ParserASTLiteral (ParserLiteralInt 1)
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
          , ParserASTLiteral (ParserLiteralInt 1)
          ]
        )


  describe "functionType" $ do
    it "parses function types" $ do
      parse functionType "" "f :: Int" `shouldParse` ParserFunctionTypeDeclaration "f" ["Int"]
      parse functionType "" "f :: Int -> Double" `shouldParse` ParserFunctionTypeDeclaration "f" ["Int", "Double"]

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
        ParserFunctionApplication "f" [ParserASTVariable "x", ParserASTLiteral (ParserLiteralInt 1)]

sampleModule :: Text
sampleModule = [st|f :: Int -> Double;
f x = 1
|]
