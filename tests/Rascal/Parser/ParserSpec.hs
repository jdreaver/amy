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

import Rascal.AST
import Rascal.Parser.Parser

spec :: Spec
spec = do

  describe "parserAST" $ do
    it "parses a small module" $ do
      parse parserAST "" sampleModule
        `shouldParse`
        AST
        [ TopLevelBindingType
          BindingType
          { bindingTypeName = "f"
          , bindingTypeTypeNames = ["Int", "Double"]
          }
        , TopLevelBindingValue
          BindingValue
          { bindingValueName = "f"
          , bindingValueArgs = ["x"]
          , bindingValueType = ()
          , bindingValueBody =
            ExpressionLiteral (LiteralInt 1)
          }
        ]

  describe "expression" $ do
    it "parses complex expressions" $ do
      parse expression "" "f (g x) 1" `shouldParse`
        ExpressionFunctionApplication (
          FunctionApplication
          "f"
          ()
          [ ExpressionParens (
             ExpressionFunctionApplication $
             FunctionApplication "g" () [ExpressionVariable (Variable "x" ())]
            )
          , ExpressionLiteral (LiteralInt 1)
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
      parse expressionParens "" "(x)" `shouldParse` ExpressionParens (ExpressionVariable (Variable "x" ()))
      parse expressionParens "" "(f x)"
        `shouldParse`
        ExpressionParens (ExpressionFunctionApplication $ FunctionApplication "f" () [ExpressionVariable (Variable "x" ())])

  describe "functionApplication" $ do
    it "parses an application" $ do
      parse functionApplication "" "f x" `shouldParse` FunctionApplication "f" () [ExpressionVariable (Variable "x" ())]
      parse functionApplication "" "f x 1"
        `shouldParse`
        FunctionApplication "f" () [ExpressionVariable (Variable "x" ()), ExpressionLiteral (LiteralInt 1)]

  describe "literal" $ do
    it "can discriminate between integer and double" $ do
      parse literal "" "1" `shouldParse` LiteralInt 1
      parse literal "" "1." `shouldParse` LiteralInt 1
      parse literal "" "1.5" `shouldParse` LiteralDouble 1.5

sampleModule :: Text
sampleModule = [st|f :: Int -> Double;
f x = 1
|]
