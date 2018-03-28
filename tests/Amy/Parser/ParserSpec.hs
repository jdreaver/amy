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

import Amy.AST
import Amy.Parser.Parser

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

    it "rejects indented top-level declarations" $ do
      parse parserAST "" "  f :: Int"
        `shouldFailWith` FancyError [SourcePos "" (mkPos 1) (mkPos 3)] [ErrorIndentation EQ (mkPos 1) (mkPos 3)]

  describe "expression" $ do
    it "parses complex expressions" $ do
      parse expression "" "f (g x) 1" `shouldParse`
        ExpressionFunctionApplication (
          FunctionApplication
          (ExpressionVariable (Variable "f" ()))
          [ ExpressionParens (
             ExpressionFunctionApplication $
             FunctionApplication
               (ExpressionVariable (Variable "g" ()))
               [ExpressionVariable (Variable "x" ())]
               ()
            )
          , ExpressionLiteral (LiteralInt 1)
          ]
          ()
        )

    it "parses multi-line expression" $ do
      parse expression "" "f 1\n 2 3" `shouldParse`
        ExpressionFunctionApplication (
          FunctionApplication
          (ExpressionVariable (Variable "f" ()))
          [ ExpressionLiteral (LiteralInt 1)
          , ExpressionLiteral (LiteralInt 2)
          , ExpressionLiteral (LiteralInt 3)
          ]
          ()
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
        ExpressionParens (
          ExpressionFunctionApplication
            (FunctionApplication
               (ExpressionVariable (Variable "f" ()))
               [ExpressionVariable (Variable "x" ())]
               ()
            ))

  describe "ifExpression" $ do
    it "parses if expressions" $ do
      parse ifExpression "" "if True then 1 else 2"
        `shouldParse`
        If
          (ExpressionLiteral (LiteralBool True))
          (ExpressionLiteral (LiteralInt 1))
          (ExpressionLiteral (LiteralInt 2))
          ()
      parse ifExpression "" "if f x then f y else g 2"
        `shouldParse`
        If
          (ExpressionFunctionApplication $
             FunctionApplication
               (ExpressionVariable (Variable "f" ()))
               [ExpressionVariable (Variable "x" ())]
               ()
          )
          (ExpressionFunctionApplication $
             FunctionApplication
               (ExpressionVariable (Variable "f" ()))
               [ExpressionVariable (Variable "y" ())]
               ()
          )
          (ExpressionFunctionApplication $
             FunctionApplication
               (ExpressionVariable (Variable "g" ()))
               [ExpressionLiteral (LiteralInt 2)]
               ()
          )
          ()

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

|]
