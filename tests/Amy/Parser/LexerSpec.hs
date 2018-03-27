{-# LANGUAGE OverloadedStrings #-}

module Amy.Parser.LexerSpec
  ( spec
  ) where

import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Amy.Parser.Lexer

spec :: Spec
spec = do

  describe "text" $ do

    it "parses strings properly" $ do
      parse text "" "\"Hello\"" `shouldParse` "Hello"

    it "parses strings with nested quotes" $ do
      parse text "" "\"Hello \\\"Bob\\\"\"" `shouldParse` "Hello \"Bob\""

  describe "sepByNonEmpty" $ do

    it "parses into a NonEmpty" $ do

      parse (sepByNonEmpty integer ",") "" "1,2,3" `shouldParse` NE.fromList [1, 2, 3]

      parse (sepByNonEmpty integer ",") "" `shouldFailOn` ""
