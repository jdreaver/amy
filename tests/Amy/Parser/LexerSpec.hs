{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Parser.LexerSpec
  ( spec
  ) where

import Data.Either (fromRight)
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

  describe "lineFold" $ do

    let
      integer = fromRight (error "Not an integer") <$> number

    it "works" $ do
      parse (lineFold integer) "" "1" `shouldParse` [1]
      parse (lineFold integer) "" "1\n 2\n 3" `shouldParse` [1, 2, 3]
      parse (spaceConsumerNewlines >> lineFold integer) "" "  1\n   2\n   3" `shouldParse` [1, 2, 3]

    it "can handle normal spaces" $ do
      parse (lineFold integer) "" "1 2 3" `shouldParse` [1, 2, 3]

    it "rejects things that aren't indented" $ do
      parse (lineFold integer >> eof) "" "1\n2"
        `shouldFailWith`
        err [SourcePos "" (mkPos 2) (mkPos 1)] (utok '2' <> eeof)

      parse (spaceConsumerNewlines >> lineFold integer >> eof) "" "  1\n  2"
        `shouldFailWith`
        err [SourcePos "" (mkPos 2) (mkPos 3)] (utok '2' <> eeof)
