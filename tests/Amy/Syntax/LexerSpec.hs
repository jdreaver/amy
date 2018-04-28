{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Syntax.LexerSpec
  ( spec
  ) where

import Data.Either (fromRight)
import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Amy.Syntax.Lexer
import Amy.Syntax.Located
import Amy.Syntax.Monad

parse' :: AmyParser a -> Text -> Either (ParseError Char Void) a
parse' parser = parse (runAmyParser parser) ""

spec :: Spec
spec = do

  describe "identifer" $ do
    it "parses identifiers" $ do
      parse' identifier "hello" `shouldParse` Located (SourceSpan "" 1 1 1 5) "hello"
      parse' identifier "hello'" `shouldParse` Located (SourceSpan "" 1 1 1 6) "hello'"

  describe "text" $ do

    it "parses strings properly" $ do
      parse' text "\"Hello\"" `shouldParse` "Hello"

    it "parses strings with nested quotes" $ do
      parse' text "\"Hello \\\"Bob\\\"\"" `shouldParse` "Hello \"Bob\""

  describe "lineFold" $ do

    let
      integer = fromRight (error "Not an integer") . locatedValue <$> number

    it "works" $ do
      parse' (lineFold integer) "1" `shouldParse` [1]
      parse' (lineFold integer) "1\n 2\n 3" `shouldParse` [1, 2, 3]
      parse' (spaceConsumerNewlines >> lineFold integer) "  1\n   2\n   3" `shouldParse` [1, 2, 3]

    it "can handle normal spaces" $ do
      parse' (lineFold integer) "1 2 3" `shouldParse` [1, 2, 3]

    it "rejects things that aren't indented" $ do
      parse' (lineFold integer >> eof) "1\n2"
        `shouldFailWith`
        err [SourcePos "" (mkPos 2) (mkPos 1)] (utok '2' <> eeof)

      parse' (spaceConsumerNewlines >> lineFold integer >> eof) "  1\n  2"
        `shouldFailWith`
        err [SourcePos "" (mkPos 2) (mkPos 3)] (utok '2' <> eeof)
