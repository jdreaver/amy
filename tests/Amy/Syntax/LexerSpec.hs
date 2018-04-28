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

  let
    integer = fromRight (error "Not an integer") . locatedValue <$> number

  describe "indentedBlock" $ do

    it "handles single items" $ do
      parse' (indentedBlock integer) "1" `shouldParse` [1]

    it "handles items with semicolons" $ do
      parse' (spaceConsumer *> indentedBlock integer <* eof) " 1\n 2; 3; 4\n 5" `shouldParse` [1..5]

  describe "lineFold" $ do

    it "works" $ do
      parse' (lineFold integer) "1" `shouldParse` [1]
      parse' (lineFold integer) "1\n 2\n 3" `shouldParse` [1, 2, 3]
      parse' (spaceConsumerNewlines >> lineFold integer) "  1\n   2\n   3" `shouldParse` [1, 2, 3]

    it "can handle normal spaces" $ do
      parse' (lineFold integer) "1 2 3" `shouldParse` [1, 2, 3]

    it "rejects things that aren't indented" $ do
      parse' (lineFold integer >> eof) "1\n2"
        `shouldFailWith`
        err [SourcePos "" (mkPos 2) (mkPos 1)] (utok '2' <> eeof <> elabel "indentation past column 1")

      parse' (spaceConsumerNewlines >> lineFold integer >> eof) "  1\n  2"
        `shouldFailWith`
        err [SourcePos "" (mkPos 2) (mkPos 3)] (utok '2' <> eeof <> elabel "indentation past column 3")
