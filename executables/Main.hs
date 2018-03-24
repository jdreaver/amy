module Main
  ( main
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import System.Exit (die)
import System.Console.Haskeline
import Text.Megaparsec

import Rascal.Renamer
import Rascal.Parser

main :: IO ()
main = runInputT defaultSettings loop
 where
  loop = do
    minput <- getInputLine "rascal> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        liftIO $ process (pack input)
        loop

process :: Text -> IO ()
process input = do
  parsed <-
    case parse parserAST "<repl>" input of
      Left err -> die $ show err
      Right p -> pure p
  print parsed

  renamed <-
    case rename parsed of
      Left err -> die $ show err
      Right r -> pure r
  print renamed
