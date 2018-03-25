module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS8
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec

import Rascal.Codegen
import Rascal.Renamer
import Rascal.Parser
import Rascal.TypeCheck

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runInputT defaultSettings loop
    [arg] -> process (pack arg)
    _ -> die "Usage: rascal [optional program text]"
 where
  loop = do
    minput <- getInputLine "rascal> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        liftIO $ process (pack input)
        loop

process :: Text -> IO ()
process input =
  let
    mapLeft :: (a -> b) -> Either a c -> Either b c
    mapLeft _ (Right r) = Right r
    mapLeft f (Left x) = Left $ f x

    showLeft errName = mapLeft (\x -> errName ++ " error! " ++ show x)

    eTyped :: Either String TypeCheckAST
    eTyped = do
      parsed <- mapLeft parseErrorPretty $ parse parserAST "<repl>" input
      renamed <- showLeft "Renamer" $ rename parsed
      typed <- showLeft "TypeCheck" $ typeCheck renamed
      pure typed
  in
    case eTyped of
      Left err -> hPutStrLn stderr err
      Right typed -> do
        codegenString <- generateLLVMIR typed
        BS8.putStrLn codegenString
