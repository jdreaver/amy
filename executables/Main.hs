module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS8
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec

import Amy.ANF
import Amy.Codegen
import Amy.Errors
import Amy.Renamer
import Amy.Syntax
import Amy.TypeCheck

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"] -> runInputT defaultSettings loop
    ["-"] -> getContents >>= process "<stdin>" . pack
    [path] -> TIO.readFile path >>= process path
    _ -> die "Usage: amy [file|repl|-]"
 where
  loop = do
    minput <- getInputLine "amy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        liftIO $ process "<repl>" (pack input)
        loop

process :: FilePath -> Text -> IO ()
process inputFile input =
  let
    eModule :: Either [Error] TModule
    eModule = do
      parsed <- first ((:[]) . ParserError) $ parse parseModule inputFile input
      renamed <- rename parsed
      first (:[]) $ inferModule renamed
  in do
    eCodegenString <- traverse (generateLLVMIR . codegenModule . normalizeModule) eModule
    either (hPutStrLn stderr . intercalate "\n" . fmap showError) BS8.putStrLn eCodegenString
