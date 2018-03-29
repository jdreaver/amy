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

import Amy.AST
import Amy.Codegen
import Amy.Errors
import Amy.Names
import Amy.Renamer
import Amy.Parser
import Amy.Type
import Amy.TypeCheck

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      -- See if there is anything in stding
      stdinString <- getContents
      if null stdinString
      then
        -- Run REPL
        runInputT defaultSettings loop
      else
        -- Assume a program was passed in
        process "<stdin>" (pack stdinString)

    -- User passed a file path
    [path] -> TIO.readFile path >>= process path
    _ -> die "Usage: amy [file]"
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
    eTyped :: Either [Error] (AST ValueName Type)
    eTyped = do
      parsed <- first ((:[]) . ParserError) $ parse parserAST inputFile input
      renamed <- rename parsed
      typed <- typeCheck renamed
      pure typed
  in
    case eTyped of
      Left err -> hPutStrLn stderr (intercalate "\n" $ showError <$> err)
      Right typed -> do
        codegenString <- generateLLVMIR typed
        BS8.putStrLn codegenString
