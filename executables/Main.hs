module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS8
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import LLVM.AST (Module)
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec

import Amy.Codegen
import Amy.Errors
import Amy.Renamer
import Amy.Parser
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
    eModule :: Either [Error] LLVM.AST.Module
    eModule = do
      parsed <- first ((:[]) . ParserError) $ parse parseModule inputFile input
      renamed <- rename parsed
      typed <- typeCheck renamed
      codegenPure typed
  in do
    eCodegenString <- traverse generateLLVMIR eModule
    either (hPutStrLn stderr . intercalate "\n" . fmap showError) BS8.putStrLn eCodegenString
