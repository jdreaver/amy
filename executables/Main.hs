module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS8
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec

import Amy.AST
import Amy.Codegen
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
        process (pack stdinString)

    -- User passed a file path
    [path] -> TIO.readFile path >>= process
    _ -> die "Usage: amy [file]"
 where
  loop = do
    minput <- getInputLine "amy> "
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

    eTyped :: Either String (AST ValueName Type)
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
