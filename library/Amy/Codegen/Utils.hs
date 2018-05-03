module Amy.Codegen.Utils
  ( identToName
  , textToName
  , stringToName
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as BSS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import LLVM.AST as LLVM

import Amy.ANF.AST as ANF

identToName :: ANF.Ident -> LLVM.Name
identToName (ANF.Ident name' _ _) = textToName name'

textToName :: Text -> Name
textToName = Name . BSS.toShort . encodeUtf8

stringToName :: String -> Name
stringToName = Name . BSS.toShort . BS8.pack
