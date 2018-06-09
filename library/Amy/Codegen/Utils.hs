{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Utils
  ( identToName
  , textToName
  , stringToName
  , textPointerType
  , textPointerName
  , textPointerConstant
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as BSS
import Data.Char (digitToInt)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import LLVM.AST as LLVM
import qualified LLVM.AST.Constant as C

import Amy.ANF.AST as ANF

identToName :: IdentName -> LLVM.Name
identToName (IdentName name') = textToName name'

textToName :: Text -> Name
textToName = Name . BSS.toShort . encodeUtf8

stringToName :: String -> Name
stringToName = Name . BSS.toShort . BS8.pack

textPointerType :: TextPointer -> LLVM.Type
textPointerType (TextPointer _ text) =
  -- Length of text plus one extra for null character string terminator
  LLVM.ArrayType (fromIntegral $ T.length text) (LLVM.IntegerType 8)

textPointerName :: TextPointer -> LLVM.Name
textPointerName (TextPointer id' _) = textToName $ "$str." <> pack (show id')

textPointerConstant :: TextPointer -> C.Constant
textPointerConstant (TextPointer _ text) = C.Array (LLVM.IntegerType 8) array
 where
  chars = (fromIntegral . digitToInt <$> unpack text) ++ [0]
  array = C.Int 8 <$> chars
