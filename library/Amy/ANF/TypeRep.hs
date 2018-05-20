{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.TypeRep
  ( typeRep
  ) where

import Data.Maybe (isNothing)
import Data.Text (Text)
import GHC.Word (Word32)

import Amy.ANF.AST as ANF
import Amy.Core.AST as C

-- | Describes how an Amy type is represented in LLVM.
data TypeRep
  = EnumRep !Word32
    -- ^ Compile as an int type with 'Word32' bits.
  | TaggedUnionRep !Text !Word32
    -- ^ Represent as a struct with a 'Word32'-sized integer tag and an integer
    -- pointer to data.
  deriving (Show, Eq, Ord)

-- | Decide how we are going to compile a type declaration.
typeRep :: C.TypeDeclaration -> ANF.Type
typeRep (C.TypeDeclaration tyName constructors) =
  case maybePrimitiveType tyName of
    Just prim -> prim
    Nothing ->
      -- Check if we can do an enum. This is when all constructors have no
      -- arguments.
      if all (isNothing . C.dataConDefinitionArgument) constructors
      then EnumType wordSize
      -- Can't do an enum. We'll have to use tagged pairs.
      else TaggedUnionType (tyConDefinitionName tyName) wordSize
 where
  -- Pick a proper integer size
  wordSize :: Word32
  wordSize =
   if | length constructors <= 2 -> 1
      | length constructors < (2 :: Int) ^ (8 :: Int) -> 8
      | otherwise -> 32

maybePrimitiveType :: C.TyConDefinition -> Maybe ANF.Type
maybePrimitiveType (C.TyConDefinition name _)
  -- TODO: Something more robust here besides text name matching.
  | name == "Int" = Just PrimIntType
  | name == "Double" = Just PrimDoubleType
  | otherwise = Nothing
