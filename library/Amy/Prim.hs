{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Encode primitive types operations for the compiler. Primitive types are
-- types that map directly to raw values on the stack, like machine integers
-- and doubles. Primitive operations are functions that are generated with a
-- machine instruction, and the code generator needs to know about them to
-- generate the machine code. Because of this, these types/operations need to
-- be included in the compiler and not in a standard library.

module Amy.Prim
  ( PrimitiveType(..)
  , readPrimitiveType
  , PrimitiveFunction(..)
  , PrimitiveFunctionName(..)
  , allPrimitiveFunctionNames
  , showPrimitiveFunctionName
  , readPrimitiveFunctionName
  , primitiveFunction
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

data PrimitiveType
  = IntType
  | DoubleType
  | BoolType
  deriving (Show, Eq)

readPrimitiveType :: Text -> Maybe PrimitiveType
readPrimitiveType t =
  case t of
    "Int" -> Just IntType
    "Double" -> Just DoubleType
    "Bool" -> Just BoolType
    _ -> Nothing

data PrimitiveFunction
  = PrimitiveFunction
  { primitiveFunctionName :: !PrimitiveFunctionName
  , primitiveFunctionType :: !(NonEmpty PrimitiveType)
  } deriving (Show, Eq)

data PrimitiveFunctionName
    -- Int
  = PrimIAdd
  | PrimISub
  | PrimIEquals
  | PrimIGreaterThan
  | PrimILessThan

    -- Double
  | PrimDAdd
  | PrimDSub
  deriving (Show, Eq, Enum, Bounded, Ord)

allPrimitiveFunctionNames :: [PrimitiveFunctionName]
allPrimitiveFunctionNames = [minBound..maxBound]

showPrimitiveFunctionName :: PrimitiveFunctionName -> Text
showPrimitiveFunctionName name =
  case name of
    PrimIAdd -> "iAdd"
    PrimISub -> "iSub"
    PrimIEquals -> "iEquals"
    PrimIGreaterThan -> "iGreaterThan"
    PrimILessThan -> "iLessThan"
    PrimDAdd -> "dAdd"
    PrimDSub -> "dSub"

readPrimitiveFunctionName :: Text -> Maybe PrimitiveFunctionName
readPrimitiveFunctionName name =
  case name of
    "iAdd" -> Just PrimIAdd
    "iSub" -> Just PrimISub
    "iEquals" -> Just PrimIEquals
    "iGreaterThan" -> Just PrimIGreaterThan
    "iLessThan" -> Just PrimILessThan

    "dAdd" -> Just PrimDAdd
    "dSub" -> Just PrimDSub

    _ -> Nothing

primitiveFunction :: PrimitiveFunctionName -> PrimitiveFunction
primitiveFunction name =
  case name of
    PrimIAdd -> PrimitiveFunction PrimIAdd [IntType, IntType, IntType]
    PrimISub -> PrimitiveFunction PrimISub [IntType, IntType, IntType]
    PrimIEquals -> PrimitiveFunction PrimIEquals [IntType, IntType, BoolType]
    PrimIGreaterThan -> PrimitiveFunction PrimIGreaterThan [IntType, IntType, BoolType]
    PrimILessThan -> PrimitiveFunction PrimILessThan [IntType, IntType, BoolType]

    PrimDAdd -> PrimitiveFunction PrimDAdd [DoubleType, DoubleType, DoubleType]
    PrimDSub -> PrimitiveFunction PrimDSub [DoubleType, DoubleType, DoubleType]
