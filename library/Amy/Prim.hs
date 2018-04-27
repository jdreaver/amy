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
  , showPrimitiveType
  , primitiveTypeId
  , PrimitiveFunction(..)
  , PrimitiveFunctionName(..)
  , allPrimitiveFunctionNames
  , allPrimitiveFunctionNamesAndIds
  , showPrimitiveFunctionName
  , readPrimitiveFunctionName
  , primitiveFunction
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data PrimitiveType
  = IntType
  | DoubleType
  | BoolType
  deriving (Show, Eq, Ord, Enum, Bounded)

readPrimitiveType :: Text -> Maybe PrimitiveType
readPrimitiveType t =
  case t of
    "Int" -> Just IntType
    "Double" -> Just DoubleType
    "Bool" -> Just BoolType
    _ -> Nothing

showPrimitiveType :: PrimitiveType -> Text
showPrimitiveType t =
  case t of
    IntType -> "Int"
    DoubleType -> "Double"
    BoolType -> "Bool"

allPrimitiveTypes :: [PrimitiveType]
allPrimitiveTypes = [minBound..maxBound]

primitiveTypeIds :: Map PrimitiveType Int
primitiveTypeIds = Map.fromList $ zip allPrimitiveTypes [0..]

primitiveTypeId :: PrimitiveType -> Int
primitiveTypeId = (Map.!) primitiveTypeIds

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

    -- Conversion
  | PrimIntToDouble
  | PrimDoubleToInt
  deriving (Show, Eq, Enum, Bounded, Ord)

allPrimitiveFunctionNames :: [PrimitiveFunctionName]
allPrimitiveFunctionNames = [minBound..maxBound]

allPrimitiveFunctionNamesAndIds :: [(Int, PrimitiveFunctionName)]
allPrimitiveFunctionNamesAndIds = zip [0..] allPrimitiveFunctionNames

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
    PrimIntToDouble -> "intToDouble"
    PrimDoubleToInt -> "doubleToInt"

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

    "intToDouble" -> Just PrimIntToDouble
    "doubleToInt" -> Just PrimDoubleToInt

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
    PrimIntToDouble -> PrimitiveFunction PrimIntToDouble [IntType, DoubleType]
    PrimDoubleToInt -> PrimitiveFunction PrimDoubleToInt [DoubleType, IntType]
