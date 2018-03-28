{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Prim
  ( PrimitiveType(..)
  , readPrimitiveType
  , PrimitiveFunction(..)
  , PrimitiveFunctionName(..)
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
  | PrimIAbs
  | PrimIEquals
  | PrimIGreaterThan
  | PrimILessThan

    -- Double
  | PrimDAdd
  | PrimDSub
  deriving (Show, Eq, Enum, Bounded)

readPrimitiveFunctionName :: Text -> Maybe PrimitiveFunctionName
readPrimitiveFunctionName name =
  case name of
    "iAdd" -> Just PrimIAdd
    "iSub" -> Just PrimISub
    "iAbs" -> Just PrimIAbs
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
    PrimIAbs -> PrimitiveFunction PrimIAbs [IntType, IntType]
    PrimIEquals -> PrimitiveFunction PrimIEquals [IntType, IntType, BoolType]
    PrimIGreaterThan -> PrimitiveFunction PrimIGreaterThan [IntType, IntType, BoolType]
    PrimILessThan -> PrimitiveFunction PrimILessThan [IntType, IntType, BoolType]

    PrimDAdd -> PrimitiveFunction PrimDAdd [DoubleType, DoubleType, DoubleType]
    PrimDSub -> PrimitiveFunction PrimDSub [DoubleType, DoubleType, DoubleType]
