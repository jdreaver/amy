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
  , PrimitiveFunction(..)
  , PrimitiveFunctionName(..)
  , allPrimitiveFunctionNames
  , allPrimitiveFunctionNamesAndIds
  , showPrimitiveFunctionName
  , readPrimitiveFunctionName
  , primitiveFunction
  ) where

import Data.Text (Text)

import Amy.Type

data PrimitiveType
  = IntType
  | DoubleType
  | BoolType
  deriving (Show, Eq, Ord)

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

data PrimitiveFunction
  = PrimitiveFunction
  { primitiveFunctionName :: !PrimitiveFunctionName
  , primitiveFunctionType :: !(Type PrimitiveType)
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
    PrimIAdd -> PrimitiveFunction PrimIAdd (TyCon IntType `TyFun` TyCon IntType `TyFun` TyCon IntType)
    PrimISub -> PrimitiveFunction PrimISub (TyCon IntType `TyFun` TyCon IntType `TyFun` TyCon IntType)
    PrimIEquals -> PrimitiveFunction PrimIEquals (TyCon IntType `TyFun` TyCon IntType `TyFun` TyCon BoolType)
    PrimIGreaterThan -> PrimitiveFunction PrimIGreaterThan (TyCon IntType `TyFun` TyCon IntType `TyFun` TyCon BoolType)
    PrimILessThan -> PrimitiveFunction PrimILessThan (TyCon IntType `TyFun` TyCon IntType `TyFun` TyCon BoolType)
    PrimDAdd -> PrimitiveFunction PrimDAdd (TyCon DoubleType `TyFun` TyCon DoubleType `TyFun` TyCon DoubleType)
    PrimDSub -> PrimitiveFunction PrimDSub (TyCon DoubleType `TyFun` TyCon DoubleType `TyFun` TyCon DoubleType)
