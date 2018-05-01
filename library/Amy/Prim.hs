{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Encode primitive types operations for the compiler. Primitive types are
-- types that map directly to raw values on the stack, like machine integers
-- and doubles. Primitive operations are functions that are generated with a
-- machine instruction, and the code generator needs to know about them to
-- generate the machine code. Because of this, these types/operations need to
-- be included in the compiler and not in a standard library.

module Amy.Prim
  ( -- * Types
    maxPrimId
  , PrimTypeDefinition(..)
  , PrimTyCon(..)
  , PrimDataCon(..)
  , allPrimTypeDefinitions
  , allPrimTyCons
  , allPrimDataCons

  , intTyCon
  , doubleTyCon
  , boolTyCon
  , falseDataCon
  , trueDataCon

    -- * Functions
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
import Data.Tuple (swap)

--
-- Primitive Type ID Generation
--

data PrimitiveType
  = IntType
  | DoubleType
  | BoolType -- This isn't really primitive, just wired in
  deriving (Show, Eq, Ord, Enum, Bounded)

showPrimType :: PrimitiveType -> Text
showPrimType IntType = "Int"
showPrimType DoubleType = "Double"
showPrimType BoolType = "Bool"

data PrimitiveDataCon
  = TrueDataCon
  | FalseDataCon
  deriving (Show, Eq, Ord, Enum, Bounded)

showPrimDataCon :: PrimitiveDataCon -> Text
showPrimDataCon TrueDataCon = "True"
showPrimDataCon FalseDataCon = "False"

allPrimitiveTypes :: [PrimitiveType]
allPrimitiveTypes = [minBound..maxBound]

allPrimitiveTypesAndIds :: [(Int, PrimitiveType)]
allPrimitiveTypesAndIds = zip [0..] allPrimitiveTypes

primitiveTypeIds :: Map PrimitiveType Int
primitiveTypeIds = Map.fromList . fmap swap $ allPrimitiveTypesAndIds

primitiveTypeId :: PrimitiveType -> Int
primitiveTypeId = (Map.!) primitiveTypeIds

maxPrimitiveTypeId :: Int
maxPrimitiveTypeId = maximum . fmap fst $ allPrimitiveTypesAndIds

allPrimitiveDataCons :: [PrimitiveDataCon]
allPrimitiveDataCons = [minBound..maxBound]

allPrimitiveDataConsAndIds :: [(Int, PrimitiveDataCon)]
allPrimitiveDataConsAndIds = zip [maxPrimitiveTypeId + 1..] allPrimitiveDataCons

primitiveDataConIds :: Map PrimitiveDataCon Int
primitiveDataConIds = Map.fromList . fmap swap $ allPrimitiveDataConsAndIds

primitiveDataConId :: PrimitiveDataCon -> Int
primitiveDataConId = (Map.!) primitiveDataConIds

maxPrimId :: Int
maxPrimId = maximum . fmap fst $ allPrimitiveDataConsAndIds

--
-- Wired-in Type Definitions
--

data PrimTypeDefinition
  = PrimTypeDefinition
  { primTypeDefinitionTypeName :: !PrimTyCon
  , primTypeDefinitionDataConstructors :: ![PrimDataCon]
  } deriving (Show, Eq)

data PrimTyCon
  = PrimTyCon
  { primTyConName :: !Text
  , primTyConId :: !Int
  } deriving (Show, Eq)

data PrimDataCon
  = PrimDataCon
  { primDataConName :: !Text
  , primDataConId :: !Int
  } deriving (Show, Eq)

mkPrimTyCon :: PrimitiveType -> PrimTyCon
mkPrimTyCon prim = PrimTyCon (showPrimType prim) (primitiveTypeId prim)

mkPrimDataCon :: PrimitiveDataCon -> PrimDataCon
mkPrimDataCon prim = PrimDataCon (showPrimDataCon prim) (primitiveDataConId prim)

-- Int
intTyCon :: PrimTyCon
intTyCon = mkPrimTyCon IntType

intTypeDefinition :: PrimTypeDefinition
intTypeDefinition = PrimTypeDefinition intTyCon []

-- Double
doubleTyCon :: PrimTyCon
doubleTyCon = mkPrimTyCon DoubleType

doubleTypeDefinition :: PrimTypeDefinition
doubleTypeDefinition = PrimTypeDefinition doubleTyCon []

-- Bool
boolTyCon :: PrimTyCon
boolTyCon = mkPrimTyCon BoolType

falseDataCon :: PrimDataCon
falseDataCon = mkPrimDataCon FalseDataCon

trueDataCon :: PrimDataCon
trueDataCon = mkPrimDataCon TrueDataCon

boolTypeDefinition :: PrimTypeDefinition
boolTypeDefinition = PrimTypeDefinition boolTyCon [falseDataCon, trueDataCon]

allPrimTypeDefinitions :: [PrimTypeDefinition]
allPrimTypeDefinitions =
  [ intTypeDefinition
  , doubleTypeDefinition
  , boolTypeDefinition
  ]

allPrimTyCons :: [PrimTyCon]
allPrimTyCons = primTypeDefinitionTypeName <$> allPrimTypeDefinitions

allPrimDataCons :: [PrimDataCon]
allPrimDataCons = concatMap primTypeDefinitionDataConstructors allPrimTypeDefinitions

--
-- Primitive Functions
--

data PrimitiveFunction
  = PrimitiveFunction
  { primitiveFunctionName :: !PrimitiveFunctionName
  , primitiveFunctionType :: !(NonEmpty PrimTyCon)
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
    PrimIAdd -> PrimitiveFunction PrimIAdd [intTyCon, intTyCon, intTyCon]
    PrimISub -> PrimitiveFunction PrimISub [intTyCon, intTyCon, intTyCon]
    PrimIEquals -> PrimitiveFunction PrimIEquals [intTyCon, intTyCon, boolTyCon]
    PrimIGreaterThan -> PrimitiveFunction PrimIGreaterThan [intTyCon, intTyCon, boolTyCon]
    PrimILessThan -> PrimitiveFunction PrimILessThan [intTyCon, intTyCon, boolTyCon]
    PrimDAdd -> PrimitiveFunction PrimDAdd [doubleTyCon, doubleTyCon, doubleTyCon]
    PrimDSub -> PrimitiveFunction PrimDSub [doubleTyCon, doubleTyCon, doubleTyCon]
    PrimIntToDouble -> PrimitiveFunction PrimIntToDouble [intTyCon, doubleTyCon]
    PrimDoubleToInt -> PrimitiveFunction PrimDoubleToInt [doubleTyCon, intTyCon]
