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

  , intTypeDefinition
  , intTyCon
  , doubleTypeDefinition
  , doubleTyCon
  , boolTypeDefinition
  , boolTyCon
  , falseDataCon
  , trueDataCon

    -- * Functions
  , PrimitiveFunctionName(..)
  , PrimitiveFunction(..)
  , allPrimitiveFunctions
  , primitiveFunctionsById
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Tuple (swap)

import Amy.ASTCommon

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

maxPrimitiveDataConId :: Int
maxPrimitiveDataConId = maximum . fmap fst $ allPrimitiveDataConsAndIds

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
  , primDataConType :: !PrimTyCon
  , primDataConSpan :: !ConstructorSpan
  , primDataConIndex :: !ConstructorIndex
  } deriving (Show, Eq)

mkPrimTypeDef :: PrimitiveType -> [PrimitiveDataCon] -> PrimTypeDefinition
mkPrimTypeDef tyCon dataCons =
  let
    tyCon' = PrimTyCon (showPrimType tyCon) (primitiveTypeId tyCon)
    span' = ConstructorSpan $ length dataCons
    indexes = ConstructorIndex <$> [0..]
    mkDataCon (i, prim) =
      PrimDataCon
      { primDataConName = showPrimDataCon prim
      , primDataConId = primitiveDataConId prim
      , primDataConType = tyCon'
      , primDataConSpan = span'
      , primDataConIndex = i
      }
    dataCons' = mkDataCon <$> zip indexes dataCons
  in PrimTypeDefinition tyCon' dataCons'

-- Int
intTypeDefinition :: PrimTypeDefinition
intTypeDefinition = mkPrimTypeDef IntType []

intTyCon :: PrimTyCon
intTyCon = primTypeDefinitionTypeName intTypeDefinition

-- Double
doubleTypeDefinition :: PrimTypeDefinition
doubleTypeDefinition = mkPrimTypeDef DoubleType []

doubleTyCon :: PrimTyCon
doubleTyCon = primTypeDefinitionTypeName doubleTypeDefinition

-- Bool

boolTypeDefinition :: PrimTypeDefinition
boolTypeDefinition = mkPrimTypeDef BoolType [FalseDataCon, TrueDataCon]

boolTyCon :: PrimTyCon
boolTyCon = primTypeDefinitionTypeName boolTypeDefinition

falseDataCon, trueDataCon :: PrimDataCon
[falseDataCon, trueDataCon] = primTypeDefinitionDataConstructors boolTypeDefinition

allPrimTypeDefinitions :: [PrimTypeDefinition]
allPrimTypeDefinitions =
  [ intTypeDefinition
  , doubleTypeDefinition
  , boolTypeDefinition
  ]

--
-- Primitive Functions
--

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
allPrimitiveFunctionNamesAndIds = zip [maxPrimitiveDataConId + 1..] allPrimitiveFunctionNames

maxPrimId :: Int
maxPrimId = maximum . fmap fst $ allPrimitiveFunctionNamesAndIds

showPrimitiveFunctionName :: PrimitiveFunctionName -> Text
showPrimitiveFunctionName name =
  case name of
    PrimIAdd -> "iAdd#"
    PrimISub -> "iSub#"
    PrimIEquals -> "iEquals#"
    PrimIGreaterThan -> "iGreaterThan#"
    PrimILessThan -> "iLessThan#"
    PrimDAdd -> "dAdd#"
    PrimDSub -> "dSub#"
    PrimIntToDouble -> "intToDouble#"
    PrimDoubleToInt -> "doubleToInt#"

primitiveFunctionType' :: PrimitiveFunctionName -> NonEmpty PrimTyCon
primitiveFunctionType' name =
  case name of
    PrimIAdd -> [intTyCon, intTyCon, intTyCon]
    PrimISub -> [intTyCon, intTyCon, intTyCon]
    PrimIEquals -> [intTyCon, intTyCon, boolTyCon]
    PrimIGreaterThan -> [intTyCon, intTyCon, boolTyCon]
    PrimILessThan -> [intTyCon, intTyCon, boolTyCon]
    PrimDAdd -> [doubleTyCon, doubleTyCon, doubleTyCon]
    PrimDSub -> [doubleTyCon, doubleTyCon, doubleTyCon]
    PrimIntToDouble -> [intTyCon, doubleTyCon]
    PrimDoubleToInt -> [doubleTyCon, intTyCon]

data PrimitiveFunction
  = PrimitiveFunction
  { primitiveFunctionName :: !PrimitiveFunctionName
  , primitiveFunctionNameText :: !Text
  , primitiveFunctionId :: !Int
  , primitiveFunctionType :: !(NonEmpty PrimTyCon)
  } deriving (Show, Eq)

allPrimitiveFunctions :: [PrimitiveFunction]
allPrimitiveFunctions =
  (\(id', prim) -> PrimitiveFunction prim (showPrimitiveFunctionName prim) id' (primitiveFunctionType' prim))
  <$> allPrimitiveFunctionNamesAndIds

primitiveFunctionsById :: Map Int PrimitiveFunction
primitiveFunctionsById = Map.fromList $ (\prim -> (primitiveFunctionId prim, prim)) <$> allPrimitiveFunctions
