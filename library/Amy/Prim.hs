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
    allPrimTypeDefinitions
  , literalType

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
  , primitiveFunctionsByName
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Amy.Literal
import Amy.Names
import Amy.Syntax.AST

--
-- Wired-in Type Definitions
--

mkPrimTypeDef :: TyConName -> [DataConName] -> TypeDeclaration
mkPrimTypeDef tyConName dataConNames =
  let
    span' = SourceSpan "<prim>" 1 1 1 1
    tyConDef = TyConDefinition tyConName [] span'
    mkDataConDef con = DataConDefinition (Located span' con) Nothing
    dataCons = mkDataConDef <$> dataConNames
  in TypeDeclaration tyConDef dataCons

-- Int
intTypeDefinition :: TypeDeclaration
intTypeDefinition = mkPrimTypeDef intTyCon []

intTyCon :: TyConName
intTyCon = "Int"

-- Double
doubleTypeDefinition :: TypeDeclaration
doubleTypeDefinition = mkPrimTypeDef doubleTyCon []

doubleTyCon :: TyConName
doubleTyCon = "Double"

-- Bool

boolTypeDefinition :: TypeDeclaration
boolTypeDefinition = mkPrimTypeDef boolTyCon [falseDataCon, trueDataCon]

boolTyCon :: TyConName
boolTyCon = "Bool"

falseDataCon, trueDataCon :: DataConName
falseDataCon = "False"
trueDataCon = "True"

allPrimTypeDefinitions :: [TypeDeclaration]
allPrimTypeDefinitions =
  [ intTypeDefinition
  , doubleTypeDefinition
  , boolTypeDefinition
  ]

literalType :: Literal -> TyConName
literalType (LiteralInt _) = intTyCon
literalType (LiteralDouble _) = doubleTyCon

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

primitiveFunctionType' :: PrimitiveFunctionName -> NonEmpty TyConName
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
  , primitiveFunctionNameText :: !IdentName
  , primitiveFunctionType :: !(NonEmpty TyConName)
  } deriving (Show, Eq)

allPrimitiveFunctions :: [PrimitiveFunction]
allPrimitiveFunctions =
  (\prim -> PrimitiveFunction prim (IdentName $ showPrimitiveFunctionName prim) (primitiveFunctionType' prim))
  <$> allPrimitiveFunctionNames

primitiveFunctionsByName :: Map IdentName PrimitiveFunction
primitiveFunctionsByName =
  Map.fromList
  $ (\prim -> (IdentName $ showPrimitiveFunctionName $ primitiveFunctionName prim, prim)) <$> allPrimitiveFunctions
