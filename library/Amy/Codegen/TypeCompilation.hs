{-# LANGUAGE MultiWayIf #-}

module Amy.Codegen.TypeCompilation
  ( TypeCompilationMethod(..)
  , typeCompilationMethod
  , findCompilationMethod
  , llvmPrimitiveType
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import GHC.Word (Word32)
import LLVM.AST as LLVM

import Amy.ANF.AST
import Amy.Codegen.Utils
import Amy.Prim

-- | Describes how a type constructor is represented in LLVM.
data TypeCompilationMethod
  = CompileEnum !Word32
    -- ^ Compile as an int type with 'Word32' bits.
  | CompileTaggedUnion !Name !Word32
    -- ^ Represent as a struct with a 'Word32'-sized integer tag and an integer
    -- pointer to data.
  deriving (Show, Eq)

-- | Decide how we are going to compile a type declaration.
typeCompilationMethod :: TypeDeclaration -> TypeCompilationMethod
typeCompilationMethod (TypeDeclaration tyName constructors) =
  -- Check if we can do an enum. This is when all constructors have no
  -- arguments.
  if all (isNothing . dataConstructorArgument) constructors
  then CompileEnum wordSize
  -- Can't do an enum. We'll have to use tagged pairs.
  else CompileTaggedUnion (textToName (tyConInfoText tyName)) wordSize
 where
  -- Pick a proper integer size
  wordSize :: Word32
  wordSize =
   if | length constructors <= 2 -> 1
      | length constructors < (2 :: Int) ^ (8 :: Int) -> 8
      | otherwise -> 32

findCompilationMethod
  :: DataConstructor
  -> Map TyConInfo TypeCompilationMethod
  -> TypeCompilationMethod
findCompilationMethod con compilationMethods =
  fromMaybe (error $ "No compilation method for " ++ show con)
  $ Map.lookup (dataConstructorType con) compilationMethods

-- | Convert from an Amy primitive type to an LLVM type
llvmPrimitiveType :: TyConInfo -> Maybe LLVM.Type
llvmPrimitiveType tyCon
  | tyCon == intTyCon' = Just (IntegerType 64)
  | tyCon == doubleTyCon' = Just (FloatingPointType DoubleFP)
  | otherwise = Nothing
 where
  intTyCon' = fromPrimTyCon intTyCon
  doubleTyCon' = fromPrimTyCon doubleTyCon
