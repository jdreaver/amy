{-# LANGUAGE MultiWayIf #-}

module Amy.Codegen.TypeCompilation
  ( TyConRep(..)
  , DataConRep(..)
  , typeCompilationMethod
  , findCompilationMethod
  , llvmPrimitiveType
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import GHC.Word (Word32)
import LLVM.AST as LLVM

import Amy.ANF.AST as ANF
import Amy.Codegen.Utils
import Amy.Prim

-- | Describes how a type constructor is represented in LLVM.
data TyConRep
  = TyConUnboxed !LLVM.Type
    -- ^ Represent a TyCon as an LLVM primitive Type like an int or double
  | TyConEnum !Word32
    -- ^ Compile as an int type with 'Word32' bits.
  | TyConTaggedUnion !Name !Word32
    -- ^ Represent as a struct with a 'Word32'-sized integer tag and an integer
    -- pointer to data.
  deriving (Show, Eq)


-- | Describes how a data constructor is represented in LLVM.
data DataConRep
  = CompileUnboxed !TyConInfo
    -- ^ Unbox to the given type
  | CompileEnum !Int !Word32
    -- ^ Compile as an integer enum. The 'Word32' is the size of the integer
    -- type.
  | CompileTaggedUnion !Name !Int !Word32
    -- ^ Compile as a pair of integer tag and pointer to data. The 'Word32' is
    -- the size of the integer type.
  deriving (Show, Eq)

-- | Decide how we are going to compile a type declaration.
typeCompilationMethod
  :: ANF.TypeDeclaration
  -> (Map ANF.ConstructorName DataConRep, (TyConInfo, TyConRep))
typeCompilationMethod decl@(ANF.TypeDeclaration tyName constructors) =
  case constructors of
    -- Single constructor around another type
    [ANF.DataConstructor conName (Just argTy)] ->
      let
        primArgTy =
          fromMaybe (error $ "Cannot unbox constructor to primitive type " ++ show decl)
          $ llvmPrimitiveType argTy
      in
        ( Map.singleton conName $ CompileUnboxed argTy
        , (tyName, TyConUnboxed primArgTy)
        )
    _ ->
      -- Check if we can do an enum. This is when all constructors have no
      -- arguments.
      if all (isNothing . dataConstructorArgument) constructors
      then
        let
          mkMethod (ANF.DataConstructor conName _, i) = (conName, CompileEnum i wordSize)
        in
          ( Map.fromList $ mkMethod <$> zip constructors [0..]
          , (tyName, TyConEnum wordSize)
          )
      -- Can't do an enum. We'll have to use tagged pairs.
      else
        let
          structName = textToName (tyConInfoText tyName)
          mkMethod (ANF.DataConstructor conName _, i) = (conName, CompileTaggedUnion structName i wordSize)
        in
          ( Map.fromList $ mkMethod <$> zip constructors [0..]
          , (tyName, TyConTaggedUnion structName wordSize)
          )
 where
  -- Pick a proper integer size
  wordSize :: Word32
  wordSize =
   if | length constructors <= 2 -> 1
      | length constructors < (2 :: Int) ^ (8 :: Int) -> 8
      | otherwise -> 32

findCompilationMethod
  :: ConstructorName
  -> Map ConstructorName DataConRep
  -> DataConRep
findCompilationMethod consName compilationMethods =
  fromMaybe (error $ "No compilation method for " ++ show consName) $ Map.lookup consName compilationMethods

-- | Convert from a amy primitive type to an LLVM type
llvmPrimitiveType :: TyConInfo -> Maybe LLVM.Type
llvmPrimitiveType tyCon
  | tyCon == intTyCon' = Just (IntegerType 64)
  | tyCon == doubleTyCon' = Just (FloatingPointType DoubleFP)
  | otherwise = Nothing
 where
  intTyCon' = fromPrimTyCon intTyCon
  doubleTyCon' = fromPrimTyCon doubleTyCon
