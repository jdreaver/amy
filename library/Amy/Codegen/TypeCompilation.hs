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

import Amy.ANF.AST
import Amy.Codegen.Utils
import Amy.Prim

-- TODO: Ever since we store much more information in DataConstructor, I don't
-- think we need a map from DataConstructor to DataConRep. If we know the
-- TyConRep for the data constructor's Type, we can surmise what to do with
-- each constructor.

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
  :: TypeDeclaration
  -> (Map DataConstructor DataConRep, (TyConInfo, TyConRep))
typeCompilationMethod decl@(TypeDeclaration tyName constructors) =
  case constructors of
    -- Single constructor around another type
    [con@(DataConstructor _ _ (Just argTy) _ _ _)] ->
      let
        primArgTy =
          fromMaybe (error $ "Cannot unbox constructor to primitive type " ++ show decl)
          $ llvmPrimitiveType argTy
      in
        ( Map.singleton con $ CompileUnboxed argTy
        , (tyName, TyConUnboxed primArgTy)
        )
    _ ->
      -- Check if we can do an enum. This is when all constructors have no
      -- arguments.
      if all (isNothing . dataConstructorArgument) constructors
      then
        let
          mkMethod con = (con, CompileEnum (fromIntegral . unConstructorIndex $ dataConstructorIndex con) wordSize)
        in
          ( Map.fromList $ mkMethod <$> constructors
          , (tyName, TyConEnum wordSize)
          )
      -- Can't do an enum. We'll have to use tagged pairs.
      else
        let
          structName = textToName (tyConInfoText tyName)
          mkMethod con = (con, CompileTaggedUnion structName (fromIntegral . unConstructorIndex $ dataConstructorIndex con) wordSize)
        in
          ( Map.fromList $ mkMethod <$> constructors
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
  :: DataConstructor
  -> Map DataConstructor DataConRep
  -> DataConRep
findCompilationMethod cons compilationMethods =
  fromMaybe (error $ "No compilation method for " ++ show cons) $ Map.lookup cons compilationMethods

-- | Convert from an Amy primitive type to an LLVM type
llvmPrimitiveType :: TyConInfo -> Maybe LLVM.Type
llvmPrimitiveType tyCon
  | tyCon == intTyCon' = Just (IntegerType 64)
  | tyCon == doubleTyCon' = Just (FloatingPointType DoubleFP)
  | otherwise = Nothing
 where
  intTyCon' = fromPrimTyCon intTyCon
  doubleTyCon' = fromPrimTyCon doubleTyCon
