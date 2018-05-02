{-# LANGUAGE MultiWayIf #-}

module Amy.Codegen.TypeCompilation
  ( TypeCompilationMethod(..)
  , typeCompilationMethod
  , findCompilationMethod
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import GHC.Word (Word32)

import Amy.ANF.AST as ANF
import Amy.Prim

data TypeCompilationMethod
  = CompileUnboxed !TyConInfo
    -- ^ Unbox to the given type
  | CompileEnum !Int !Word32
    -- ^ Compile as an integer enum. The 'Word32' is the size of the integer
    -- type.
  | CompileTaggedPairs !Int !Word32
    -- ^ Compile as a pair of integer tag and pointer to data. The 'Word32' is
    -- the size of the integer type.
  deriving (Show, Eq)

-- | Decide how we are going to compile a type declaration.
typeCompilationMethod
  :: ANF.TypeDeclaration
  -> (Map ANF.ConstructorName TypeCompilationMethod, (TyConInfo, Maybe TyConInfo))
typeCompilationMethod (ANF.TypeDeclaration tyName constructors) =
  case constructors of
    -- Single constructor around another type
    [ANF.DataConstructor conName (Just argTy)] ->
      (Map.singleton conName $ CompileUnboxed argTy, (tyName, Just argTy))

    _ ->
      -- Check if we can do an enum. This is when all constructors have no
      -- arguments.
      if all (isNothing . dataConstructorArgument) constructors
      then
        let
          mkMethod (ANF.DataConstructor conName _, i) = (conName, CompileEnum i wordSize)
        in
          ( Map.fromList $ mkMethod <$> zip constructors [0..]
          , (tyName, Just (fromPrimTyCon intTyCon))
          )
      -- Can't do an enum. We'll have to use tagged pairs.
      else
        let
          mkMethod (ANF.DataConstructor conName _, i) = (conName, CompileTaggedPairs i wordSize)
        in
          ( Map.fromList $ mkMethod <$> zip constructors [0..]
          , (tyName, Nothing)
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
  -> Map ConstructorName TypeCompilationMethod
  -> TypeCompilationMethod
findCompilationMethod consName compilationMethods =
  fromMaybe (error $ "No compilation method for " ++ show consName) $ Map.lookup consName compilationMethods
