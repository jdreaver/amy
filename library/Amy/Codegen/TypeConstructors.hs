module Amy.Codegen.TypeConstructors
  ( TypeCompilationMethod(..)
  , typeCompilationMethod
  , findCompilationMethod
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Amy.ANF.AST as ANF
import Amy.Prim

data TypeCompilationMethod
  = CompileUnboxed !PrimitiveType
    -- ^ Unbox to the given type
  | CompileEnum !Int
    -- ^ Compile as an integer enum
  | CompileTaggedPairs !Int
    -- ^ Compile as a pair of integer tag and pointer to data
  deriving (Show, Eq)

-- | Decide how we are going to compile a type declaration.
typeCompilationMethod
  :: ANF.TypeDeclaration
  -> (Map ANF.ConstructorName TypeCompilationMethod, (TyConInfo, Maybe PrimitiveType))
typeCompilationMethod (ANF.TypeDeclaration tyName cons) =
  case cons of
    (ANF.DataConstructor conName Nothing) -> (Map.singleton conName (CompileEnum 0), (tyName, Just IntType))
    (ANF.DataConstructor conName (Just argTy)) ->
      let argTy' = assertPrimitiveType argTy
      in (Map.singleton conName $ CompileUnboxed argTy', (tyName, Just argTy'))

findCompilationMethod
  :: ConstructorName
  -> Map ConstructorName TypeCompilationMethod
  -> TypeCompilationMethod
findCompilationMethod consName compilationMethods =
  fromMaybe (error $ "No compilation method for " ++ show consName) $ Map.lookup consName compilationMethods

assertPrimitiveType :: TyConInfo -> PrimitiveType
assertPrimitiveType info@(ANF.TyConInfo _ _ mPrim) =
  fromMaybe (error $ "Cannot unbox, not a primitive type! " ++ show info) mPrim
