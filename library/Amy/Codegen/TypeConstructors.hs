module Amy.Codegen.TypeConstructors
  ( TypeCompilationMethod(..)
  , typeCompilationMethod
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
typeCompilationMethod :: ANF.TypeDeclaration -> Map ANF.ConstructorName TypeCompilationMethod
typeCompilationMethod (ANF.TypeDeclaration _ cons) =
  case cons of
    (ANF.DataConstructor conName Nothing) -> Map.singleton conName (CompileEnum 0)
    (ANF.DataConstructor conName (Just tyArg)) -> Map.singleton conName (CompileUnboxed $ assertPrimitiveType tyArg)

assertPrimitiveType :: TyConInfo -> PrimitiveType
assertPrimitiveType info@(ANF.TyConInfo _ _ mPrim) =
  fromMaybe (error $ "Cannot unbox, not a primitive type! " ++ show info) mPrim
