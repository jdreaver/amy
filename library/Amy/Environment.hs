module Amy.Environment
  ( Environment(..)
  , emptyEnvironment
  , mergeEnvironments
  , primEnvironment
  ) where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Amy.Kind
import Amy.Names
import Amy.Prim
import Amy.Syntax.AST

data Environment
  = Environment
  { environmentIdentTypes :: !(Map IdentName Type)
  , environmentDataConTypes :: !(Map DataConName Type)
  , environmentTyConKinds :: !(Map TyConName Kind)
  } deriving (Show, Eq)

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
  { environmentIdentTypes = Map.empty
  , environmentDataConTypes = Map.empty
  , environmentTyConKinds = Map.empty
  }

mergeEnvironments :: Environment -> Environment -> Environment
mergeEnvironments env1 env2 =
  Environment
  { environmentIdentTypes = environmentIdentTypes env1 <> environmentIdentTypes env2
  , environmentDataConTypes = environmentDataConTypes env1 <> environmentDataConTypes env2
  , environmentTyConKinds = environmentTyConKinds env1 <> environmentTyConKinds env2
  }

primEnvironment :: Environment
primEnvironment =
  let
    primFuncTypes = convertPrimitiveFunctionType <$> allPrimitiveFunctions
    primDataConTypes = concatMap dataConTypes (fst <$> allPrimTypeDefinitions)
    primTyConKinds =
      (\(decl, kind) -> (locatedValue . tyConDefinitionName . typeDeclarationTypeName $ decl, kind))
      <$> allPrimTypeDefinitions
  in
    Environment
    { environmentIdentTypes = Map.fromList primFuncTypes
    , environmentDataConTypes = Map.fromList $ first locatedValue <$> primDataConTypes
    , environmentTyConKinds = Map.fromList primTyConKinds
    }

convertPrimitiveFunctionType :: PrimitiveFunction -> (IdentName, Type)
convertPrimitiveFunctionType (PrimitiveFunction _ name ty) =
  ( name
  , foldTyFun $ TyCon . notLocated <$> ty
  )
