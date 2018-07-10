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
  , environmentDataConInfos :: !(Map DataConName DataConInfo)
  , environmentTyConKinds :: !(Map TyConName Kind)
  } deriving (Show, Eq)

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
  { environmentIdentTypes = Map.empty
  , environmentDataConInfos = Map.empty
  , environmentTyConKinds = Map.empty
  }

mergeEnvironments :: Environment -> Environment -> Environment
mergeEnvironments env1 env2 =
  Environment
  { environmentIdentTypes = environmentIdentTypes env1 <> environmentIdentTypes env2
  , environmentDataConInfos = environmentDataConInfos env1 <> environmentDataConInfos env2
  , environmentTyConKinds = environmentTyConKinds env1 <> environmentTyConKinds env2
  }

primEnvironment :: Environment
primEnvironment =
  let
    primFuncTypes =
      (\(PrimitiveFunction _ name ty) -> (name, foldTyFun $ TyCon . notLocated <$> ty))
      <$> allPrimitiveFunctions
    primDataConInfos = concatMap dataConInfos (fst <$> allPrimTypeDefinitions)
    primTyConKinds =
      (\(decl, kind) -> (locatedValue . tyConDefinitionName . typeDeclarationTypeName $ decl, kind))
      <$> allPrimTypeDefinitions
  in
    Environment
    { environmentIdentTypes = Map.fromList primFuncTypes
    , environmentDataConInfos = Map.fromList $ first locatedValue <$> primDataConInfos
    , environmentTyConKinds = Map.fromList primTyConKinds
    }

