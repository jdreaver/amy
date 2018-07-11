module Amy.Environment
  ( Environment(..)
  , emptyEnvironment
  , mergeEnvironments
  , primEnvironment
  , DataConInfo(..)
  , dataConInfos
  ) where

import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)

import qualified Amy.ANF.AST as ANF
import Amy.ANF.TypeRep
import Amy.Kind
import Amy.Names
import Amy.Prim
import Amy.Syntax.AST

data Environment
  = Environment
  { environmentIdentTypes :: !(Map IdentName Type)
  , environmentDataConInfos :: !(Map DataConName DataConInfo)
  , environmentTyConKinds :: !(Map TyConName Kind)
  , environmentANFTypeReps :: !(Map TyConName ANF.Type)
  , environmentFunctionTypes :: !(Map IdentName ([ANF.Type], ANF.Type))
  } deriving (Show, Eq)

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
  { environmentIdentTypes = Map.empty
  , environmentDataConInfos = Map.empty
  , environmentTyConKinds = Map.empty
  , environmentANFTypeReps = Map.empty
  , environmentFunctionTypes = Map.empty
  }

mergeEnvironments :: Environment -> Environment -> Environment
mergeEnvironments env1 env2 =
  Environment
  { environmentIdentTypes = environmentIdentTypes env1 <> environmentIdentTypes env2
  , environmentDataConInfos = environmentDataConInfos env1 <> environmentDataConInfos env2
  , environmentTyConKinds = environmentTyConKinds env1 <> environmentTyConKinds env2
  , environmentANFTypeReps = environmentANFTypeReps env1 <> environmentANFTypeReps env2
  , environmentFunctionTypes = environmentFunctionTypes env1 <> environmentFunctionTypes env2
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
    primTypeReps =
      (\t -> (locatedValue . tyConDefinitionName . typeDeclarationTypeName $ t, typeRep t))
      . fst
      <$> allPrimTypeDefinitions
  in
    Environment
    { environmentIdentTypes = Map.fromList primFuncTypes
    , environmentDataConInfos = Map.fromList $ first locatedValue <$> primDataConInfos
    , environmentTyConKinds = Map.fromList primTyConKinds
    , environmentANFTypeReps = Map.fromList primTypeReps
    , environmentFunctionTypes = Map.empty
    }

data DataConInfo
  = DataConInfo
  { dataConInfoTypeDeclaration :: !TypeDeclaration
  , dataConInfoDataConDefinition :: !DataConDefinition
  , dataConInfoType :: !Type
  , dataConInfoANFType :: !ANF.Type
  , dataConInfoConstructorIndex :: !Int
  } deriving (Show, Eq)

dataConInfos :: TypeDeclaration -> [(Located DataConName, DataConInfo)]
dataConInfos tyDecl@(TypeDeclaration (TyConDefinition tyConName tyVars) dataConDefs) = mkDataConPair <$> zip [0..] dataConDefs
 where
  mkDataConPair (index, dataDef@(DataConDefinition name mTyArg)) =
    let
      tyVars' = TyVar . fromLocated <$> tyVars
      tyApp = foldTyApp $ NE.fromList $ TyCon (fromLocated tyConName) : tyVars'
      ty = foldTyFun (NE.fromList $ maybeToList mTyArg ++ [tyApp])
      tyForall = maybe ty (\varsNE -> TyForall varsNE ty) (NE.nonEmpty $ fromLocated <$> tyVars)
      anfTy = typeRep tyDecl
    in (name, DataConInfo tyDecl dataDef tyForall anfTy index)
