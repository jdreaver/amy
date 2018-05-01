{-# LANGUAGE TupleSections #-}

module Amy.Codegen.CaseBlocks
  ( CaseBlocks(..)
  , caseBlocks
  , CaseBlock(..)
  , LiteralPattern(..)
  , VarPattern(..)
  , ConsPattern(..)
  , ValPattern(..)
  , literalConstant
  ) where

import Data.Bifunctor (second)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Maybe (mapMaybe)
import LLVM.AST as LLVM
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float as F

import Amy.ANF.AST as ANF
import Amy.Codegen.TypeConstructors
import Amy.Literal

data CaseBlocks
  = CaseBlocks
  { caseBlocksDefaultBlock :: !(Maybe (CaseBlock VarPattern))
  , caseBlocksValueBlocks :: ![CaseBlock ValPattern]
  } deriving (Show, Eq)

data CaseBlock a
  = CaseBlock
  { caseBlockExpr :: !ANF.Expr
  , caseBlockName :: !Name
  , caseBlockConstant :: !(Maybe C.Constant)
  , caseBlockPattern :: !a
  } deriving (Show, Eq)

newtype LiteralPattern = LiteralPattern Literal
  deriving (Show, Eq)

newtype VarPattern = VarPattern (ANF.Typed ANF.Ident)
  deriving (Show, Eq)

newtype ConsPattern = ConsPattern ANF.ConstructorPattern
  deriving (Show, Eq)

data ValPattern
  = LiteralValPattern !LiteralPattern
  | ConsValPattern !ConsPattern
  deriving (Show, Eq)

caseBlocks
  :: (String -> Name)
  -> Map ANF.ConstructorName TypeCompilationMethod
  -> [ANF.Match]
  -> CaseBlocks
caseBlocks mkBlockName compilationMethods matches =
  let
    -- Extract patterns
    literalPatterns = mapMaybe (blockPattern literalPattern) matches
    varPatterns = mapMaybe (blockPattern varPattern) matches
    consPatterns = mapMaybe (blockPattern consPattern) matches

    -- This is the first wildcard match
    firstVarPattern = if null varPatterns then Nothing else Just (head varPatterns)
    defaultBlock = (\(expr, pat) -> CaseBlock expr (mkBlockName "default.") Nothing pat) <$> firstVarPattern

    -- Combine the cons and lit matches. TODO: Is it bad that they are out of
    -- their original order?
    mkValBlockName = mkBlockName . (\i -> "case." ++ i ++ ".") . show
    valuePatterns = (second LiteralValPattern <$> literalPatterns) ++ (second ConsValPattern <$> consPatterns)
    valueBlock ((expr, pat), i) =
      CaseBlock
      { caseBlockExpr = expr
      , caseBlockName = mkValBlockName i
      , caseBlockConstant =
          Just $
            case pat of
              LiteralValPattern (LiteralPattern lit) -> literalConstant lit
              ConsValPattern (ConsPattern cons) -> error "Handle this"
      , caseBlockPattern = pat
      }
    valueBlocks = valueBlock <$> zip valuePatterns [(0 :: Int)..]
  in
    CaseBlocks
    { caseBlocksDefaultBlock = defaultBlock
    , caseBlocksValueBlocks = valueBlocks
    }

blockPattern :: (ANF.Pattern -> Maybe a) -> ANF.Match -> Maybe (ANF.Expr, a)
blockPattern maybePat (ANF.Match pat expr) = (expr,) <$> maybePat pat

literalPattern :: ANF.Pattern -> Maybe LiteralPattern
literalPattern (ANF.PatternLit lit) = Just $ LiteralPattern lit
literalPattern _ = Nothing

varPattern :: ANF.Pattern -> Maybe VarPattern
varPattern (ANF.PatternVar ident) = Just $ VarPattern ident
varPattern _ = Nothing

consPattern :: ANF.Pattern -> Maybe ConsPattern
consPattern (ANF.PatternCons consPat) = Just $ ConsPattern consPat
consPattern _ = Nothing

literalConstant :: Literal -> C.Constant
literalConstant lit =
  case lit of
    LiteralInt i -> C.Int 64 (fromIntegral i)
    LiteralDouble x -> C.Float (F.Double x)
    LiteralBool x -> C.Int 1 $ if x then 1 else 0

constructorConstant
  :: Map ANF.ConstructorName TypeCompilationMethod
  -> ANF.ConstructorPattern
  -> C.Constant
constructorConstant compilationMethods (ANF.ConstructorPattern (ANF.Typed _ consName) mArg _) =
  let method = fromMaybe (error $ "No compilation method for " ++ show consName) $ Map.lookup consName compilationMethods
  in
    case (method, mArg) of
      (CompileUnboxed _, Just arg) -> arg
