{-# LANGUAGE TupleSections #-}

module Amy.Codegen.CaseBlocks
  ( CaseBlocks(..)
  , caseBlocks
  , CaseLiteralBlock(..)
  , CaseVarBlock(..)
  , CaseEndBlock(..)
  , LiteralPattern(..)
  , VarPattern(..)
  , literalConstant
  ) where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import LLVM.AST as LLVM
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float as F

import Amy.ANF.AST as ANF
import Amy.Codegen.TypeCompilation
import Amy.Literal

data CaseBlocks
  = CaseBlocks
  { caseBlocksSwitchDefaultBlockName :: !Name
  , caseBlocksDefaultBlock :: !(Maybe CaseVarBlock)
  , caseBlocksLiteralBlocks :: ![CaseLiteralBlock]
  , caseBlocksEndBlock :: !CaseEndBlock
  } deriving (Show, Eq)

data CaseLiteralBlock
  = CaseLiteralBlock
  { caseLiteralBlockExpr :: !Expr
  , caseLiteralBlockName :: !Name
  , caseLiteralBlockNextName :: !Name
  , caseLiteralBlockConstant :: !C.Constant
  } deriving (Show, Eq)

data CaseVarBlock
  = CaseVarBlock
  { caseVarBlockExpr :: !Expr
  , caseVarBlockName :: !Name
  , caseVarBlockNextName :: !Name
  , caseVarBlockIdent :: !Ident
  } deriving (Show, Eq)

data CaseEndBlock
  = CaseEndBlock
  { caseEndBlockName :: !Name
  , caseEndBlockOperandName :: !Name
  , caseEndBlockType :: !ANF.Type
  } deriving (Show, Eq)

data LiteralPattern
  = LitLiteralPattern !Literal
  | ConsEnumPattern !DataConstructor
  -- Not implemented yet: ConsLiteralPattern !(Typed ConstructorName, Literal)
  deriving (Show, Eq)

-- TODO: Once we have proper case-in-case desugaring, refactor this logic to
-- remove all the special cases for constructors.

literalPattern :: Pattern -> Maybe LiteralPattern
literalPattern (PLit lit) = Just $ LitLiteralPattern lit
literalPattern (PCons (PatCons consName Nothing _)) = Just $ ConsEnumPattern consName
literalPattern _ = Nothing

data VarPattern
  = VarVarPattern !Ident
  | ConsVarPattern !DataConstructor !Ident
  -- TODO: Wildcard pattern
  deriving (Show, Eq)

varPattern :: Pattern -> Maybe VarPattern
varPattern (PVar (Typed _ ident)) = Just $ VarVarPattern ident
varPattern (PCons (PatCons cons (Just (Typed _ arg)) _)) = Just $ ConsVarPattern cons arg
varPattern _ = Nothing

caseBlocks
  :: (String -> Name)
  -> Map DataConstructor DataConRep
  -> Case
  -> CaseBlocks
caseBlocks mkBlockName compilationMethods (Case _ matches ty) =
  let
    -- Extract patterns
    blockPattern :: (Pattern -> Maybe a) -> Match -> Maybe (Expr, a)
    blockPattern maybePat (Match pat expr) = (expr,) <$> maybePat pat
    literalPatterns = mapMaybe (blockPattern literalPattern) (toList matches)
    varPatterns = mapMaybe (blockPattern varPattern) (toList matches)

    -- TODO: Ensure that no patterns fell through the cracks. The length of the
    -- literal plus var patterns should equal the langth of all the matches.

    -- Compute names for everything
    defaultBlockName = mkBlockName "case.default."
    endBlockName = mkBlockName "case.end."
    endOpName = mkBlockName "end."
    literalBlockNames = mkBlockName . (\i -> "case." ++ i ++ ".") . show <$> [0 .. (length literalPatterns - 1)]
    nextLiteralBlockNames = drop 1 literalBlockNames ++ [endBlockName]

    defaultBlockNextName =
      case literalBlockNames of
        [] -> endBlockName
        firstBlockName:_ -> firstBlockName

    -- Figure out the default block from the variable matches
    -- TODO: Have a proper default block if we can't find one,
    -- hopefully something that prints an error? Having a default should
    -- probably be handled in Core/ANF..
    firstVarPattern = if null varPatterns then Nothing else Just (head varPatterns)
    varBlock (expr, pat) =
      CaseVarBlock
      { caseVarBlockExpr = expr
      , caseVarBlockName = defaultBlockName
      , caseVarBlockNextName = defaultBlockNextName
      , caseVarBlockIdent =
          case pat of
            VarVarPattern ident -> ident
            ConsVarPattern _ ident -> ident
      }
    defaultBlock = varBlock <$> firstVarPattern
    switchDefaultBlockName =
      case (defaultBlock, literalBlockNames) of
        (Just block, _) -> caseVarBlockName block
        (Nothing, firstBlockName:_) -> firstBlockName
        (Nothing, []) -> endBlockName

    -- Combine the cons and lit matches. TODO: Is it bad that they are out of
    -- their original order?
    literalBlock ((expr, pat), (blockName, nextBlockName)) =
      CaseLiteralBlock
      { caseLiteralBlockExpr = expr
      , caseLiteralBlockName = blockName
      , caseLiteralBlockNextName = nextBlockName
      , caseLiteralBlockConstant =
          case pat of
            LitLiteralPattern lit -> literalConstant lit
            ConsEnumPattern cons -> constructorConstant compilationMethods cons
      }
    literalBlocks = literalBlock <$> zip literalPatterns (zip literalBlockNames nextLiteralBlockNames)

    -- Compute end block
    endBlock =
      CaseEndBlock
      { caseEndBlockName = endBlockName
      , caseEndBlockOperandName = endOpName
      , caseEndBlockType = ty
      }
  in
    CaseBlocks
    { caseBlocksSwitchDefaultBlockName = switchDefaultBlockName
    , caseBlocksDefaultBlock = defaultBlock
    , caseBlocksLiteralBlocks = literalBlocks
    , caseBlocksEndBlock = endBlock
    }

literalConstant :: Literal -> C.Constant
literalConstant lit =
  case lit of
    LiteralInt i -> C.Int 64 (fromIntegral i)
    LiteralDouble x -> C.Float (F.Double x)

constructorConstant
  :: Map DataConstructor DataConRep
  -> DataConstructor
  -> C.Constant
constructorConstant compilationMethods cons =
  case findCompilationMethod cons compilationMethods of
    CompileUnboxed _ -> error $ "Cannot unbox, we have an enum! " ++ show cons
    CompileEnum i intBits -> C.Int intBits (fromIntegral i)
    CompileTaggedUnion _ _ _ -> error $ "Cannot compile tagged pairs, we have an enum! " ++ show cons
