module Amy.Codegen.CaseBlocks
  ( CaseBlocks(..)
  , caseBlocks
  , CaseLiteralBlock(..)
  , CaseDefaultBlock(..)
  , CaseEndBlock(..)
  , literalConstant
  ) where

import Data.Map.Strict (Map)
import LLVM.AST as LLVM
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float as F

import Amy.ANF.AST as ANF
import Amy.Codegen.TypeCompilation
import Amy.Literal

data CaseBlocks
  = CaseBlocks
  { caseBlocksSwitchDefaultBlockName :: !Name
  , caseBlocksLiteralBlocks :: ![CaseLiteralBlock]
  , caseBlocksDefaultBlock :: !(Maybe CaseDefaultBlock)
  , caseBlocksEndBlock :: !CaseEndBlock
  } deriving (Show, Eq)

data CaseLiteralBlock
  = CaseLiteralBlock
  { caseLiteralBlockExpr :: !Expr
  , caseLiteralBlockName :: !Name
  , caseLiteralBlockNextName :: !Name
  , caseLiteralBlockConstant :: !C.Constant
  , caseLiteralBlockBind :: !(Maybe (Typed Ident))
  } deriving (Show, Eq)

data CaseDefaultBlock
  = CaseDefaultBlock
  { caseDefaultBlockExpr :: !Expr
  , caseDefaultBlockName :: !Name
  , caseDefaultBlockNextName :: !Name
  , caseDefaultBlockIdent :: !(Typed Ident)
  } deriving (Show, Eq)

data CaseEndBlock
  = CaseEndBlock
  { caseEndBlockName :: !Name
  , caseEndBlockOperandName :: !Name
  , caseEndBlockType :: !ANF.Type
  } deriving (Show, Eq)

caseBlocks
  :: (String -> Name)
  -> Map DataConstructor DataConRep
  -> Case
  -> CaseBlocks
caseBlocks mkBlockName compilationMethods (Case _ bind matches mDefault ty) =
  let
    -- Compute names for everything
    defaultBlockName = mkBlockName "case.default."
    endBlockName = mkBlockName "case.end."
    endOpName = mkBlockName "end."
    literalBlockNames = mkBlockName . (\i -> "case." ++ i ++ ".") . show <$> [0 .. (length matches - 1)]
    nextLiteralBlockNames = drop 1 literalBlockNames ++ [endBlockName]

    -- Compute default block
    defaultBlockNextName =
      case literalBlockNames of
        [] -> endBlockName
        firstBlockName:_ -> firstBlockName
    mkDefaultBlock expr =
      CaseDefaultBlock
      { caseDefaultBlockExpr = expr
      , caseDefaultBlockName = defaultBlockName
      , caseDefaultBlockNextName = defaultBlockNextName
      , caseDefaultBlockIdent = bind
      }
    defaultBlock = mkDefaultBlock <$> mDefault
    switchDefaultBlockName =
      case (defaultBlock, literalBlockNames) of
        (Just block, _) -> caseDefaultBlockName block
        (Nothing, firstBlockName:_) -> firstBlockName
        (Nothing, []) -> endBlockName

    -- Compute literal blocks
    literalBlock (Match pat expr, (blockName, nextBlockName)) =
      let
        (constant, mBind) =
          case pat of
            PLit lit -> (literalConstant lit, Nothing)
            PCons (PatCons (DataConInfo _ con) mArg _) ->
              (constructorConstant compilationMethods con, mArg)
      in
        CaseLiteralBlock
        { caseLiteralBlockExpr = expr
        , caseLiteralBlockName = blockName
        , caseLiteralBlockNextName = nextBlockName
        , caseLiteralBlockConstant = constant
        , caseLiteralBlockBind = mBind
        }
    literalBlocks = literalBlock <$> zip matches (zip literalBlockNames nextLiteralBlockNames)

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
    , caseBlocksLiteralBlocks = literalBlocks
    , caseBlocksDefaultBlock = defaultBlock
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
    CompileEnum i intBits -> C.Int intBits (fromIntegral i)
    CompileTaggedUnion _ i intBits -> C.Int intBits (fromIntegral i)
