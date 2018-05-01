{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Monad
  ( runBlockGen
  , BlockGen
  , addInstruction
  , terminateBlock
  , currentBlockName
  , addSymbolToTable
  , lookupSymbol
  , freshId
  , freshUnName
  , topLevelType
  , compilationMethods
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LLVM.AST as LLVM

import Amy.ANF.AST as ANF
import Amy.Codegen.TypeConstructors

newtype BlockGen a = BlockGen (ReaderT BlockGenRead (State BlockGenState) a)
  deriving (Functor, Applicative, Monad, MonadReader BlockGenRead, MonadState BlockGenState)

runBlockGen :: [(ANF.Ident, ANF.Type)] -> [ANF.TypeDeclaration] -> BlockGen Operand -> [BasicBlock]
runBlockGen topLevelTypes typeDeclarations (BlockGen action) =
  let
    typeMap = Map.fromList topLevelTypes
    compilationMethods' = Map.unions $ typeCompilationMethod <$> typeDeclarations
    readState = BlockGenRead typeMap compilationMethods'
    (operand, BlockGenState lastBlock blockStack _ _) = runState (runReaderT action readState) (blockGenState "entry")
    blocks = reverse $ makeBasicBlock lastBlock (Do $ Ret (Just operand) []) : blockStack
  in blocks

data BlockGenState
  = BlockGenState
  { blockGenStateCurrentBlock :: !PartialBlock
  , blockGenStateBlockStack :: ![BasicBlock]
  , blockGenStateSymbolTable :: !(Map ANF.Ident Operand)
  , blockGenStateLastId :: !Word
  } deriving (Show, Eq)

data BlockGenRead
  = BlockGenRead
  { blockGenReadTopLevelTypes :: !(Map ANF.Ident ANF.Type)
  , blockGenReadCompilationMethods :: !(Map ANF.ConstructorName TypeCompilationMethod)
  }

blockGenState :: LLVM.Name -> BlockGenState
blockGenState name' = BlockGenState (partialBlock name') [] Map.empty 0

-- | In-progress 'BasicBlock' without terminator
data PartialBlock
  = PartialBlock
  { partialBlockName :: LLVM.Name
  , partialBlockInstructions :: [Named Instruction] -- NB: In reverse order
  } deriving (Show, Eq)

partialBlock :: LLVM.Name -> PartialBlock
partialBlock name' = PartialBlock name' []

makeBasicBlock :: PartialBlock -> Named Terminator -> BasicBlock
makeBasicBlock (PartialBlock name' instructions) = BasicBlock name' (reverse instructions)

addInstruction :: Named Instruction -> BlockGen ()
addInstruction instr =
  modify' $ \s -> s { blockGenStateCurrentBlock = addInstruction' (blockGenStateCurrentBlock s) }
 where
  addInstruction' block = block { partialBlockInstructions = instr : partialBlockInstructions block }

terminateBlock :: Named Terminator -> LLVM.Name -> BlockGen ()
terminateBlock term newName =
  modify'
  (\s@(BlockGenState current stack _ _ ) ->
     s
     { blockGenStateCurrentBlock = partialBlock newName
     , blockGenStateBlockStack = makeBasicBlock current term : stack
     }
  )

currentBlockName :: BlockGen LLVM.Name
currentBlockName = gets (partialBlockName . blockGenStateCurrentBlock)

addSymbolToTable :: ANF.Ident -> Operand -> BlockGen ()
addSymbolToTable ident op = modify' (\s -> s { blockGenStateSymbolTable = Map.insert ident op (blockGenStateSymbolTable s) })

lookupSymbol :: ANF.Ident -> BlockGen (Maybe Operand)
lookupSymbol ident =
  Map.lookup ident <$> gets blockGenStateSymbolTable

freshId :: BlockGen Word
freshId = do
  id' <- gets blockGenStateLastId
  modify' (\s -> s { blockGenStateLastId = 1 + blockGenStateLastId s })
  pure id'

freshUnName :: BlockGen LLVM.Name
freshUnName = UnName <$> freshId

topLevelType :: ANF.Ident -> BlockGen (Maybe ANF.Type)
topLevelType ident = asks (Map.lookup ident . blockGenReadTopLevelTypes)

compilationMethods :: BlockGen (Map ANF.ConstructorName TypeCompilationMethod)
compilationMethods = asks blockGenReadCompilationMethods
