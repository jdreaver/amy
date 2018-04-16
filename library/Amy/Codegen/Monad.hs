{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Monad
  ( runBlockGen
  , BlockGen
  , addInstruction
  , terminateBlock
  , addSymbolToTable
  , lookupSymbol
  , freshId
  , freshUnName
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import LLVM.AST as LLVM

import Amy.Names as Amy

newtype BlockGen a = BlockGen (State BlockGenState a)
  deriving (Functor, Applicative, Monad, MonadState BlockGenState)

runBlockGen :: BlockGen Operand -> [BasicBlock]
runBlockGen (BlockGen action) =
  let
    (operand, BlockGenState lastBlock blockStack _ _) = runState action (blockGenState "entry")
    blocks = reverse $ makeBasicBlock lastBlock (Do $ Ret (Just operand) []) : blockStack
  in blocks

data BlockGenState
  = BlockGenState
  { blockGenStateCurrentBlock :: !PartialBlock
  , blockGenStateBlockStack :: ![BasicBlock]
  , blockGenStateSymbolTable :: !(Map Amy.Name Operand)
  , blockGenStateLastId :: !Word
  } deriving (Show, Eq)

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
makeBasicBlock (PartialBlock name' instructions) terminator = BasicBlock name' (reverse instructions) terminator

addInstruction :: Named Instruction -> BlockGen ()
addInstruction instr =
  modify' $ \s -> s { blockGenStateCurrentBlock = addInstruction' (blockGenStateCurrentBlock s) }
 where
  addInstruction' block = block { partialBlockInstructions = instr : partialBlockInstructions block }

terminateBlock :: Named Terminator -> LLVM.Name -> BlockGen ()
terminateBlock term newName =
  modify'
  (\s@(BlockGenState current stack _ _) ->
     s
     { blockGenStateCurrentBlock = partialBlock newName
     , blockGenStateBlockStack = makeBasicBlock current term : stack
     }
  )

addSymbolToTable :: Amy.Name -> Operand -> BlockGen ()
addSymbolToTable name' op = modify' (\s -> s { blockGenStateSymbolTable = Map.insert name' op (blockGenStateSymbolTable s) })

lookupSymbol :: Amy.Name -> BlockGen (Maybe Operand)
lookupSymbol name' =
  Map.lookup name' <$> gets blockGenStateSymbolTable

freshId :: BlockGen Word
freshId = do
  id' <- gets blockGenStateLastId
  modify' (\s -> s { blockGenStateLastId = 1 + blockGenStateLastId s })
  pure id'

freshUnName :: BlockGen LLVM.Name
freshUnName = UnName <$> freshId
