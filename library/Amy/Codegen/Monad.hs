{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Monad
  ( runBlockGen
  , BlockGen
  , addInstruction
  , terminateBlock
  ) where

import Control.Monad.State.Strict
import LLVM.AST as LLVM

newtype BlockGen a = BlockGen (State BlockGenState a)
  deriving (Functor, Applicative, Monad, MonadState BlockGenState)

runBlockGen :: BlockGen Operand -> [BasicBlock]
runBlockGen (BlockGen action) =
  let
    (operand, BlockGenState lastBlock blockStack) = runState action (BlockGenState (partialBlock "entry") [])
    blocks = reverse $ makeBasicBlock lastBlock (Do $ Ret (Just operand) []) : blockStack
  in blocks

data BlockGenState
  = BlockGenState
  { blockGenStateCurrentBlock :: PartialBlock
  , blockGenStateBlockStack :: [BasicBlock]
  } deriving (Show, Eq)

-- | In-progress 'BasicBlock' without terminator
data PartialBlock
  = PartialBlock
  { partialBlockName :: LLVM.Name
  , partialBlockInstructions :: [Named Instruction] -- NB: In reverse order
  } deriving (Show, Eq)

partialBlock :: LLVM.Name -> PartialBlock
partialBlock name' = PartialBlock name' []

makeBasicBlock :: PartialBlock -> Named Terminator -> BasicBlock
makeBasicBlock (PartialBlock name' instructions) terminator = BasicBlock name' instructions terminator

addInstruction :: Named Instruction -> BlockGen ()
addInstruction instr =
  modify' $ \s -> s { blockGenStateCurrentBlock = addInstruction' (blockGenStateCurrentBlock s) }
 where
  addInstruction' block = block { partialBlockInstructions = instr : partialBlockInstructions block }

terminateBlock :: Named Terminator -> LLVM.Name -> BlockGen ()
terminateBlock term newName =
  modify'
  (\(BlockGenState current stack) ->
     BlockGenState
     (partialBlock newName)
     (makeBasicBlock current term : stack)
  )
