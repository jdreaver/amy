{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Monad
  ( runCodeGen
  , CodeGen
  , CodeGenState
  , runBlockGen
  , BlockGen
  , addInstruction
  , terminateBlock
  , currentBlockName
  , freshId
  , freshUnName
  , topLevelType
  , genExternalGlobal
  , genExternalType
  ) where

import Control.Monad.State.Strict
import qualified Control.Monad.Trans.State.Strict as TS
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LLVM.AST as LLVM
import qualified LLVM.AST.Global as G

import Amy.ANF.AST as ANF

newtype CodeGen a = CodeGen (State CodeGenState a)
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)

runCodeGen :: [(IdentName, ANF.Type)] -> CodeGen [Definition] -> [Definition]
runCodeGen topLevelTypes (CodeGen action) =
  let
    typeMap = Map.fromList topLevelTypes
    cgState = CodeGenState typeMap Map.empty
    (result, state') = runState action cgState
    externalDefs = fmap snd . Map.toAscList . codeGenStateExternalFunctions $ state'
  in externalDefs ++ result

data CodeGenState
  = CodeGenState
  { codeGenStateTopLevelTypes :: !(Map IdentName ANF.Type)
  , codeGenStateExternalFunctions :: !(Map Name Definition)
  }

newtype BlockGen a = BlockGen (StateT BlockGenState CodeGen a)
  deriving (Functor, Applicative, Monad, MonadState BlockGenState)

runBlockGen :: BlockGen Operand -> CodeGen [BasicBlock]
runBlockGen (BlockGen action) = do
  (operand, BlockGenState lastBlock blockStack _) <- runStateT action (blockGenState "entry")
  pure $ reverse $ makeBasicBlock lastBlock (Do $ Ret (Just operand) []) : blockStack

data BlockGenState
  = BlockGenState
  { blockGenStateCurrentBlock :: !PartialBlock
  , blockGenStateBlockStack :: ![BasicBlock]
    -- TOOD: We need to make sure this last ID is higher than the max ID from
    -- the ANF AST. We also need to share this last ID across BlockGen
    -- invocations from different functions.
  , blockGenStateLastId :: !Word
  } deriving (Show, Eq)

blockGenState :: LLVM.Name -> BlockGenState
blockGenState name' = BlockGenState (partialBlock name') [] 0

liftCodeGen :: CodeGen a -> BlockGen a
liftCodeGen = BlockGen . lift

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
  (\s@(BlockGenState current stack _ ) ->
     s
     { blockGenStateCurrentBlock = partialBlock newName
     , blockGenStateBlockStack = makeBasicBlock current term : stack
     }
  )

currentBlockName :: BlockGen LLVM.Name
currentBlockName = gets (partialBlockName . blockGenStateCurrentBlock)

freshId :: BlockGen Word
freshId = do
  id' <- gets blockGenStateLastId
  modify' (\s -> s { blockGenStateLastId = 1 + blockGenStateLastId s })
  pure id'

freshUnName :: BlockGen LLVM.Name
freshUnName = UnName <$> freshId

topLevelType :: IdentName -> BlockGen (Maybe ANF.Type)
topLevelType ident = liftCodeGen $ CodeGen $ TS.gets (Map.lookup ident . codeGenStateTopLevelTypes)

genExternalGlobal :: Global -> BlockGen ()
genExternalGlobal global = genExternalDefinition (G.name global) (GlobalDefinition global)

genExternalType :: Name -> LLVM.Type -> BlockGen ()
genExternalType name ty = genExternalDefinition name (TypeDefinition name (Just ty))

genExternalDefinition :: Name -> Definition -> BlockGen ()
genExternalDefinition name def = liftCodeGen $ CodeGen $ do
  mExistingDef <- TS.gets (Map.lookup name . codeGenStateExternalFunctions)
  for_ mExistingDef $ \existingDef ->
    when (existingDef /= def) $
      error $ "Definitions don't match! " ++ show (name, existingDef, def)
  TS.modify' $ \s ->
    s { codeGenStateExternalFunctions = Map.insert name def (codeGenStateExternalFunctions s) }
