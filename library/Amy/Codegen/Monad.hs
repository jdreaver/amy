{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Monad
  ( runCodeGen
  , CodeGen
  , runBlockGen
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

newtype CodeGen a = CodeGen (Reader CodeGenRead a)
  deriving (Functor, Applicative, Monad, MonadReader CodeGenRead)

runCodeGen :: [(ANF.Ident, ANF.Type)] -> [ANF.TypeDeclaration] -> CodeGen a -> a
runCodeGen topLevelTypes typeDeclarations (CodeGen action) =
  let
    typeMap = Map.fromList topLevelTypes
    compilationMethods' = Map.unions $ typeCompilationMethod <$> typeDeclarations
    readState = CodeGenRead typeMap compilationMethods'
  in runReader action readState

data CodeGenRead
  = CodeGenRead
  { codeGenReadTopLevelTypes :: !(Map ANF.Ident ANF.Type)
  , codeGenReadCompilationMethods :: !(Map ANF.ConstructorName TypeCompilationMethod)
  }

newtype BlockGen a = BlockGen (StateT BlockGenState CodeGen a)
  deriving (Functor, Applicative, Monad, MonadReader CodeGenRead, MonadState BlockGenState)

runBlockGen :: BlockGen Operand -> CodeGen [BasicBlock]
runBlockGen (BlockGen action) = do
  (operand, BlockGenState lastBlock blockStack _ _) <- runStateT action (blockGenState "entry")
  pure $ reverse $ makeBasicBlock lastBlock (Do $ Ret (Just operand) []) : blockStack

data BlockGenState
  = BlockGenState
  { blockGenStateCurrentBlock :: !PartialBlock
  , blockGenStateBlockStack :: ![BasicBlock]
  , blockGenStateSymbolTable :: !(Map ANF.Ident Operand)
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

topLevelType :: (MonadReader CodeGenRead m) => ANF.Ident -> m (Maybe ANF.Type)
topLevelType ident = asks (Map.lookup ident . codeGenReadTopLevelTypes)

compilationMethods :: (MonadReader CodeGenRead m) => m (Map ANF.ConstructorName TypeCompilationMethod)
compilationMethods = asks codeGenReadCompilationMethods
