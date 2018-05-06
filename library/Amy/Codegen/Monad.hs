{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Monad
  ( runCodeGen
  , CodeGen
  , CodeGenRead
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
  , getTyConRep
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import LLVM.AST as LLVM

import Amy.ANF.AST as ANF
import Amy.Codegen.TypeCompilation
import Amy.Prim

newtype CodeGen a = CodeGen (Reader CodeGenRead a)
  deriving (Functor, Applicative, Monad, MonadReader CodeGenRead)

runCodeGen :: [(Ident, ANF.Type)] -> [TypeDeclaration] -> CodeGen a -> a
runCodeGen topLevelTypes typeDeclarations (CodeGen action) =
  let
    typeMap = Map.fromList topLevelTypes
    allTypeDeclarations = typeDeclarations ++ (fromPrimTypeDefinition <$> allPrimTypeDefinitions)
    (dataConCompilationMethods, tyConReps) =
      unzip $ typeCompilationMethod <$> allTypeDeclarations
    dataConCompilationMethods' = Map.unions dataConCompilationMethods
    tyConRepsMap = Map.fromList tyConReps
    readState = CodeGenRead typeMap dataConCompilationMethods' tyConRepsMap
  in runReader action readState

data CodeGenRead
  = CodeGenRead
  { codeGenReadTopLevelTypes :: !(Map Ident ANF.Type)
  , codeGenReadDataConReps :: !(Map DataConstructor DataConRep)
  , codeGenReadTyConReps :: !(Map TyConInfo TyConRep)
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
  , blockGenStateSymbolTable :: !(Map Ident Operand)
    -- TOOD: We need to make sure this last ID is higher than the max ID from
    -- the ANF AST. We also need to share this last ID across BlockGen
    -- invocations from different functions.
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

addSymbolToTable :: Ident -> Operand -> BlockGen ()
addSymbolToTable ident op = modify' (\s -> s { blockGenStateSymbolTable = Map.insert ident op (blockGenStateSymbolTable s) })

lookupSymbol :: Ident -> BlockGen (Maybe Operand)
lookupSymbol ident =
  Map.lookup ident <$> gets blockGenStateSymbolTable

freshId :: BlockGen Word
freshId = do
  id' <- gets blockGenStateLastId
  modify' (\s -> s { blockGenStateLastId = 1 + blockGenStateLastId s })
  pure id'

freshUnName :: BlockGen LLVM.Name
freshUnName = UnName <$> freshId

topLevelType :: (MonadReader CodeGenRead m) => Ident -> m (Maybe ANF.Type)
topLevelType ident = asks (Map.lookup ident . codeGenReadTopLevelTypes)

compilationMethods :: (MonadReader CodeGenRead m) => m (Map DataConstructor DataConRep)
compilationMethods = asks codeGenReadDataConReps

getTyConRep :: (MonadReader CodeGenRead m) => TyConInfo -> m TyConRep
getTyConRep tyCon = asks (fromMaybe err . Map.lookup tyCon . codeGenReadTyConReps)
  where
   err = error $ "Couldn't find TyConRep of TyConInfo " ++ show tyCon
