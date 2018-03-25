{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rascal.Codegen.Monad
  ( FunctionGen
  , runFunctionGen
  , runGenBlock
  , generateUnName
  , addInstruction
  , addUnNamedInstruction
  ) where

import Control.Monad.State.Strict
import LLVM.AST

newtype FunctionGen a = FunctionGen { _unFunctionGen :: State FunctionGenState a }
  deriving (Functor, Applicative, Monad, MonadState FunctionGenState)

runFunctionGen :: FunctionGen a -> (a, FunctionGenState)
runFunctionGen (FunctionGen action) = runState action defaultFunctionGenState

-- | Code generation state when generating a single function.
data FunctionGenState
  = FunctionGenState
  { functionGenStateLastId :: !Word
    -- ^ Last incrementing ID. Used to generate intermediate instruction names.
  , functionGenStateInstructionStack :: [Named Instruction]
    -- ^ Stack of instructions for the current function (needs to be reversed
    -- before sending to LLVM).
  } deriving (Show, Eq)

defaultFunctionGenState :: FunctionGenState
defaultFunctionGenState =
  FunctionGenState
  { functionGenStateLastId = 0
  , functionGenStateInstructionStack = []
  }

-- | Runs the 'FunctionGen' action and produces the 'BasicBlock' for it.
runGenBlock
  :: Name
  -> FunctionGen Operand
  -> BasicBlock
runGenBlock name' action =
  let
    (returnOp, state') = runFunctionGen action
    instructions = reverse $ functionGenStateInstructionStack state'
    terminator = Do $ Ret (Just returnOp) []
  in BasicBlock name' instructions terminator

-- | Generate a new unique ID and increment the last ID of the state.
generateId :: FunctionGen Word
generateId = do
  modify $ \s -> s { functionGenStateLastId = functionGenStateLastId s + 1 }
  gets functionGenStateLastId

-- | Generate a new name using 'UnName' and 'generateId'
generateUnName :: FunctionGen Name
generateUnName = UnName <$> generateId

-- | Adds an instruction to the stack
addInstruction :: Named Instruction -> FunctionGen ()
addInstruction instruction =
  modify' $ \s -> s { functionGenStateInstructionStack = instruction : functionGenStateInstructionStack s }

-- | Add an instruction to the stack and return the 'UnName'
addUnNamedInstruction :: Instruction -> FunctionGen Name
addUnNamedInstruction instruction = do
  instructionName <- generateUnName
  addInstruction (instructionName := instruction)
  pure instructionName
