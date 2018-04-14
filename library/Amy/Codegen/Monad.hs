{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Monad
  ( FunctionGen
  , CodegenIdentifier(..)
  , runGenBlocks
  , generateId
  , currentId
  , generateUnName
  , startNewBlock
  , addNameToSymbolTable
  , lookupSymbol
  , lookupSymbolOrError
  , addInstruction
  , addUnNamedInstruction
  , instr
  , br
  , cbr
  , phi
  , ret
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import LLVM.AST as LLVM

import Amy.Errors
import Amy.Names as Amy

newtype FunctionGen a = FunctionGen (ExceptT [Error] (State FunctionGenState) a)
  deriving (Functor, Applicative, Monad, MonadState FunctionGenState, MonadError [Error])

execFunctionGen :: FunctionGen a -> Either [Error] FunctionGenState
execFunctionGen (FunctionGen action) =
  let
    (result, state') = runState (runExceptT action) (defaultFunctionGenState "entry")
  in const state' <$> result

-- | Code generation state when generating a single function.
data FunctionGenState
  = FunctionGenState
  { functionGenStateLastId :: !Word
    -- ^ Last incrementing ID. Used to generate intermediate instruction names.
  , functionGenStateBlockStack :: !(NonEmpty BlockGenState)
    -- ^ Stack of simple blocks. Needs to be reversed before generating LLVM.
  , functionGenStateSymbolTable :: !(Map Amy.Name CodegenIdentifier)
    -- ^ Map from Amy variable names to operands
  } deriving (Show, Eq)

data CodegenIdentifier
  = LocalOperand !Operand
    -- ^ Operand for a variable defined locally
  | GlobalFunctionNoArgs !Operand
    -- ^ Operand for a globally-defined function with no arguments
  | GlobalFunction !Operand
    -- ^ Name of a globally-defined function with arguments
  deriving (Show, Eq)

data BlockGenState
  = BlockGenState
  { blockGenStateBlockName :: !LLVM.Name
    -- ^ Name of the block
  , blockGenStateInstructionStack :: ![Named Instruction]
    -- ^ Stack of instructions for the current function (needs to be reversed
    -- before sending to LLVM).
  , blockGenStateTerminator :: !(Maybe (Named Terminator))
    -- ^ Final terminator of block
  } deriving (Show, Eq)

defaultFunctionGenState :: LLVM.Name -> FunctionGenState
defaultFunctionGenState name' =
  FunctionGenState
  { functionGenStateLastId = 0
  , functionGenStateBlockStack = defaultBlockGenState name' :| []
  , functionGenStateSymbolTable = Map.empty
  }

defaultBlockGenState :: LLVM.Name -> BlockGenState
defaultBlockGenState name' =
  BlockGenState
  { blockGenStateBlockName =  name'
  , blockGenStateInstructionStack = []
  , blockGenStateTerminator = Nothing
  }

-- | Runs the 'FunctionGen' action and produces the 'BasicBlock's for it.
runGenBlocks
  :: FunctionGen Operand
  -> Either [Error] [BasicBlock]
runGenBlocks action = do
  state' <- execFunctionGen (action >>= ret)
  let
    blockStates = reverse $ toList $ functionGenStateBlockStack state'
    genBlock :: BlockGenState -> BasicBlock
    genBlock blockState =
      BasicBlock
        (blockGenStateBlockName blockState)
        (reverse $ blockGenStateInstructionStack blockState)
        (fromMaybe (error ("Block has no terminator " ++ show blockState)) $ blockGenStateTerminator blockState)
  pure $ genBlock <$> blockStates

-- | Generate a new unique ID and increment the last ID of the state.
generateId :: FunctionGen Word
generateId = do
  modify $ \s -> s { functionGenStateLastId = functionGenStateLastId s + 1 }
  currentId

-- | Get the current latest ID
currentId :: FunctionGen Word
currentId = gets functionGenStateLastId

-- | Generate a new name using 'UnName' and 'generateId'
generateUnName :: FunctionGen LLVM.Name
generateUnName = UnName <$> generateId

startNewBlock :: LLVM.Name -> FunctionGen ()
startNewBlock blockName =
  modify' $ \s ->
    s
    { functionGenStateBlockStack = NE.cons newBlock (functionGenStateBlockStack s)
    }
 where
  newBlock = defaultBlockGenState blockName

modifyCurrentBlock :: (BlockGenState -> BlockGenState) -> FunctionGen ()
modifyCurrentBlock f =
  modify' $ \s ->
    let
      blockStack = functionGenStateBlockStack s
      currentBlock = NE.head blockStack
      restBlocks = NE.tail blockStack
    in
      s
      { functionGenStateBlockStack = f currentBlock :| restBlocks
      }

addNameToSymbolTable :: Amy.Name -> CodegenIdentifier -> FunctionGen ()
addNameToSymbolTable name ident =
  modify' $
    \s ->
      s
      { functionGenStateSymbolTable =
        Map.insert name ident (functionGenStateSymbolTable s)
      }

lookupSymbol :: Amy.Name -> FunctionGen (Maybe CodegenIdentifier)
lookupSymbol name = Map.lookup name <$> gets functionGenStateSymbolTable

lookupSymbolOrError :: Amy.Name -> FunctionGen CodegenIdentifier
lookupSymbolOrError name = maybe (throwError [CodegenMissingSymbol name]) pure =<< lookupSymbol name

-- | Adds an instruction to the stack
addInstruction :: Named Instruction -> FunctionGen ()
addInstruction instruction =
  modifyCurrentBlock $ \block ->
    block
    { blockGenStateInstructionStack =
      instruction : blockGenStateInstructionStack block
    }

-- | Add an instruction to the stack and return the 'UnName'
addUnNamedInstruction :: Instruction -> FunctionGen LLVM.Name
addUnNamedInstruction instruction = do
  instructionName <- generateUnName
  addInstruction (instructionName := instruction)
  pure instructionName

instr :: Type -> Instruction -> FunctionGen Operand
instr ty = fmap (LocalReference ty) . addUnNamedInstruction

-- | Sets the 'Terminator' for the current block
terminator :: Named Terminator -> FunctionGen ()
terminator term =
  modifyCurrentBlock $ \block ->
    block
    { blockGenStateTerminator = Just term
    }

br :: LLVM.Name -> FunctionGen ()
br val = terminator $ Do $ Br val []

cbr :: Operand -> LLVM.Name -> LLVM.Name -> FunctionGen ()
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, LLVM.Name)] -> FunctionGen Operand
phi ty incoming = instr ty $ Phi ty incoming []

ret :: Operand -> FunctionGen ()
ret val = terminator $ Do $ Ret (Just val) []
