{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Closures
  ( createClosure
  , callClosure
  , closureWrapperDefinition
  , knownFunctionApplication
  , closureStructName
  ) where

import Data.Foldable (for_)
import Data.Traversable (for)
import LLVM.AST as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as LLVM
import qualified LLVM.AST.Linkage as L

import Amy.Codegen.Malloc
import Amy.Codegen.Monad
import Amy.Codegen.TypeConversion
import Amy.Codegen.Utils

createClosure :: LLVM.Name -> LLVM.Name -> Int -> BlockGen Operand
createClosure name' func arity = do
  -- Make sure create_closure definition is generated
  genExternalType closureStructName closureStructType
  genExternalGlobal createClosureDefinition

  -- Bitcast function pointer
  let
    funcTy =
      PointerType
      FunctionType
      { resultType = closurePointerType
      , argumentTypes = [int64ArrayType]
      , isVarArg = False
      }
      (AddrSpace 0)
    funcOp = ConstantOperand $ C.GlobalReference funcTy func

  -- Call create_closure RTS function
  let
    createClosureOp = ConstantOperand $ C.GlobalReference (PointerType createClosureFunctionType (AddrSpace 0)) createClosureFunctionName
    createClosureArgs =
      (\arg -> (arg, [])) <$>
      [ ConstantOperand $ C.Int 8 (fromIntegral arity)
      , funcOp
      ]
  namedInstruction
    (Just name')
    (Call Nothing CC.C [] (Right createClosureOp) createClosureArgs [] [])
    (PointerType (NamedTypeReference closureStructName) (AddrSpace 0))

callClosure :: LLVM.Name -> Operand -> [Operand] -> Type -> BlockGen Operand
callClosure name' closureOp argOps retTy = do
  -- Make sure call_closure definition is generated
  genExternalType closureStructName closureStructType
  genExternalGlobal callClosureDefinition

  -- Pack args into array
  argsArrayOp <- packInt64Array argOps

  -- Call call_closure RTS function
  let
    callClosureOp = ConstantOperand $ C.GlobalReference (PointerType callClosureFunctionType (AddrSpace 0)) callClosureFunctionName
    callClosureArgs =
      (\arg -> (arg, [])) <$>
      [ closureOp
      , ConstantOperand $ C.Int 8 (fromIntegral $ length argOps)
      , argsArrayOp
      ]
  callOp <-
    namedInstruction
    Nothing
    (Call Nothing CC.C [] (Right callClosureOp) callClosureArgs [] [])
    (PointerType (NamedTypeReference closureStructName) (AddrSpace 0))

  convertClosurePointerOperand name' callOp retTy

packInt64Array :: [Operand] -> BlockGen Operand
packInt64Array operands = do
  -- Malloc array
  arrayOpName <- freshUnName
  let
    arraySize = fromIntegral $ 64 * length operands
  arrayOp <- callMalloc arrayOpName (Just arraySize) (IntegerType 64)

  -- Pack array
  for_ (zip operands [0..]) $ \(argOp, i) -> do
    let ptrTy = PointerType (IntegerType 64) (AddrSpace 0)
    ptrOp <- namedInstruction Nothing (GetElementPtr False arrayOp [ConstantOperand (C.Int 32 i)] []) ptrTy
    ptrOp' <- bitcastFromInt64Ptr ptrOp argOp
    addInstruction $ Do $ Store False ptrOp' argOp Nothing 0 []

  pure arrayOp

 where
  bitcastFromInt64Ptr ptrOp argOp =
    case operandType argOp of
      IntegerType 64 -> pure ptrOp
      ty ->
        let ptrTy = PointerType ty (AddrSpace 0)
        in namedInstruction Nothing (BitCast ptrOp ptrTy []) ptrTy

--
-- Conversion to/from Closure pointer
--

convertClosurePointerOperand :: LLVM.Name -> Operand -> Type -> BlockGen Operand
convertClosurePointerOperand name' operand targetTy =
  if targetTy == closurePointerType
  then
    bindOpToName name' operand
  else
    case targetTy of
      PointerType _ _ -> namedInstruction (Just name') (BitCast operand targetTy []) targetTy
      IntegerType _ -> namedInstruction (Just name') (PtrToInt operand targetTy []) targetTy
      _ -> do
        intOp <- namedInstruction Nothing (PtrToInt operand (IntegerType 64) []) (IntegerType 64)
        namedInstruction (Just name') (BitCast intOp targetTy []) targetTy

convertToClosurePointerOperand :: Operand -> BlockGen Operand
convertToClosurePointerOperand operand =
  if operandType operand == closurePointerType
  then pure operand
  else
    case operandType operand of
      PointerType _ _ -> namedInstruction Nothing (BitCast operand closurePointerType []) closurePointerType
      IntegerType _ -> namedInstruction Nothing (IntToPtr operand closurePointerType []) closurePointerType
      _ -> do
        intOp <- namedInstruction Nothing (BitCast operand (IntegerType 64) []) (IntegerType 64)
        namedInstruction Nothing (IntToPtr intOp closurePointerType []) (IntegerType 64)

--
-- Wrapper Functions
--

closureWrapperDefinition :: LLVM.Name -> LLVM.Name -> [Type] -> Type -> CodeGen Definition
closureWrapperDefinition wrapperName originalName argTys retTy = do
  blocks <- runBlockGen $ closureWrapperBlock originalName argTys retTy
  pure $
    GlobalDefinition
    functionDefaults
    { name = wrapperName
    , parameters = ([Parameter int64ArrayType "env" []], False)
    , LLVM.returnType = closurePointerType
    , basicBlocks = blocks
    , linkage = L.Private
    }

closureWrapperBlock :: LLVM.Name -> [Type] -> Type -> BlockGen Operand
closureWrapperBlock funcName argTys retTy = do
  let envOp = LocalReference int64ArrayType "env"

  -- Unpack arguments
  argOps <- for (zip argTys [0..]) $ \(argTy, i) -> do
    let ptrTy = PointerType (IntegerType 64) (AddrSpace 0)
    ptrOp <- namedInstruction Nothing (GetElementPtr False envOp [ConstantOperand (C.Int 32 i)] []) ptrTy
    ptrOp' <- bitcastFromInt64Ptr ptrOp argTy
    namedInstruction Nothing (Load False ptrOp' Nothing 0 []) argTy

  -- Call function
  let
    funcTy =
      FunctionType
      { resultType = retTy
      , argumentTypes = argTys
      , isVarArg = False
      }
    funcOp = ConstantOperand $ C.GlobalReference (PointerType funcTy (AddrSpace 0)) funcName
    funcArgs = (\arg -> (arg, [])) <$> argOps
  callOp <- namedInstruction Nothing (Call Nothing CC.C [] (Right funcOp) funcArgs [] []) retTy

  -- Cast and return
  convertToClosurePointerOperand callOp
 where
  bitcastFromInt64Ptr ptrOp argTy =
    case argTy of
      IntegerType 64 -> pure ptrOp
      ty ->
        let ptrTy = PointerType ty (AddrSpace 0)
        in namedInstruction Nothing (BitCast ptrOp ptrTy []) ptrTy

--
-- Known function application
--

knownFunctionApplication :: Name -> Name -> [Operand] -> [Type] -> Type -> Type -> BlockGen Operand
knownFunctionApplication name' funcName argOps argTys originalReturnTy returnTy = do
  -- Convert arguments to pointers if we have to
  argOps' <- traverse (uncurry (maybeConvertPointer Nothing)) (zip argOps argTys)

  -- Add call instruction
  let
    funcTy =
      FunctionType
      { resultType = returnTy
      , argumentTypes = argTys
      , isVarArg = False
      }
    funcOperand = ConstantOperand $ C.GlobalReference (LLVM.PointerType funcTy (AddrSpace 0)) funcName
    callInstruction = Call Nothing CC.C [] (Right funcOperand) ((\arg -> (arg, [])) <$> argOps') [] []
  if returnTy == originalReturnTy
    then do
      addInstruction $ name' := callInstruction
      pure $ LocalReference returnTy name'
    else do
      callName <- freshUnName
      addInstruction $ callName := callInstruction
      maybeConvertPointer (Just name') (LocalReference returnTy callName) originalReturnTy

--
-- Function signatures from RTS
--

int64ArrayType :: Type
int64ArrayType = PointerType (IntegerType 64) (AddrSpace 0)

closureStructName :: LLVM.Name
closureStructName = "struct.Closure"

closurePointerType :: Type
closurePointerType = PointerType (NamedTypeReference closureStructName) (AddrSpace 0)

closureStructType :: Type
closureStructType =
  StructureType False
  [ IntegerType 8
  , closureFuncPointerType
  , IntegerType 8
  , int64ArrayType
  ]

closureFuncPointerType :: Type
closureFuncPointerType =
  PointerType
  FunctionType
  { resultType = closurePointerType
  , argumentTypes = [int64ArrayType]
  , isVarArg = False
  }
  (AddrSpace 0)

-- N.B. All of these function signatures need to match what is in rts.c

createClosureDefinition :: Global
createClosureDefinition =
  functionDefaults
  { name = createClosureFunctionName
  , parameters =
    (
      [ Parameter (IntegerType 8) (UnName 0) [] -- arity
      , Parameter closureFuncPointerType (UnName 1) [] -- *f
      ]
    , False
    )
  , LLVM.returnType = closurePointerType
  }

createClosureFunctionType :: Type
createClosureFunctionType =
  FunctionType
  { resultType = closurePointerType
  , argumentTypes =
    [ IntegerType 8 -- arity
    , closureFuncPointerType -- *f
    ]
  , isVarArg = False
  }

createClosureFunctionName :: Name
createClosureFunctionName = "create_closure"

callClosureDefinition :: Global
callClosureDefinition =
  functionDefaults
  { name = callClosureFunctionName
  , parameters =
    (
      [ Parameter closurePointerType (UnName 0) [] -- closure
      , Parameter (IntegerType 8) (UnName 1) [] -- numargs
      , Parameter int64ArrayType (UnName 2) [] -- args
      ]
    , False
    )
  , LLVM.returnType = closurePointerType
  }

callClosureFunctionType :: Type
callClosureFunctionType =
  FunctionType
  { resultType = closurePointerType
  , argumentTypes =
    [ closurePointerType -- closure
    , IntegerType 8 -- numargs
    , int64ArrayType -- args
    ]
  , isVarArg = False
  }

callClosureFunctionName :: Name
callClosureFunctionName = "call_closure"
