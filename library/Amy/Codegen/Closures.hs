{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Closures
  ( createClosure
  , callClosure
  , closureStructName
  ) where

import Data.Foldable (for_)
import LLVM.AST as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as LLVM

import Amy.Codegen.Malloc
import Amy.Codegen.Monad
import Amy.Codegen.TypeConversion

createClosure :: LLVM.Name -> LLVM.Name -> Type -> Int -> [Operand] -> BlockGen Operand
createClosure name' func funcTy arity argOps = do
  -- Make sure create_closure definition is generated
  genExternalType closureStructName closureStructType
  genExternalGlobal createClosureDefinition

  -- Bitcast function pointer
  funcOp <-
    namedInstruction
      Nothing
      (BitCast (ConstantOperand $ C.GlobalReference funcTy func) closureFuncPointerType [])
      closureFuncPointerType

  -- Pack args into an array
  argsArrayOp <- packInt64Array argOps

  -- Call create_closure RTS function
  let
    createClosureOp = ConstantOperand $ C.GlobalReference (PointerType createClosureFunctionType (AddrSpace 0)) createClosureFunctionName
    createClosureArgs =
      (\arg -> (arg, [])) <$>
      [ ConstantOperand $ C.Int 8 (fromIntegral arity)
      , funcOp
      , ConstantOperand $ C.Int 8 (fromIntegral $ length argOps)
      , argsArrayOp
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
    arrayOpType = ArrayType (fromIntegral $ length operands) (IntegerType 64)
  arrayOp <- callMalloc arrayOpName arrayOpType

  -- Pack array
  for_ (zip operands [0..]) $ \(argOp, i) -> do
    let ptrTy = PointerType (IntegerType 64) (AddrSpace 0)
    ptrOp <- namedInstruction Nothing (GetElementPtr False arrayOp [ConstantOperand (C.Int 32 1), ConstantOperand (C.Int 32 i)] []) ptrTy
    ptrOp' <- bitcastFromInt64Ptr ptrOp argOp
    addInstruction $ Do $ Store False ptrOp' argOp Nothing 0 []

  -- Bitcast to just a pointer
  let arrayPtrTy = PointerType (IntegerType 64) (AddrSpace 0)
  namedInstruction Nothing (BitCast arrayOp arrayPtrTy []) arrayPtrTy

 where
  bitcastFromInt64Ptr ptrOp argOp =
    case operandType argOp of
      IntegerType 64 -> pure ptrOp
      ty ->
        let ptrTy = PointerType ty (AddrSpace 0)
        in namedInstruction Nothing (BitCast ptrOp ptrTy []) ptrTy

namedInstruction :: Maybe LLVM.Name -> Instruction -> Type -> BlockGen Operand
namedInstruction mName instruction ty = do
  name' <- maybe freshUnName pure mName
  let op = LocalReference ty name'
  addInstruction $ name' := instruction
  pure op

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
  { resultType = VoidType
  , argumentTypes = []
  , isVarArg = True
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
      , Parameter (IntegerType 8) (UnName 2) [] -- numargs
      , Parameter int64ArrayType (UnName 3) [] -- env
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
    , IntegerType 8 -- numargs
    , int64ArrayType -- env
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
