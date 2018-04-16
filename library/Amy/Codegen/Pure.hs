{-# LANGUAGE OverloadedStrings #-}

module Amy.Codegen.Pure
  ( codegenModule
  ) where

import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import LLVM.AST as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Global as LLVM

import Amy.ANF
import Amy.Codegen.Monad
import Amy.Literal
import Amy.Names as Amy
import Amy.Prim
import Amy.Type as T

codegenModule :: ANFModule -> Module
codegenModule (ANFModule bindings externs) =
  defaultModule
  { moduleName = "amy-module"
  , moduleDefinitions = (codegenExtern <$> externs) ++ (codegenTopLevelBinding <$> bindings)
  }

codegenExtern :: ANFExtern -> Definition
codegenExtern extern =
  let
    types = anfExternType extern
    paramTypes = llvmType <$> NE.init (typeToNonEmpty types)
    params =
      (\ty -> Parameter ty (UnName 0) []) <$> paramTypes
    returnType' = llvmType . NE.last . typeToNonEmpty $ types
  in
    GlobalDefinition
    functionDefaults
    { name = nameToLLVM $ anfExternName extern
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    }

codegenTopLevelBinding :: ANFBinding -> Definition
codegenTopLevelBinding binding =
  let
    argToParam (Typed ty name') = Parameter (llvmType ty) (nameToLLVM name') []
    params = argToParam <$> anfBindingArgs binding
    returnType' = llvmType $ anfBindingReturnType binding
    blocks = codegenExpr $ anfBindingBody binding
  in
    GlobalDefinition
    functionDefaults
    { name = nameToLLVM $ anfBindingName binding
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    , basicBlocks = blocks
    }

codegenExpr :: ANFExpr -> [BasicBlock]
codegenExpr expr = runBlockGen $ codegenExpr' expr

codegenExpr' :: ANFExpr -> BlockGen Operand
codegenExpr' (ANFEVal val) = pure $ valOperand val
codegenExpr' (ANFELet (ANFLet bindings expr)) = do
  -- TODO: If the binding body is just a literal, then we have to somehow
  -- assign that literal to the binding name.
  traverse_ codegenExpr' (anfBindingBody <$> bindings)
  codegenExpr' expr
codegenExpr' (ANFEIf (ANFIf pred' then' else' ifId ty)) = do
  let
    one = ConstantOperand $ C.Int 32 1
    true = one

  -- Name blocks and operands
  let
    mkIfName nameBase = LLVM.Name $ stringToShortBS $ nameBase ++ show ifId
    testOpName = mkIfName "test."
    testOpRef = LocalReference (IntegerType 1) testOpName
    thenBlockName = mkIfName "if.then."
    elseBlockName = mkIfName "if.else."
    endBlockName = mkIfName "if.end."
    endOpName = mkIfName "end."
    ty' = llvmType ty
    endOpRef = LocalReference ty' endOpName

  -- Generate predicate operation
  addInstruction (testOpName := ICmp IP.EQ true (valOperand pred') [])
  terminateBlock (Do $ CondBr testOpRef thenBlockName elseBlockName []) thenBlockName

  -- Generate then block
  thenOp <- codegenExpr' then'
  terminateBlock (Do $ Br endBlockName []) elseBlockName

  -- Generate else block
  elseOp <- codegenExpr' else'
  terminateBlock (Do $ Br endBlockName []) endBlockName

  -- Generate end block
  addInstruction $ endOpName := Phi ty' [(thenOp, thenBlockName), (elseOp, elseBlockName)] []
  pure endOpRef
codegenExpr' (ANFEApp (ANFApp func args' returnTy)) = do
  let
    argOps = valOperand <$> args'
  case func of
    ANFLit lit -> error $ "Tried to apply function on literal " ++ show lit
    ANFVar (Typed ty name') -> do
      let
        opName = LLVM.Name $ stringToShortBS $ "func." ++ show (1 :: Int) -- TODO Get an ID here
        instruction =
          case name' of
            PrimitiveName primName -> primitiveFunctionInstruction primName argOps
            IdentName (Ident nameText _ isTopLevel) ->
              let
                ty' = llvmType ty
                name'' = LLVM.Name $ textToShortBS nameText
                funcOperand =
                  if isTopLevel
                  then ConstantOperand $ C.GlobalReference ty' name''
                  else LocalReference ty' name''
                args'' = (\arg -> (arg, [])) <$> argOps
              in Call Nothing CC.C [] (Right funcOperand) args'' [] []
      addInstruction $ opName := instruction
      pure $ LocalReference (llvmPrimitiveType $ assertPrimitiveType returnTy) opName

valOperand :: ANFVal -> Operand
valOperand (ANFVar (Typed ty name')) = LocalReference (llvmType ty) (nameToLLVM name')
valOperand (ANFLit lit) =
  ConstantOperand $
    case lit of
      LiteralInt i -> C.Int 32 (fromIntegral i)
      LiteralDouble x -> C.Float (F.Double x)
      LiteralBool x -> C.Int 1 $ if x then 1 else 0

primitiveFunctionInstruction
  :: PrimitiveFunctionName
  -> [Operand]
  -> Instruction
primitiveFunctionInstruction primFuncName argumentOperands =
  let
    -- TODO: Better error checking here if number of operands doesn't match
    -- expected number. However, this should be type checked, so a simple panic
    -- should suffice.
    [op0, op1] = argumentOperands
    instruction =
      case primFuncName of
        PrimIAdd -> Add False False op0 op1 []
        PrimISub -> Sub False False op0 op1 []
        PrimIEquals -> ICmp IP.EQ op0 op1 []
        PrimIGreaterThan -> ICmp IP.SGT op0 op1 []
        PrimILessThan -> ICmp IP.SLT op0 op1 []

        PrimDAdd -> FAdd noFastMathFlags op0 op1 []
        PrimDSub -> FSub noFastMathFlags op0 op1 []
  in instruction

-- TODO: Add tests for this
llvmType :: T.Type PrimitiveType -> LLVM.Type
llvmType = go . typeToNonEmpty
 where
  go (ty :| []) =
    case ty of
      TyCon prim -> llvmPrimitiveType prim
      TyVar _ -> error "Can't handle polymorphic type arguments yet"
      t@TyArr{} -> mkFunctionType (t :| [])
  go ts = mkFunctionType ts
  mkFunctionType ts =
    PointerType
      FunctionType
      { resultType = llvmType $ NE.last ts
      , argumentTypes = llvmType <$> NE.init ts
      , isVarArg = False
      }
      (AddrSpace 0)

nameToLLVM :: Amy.Name -> LLVM.Name
nameToLLVM (PrimitiveName prim) = LLVM.Name $ stringToShortBS $ show prim
nameToLLVM (IdentName (Ident name' _ _)) = LLVM.Name $ textToShortBS name'

-- | Convert from a amy primitive type to an LLVM type
llvmPrimitiveType :: PrimitiveType -> LLVM.Type
llvmPrimitiveType IntType = IntegerType 32
llvmPrimitiveType DoubleType = FloatingPointType DoubleFP
llvmPrimitiveType BoolType = IntegerType 1

assertPrimitiveType :: T.Type PrimitiveType -> PrimitiveType
assertPrimitiveType (TyCon prim) = prim
assertPrimitiveType t = error $ "Expected PrimitiveType, got " ++ show t

textToShortBS :: Text -> ShortByteString
textToShortBS = BSS.toShort . encodeUtf8

stringToShortBS :: String -> ShortByteString
stringToShortBS = BSS.toShort . BS8.pack
