{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amy.Codegen.Pure
  ( codegenModule
  ) where

import Control.Monad.Reader
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable (for)
import LLVM.AST as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as LLVM
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Linkage as L

import Amy.ANF as ANF
import Amy.Codegen.CaseBlocks
import Amy.Codegen.Monad
import Amy.Codegen.TypeCompilation
import Amy.Codegen.TypeConversion
import Amy.Codegen.Utils
import Amy.Prim

codegenModule :: ANF.Module -> LLVM.Module
codegenModule (ANF.Module bindings externs typeDeclarations) =
  let
    topLevelTypes =
      ((\(ANF.Binding name' (ANF.Forall _ ty) _ _ _) -> (name', ty)) <$> bindings)
      ++ ((\(ANF.Extern name' ty) -> (name', ty)) <$> externs)
    definitions = runCodeGen topLevelTypes typeDeclarations $ do
      externs' <- traverse codegenExtern externs
      typeDefs <- catMaybes <$> traverse codegenTypeDeclaration typeDeclarations
      bindings' <- traverse codegenTopLevelBinding bindings
      pure $ externs' ++ typeDefs ++ bindings'
  in
    defaultModule
    { moduleName = "amy-module"
    , moduleDefinitions = definitions
    }

codegenExtern :: ANF.Extern -> CodeGen Definition
codegenExtern extern = do
  let
    (paramTypes, returnType') = argAndReturnTypes (ANF.externType extern)
    mkParam ty = do
      ty' <- llvmType ty
      pure $ Parameter ty' (UnName 0) []
  params <- traverse mkParam paramTypes
  retTy <- llvmType returnType'
  pure $
    GlobalDefinition
    functionDefaults
    { name = identToName $ ANF.externName extern
    , parameters = (params, False)
    , LLVM.returnType = retTy
    }

codegenTypeDeclaration :: ANF.TypeDeclaration -> CodeGen (Maybe Definition)
codegenTypeDeclaration (ANF.TypeDeclaration tyCon _) = do
  tyConRep <- getTyConRep tyCon
  pure $
    case tyConRep of
      TyConEnum _ -> Nothing
      TyConTaggedUnion name' intBits ->
        Just $
          TypeDefinition
          name'
          (Just $
            StructureType
            False
            [ IntegerType intBits
            , PointerType (IntegerType 64) (AddrSpace 0)
            ]
          )

codegenTopLevelBinding :: ANF.Binding -> CodeGen Definition
codegenTopLevelBinding binding = do
  let
    argToParam (ANF.Typed ty ident) = do
      ty' <- llvmType ty
      pure $ Parameter ty' (identToName ident) []
  params <- traverse argToParam (ANF.bindingArgs binding)
  returnType' <- llvmType $ ANF.bindingReturnType binding
  blocks <- codegenExpr $ ANF.bindingBody binding
  pure $
    GlobalDefinition
    functionDefaults
    { name = identToName $ ANF.bindingName binding
    , parameters = (params, False)
    , LLVM.returnType = returnType'
    , basicBlocks = blocks
    , linkage =
        if ANF.identText (ANF.bindingName binding) == "main"
          then L.External
          else L.Private
    }

argAndReturnTypes :: ANF.Type -> ([ANF.Type], ANF.Type)
argAndReturnTypes ty = (NE.init tyNE, NE.last tyNE)
 where
  tyNE = typeToNonEmpty ty

codegenExpr :: ANF.Expr -> CodeGen [BasicBlock]
codegenExpr expr = runBlockGen $ codegenExpr' expr

codegenExpr' :: ANF.Expr -> BlockGen Operand
codegenExpr' (ANF.EVal val) = valOperand val
codegenExpr' (ANF.ELet (ANF.Let bindings expr)) = do
  for_ bindings $ \binding -> do
    op <- codegenExpr' (ANF.bindingBody binding)
    addSymbolToTable (ANF.bindingName binding) op
  codegenExpr' expr
codegenExpr' (ANF.ECase case'@(ANF.Case scrutinee _ _ _ _)) = do
  caseId <- freshId

  compilationMethods' <- compilationMethods
  let
    mkCaseName nameBase = stringToName $ nameBase ++ show caseId
    (CaseBlocks switchDefaultBlockName literalBlocks mDefaultBlock endBlock) = caseBlocks mkCaseName compilationMethods' case'
    (CaseEndBlock endBlockName endOpName endType) = endBlock

  -- Generate the switch statement
  scrutineeOp <- valOperand scrutinee
  let
    switchNames = (\(CaseLiteralBlock _ name' _ constant) -> (constant, name')) <$> literalBlocks
  terminateBlock (Do $ Switch scrutineeOp switchDefaultBlockName switchNames []) switchDefaultBlockName

  -- Generate case blocks
  let
    generateBlockExpr expr nextBlockName = do
      op <- codegenExpr' expr
      -- N.B. The block name could have changed while generating the
      -- expression, so we need to get the "actual" block name.
      finalBlockName <- currentBlockName
      terminateBlock (Do $ Br endBlockName []) nextBlockName
      pure (op, finalBlockName)
    generateCaseDefaultBlock (CaseDefaultBlock expr _ nextBlockName ident) = do
      addSymbolToTable ident scrutineeOp
      generateBlockExpr expr nextBlockName
    generateCaseLiteralBlock (CaseLiteralBlock expr _ nextBlockName _) =
      generateBlockExpr expr nextBlockName

  mDefaultOpAndBlock <- traverse generateCaseDefaultBlock mDefaultBlock
  matchOpsAndBlocks <- traverse generateCaseLiteralBlock literalBlocks

  -- Generate end block
  endTy <- llvmType endType
  let
    allOpsAndBlocks = maybe id (:) mDefaultOpAndBlock matchOpsAndBlocks
    endOpRef = LocalReference endTy endOpName
  addInstruction $ endOpName := Phi endTy allOpsAndBlocks []
  pure endOpRef
codegenExpr' (ANF.EApp (ANF.App (ANF.VVal (ANF.Typed originalTy ident)) args' returnTy)) = do
  ty <- fromMaybe originalTy <$> topLevelType ident
  funcOperand <- valOperand (ANF.Var $ ANF.VVal $ ANF.Typed ty ident)
  let
    (argTys', returnTy') = argAndReturnTypes ty
  -- Convert arguments to pointers if we have to
  argOps <- for (zip args' argTys') $ \(arg, argTy) -> do
    originalOp <- valOperand arg
    argTy' <- llvmType argTy
    maybeConvertPointer originalOp argTy'

  -- Add call instruction
  opName <- freshUnName
  addInstruction $ opName := Call Nothing CC.C [] (Right funcOperand) ((\arg -> (arg, [])) <$> argOps) [] []

  -- Return operand, maybe converting it too
  returnTyLLVM <- llvmType returnTy
  returnTyLLVM' <- llvmType returnTy'
  maybeConvertPointer (LocalReference returnTyLLVM' opName) returnTyLLVM
codegenExpr' (ANF.EApp app@(ANF.App (ANF.VCons (ANF.Typed _ cons)) args' _)) = do
  method <- findCompilationMethod (dataConInfoCons cons) <$> compilationMethods
  case method of
    CompileEnum i intBits -> pure $ ConstantOperand $ C.Int intBits (fromIntegral i)
    CompileTaggedUnion structName intTag intBits -> do
      -- Allocate struct
      allocName <- freshUnName
      let allocOp = LocalReference (PointerType (NamedTypeReference structName) (AddrSpace 0)) allocName
      addInstruction $ allocName := Alloca (NamedTypeReference structName) Nothing 0 []

      -- Set the tag
      tagPtrName <- freshUnName
      let
        tagPtrOp = LocalReference (PointerType (IntegerType 32) (AddrSpace 0)) tagPtrName
        gepIndex i = ConstantOperand $ C.Int 32 i
        tagOp = ConstantOperand $ C.Int intBits (fromIntegral intTag)
      addInstruction $ tagPtrName := GetElementPtr False allocOp [gepIndex 0, gepIndex 0] []
      addInstruction $ Do $ Store False tagPtrOp tagOp Nothing 0 []

      -- Set data
      argOp <-
        case args' of
          [arg] -> valOperand arg
          _ -> error $ "Can't set tagged union data because there isn't exactly one argument " ++ show app
      argOp' <- maybeConvertPointer argOp (PointerType (IntegerType 64) (AddrSpace 0))
      dataPtrName <- freshUnName
      let dataPtrOp = LocalReference (PointerType (IntegerType 64) (AddrSpace 0)) dataPtrName
      addInstruction $ dataPtrName := GetElementPtr False allocOp [gepIndex 0, gepIndex 1] []
      addInstruction $ Do $ Store False dataPtrOp argOp' Nothing 0 []

      -- Return pointer
      pure allocOp
codegenExpr' (ANF.EPrimOp (ANF.App prim args' returnTy)) = do
  opName <- freshUnName
  argOps <- traverse valOperand args'
  addInstruction $ opName := primitiveFunctionInstruction prim argOps
  returnTy' <- llvmType returnTy
  pure $ LocalReference returnTy' opName

valOperand :: ANF.Val -> BlockGen Operand
valOperand (ANF.Var (ANF.VVal (ANF.Typed ty ident))) =
  let
    ident' = identToName ident
  in
    case ident of
      (ANF.Ident _ _ True) -> do
        funcTy <- mkFunctionType ty
        pure $ ConstantOperand $ C.GlobalReference funcTy ident'
      _ -> do
        ty' <- llvmType ty
        fromMaybe (LocalReference ty' ident') <$> lookupSymbol ident
valOperand (ANF.Var var@(ANF.VCons (ANF.Typed _ cons))) = do
  method <- findCompilationMethod (dataConInfoCons cons) <$> compilationMethods
  case method of
    CompileEnum i intBits -> pure $ ConstantOperand $ C.Int intBits (fromIntegral i)
    CompileTaggedUnion _ _ _ -> error $ "Can't compile tagged pairs yet " ++ show var
valOperand (ANF.Lit lit) = pure $ ConstantOperand $ literalConstant lit

primitiveFunctionInstruction
  :: PrimitiveFunction
  -> [Operand]
  -> Instruction
primitiveFunctionInstruction (PrimitiveFunction primFuncName _ _ _) argumentOperands =
  let
    -- TODO: Better error checking here if number of operands doesn't match
    -- expected number. However, this should be type checked, so a simple panic
    -- should suffice.
    [op] = argumentOperands
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

        PrimIntToDouble -> UIToFP op (FloatingPointType DoubleFP) []
        PrimDoubleToInt -> FPToUI op (IntegerType 64) []
  in instruction

typeToNonEmpty :: ANF.Type -> NonEmpty ANF.Type
typeToNonEmpty (t1 `ANF.TyFun` t2) = NE.cons t1 (typeToNonEmpty t2)
typeToNonEmpty ty = ty :| []

-- TODO: Add tests for this
llvmType :: forall m. (MonadReader CodeGenRead m) => ANF.Type -> m LLVM.Type
llvmType ty = go (typeToNonEmpty ty)
 where
  go :: NonEmpty ANF.Type -> m LLVM.Type
  go (ty' :| []) =
    case ty' of
      ANF.TyCon tyName ->
        case llvmPrimitiveType tyName of
          Just prim -> pure prim
          Nothing -> do
            rep <- getTyConRep tyName
            case rep of
              TyConEnum intBits -> pure $ IntegerType intBits
              TyConTaggedUnion structName _ -> pure $ PointerType (NamedTypeReference structName) (AddrSpace 0)
      ANF.TyVar _ -> pure $ PointerType (IntegerType 64) (AddrSpace 0)
      ANF.TyFun{} -> mkFunctionType ty
  go _ = mkFunctionType ty

mkFunctionType :: (MonadReader CodeGenRead m) => ANF.Type -> m LLVM.Type
mkFunctionType ty = do
  resType <- llvmType $ NE.last ts
  argTypes <- traverse llvmType (NE.init ts)
  pure $
    PointerType
    FunctionType
    { resultType = resType
    , argumentTypes = argTypes
    , isVarArg = False
    }
    (AddrSpace 0)
 where
  ts = typeToNonEmpty ty
