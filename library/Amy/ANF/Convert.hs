{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Convert
  ( normalizeModule
  ) where

import Data.Foldable (toList)
import Data.Maybe (mapMaybe)

import Amy.ANF.AST
import Amy.ANF.Monad
import Amy.Names
import Amy.Prim
import Amy.Type
import Amy.TypeCheck.AST

normalizeModule :: TModule -> ANFModule
normalizeModule module' =
  let
    moduleNames = tModuleNames module'
    moduleIdentIds = identId <$> mapMaybe identName moduleNames
    maxId =
      if null moduleIdentIds
      then 0
      else maximum moduleIdentIds
    bindings' = runANFConvert (maxId + 1) $ traverse normalizeBinding (tModuleBindings module')
    externs' = mkANFExtern <$> tModuleExterns module'
  in ANFModule bindings' externs'

mkANFExtern :: TExtern -> ANFExtern
mkANFExtern (TExtern name ty) = ANFExtern name ty

normalizeExpr :: TExpr -> (ANFExpr -> ANFConvert ANFExpr) -> ANFConvert ANFExpr
normalizeExpr (TELit lit) c = c $ ANFEVal $ ANFLit lit
normalizeExpr (TEVar var) c = c $ ANFEVal $ ANFVar var
normalizeExpr (TEIf (TIf pred' then' else')) c =
  normalizeName pred' $ \predVal -> do
    then'' <- normalizeTerm then'
    else'' <- normalizeTerm else'
    let ty = expressionType then'
    c $ ANFEIf $ ANFIf predVal then'' else'' ty
normalizeExpr (TELet (TLet bindings expr)) c = do
  bindings' <- traverse normalizeBinding bindings
  expr' <- normalizeExpr expr c
  pure $ ANFELet $ ANFLet bindings' expr'
normalizeExpr (TEApp (TApp func args retTy)) c =
  normalizeList normalizeName (toList args) $ \argVals ->
  normalizeName func $ \funcVal ->
  case funcVal of
    (ANFLit lit) -> error $ "Encountered lit function application " ++ show lit
    (ANFVar (Typed _ (PrimitiveName prim))) -> c $ ANFEPrimOp $ ANFApp prim argVals retTy
    (ANFVar (Typed ty (IdentName ident))) -> c $ ANFEApp $ ANFApp (Typed ty ident) argVals retTy
normalizeExpr (TEParens expr) c = normalizeExpr expr c

normalizeTerm :: TExpr -> ANFConvert ANFExpr
normalizeTerm expr = normalizeExpr expr pure

normalizeName :: TExpr -> (ANFVal -> ANFConvert ANFExpr) -> ANFConvert ANFExpr
normalizeName (TELit lit) c = c $ ANFLit lit
normalizeName (TEVar tvar@(Typed ty var)) c =
  case (ty, var) of
    -- Top-level values need to be first called as functions
    (TyCon _, IdentName ident@(Ident _ _ True)) -> mkNormalizeLet (ANFEApp $ ANFApp (Typed ty ident) [] ty) ty c
    -- Not a top-level value, just return
    _ -> c $ ANFVar tvar
normalizeName expr c = do
  expr' <- normalizeTerm expr
  let exprType = expressionType expr
  mkNormalizeLet expr' exprType c

mkNormalizeLet :: ANFExpr -> Type PrimitiveType -> (ANFVal -> ANFConvert ANFExpr) -> ANFConvert ANFExpr
mkNormalizeLet expr exprType c = do
  newName <- freshIdent "t"
  body <- c $ ANFVar (Typed exprType (IdentName newName))
  pure $ ANFELet $ ANFLet [ANFBinding newName (Forall [] exprType) [] exprType expr] body

normalizeBinding :: TBinding -> ANFConvert ANFBinding
normalizeBinding (TBinding name ty args retTy body) = do
  body' <- normalizeTerm body
  pure $ ANFBinding name ty args retTy body'

-- | Helper for normalizing lists of things
normalizeList :: (Monad m) => (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
normalizeList _ [] c = c []
normalizeList norm (x:xs) c =
  norm x $ \v -> normalizeList norm xs $ \vs -> c (v:vs)
