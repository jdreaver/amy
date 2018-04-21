{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Convert
  ( normalizeModule
  ) where

import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

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
    bindings' = runANFConvert (maxId + 1) $ traverse (normalizeBinding (Just "res")) (tModuleBindings module')
    externs' = mkANFExtern <$> tModuleExterns module'
  in ANFModule bindings' externs'

mkANFExtern :: TExtern -> ANFExtern
mkANFExtern (TExtern name ty) = ANFExtern name ty

normalizeExpr
  :: Text -- ^ Base name for generated variables
  -> TExpr -- ^ Expression to normalize
  -> (ANFExpr -> ANFConvert ANFExpr) -- ^ Logical continuation (TODO: Is this needed?)
  -> ANFConvert ANFExpr
normalizeExpr _ (TELit lit) c = c $ ANFEVal $ ANFLit lit
normalizeExpr _ (TEVar var) c = c $ ANFEVal $ ANFVar var
normalizeExpr name (TEIf (TIf pred' then' else')) c =
  normalizeName name pred' $ \predVal -> do
    then'' <- normalizeTerm name then'
    else'' <- normalizeTerm name else'
    let ty = expressionType then'
    c $ ANFEIf $ ANFIf predVal then'' else'' ty
normalizeExpr name (TELet (TLet bindings expr)) c = do
  bindings' <- traverse (normalizeBinding Nothing) bindings
  expr' <- normalizeExpr name expr c
  pure $ ANFELet $ ANFLet bindings' expr'
normalizeExpr name (TEApp (TApp func args retTy)) c =
  normalizeList (normalizeName name) (toList args) $ \argVals ->
  normalizeName name func $ \funcVal ->
  case funcVal of
    (ANFLit lit) -> error $ "Encountered lit function application " ++ show lit
    (ANFVar (Typed _ (PrimitiveName prim))) -> c $ ANFEPrimOp $ ANFApp prim argVals retTy
    (ANFVar (Typed ty (IdentName ident))) -> c $ ANFEApp $ ANFApp (Typed ty ident) argVals retTy
normalizeExpr name (TEParens expr) c = normalizeExpr name expr c

normalizeTerm :: Text -> TExpr -> ANFConvert ANFExpr
normalizeTerm name expr = normalizeExpr name expr pure

normalizeName :: Text -> TExpr -> (ANFVal -> ANFConvert ANFExpr) -> ANFConvert ANFExpr
normalizeName _ (TELit lit) c = c $ ANFLit lit
normalizeName name (TEVar tvar@(Typed ty var)) c =
  case (ty, var) of
    -- Top-level values need to be first called as functions
    (TyCon _, IdentName ident@(Ident _ _ True)) ->
      mkNormalizeLet name (ANFEApp $ ANFApp (Typed ty ident) [] ty) ty c
    -- Not a top-level value, just return
    _ -> c $ ANFVar tvar
normalizeName name expr c = do
  expr' <- normalizeTerm name expr
  let exprType = expressionType expr
  mkNormalizeLet name expr' exprType c

mkNormalizeLet :: Text -> ANFExpr -> Type PrimitiveType -> (ANFVal -> ANFConvert ANFExpr) -> ANFConvert ANFExpr
mkNormalizeLet name expr exprType c = do
  newIdent <- freshIdent name
  body <- c $ ANFVar (Typed exprType (IdentName newIdent))
  pure $ ANFELet $ ANFLet [ANFBinding newIdent (Forall [] exprType) [] exprType expr] body

normalizeBinding :: Maybe Text -> TBinding -> ANFConvert ANFBinding
normalizeBinding mName (TBinding ident@(Ident name _ _) ty args retTy body) = do
  -- If we are given a base name, then use it. Otherwise use the binding name
  -- as the base name for all sub expressions.
  let subName = fromMaybe name mName
  body' <- normalizeTerm subName body
  pure $ ANFBinding ident ty args retTy body'

-- | Helper for normalizing lists of things
normalizeList :: (Monad m) => (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
normalizeList _ [] c = c []
normalizeList norm (x:xs) c =
  norm x $ \v -> normalizeList norm xs $ \vs -> c (v:vs)
