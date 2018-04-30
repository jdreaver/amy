{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Convert
  ( normalizeModule
  ) where

import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Amy.ANF.AST as ANF
import Amy.ANF.Monad
import Amy.Core.AST as C

normalizeModule :: C.Module -> ANF.Module
normalizeModule module'@(C.Module bindings externs) =
  let
    -- Compute max ID from module
    moduleNames' = C.moduleNames module'
    moduleIdentIds = C.identId <$> moduleNames'
    maxId =
      if null moduleIdentIds
      then 0
      else maximum moduleIdentIds

    -- Record top-level names
    topLevelNames = (C.bindingName <$> bindings) ++ (C.externName <$> externs)

    -- Actual conversion
    convertState = anfConvertState (maxId + 1) topLevelNames
    bindings' = runANFConvert convertState $ traverse (normalizeBinding (Just "res")) bindings
    externs' = mkANFExtern <$> externs
  in ANF.Module bindings' externs'

mkANFExtern :: C.Extern -> ANF.Extern
mkANFExtern (C.Extern name ty) = ANF.Extern (convertTIdent True name) (convertTType ty)

convertTIdent :: Bool -> C.Ident -> ANF.Ident
convertTIdent isTopLevel (C.Ident name id' mPrim) = ANF.Ident name id' mPrim isTopLevel

convertTIdent' :: C.Ident -> ANFConvert ANF.Ident
convertTIdent' ident@(C.Ident name id' mPrim) =
  ANF.Ident name id' mPrim <$> isIdentTopLevel ident

convertTScheme :: C.Scheme -> ANF.Scheme
convertTScheme (C.Forall vars ty) = ANF.Forall (convertTTypeName <$> vars) (convertTType ty)

convertTType :: C.Type -> ANF.Type
convertTType (C.TyCon name) = ANF.TyCon (convertTTypeName name)
convertTType (C.TyVar name) = ANF.TyVar (convertTTypeName name)
convertTType (C.TyFun ty1 ty2) = ANF.TyFun (convertTType ty1) (convertTType ty2)

convertTTypeName :: C.TypeName -> ANF.TypeName
convertTTypeName (C.TypeName name' id' mPrim) = ANF.TypeName name' id' mPrim

normalizeExpr
  :: Text -- ^ Base name for generated variables
  -> C.Expr -- ^ Expression to normalize
  -> (ANF.Expr -> ANFConvert ANF.Expr) -- ^ Logical continuation (TODO: Is this needed?)
  -> ANFConvert ANF.Expr
normalizeExpr _ (C.ELit lit) c = c $ ANF.EVal $ ANF.Lit lit
normalizeExpr name var@C.EVar{} c = normalizeName name var (c . ANF.EVal)
normalizeExpr name expr@(C.ECase (C.Case scrutinee matches)) c =
  normalizeName name scrutinee $ \scrutineeVal -> do
    matches' <- traverse normalizeMatch matches
    let ty = convertTType $ expressionType expr
    c $ ANF.ECase (ANF.Case scrutineeVal matches' ty)
normalizeExpr name (C.ELet (C.Let bindings expr)) c = do
  bindings' <- traverse (normalizeBinding Nothing) bindings
  expr' <- normalizeExpr name expr c
  pure $ ANF.ELet $ ANF.Let bindings' expr'
normalizeExpr name (C.EApp (C.App func args retTy)) c =
  normalizeList (normalizeName name) (toList args) $ \argVals ->
  normalizeName name func $ \funcVal ->
  case funcVal of
    (ANF.Lit lit) -> error $ "Encountered lit function application " ++ show lit
    (ANF.Var (ANF.Typed _ (ANF.Ident _ _ (Just prim) _))) -> c $ ANF.EPrimOp $ ANF.App prim argVals (convertTType retTy)
    (ANF.Var (ANF.Typed ty ident)) -> c $ ANF.EApp $ ANF.App (ANF.Typed ty ident) argVals (convertTType retTy)
normalizeExpr name (C.EParens expr) c = normalizeExpr name expr c

normalizeTerm :: Text -> C.Expr -> ANFConvert ANF.Expr
normalizeTerm name expr = normalizeExpr name expr pure

normalizeName :: Text -> C.Expr -> (ANF.Val -> ANFConvert ANF.Expr) -> ANFConvert ANF.Expr
normalizeName _ (C.ELit lit) c = c $ ANF.Lit lit
normalizeName name (C.EVar (C.Typed ty ident)) c = do
  let ty' = convertTType ty
  ident' <- convertTIdent' ident
  case (ty, ident') of
    -- Top-level values need to be first called as functions
    (C.TyCon _, ANF.Ident _ _ _ True) ->
      mkNormalizeLet name (ANF.EApp $ ANF.App (ANF.Typed (convertTType ty) ident') [] ty') ty' c
    -- Not a top-level value, just return
    _ -> c $ ANF.Var (ANF.Typed ty' ident')
normalizeName name expr c = do
  expr' <- normalizeTerm name expr
  let exprType = expressionType expr
  mkNormalizeLet name expr' (convertTType exprType) c

mkNormalizeLet :: Text -> ANF.Expr -> ANF.Type -> (ANF.Val -> ANFConvert ANF.Expr) -> ANFConvert ANF.Expr
mkNormalizeLet name expr exprType c = do
  newIdent <- freshIdent name
  body <- c $ ANF.Var (ANF.Typed exprType newIdent)
  pure $ ANF.ELet $ ANF.Let [ANF.Binding newIdent (ANF.Forall [] exprType) [] exprType expr] body

normalizeBinding :: Maybe Text -> C.Binding -> ANFConvert ANF.Binding
normalizeBinding mName (C.Binding ident@(C.Ident name _ _) ty args retTy body) = do
  -- If we are given a base name, then use iC. Otherwise use the binding name
  -- as the base name for all sub expressions.
  let subName = fromMaybe name mName
  body' <- normalizeTerm subName body
  let convertArg (C.Typed ty' arg) = ANF.Typed (convertTType ty') <$> convertTIdent' arg
  ident' <- convertTIdent' ident
  args' <- traverse convertArg args
  pure $ ANF.Binding ident' (convertTScheme ty) args' (convertTType retTy) body'

normalizeMatch :: C.Match -> ANFConvert ANF.Match
normalizeMatch (C.Match pat body) = do
  let pat' = convertTPattern pat
  body' <- normalizeTerm "case" body
  pure $ ANF.Match pat' body'

convertTPattern :: C.Pattern -> ANF.Pattern
convertTPattern (C.PatternLit lit) = ANF.PatternLit lit
convertTPattern (C.PatternVar (C.Typed ty ident)) =
  ANF.PatternVar $ ANF.Typed (convertTType ty) $ convertTIdent False ident

-- | Helper for normalizing lists of things
normalizeList :: (Monad m) => (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
normalizeList _ [] c = c []
normalizeList norm (x:xs) c =
  norm x $ \v -> normalizeList norm xs $ \vs -> c (v:vs)
