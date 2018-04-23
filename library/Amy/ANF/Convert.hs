{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Convert
  ( normalizeModule
  ) where

import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Amy.ANF.AST
import Amy.ANF.Monad
import Amy.TypeCheck.AST

normalizeModule :: TModule -> ANFModule
normalizeModule module'@(TModule bindings externs) =
  let
    -- Compute max ID from module
    moduleNames = tModuleNames module'
    moduleIdentIds = tIdentId <$> moduleNames
    maxId =
      if null moduleIdentIds
      then 0
      else maximum moduleIdentIds

    -- Record top-level names
    topLevelNames = (tBindingName <$> bindings) ++ (tExternName <$> externs)

    -- Actual conversion
    convertState = anfConvertState (maxId + 1) topLevelNames
    bindings' = runANFConvert convertState $ traverse (normalizeBinding (Just "res")) bindings
    externs' = mkANFExtern <$> externs
  in ANFModule bindings' externs'

mkANFExtern :: TExtern -> ANFExtern
mkANFExtern (TExtern name ty) = ANFExtern (convertTIdent True name) (convertTType ty)

convertTIdent :: Bool -> TIdent -> ANFIdent
convertTIdent isTopLevel (TIdent name id' mPrim) = ANFIdent name id' mPrim isTopLevel

convertTIdent' :: TIdent -> ANFConvert ANFIdent
convertTIdent' ident@(TIdent name id' mPrim) = do
  isTopLevel <- isIdentTopLevel ident
  pure $ ANFIdent name id' mPrim isTopLevel

convertTScheme :: TScheme -> ANFScheme
convertTScheme (TForall vars ty) = ANFForall (convertTTypeName <$> vars) (convertTType ty)

convertTType :: TType -> ANFType
convertTType (TTyCon name) = ANFTyCon (convertTTypeName name)
convertTType (TTyVar name) = ANFTyVar (convertTTypeName name)
convertTType (TTyFun ty1 ty2) = ANFTyFun (convertTType ty1) (convertTType ty2)

convertTTypeName :: TTypeName -> ANFTypeName
convertTTypeName (TTypeName name' id' mPrim) = ANFTypeName name' id' mPrim

normalizeExpr
  :: Text -- ^ Base name for generated variables
  -> TExpr -- ^ Expression to normalize
  -> (ANFExpr -> ANFConvert ANFExpr) -- ^ Logical continuation (TODO: Is this needed?)
  -> ANFConvert ANFExpr
normalizeExpr _ (TELit lit) c = c $ ANFEVal $ ANFLit lit
normalizeExpr name var@TEVar{} c = normalizeName name var (c . ANFEVal)
normalizeExpr name (TEIf (TIf pred' then' else')) c =
  normalizeName name pred' $ \predVal -> do
    then'' <- normalizeTerm name then'
    else'' <- normalizeTerm name else'
    let ty = expressionType then'
    c $ ANFEIf $ ANFIf predVal then'' else'' (convertTType ty)
normalizeExpr name (TELet (TLet bindings expr)) c = do
  bindings' <- traverse (normalizeBinding Nothing) bindings
  expr' <- normalizeExpr name expr c
  pure $ ANFELet $ ANFLet bindings' expr'
normalizeExpr name (TEApp (TApp func args retTy)) c =
  normalizeList (normalizeName name) (toList args) $ \argVals ->
  normalizeName name func $ \funcVal ->
  case funcVal of
    (ANFLit lit) -> error $ "Encountered lit function application " ++ show lit
    (ANFVar (ANFTyped _ (ANFIdent _ _ (Just prim) _))) -> c $ ANFEPrimOp $ ANFApp prim argVals (convertTType retTy)
    (ANFVar (ANFTyped ty ident)) -> c $ ANFEApp $ ANFApp (ANFTyped ty ident) argVals (convertTType retTy)
normalizeExpr name (TEParens expr) c = normalizeExpr name expr c

normalizeTerm :: Text -> TExpr -> ANFConvert ANFExpr
normalizeTerm name expr = normalizeExpr name expr pure

normalizeName :: Text -> TExpr -> (ANFVal -> ANFConvert ANFExpr) -> ANFConvert ANFExpr
normalizeName _ (TELit lit) c = c $ ANFLit lit
normalizeName name (TEVar (TTyped ty ident)) c = do
  let ty' = convertTType ty
  ident' <- convertTIdent' ident
  case (ty, ident') of
    -- Top-level values need to be first called as functions
    (TTyCon _, ANFIdent _ _ _ True) ->
      mkNormalizeLet name (ANFEApp $ ANFApp (ANFTyped (convertTType ty) ident') [] ty') ty' c
    -- Not a top-level value, just return
    _ -> c $ ANFVar (ANFTyped ty' ident')
normalizeName name expr c = do
  expr' <- normalizeTerm name expr
  let exprType = expressionType expr
  mkNormalizeLet name expr' (convertTType exprType) c

mkNormalizeLet :: Text -> ANFExpr -> ANFType -> (ANFVal -> ANFConvert ANFExpr) -> ANFConvert ANFExpr
mkNormalizeLet name expr exprType c = do
  newIdent <- freshIdent name
  body <- c $ ANFVar (ANFTyped exprType newIdent)
  pure $ ANFELet $ ANFLet [ANFBinding newIdent (ANFForall [] exprType) [] exprType expr] body

normalizeBinding :: Maybe Text -> TBinding -> ANFConvert ANFBinding
normalizeBinding mName (TBinding ident@(TIdent name _ _) ty args retTy body) = do
  -- If we are given a base name, then use it. Otherwise use the binding name
  -- as the base name for all sub expressions.
  let subName = fromMaybe name mName
  body' <- normalizeTerm subName body
  let convertArg (TTyped ty' arg) = ANFTyped (convertTType ty') <$> convertTIdent' arg
  ident' <- convertTIdent' ident
  args' <- traverse convertArg args
  pure $ ANFBinding ident' (convertTScheme ty) args' (convertTType retTy) body'

-- | Helper for normalizing lists of things
normalizeList :: (Monad m) => (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
normalizeList _ [] c = c []
normalizeList norm (x:xs) c =
  norm x $ \v -> normalizeList norm xs $ \vs -> c (v:vs)
