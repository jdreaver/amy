{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Convert
  ( normalizeModule
  ) where

import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Amy.ANF.AST
import Amy.ANF.Monad
import Amy.TypeCheck.AST as T

normalizeModule :: T.Module -> ANFModule
normalizeModule module'@(T.Module bindings externs) =
  let
    -- Compute max ID from module
    moduleNames' = T.moduleNames module'
    moduleIdentIds = T.identId <$> moduleNames'
    maxId =
      if null moduleIdentIds
      then 0
      else maximum moduleIdentIds

    -- Record top-level names
    topLevelNames = (T.bindingName <$> bindings) ++ (T.externName <$> externs)

    -- Actual conversion
    convertState = anfConvertState (maxId + 1) topLevelNames
    bindings' = runANFConvert convertState $ traverse (normalizeBinding (Just "res")) bindings
    externs' = mkANFExtern <$> externs
  in ANFModule bindings' externs'

mkANFExtern :: T.Extern -> ANFExtern
mkANFExtern (T.Extern name ty) = ANFExtern (convertTIdent True name) (convertTType ty)

convertTIdent :: Bool -> T.Ident -> ANFIdent
convertTIdent isTopLevel (T.Ident name id' mPrim) = ANFIdent name id' mPrim isTopLevel

convertTIdent' :: T.Ident -> ANFConvert ANFIdent
convertTIdent' ident@(T.Ident name id' mPrim) =
  ANFIdent name id' mPrim <$> isIdentTopLevel ident

convertTScheme :: T.Scheme -> ANFScheme
convertTScheme (T.Forall vars ty) = ANFForall (convertTTypeName <$> vars) (convertTType ty)

convertTType :: T.Type -> ANFType
convertTType (T.TyCon name) = ANFTyCon (convertTTypeName name)
convertTType ty@(T.TyVar _ TyVarGenerated) = error $ "Found generated type name, bad! " ++ show ty
convertTType (T.TyVar name TyVarNotGenerated) = ANFTyVar (convertTTypeName name)
convertTType (T.TyFun ty1 ty2) = ANFTyFun (convertTType ty1) (convertTType ty2)

convertTTypeName :: T.TypeName -> ANFTypeName
convertTTypeName (T.TypeName name' id' mPrim) = ANFTypeName name' id' mPrim

normalizeExpr
  :: Text -- ^ Base name for generated variables
  -> T.Expr -- ^ Expression to normalize
  -> (ANFExpr -> ANFConvert ANFExpr) -- ^ Logical continuation (TODO: Is this needed?)
  -> ANFConvert ANFExpr
normalizeExpr _ (T.ELit lit) c = c $ ANFEVal $ ANFLit lit
normalizeExpr name var@T.EVar{} c = normalizeName name var (c . ANFEVal)
normalizeExpr name (T.EIf (T.If pred' then' else')) c =
  -- Desugar "if" into "case"
  -- TODO: Do this in a real desugarer like AST -> Core
  let
    matches =
      NE.fromList
      [ T.Match (T.PatternLit (LiteralBool True)) then'
      , T.Match (T.PatternLit (LiteralBool False)) else'
      ]
  in normalizeExpr name (T.ECase (T.Case pred' matches)) c
normalizeExpr name expr@(T.ECase (T.Case scrutinee matches)) c =
  normalizeName name scrutinee $ \scrutineeVal -> do
    matches' <- traverse normalizeMatch matches
    let ty = convertTType $ expressionType expr
    c $ ANFECase (ANFCase scrutineeVal matches' ty)
normalizeExpr name (T.ELet (T.Let bindings expr)) c = do
  bindings' <- traverse (normalizeBinding Nothing) bindings
  expr' <- normalizeExpr name expr c
  pure $ ANFELet $ ANFLet bindings' expr'
normalizeExpr name (T.EApp (T.App func args retTy)) c =
  normalizeList (normalizeName name) (toList args) $ \argVals ->
  normalizeName name func $ \funcVal ->
  case funcVal of
    (ANFLit lit) -> error $ "Encountered lit function application " ++ show lit
    (ANFVar (ANFTyped _ (ANFIdent _ _ (Just prim) _))) -> c $ ANFEPrimOp $ ANFApp prim argVals (convertTType retTy)
    (ANFVar (ANFTyped ty ident)) -> c $ ANFEApp $ ANFApp (ANFTyped ty ident) argVals (convertTType retTy)
normalizeExpr name (T.EParens expr) c = normalizeExpr name expr c

normalizeTerm :: Text -> T.Expr -> ANFConvert ANFExpr
normalizeTerm name expr = normalizeExpr name expr pure

normalizeName :: Text -> T.Expr -> (ANFVal -> ANFConvert ANFExpr) -> ANFConvert ANFExpr
normalizeName _ (T.ELit lit) c = c $ ANFLit lit
normalizeName name (T.EVar (T.Typed ty ident)) c = do
  let ty' = convertTType ty
  ident' <- convertTIdent' ident
  case (ty, ident') of
    -- Top-level values need to be first called as functions
    (T.TyCon _, ANFIdent _ _ _ True) ->
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

normalizeBinding :: Maybe Text -> T.Binding -> ANFConvert ANFBinding
normalizeBinding mName (T.Binding ident@(T.Ident name _ _) ty args retTy body) = do
  -- If we are given a base name, then use it. Otherwise use the binding name
  -- as the base name for all sub expressions.
  let subName = fromMaybe name mName
  body' <- normalizeTerm subName body
  let convertArg (T.Typed ty' arg) = ANFTyped (convertTType ty') <$> convertTIdent' arg
  ident' <- convertTIdent' ident
  args' <- traverse convertArg args
  pure $ ANFBinding ident' (convertTScheme ty) args' (convertTType retTy) body'

normalizeMatch :: T.Match -> ANFConvert ANFMatch
normalizeMatch (T.Match pat body) = do
  let pat' = convertTPattern pat
  body' <- normalizeTerm "case" body
  pure $ ANFMatch pat' body'

convertTPattern :: T.Pattern -> ANFPattern
convertTPattern (T.PatternLit lit) = ANFPatternLit lit
convertTPattern (T.PatternVar (T.Typed ty ident)) =
  ANFPatternVar $ ANFTyped (convertTType ty) $ convertTIdent False ident

-- | Helper for normalizing lists of things
normalizeList :: (Monad m) => (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
normalizeList _ [] c = c []
normalizeList norm (x:xs) c =
  norm x $ \v -> normalizeList norm xs $ \vs -> c (v:vs)
