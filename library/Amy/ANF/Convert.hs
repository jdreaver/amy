{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Convert
  ( normalizeModule
  ) where

import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Amy.ANF.AST as ANF
import Amy.ANF.Monad
import Amy.TypeCheck.AST as T

normalizeModule :: T.Module -> ANF.Module
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
  in ANF.Module bindings' externs'

mkANFExtern :: T.Extern -> ANF.Extern
mkANFExtern (T.Extern name ty) = ANF.Extern (convertTIdent True name) (convertTType ty)

convertTIdent :: Bool -> T.Ident -> ANF.Ident
convertTIdent isTopLevel (T.Ident name id' mPrim) = ANF.Ident name id' mPrim isTopLevel

convertTIdent' :: T.Ident -> ANFConvert ANF.Ident
convertTIdent' ident@(T.Ident name id' mPrim) =
  ANF.Ident name id' mPrim <$> isIdentTopLevel ident

convertTScheme :: T.Scheme -> ANF.Scheme
convertTScheme (T.Forall vars ty) = ANF.Forall (convertTTypeName <$> vars) (convertTType ty)

convertTType :: T.Type -> ANF.Type
convertTType (T.TyCon name) = ANF.TyCon (convertTTypeName name)
convertTType ty@(T.TyVar _ TyVarGenerated) = error $ "Found generated type name, bad! " ++ show ty
convertTType (T.TyVar name TyVarNotGenerated) = ANF.TyVar (convertTTypeName name)
convertTType (T.TyFun ty1 ty2) = ANF.TyFun (convertTType ty1) (convertTType ty2)

convertTTypeName :: T.TypeName -> ANF.TypeName
convertTTypeName (T.TypeName name' id' mPrim) = ANF.TypeName name' id' mPrim

normalizeExpr
  :: Text -- ^ Base name for generated variables
  -> T.Expr -- ^ Expression to normalize
  -> (ANF.Expr -> ANFConvert ANF.Expr) -- ^ Logical continuation (TODO: Is this needed?)
  -> ANFConvert ANF.Expr
normalizeExpr _ (T.ELit lit) c = c $ ANF.EVal $ ANF.Lit lit
normalizeExpr name var@T.EVar{} c = normalizeName name var (c . ANF.EVal)
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
    c $ ANF.ECase (ANF.Case scrutineeVal matches' ty)
normalizeExpr name (T.ELet (T.Let bindings expr)) c = do
  bindings' <- traverse (normalizeBinding Nothing) bindings
  expr' <- normalizeExpr name expr c
  pure $ ANF.ELet $ ANF.Let bindings' expr'
normalizeExpr name (T.EApp (T.App func args retTy)) c =
  normalizeList (normalizeName name) (toList args) $ \argVals ->
  normalizeName name func $ \funcVal ->
  case funcVal of
    (ANF.Lit lit) -> error $ "Encountered lit function application " ++ show lit
    (ANF.Var (ANF.Typed _ (ANF.Ident _ _ (Just prim) _))) -> c $ ANF.EPrimOp $ ANF.App prim argVals (convertTType retTy)
    (ANF.Var (ANF.Typed ty ident)) -> c $ ANF.EApp $ ANF.App (ANF.Typed ty ident) argVals (convertTType retTy)
normalizeExpr name (T.EParens expr) c = normalizeExpr name expr c

normalizeTerm :: Text -> T.Expr -> ANFConvert ANF.Expr
normalizeTerm name expr = normalizeExpr name expr pure

normalizeName :: Text -> T.Expr -> (ANF.Val -> ANFConvert ANF.Expr) -> ANFConvert ANF.Expr
normalizeName _ (T.ELit lit) c = c $ ANF.Lit lit
normalizeName name (T.EVar (T.Typed ty ident)) c = do
  let ty' = convertTType ty
  ident' <- convertTIdent' ident
  case (ty, ident') of
    -- Top-level values need to be first called as functions
    (T.TyCon _, ANF.Ident _ _ _ True) ->
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

normalizeBinding :: Maybe Text -> T.Binding -> ANFConvert ANF.Binding
normalizeBinding mName (T.Binding ident@(T.Ident name _ _) ty args retTy body) = do
  -- If we are given a base name, then use it. Otherwise use the binding name
  -- as the base name for all sub expressions.
  let subName = fromMaybe name mName
  body' <- normalizeTerm subName body
  let convertArg (T.Typed ty' arg) = ANF.Typed (convertTType ty') <$> convertTIdent' arg
  ident' <- convertTIdent' ident
  args' <- traverse convertArg args
  pure $ ANF.Binding ident' (convertTScheme ty) args' (convertTType retTy) body'

normalizeMatch :: T.Match -> ANFConvert ANF.Match
normalizeMatch (T.Match pat body) = do
  let pat' = convertTPattern pat
  body' <- normalizeTerm "case" body
  pure $ ANF.Match pat' body'

convertTPattern :: T.Pattern -> ANF.Pattern
convertTPattern (T.PatternLit lit) = ANF.PatternLit lit
convertTPattern (T.PatternVar (T.Typed ty ident)) =
  ANF.PatternVar $ ANF.Typed (convertTType ty) $ convertTIdent False ident

-- | Helper for normalizing lists of things
normalizeList :: (Monad m) => (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
normalizeList _ [] c = c []
normalizeList norm (x:xs) c =
  norm x $ \v -> normalizeList norm xs $ \vs -> c (v:vs)
