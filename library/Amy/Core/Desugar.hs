module Amy.Core.Desugar
  ( desugarModule
  ) where

import qualified Data.List.NonEmpty as NE

import Amy.Core.AST as C
import Amy.Core.Monad
import Amy.Prim
import Amy.TypeCheck.AST as T

desugarModule :: T.Module -> C.Module
desugarModule (T.Module bindings externs typeDeclarations maxId) =
  runDesugar (maxId + 1) $ do
    bindings' <- traverse desugarBinding bindings
    let externs' = desugarExtern <$> externs
    let typeDeclarations' = desugarTypeDeclaration <$> typeDeclarations
    maxId' <- freshId
    pure $ C.Module bindings' externs' typeDeclarations' maxId'

desugarExtern :: T.Extern -> C.Extern
desugarExtern (T.Extern ident ty) =
  C.Extern (desugarIdent ident) (desugarType ty)

desugarTypeDeclaration :: T.TypeDeclaration -> C.TypeDeclaration
desugarTypeDeclaration (T.TypeDeclaration tyName cons) =
  C.TypeDeclaration (desugarTyConInfo tyName) (desugarDataConstructor <$> cons)

desugarDataConstructor :: T.DataConstructor -> C.DataConstructor
desugarDataConstructor (T.DataConstructor conName mTyArg) =
  C.DataConstructor (desugarConstructorName conName) (desugarTyConInfo <$> mTyArg)

desugarBinding :: T.Binding -> Desugar C.Binding
desugarBinding (T.Binding ident scheme args retTy body) =
  C.Binding
    (desugarIdent ident)
    (desugarScheme scheme)
    (desugarTypedIdent <$> args)
    (desugarType retTy)
    <$> desugarExpr body

desugarExpr :: T.Expr -> Desugar C.Expr
desugarExpr (T.ELit lit) = pure $ C.ELit lit
desugarExpr (T.EVar var) = pure $ C.EVar (desugarVar var)
desugarExpr (T.ECase (T.Case scrutinee matches)) = do
  -- TODO: Desugar case expressions
  scrutinee' <- desugarExpr scrutinee
  matches' <- traverse desugarMatch matches
  pure $ C.ECase (C.Case scrutinee' matches')
desugarExpr (T.EIf (T.If pred' then' else')) =
  let
    boolTyCon' = T.TyCon $ T.fromPrimTyCon boolTyCon
    mkBoolPatCons cons =
      T.PatCons (T.Typed boolTyCon' $ T.fromPrimDataCon cons) Nothing boolTyCon'
    matches =
      NE.fromList
      [ T.Match (T.PCons $ mkBoolPatCons trueDataCon) then'
      , T.Match (T.PCons $ mkBoolPatCons falseDataCon) else'
      ]
  in desugarExpr (T.ECase (T.Case pred' matches))
desugarExpr (T.ELet (T.Let bindings body)) = do
  bindings' <- traverse desugarBinding bindings
  body' <- desugarExpr body
  pure $ C.ELet (C.Let bindings' body')
desugarExpr (T.EApp (T.App func args ty)) = do
  func' <- desugarExpr func
  args' <- traverse desugarExpr args
  pure $ C.EApp (C.App func' args' (desugarType ty))
desugarExpr (T.EParens expr) = C.EParens <$> desugarExpr expr

desugarVar :: T.Var -> C.Var
desugarVar (T.VVal ident) = C.VVal $ desugarTypedIdent ident
desugarVar (T.VCons cons) = C.VCons $ desugarTypedConstructorName cons

desugarMatch :: T.Match -> Desugar C.Match
desugarMatch (T.Match pat body) = do
  pat' <- desugarPattern pat
  expr' <- desugarExpr body
  pure $ C.Match pat' expr'

desugarPattern :: T.Pattern -> Desugar C.Pattern
desugarPattern (T.PLit lit) = pure $ C.PLit lit
desugarPattern (T.PVar var) = pure $ C.PVar (desugarTypedIdent var)
desugarPattern pat@(T.PCons (T.PatCons cons mArg retTy)) = do
  let cons' = desugarTypedConstructorName cons
  mArg' <- traverse desugarPattern mArg
  let retTy' = desugarType retTy
  case mArg' of
    Nothing -> pure $ C.PCons (C.PatCons cons' Nothing retTy')
    Just (C.PVar ident) -> pure $ C.PCons (C.PatCons cons' (Just ident) retTy')
    _ -> error $ "Cant convert nested patterns yet " ++ show pat
desugarPattern (T.PParens pat) = desugarPattern pat

desugarIdent :: T.Ident -> C.Ident
desugarIdent (T.Ident name id') = C.Ident name id'

desugarConstructorName :: T.ConstructorName -> C.ConstructorName
desugarConstructorName (T.ConstructorName name id') = C.ConstructorName name id'

desugarTypedIdent :: T.Typed T.Ident -> C.Typed C.Ident
desugarTypedIdent (T.Typed ty ident) = C.Typed (desugarType ty) (desugarIdent ident)

desugarTypedConstructorName :: T.Typed T.ConstructorName -> C.Typed C.ConstructorName
desugarTypedConstructorName (T.Typed ty constructorName) = C.Typed (desugarType ty) (desugarConstructorName constructorName)

desugarScheme :: T.Scheme -> C.Scheme
desugarScheme (T.Forall vars ty) = C.Forall (desugarTyVarInfo <$> vars) (desugarType ty)

desugarType :: T.Type -> C.Type
desugarType (T.TyCon info) = C.TyCon (desugarTyConInfo info)
desugarType (T.TyVar info) = C.TyVar (desugarTyVarInfo info)
desugarType (T.TyFun ty1 ty2) = C.TyFun (desugarType ty1) (desugarType ty2)

desugarTyConInfo :: T.TyConInfo -> C.TyConInfo
desugarTyConInfo (T.TyConInfo name id') = C.TyConInfo name id'

desugarTyVarInfo :: T.TyVarInfo -> C.TyVarInfo
desugarTyVarInfo ty@(T.TyVarInfo name id' gen) =
  case gen of
    TyVarGenerated -> error $ "Found generated type name, bad! " ++ show ty
    TyVarNotGenerated -> C.TyVarInfo name id'
