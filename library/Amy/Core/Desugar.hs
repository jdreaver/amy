{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.Desugar
  ( desugarModule
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, maybeToList)

import Amy.Core.AST as C
import Amy.Core.Monad
import Amy.Core.PatternCompiler as PC
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
desugarDataConstructor (T.DataConstructor conName id' mTyArg tyCon span' index) =
  C.DataConstructor
  { C.dataConstructorName = conName
  , C.dataConstructorId = id'
  , C.dataConstructorArgument = desugarTyConInfo <$> mTyArg
  , C.dataConstructorType = desugarTyConInfo tyCon
  , C.dataConstructorSpan = span'
  , C.dataConstructorIndex = index
  }

desugarDataConInfo :: T.DataConInfo -> C.DataConInfo
desugarDataConInfo (T.DataConInfo typeDecl dataCon) =
  C.DataConInfo (desugarTypeDeclaration typeDecl) (desugarDataConstructor dataCon)

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
  -- Desugar the case expression
  scrutinee' <- desugarExpr scrutinee
  scrutineeIdent@(C.Ident scrutineeVar scrutineeVarId) <- freshIdent "c"
  let
    equations = NE.toList $ matchToEquation <$> matches
    caseExpr = PC.match identVarSubst mkIdent [T.Ident scrutineeVar scrutineeVarId] equations
  caseExpr' <- restoreCaseExpr caseExpr

  -- Bind the scrutinee to a variable
  let
    scrutineeBinding =
      C.Binding
      { C.bindingName = scrutineeIdent
      , C.bindingType = desugarScheme $ T.Forall [] $ T.expressionType scrutinee
      , C.bindingArgs = []
      , C.bindingReturnType = desugarType $ T.expressionType scrutinee
      , C.bindingBody = scrutinee'
      }
  pure $ C.ELet $ C.Let [scrutineeBinding] caseExpr'
desugarExpr (T.EIf (T.If pred' then' else')) =
  let
    boolTyDef = fromPrimTypeDef boolTypeDefinition
    boolTyCon' = T.TyCon $ T.fromPrimTyCon boolTyCon
    mkBoolPatCons cons =
      T.PatCons (T.DataConInfo boolTyDef $ T.fromPrimDataCon cons) Nothing boolTyCon'
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
desugarVar (T.VCons (T.Typed ty cons)) = C.VCons $ C.Typed (desugarType ty) (desugarDataConInfo cons)

desugarIdent :: T.Ident -> C.Ident
desugarIdent (T.Ident name id') = C.Ident name id'

desugarTypedIdent :: T.Typed T.Ident -> C.Typed C.Ident
desugarTypedIdent (T.Typed ty ident) = C.Typed (desugarType ty) (desugarIdent ident)

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

--
-- Case Expressions
--

identVarSubst :: VarSubst T.Expr T.Ident
identVarSubst = VarSubst substExpr

substExpr :: T.Expr -> T.Ident -> T.Ident -> T.Expr
substExpr e@(T.ELit _) _ _ = e
substExpr (T.EVar v) var newVar =
  case v of
    T.VVal (T.Typed ty ident) -> T.EVar (T.VVal $ T.Typed ty $ replaceIdent ident var newVar)
    T.VCons _ -> T.EVar v
substExpr (T.EIf (T.If pred' then' else')) var newVar =
  T.EIf (T.If (substExpr pred' var newVar) (substExpr then' var newVar) (substExpr else' var newVar))
substExpr (T.ECase (T.Case scrut alts)) var newVar =
  T.ECase (T.Case (substExpr scrut var newVar) ((\m -> substMatch m var newVar) <$> alts))
substExpr (T.ELet (T.Let bindings body)) var newVar =
  T.ELet (T.Let ((\b -> substBinding b var newVar) <$> bindings) (substExpr body var newVar))
substExpr (T.EApp (T.App f args ty)) var newVar =
  T.EApp (T.App (substExpr f var newVar) ((\arg -> substExpr arg var newVar) <$> args) ty)
substExpr (T.EParens expr) var newVar = T.EParens (substExpr expr var newVar)

substBinding :: T.Binding -> T.Ident -> T.Ident -> T.Binding
substBinding binding var newVar = binding { T.bindingBody = substExpr (T.bindingBody binding) var newVar }

substMatch :: T.Match -> T.Ident -> T.Ident -> T.Match
substMatch match' var newVar = match' { T.matchBody = substExpr (T.matchBody match') var newVar }

replaceIdent :: T.Ident -> T.Ident -> T.Ident -> T.Ident
replaceIdent var oldVar newVar = if var == oldVar then newVar else var

mkIdent :: MakeVar T.Ident
mkIdent = MakeVar $ flip T.Ident

matchToEquation :: T.Match -> PC.Equation T.Expr T.DataConInfo T.Ident
matchToEquation (T.Match pat body) = ([convertPattern pat], body)

convertPattern :: T.Pattern -> PC.Pattern T.DataConInfo T.Ident
convertPattern (T.PLit lit) = PC.PCon (PC.ConLit lit) []
convertPattern (T.PVar (T.Typed _ ident)) = PC.PVar ident
convertPattern (T.PCons (T.PatCons info mArg _)) =
  let
    argPats = convertPattern <$> maybeToList mArg
    arity = length argPats
    (ConstructorSpan span') = T.dataConstructorSpan $ T.dataConInfoCons info
  in PC.PCon (PC.Con info arity span') argPats
convertPattern (T.PParens pat) = convertPattern pat

restoreCaseExpr :: PC.CaseExpr T.Expr T.DataConInfo T.Ident -> Desugar C.Expr
restoreCaseExpr (PC.Case scrutinee clauses mDefault) = do
  let
    scrutineeTy = desugarType $ T.TyCon $ T.fromPrimTyCon boolTyCon -- TODO: FIXME
    scrutineeIdent = desugarIdent scrutinee
    scrutinee' = C.EVar $ C.VVal $ C.Typed scrutineeTy scrutineeIdent
  clauses' <- traverse restoreClause clauses
  defaultClause <-
    case mDefault of
      Nothing -> pure []
      Just def -> do
        def' <- restoreCaseExpr def
        pure [C.Match (C.PVar $ C.Typed scrutineeTy scrutineeIdent) def']
  let
    matches =
      fromMaybe (error "Somehow ended up with no matches")
      $ NE.nonEmpty (clauses' ++ defaultClause)
  pure $ C.ECase $ C.Case scrutinee' matches
restoreCaseExpr (PC.Expr expr) = desugarExpr expr
restoreCaseExpr Error = error "Found inexhaustive pattern match"

restoreClause :: PC.Clause T.Expr T.DataConInfo T.Ident -> Desugar C.Match
restoreClause (PC.Clause (PC.ConLit lit) [] caseExpr) =
  C.Match (C.PLit lit) <$> restoreCaseExpr caseExpr
restoreClause clause@(PC.Clause (PC.ConLit _) _ _) =
  error $ "Encountered literal clause with arguments! " ++ show clause
restoreClause (PC.Clause (PC.Con con _ _) args caseExpr) = do
  let
    con' = desugarDataConInfo con
    patTy = C.TyCon $ C.dataConstructorType $ C.dataConInfoCons con'
    mArgTy = C.TyCon <$> C.dataConstructorArgument (C.dataConInfoCons con')
    arg =
      case args of
        [] -> Nothing
        [x] ->
          let
            x' = desugarIdent x
            argTy = fromMaybe (error "Couldn't get arg type") mArgTy
          in Just (C.Typed argTy x')
        xs -> error $ "Encountered too many arguments! " ++ show xs
    pat = C.PCons $ C.PatCons con' arg patTy
  expr <- restoreCaseExpr caseExpr
  pure $ C.Match pat expr
