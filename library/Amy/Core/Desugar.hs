{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.Desugar
  ( desugarModule
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)

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
  C.TypeDeclaration (desugarTyConDefinition tyName) (desugarDataConstructor <$> cons)

desugarDataConstructor :: T.DataConstructor -> C.DataConstructor
desugarDataConstructor (T.DataConstructor conName id' mTyArg tyCon span' index) =
  C.DataConstructor
  { C.dataConstructorName = conName
  , C.dataConstructorId = id'
  , C.dataConstructorArgument = desugarTypeTerm <$> mTyArg
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
  let scrutineeTy = desugarType $ T.expressionType scrutinee
  scrutineeIdent <- freshIdent "c"
  equations <- NE.toList <$> traverse matchToEquation matches
  caseExpr <- PC.match [C.Typed scrutineeTy scrutineeIdent] equations
  caseExpr' <- restoreCaseExpr caseExpr
  pure $
    case caseExpr' of
      (C.ECase case') -> C.ECase $ case' { C.caseScrutinee = scrutinee' }
      e ->
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
        in C.ELet $ C.Let [scrutineeBinding] e

desugarExpr (T.EIf (T.If pred' then' else')) =
  let
    boolTyDef = fromPrimTypeDef boolTypeDefinition
    boolTyCon' = T.TyTerm $ T.TyCon $ T.fromPrimTyCon boolTyCon
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
desugarType (T.TyTerm t) = C.TyTerm (desugarTypeTerm t)
desugarType (T.TyFun ty1 ty2) = C.TyFun (desugarType ty1) (desugarType ty2)

desugarTypeTerm :: T.TypeTerm -> C.TypeTerm
desugarTypeTerm (T.TyCon info) = C.TyCon (desugarTyConInfo info)
desugarTypeTerm (T.TyVar info) = C.TyVar (desugarTyVarInfo info)
desugarTypeTerm (T.TyParens t) = desugarTypeTerm t

desugarTyConDefinition :: T.TyConDefinition -> C.TyConDefinition
desugarTyConDefinition (T.TyConDefinition name id' args kind) = C.TyConDefinition name id' (desugarTyVarInfo <$> args) kind

desugarTyConInfo :: T.TyConInfo -> C.TyConInfo
desugarTyConInfo (T.TyConInfo name id' args tyDef) = C.TyConInfo name id' (desugarTypeTerm <$> args) (desugarTyConDefinition tyDef)

desugarTyVarInfo :: T.TyVarInfo -> C.TyVarInfo
desugarTyVarInfo (T.TyVarInfo name id' kind _) = C.TyVarInfo name id' kind

--
-- Case Expressions
--

-- TODO: Just use the Core AST in the pattern compiler so we don't have to have
-- all this silly conversion logic.

matchToEquation :: T.Match -> Desugar (PC.Equation C.DataConInfo)
matchToEquation (T.Match pat body) = do
  let pat' = convertPattern pat
  body' <- desugarExpr body
  pure ([pat'], body')

convertPattern :: T.Pattern -> PC.InputPattern C.DataConInfo
convertPattern (T.PLit lit) = PC.PCon (PC.ConLit lit) []
convertPattern (T.PVar ident) = PC.PVar $ desugarTypedIdent ident
convertPattern (T.PCons (T.PatCons info mArg _)) =
  let
    info' = desugarDataConInfo info
    argPats = convertPattern <$> maybeToList mArg
    argTys = maybeToList $ desugarType . patternType <$> mArg
    (ConstructorSpan span') = C.dataConstructorSpan $ C.dataConInfoCons info'
  in PC.PCon (PC.Con info' argTys span') argPats
convertPattern (T.PParens pat) = convertPattern pat

restoreCaseExpr :: PC.CaseExpr C.DataConInfo -> Desugar C.Expr
restoreCaseExpr (PC.CaseExpr scrutinee clauses mDefault) = do
  let
    scrutinee' = C.EVar $ C.VVal scrutinee
  clauses' <- traverse restoreClause clauses
  defaultClause <- traverse restoreCaseExpr mDefault
  pure $ C.ECase $ C.Case scrutinee' scrutinee clauses' defaultClause
restoreCaseExpr (PC.Expr expr) = pure expr
restoreCaseExpr Error = error "Found inexhaustive pattern match"

restoreClause :: PC.Clause C.DataConInfo -> Desugar C.Match
restoreClause (PC.Clause (PC.ConLit lit) [] caseExpr) =
  C.Match (C.PLit lit) <$> restoreCaseExpr caseExpr
restoreClause clause@(PC.Clause (PC.ConLit _) _ _) =
  error $ "Encountered literal clause with arguments! " ++ show clause
restoreClause (PC.Clause (PC.Con con _ _) args caseExpr) = do
  let
    patTy = C.TyTerm $ C.TyCon $ C.dataConstructorType $ C.dataConInfoCons con
    arg =
      case args of
        [] -> Nothing
        [x] -> Just x
        xs -> error $ "Encountered too many arguments! " ++ show xs
    pat = C.PCons $ C.PatCons con arg patTy
  expr <- restoreCaseExpr caseExpr
  pure $ C.Match pat expr
