{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.Desugar
  ( desugarModule
  ) where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)

import Amy.Core.AST as C
import Amy.Core.Monad
import Amy.Core.PatternCompiler as PC
import Amy.Prim
import Amy.TypeCheck.AST as T

desugarModule :: T.Module -> C.Module
desugarModule (T.Module bindings externs typeDeclarations) = do
  let typeDeclarations' = desugarTypeDeclaration <$> typeDeclarations
  runDesugar typeDeclarations' $ do
    bindings' <- traverse (traverse desugarBinding) bindings
    let externs' = desugarExtern <$> externs
    pure $ C.Module bindings' externs' typeDeclarations'

desugarExtern :: T.Extern -> C.Extern
desugarExtern (T.Extern ident ty) = C.Extern ident (desugarType ty)

desugarTypeDeclaration :: TypeDeclaration -> TypeDeclaration
desugarTypeDeclaration (TypeDeclaration tyName cons) =
  TypeDeclaration tyName (desugarDataConDefinition <$> cons)

desugarDataConDefinition :: DataConDefinition -> DataConDefinition
desugarDataConDefinition (DataConDefinition conName mTyArg) =
  DataConDefinition
  { dataConDefinitionName = conName
  , dataConDefinitionArgument = desugarType <$> mTyArg
  }

desugarBinding :: T.Binding -> Desugar C.Binding
desugarBinding (T.Binding ident ty args retTy body) =
  C.Binding
    ident
    (desugarType ty)
    (desugarTypedIdent <$> args)
    (desugarType retTy)
    <$> desugarExpr body

desugarExpr :: T.Expr -> Desugar C.Expr
desugarExpr (T.ELit lit) = pure $ C.ELit lit
desugarExpr (T.ERecord rows) =
  C.ERecord <$> traverse (\(Typed ty expr) -> Typed (desugarType ty) <$> desugarExpr expr) rows
desugarExpr (T.ERecordSelect expr label ty) = do
  expr' <- desugarExpr expr
  let ty' = desugarType ty
  pure $ C.ERecordSelect expr' label ty'
desugarExpr (T.EVar ident) = pure $ C.EVar $ desugarTypedIdent ident
desugarExpr (T.ECon (Typed ty con)) = pure $ C.ECon $ Typed (desugarType ty) con
desugarExpr (T.ECase (T.Case scrutinee matches)) = do
  -- Desugar the case expression
  scrutinee' <- desugarExpr scrutinee
  let scrutineeTy = desugarType $ T.expressionType scrutinee
  scrutineeIdent <- freshIdent "c"
  equations <- NE.toList <$> traverse matchToEquation matches
  caseExpr <- PC.match [Typed scrutineeTy scrutineeIdent] equations
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
            , C.bindingType = desugarType $ T.expressionType scrutinee
            , C.bindingArgs = []
            , C.bindingReturnType = desugarType $ T.expressionType scrutinee
            , C.bindingBody = scrutinee'
            }
        in C.ELet $ C.Let (scrutineeBinding :| []) e
desugarExpr (T.ELam (T.Lambda args body ty)) = do
  let args' = desugarTypedIdent <$> args
  body' <- desugarExpr body
  let ty' = desugarType ty
  pure $ C.ELam $ C.Lambda args' body' ty'
desugarExpr (T.EIf (T.If pred' then' else')) =
  let
    boolTyCon' = TyCon boolTyCon
    mkBoolPatCons cons = T.PatCons cons Nothing boolTyCon'
    matches =
      NE.fromList
      [ T.Match (T.PCons $ mkBoolPatCons trueDataCon) then'
      , T.Match (T.PCons $ mkBoolPatCons falseDataCon) else'
      ]
  in desugarExpr (T.ECase (T.Case pred' matches))
desugarExpr (T.ELet (T.Let bindings body)) = do
  bindings' <- traverse (traverse desugarBinding) bindings
  body' <- desugarExpr body
  -- N.B. Core only allows on binding group per let expression.
  pure $ foldl' (\bod binds -> C.ELet (C.Let binds bod)) body' (reverse bindings')
desugarExpr (T.EApp (T.App func arg ty)) = do
  func' <- desugarExpr func
  arg' <- desugarExpr arg
  pure $ C.EApp (C.App func' arg' (desugarType ty))
desugarExpr (T.EParens expr) = C.EParens <$> desugarExpr expr

desugarTypedIdent :: Typed IdentName -> Typed IdentName
desugarTypedIdent (Typed ty ident) = Typed (desugarType ty) ident

desugarType :: Type -> Type
desugarType = removeLocatedType . removeTyExistVar

--
-- Case Expressions
--

-- TODO: Just use the Core AST in the pattern compiler so we don't have to have
-- all this silly conversion logic. Then again, maybe we don't want to do this
-- so we can keep it general, which could be useful with row types in the
-- future. However, I'm sure row types will be different enough that we might
-- need the pattern compiler to specifically know about them.

matchToEquation :: T.Match -> Desugar PC.Equation
matchToEquation (T.Match pat body) = do
  pat' <- convertPattern pat
  body' <- desugarExpr body
  pure ([pat'], body')

convertPattern :: T.Pattern -> Desugar PC.InputPattern
convertPattern (T.PLit lit) = pure $ PC.PCon (PC.ConLit lit) []
convertPattern (T.PVar ident) = pure $ PC.PVar $ desugarTypedIdent ident
convertPattern (T.PCons (T.PatCons con mArg _)) = do
  (tyDecl, _) <- lookupDataConType con
  argPats <- traverse convertPattern $ maybeToList mArg
  let
    argTys = maybeToList $ desugarType . patternType <$> mArg
    span' = length $ typeDeclarationConstructors tyDecl
  pure $ PC.PCon (PC.Con con argTys span') argPats
convertPattern (T.PParens pat) = convertPattern pat

restoreCaseExpr :: PC.CaseExpr -> Desugar C.Expr
restoreCaseExpr (PC.CaseExpr scrutinee clauses mDefault) = do
  let
    scrutinee' = C.EVar scrutinee
  clauses' <- traverse restoreClause clauses
  defaultClause <- traverse restoreCaseExpr mDefault
  pure $ C.ECase $ C.Case scrutinee' scrutinee clauses' defaultClause
restoreCaseExpr (PC.Expr expr) = pure expr
restoreCaseExpr Error = error "Found inexhaustive pattern match"

restoreClause :: PC.Clause -> Desugar C.Match
restoreClause (PC.Clause (PC.ConLit lit) [] caseExpr) =
  C.Match (C.PLit lit) <$> restoreCaseExpr caseExpr
restoreClause clause@(PC.Clause (PC.ConLit _) _ _) =
  error $ "Encountered literal clause with arguments! " ++ show clause
restoreClause (PC.Clause (PC.Con con _ _) args caseExpr) = do
  (tyDecl, _) <- lookupDataConType con
  let
    patTy = TyCon $ locatedValue $ tyConDefinitionName $ typeDeclarationTypeName tyDecl
    arg =
      case args of
        [] -> Nothing
        [x] -> Just x
        xs -> error $ "Encountered too many arguments! " ++ show xs
    pat = C.PCons $ C.PatCons con arg patTy
  expr <- restoreCaseExpr caseExpr
  pure $ C.Match pat expr
