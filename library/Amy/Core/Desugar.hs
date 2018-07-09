{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.Desugar
  ( desugarModule
  ) where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)

import Amy.Core.AST as C
import Amy.Core.Monad
import Amy.Core.PatternCompiler as PC
import Amy.Prim
import Amy.Syntax.AST as S

desugarModule :: S.Module -> C.Module
desugarModule (S.Module _ typeDeclarations externs bindings) = do
  let typeDeclarations' = desugarTypeDeclaration <$> typeDeclarations
  runDesugar typeDeclarations' $ do
    bindings' <- traverse (traverse desugarBinding) bindings
    let externs' = desugarExtern <$> externs
    pure $ C.Module bindings' externs' typeDeclarations'

desugarExtern :: S.Extern -> C.Extern
desugarExtern (S.Extern (Located _ ident) ty) = C.Extern ident (desugarType ty)

desugarTypeDeclaration :: TypeDeclaration -> TypeDeclaration
desugarTypeDeclaration (TypeDeclaration tyName cons) =
  TypeDeclaration tyName (desugarDataConDefinition <$> cons)

desugarDataConDefinition :: DataConDefinition -> DataConDefinition
desugarDataConDefinition (DataConDefinition conName mTyArg) =
  DataConDefinition
  { dataConDefinitionName = conName
  , dataConDefinitionArgument = desugarType <$> mTyArg
  }

desugarBinding :: S.Binding -> Desugar C.Binding
desugarBinding (S.Binding (Located _ ident) ty args retTy body) =
  C.Binding
    ident
    (desugarType ty)
    (desugarTypedIdent . fmap locatedValue <$> args)
    (desugarType retTy)
    <$> desugarExpr body

desugarExpr :: S.Expr -> Desugar C.Expr
desugarExpr (S.ELit (Located _ lit)) = pure $ C.ELit lit
desugarExpr (S.ERecord _ rows) =
  C.ERecord
  . Map.mapKeys locatedValue
  <$> traverse (\(Typed ty expr) -> Typed (desugarType ty) <$> desugarExpr expr) rows
desugarExpr (S.ERecordSelect expr (Located _ label) ty) = do
  expr' <- desugarExpr expr
  let ty' = desugarType ty
  pure $ C.ERecordSelect expr' label ty'
desugarExpr (S.EVar ident) = pure $ C.EVar $ desugarTypedIdent $ locatedValue <$> ident
desugarExpr (S.ECon (Typed ty (Located _ con))) = pure $ C.ECon $ Typed (desugarType ty) con
desugarExpr (S.ECase (S.Case scrutinee matches _x)) = do
  -- Desugar the case expression
  scrutinee' <- desugarExpr scrutinee
  let scrutineeTy = desugarType $ S.expressionType scrutinee
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
            , C.bindingType = desugarType $ S.expressionType scrutinee
            , C.bindingArgs = []
            , C.bindingReturnType = desugarType $ S.expressionType scrutinee
            , C.bindingBody = scrutinee'
            }
        in C.ELet $ C.Let (scrutineeBinding :| []) e
desugarExpr (S.ELam (S.Lambda args body _ ty)) = do
  let args' = desugarTypedIdent . fmap locatedValue <$> args
  body' <- desugarExpr body
  let ty' = desugarType ty
  pure $ C.ELam $ C.Lambda args' body' ty'
desugarExpr (S.EIf (S.If pred' then' else' _)) =
  let
    boolTyCon' = TyCon (notLocated boolTyCon)
    loc = mkSourceSpan "<generated>" 1 1 1 1
    mkBoolPatCons cons = S.PatCons (Located loc cons) Nothing boolTyCon'
    matches =
      NE.fromList
      [ S.Match (S.PCons $ mkBoolPatCons trueDataCon) then'
      , S.Match (S.PCons $ mkBoolPatCons falseDataCon) else'
      ]
  in desugarExpr (S.ECase (S.Case pred' matches loc))
desugarExpr (S.ELet (S.Let bindings body _)) = do
  bindings' <- traverse (traverse desugarBinding) bindings
  body' <- desugarExpr body
  -- N.B. Core only allows on binding group per let expression.
  pure $ foldl' (\bod binds -> C.ELet (C.Let binds bod)) body' (reverse bindings')
desugarExpr (S.EApp (S.App func arg ty)) = do
  func' <- desugarExpr func
  arg' <- desugarExpr arg
  pure $ C.EApp (C.App func' arg' (desugarType ty))
desugarExpr (S.EParens expr) = C.EParens <$> desugarExpr expr

desugarTypedIdent :: Typed IdentName -> Typed IdentName
desugarTypedIdent (Typed ty ident) = Typed (desugarType ty) ident

desugarType :: Type -> Type
desugarType = removeTyExistVar

--
-- Case Expressions
--

-- TODO: Just use the Core AST in the pattern compiler so we don't have to have
-- all this silly conversion logic. Then again, maybe we don't want to do this
-- so we can keep it general, which could be useful with row types in the
-- future. However, I'm sure row types will be different enough that we might
-- need the pattern compiler to specifically know about them.

matchToEquation :: S.Match -> Desugar PC.Equation
matchToEquation (S.Match pat body) = do
  pat' <- convertPattern pat
  body' <- desugarExpr body
  pure ([pat'], body')

convertPattern :: S.Pattern -> Desugar PC.InputPattern
convertPattern (S.PLit (Located _ lit)) = pure $ PC.PCon (PC.ConLit lit) []
convertPattern (S.PVar ident) = pure $ PC.PVar $ desugarTypedIdent (locatedValue <$> ident)
convertPattern (S.PCons (S.PatCons (Located _ con) mArg _)) = do
  (tyDecl, _) <- lookupDataConType con
  argPats <- traverse convertPattern $ maybeToList mArg
  let
    argTys = maybeToList $ desugarType . patternType <$> mArg
    span' = length $ typeDeclarationConstructors tyDecl
  pure $ PC.PCon (PC.Con con argTys span') argPats
convertPattern (S.PParens pat) = convertPattern pat

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
    patTy = TyCon $ fromLocated $ tyConDefinitionName $ typeDeclarationTypeName tyDecl
    arg =
      case args of
        [] -> Nothing
        [x] -> Just x
        xs -> error $ "Encountered too many arguments! " ++ show xs
    pat = C.PCons $ C.PatCons con arg patTy
  expr <- restoreCaseExpr caseExpr
  pure $ C.Match pat expr
