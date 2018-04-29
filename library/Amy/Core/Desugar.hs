module Amy.Core.Desugar
  ( desugarModule
  ) where

import qualified Data.List.NonEmpty as NE

import Amy.Core.AST as C
import Amy.TypeCheck.AST as T

desugarModule :: T.Module -> C.Module
desugarModule (T.Module bindings externs) =
  C.Module (desugarBinding <$> bindings) (desugarExtern <$> externs)

desugarExtern :: T.Extern -> C.Extern
desugarExtern (T.Extern ident ty) =
  C.Extern (desugarIdent ident) (desugarType ty)

desugarBinding :: T.Binding -> C.Binding
desugarBinding (T.Binding ident scheme args retTy body) =
  C.Binding
    (desugarIdent ident)
    (desugarScheme scheme)
    (desugarTypedIdent <$> args)
    (desugarType retTy)
    (desugarExpr body)

desugarExpr :: T.Expr -> C.Expr
desugarExpr (T.ELit lit) = C.ELit lit
desugarExpr (T.EVar var) = C.EVar (desugarTypedIdent var)
desugarExpr (T.ECase (T.Case scrutinee matches)) =
  C.ECase (C.Case (desugarExpr scrutinee) (desugarMatch <$> matches))
desugarExpr (T.EIf (T.If pred' then' else')) =
  let
    matches =
      NE.fromList
      [ T.Match (T.PatternLit (LiteralBool True)) then'
      , T.Match (T.PatternLit (LiteralBool False)) else'
      ]
  in desugarExpr (T.ECase (T.Case pred' matches))
desugarExpr (T.ELet (T.Let bindings body)) =
  C.ELet (C.Let (desugarBinding <$> bindings) (desugarExpr body))
desugarExpr (T.EApp (T.App func args ty)) =
  C.EApp (C.App (desugarExpr func) (desugarExpr <$> args) (desugarType ty))
desugarExpr (T.EParens expr) = C.EParens (desugarExpr expr)

desugarMatch :: T.Match -> C.Match
desugarMatch (T.Match pat body) = C.Match (desugarPattern pat) (desugarExpr body)

desugarPattern :: T.Pattern -> C.Pattern
desugarPattern (T.PatternLit lit) = C.PatternLit lit
desugarPattern (T.PatternVar var) = C.PatternVar (desugarTypedIdent var)

desugarIdent :: T.Ident -> C.Ident
desugarIdent (T.Ident name id' mPrim) = C.Ident name id' mPrim

desugarTypedIdent :: T.Typed T.Ident -> C.Typed C.Ident
desugarTypedIdent (T.Typed ty ident) = C.Typed (desugarType ty) (desugarIdent ident)

desugarScheme :: T.Scheme -> C.Scheme
desugarScheme (T.Forall vars ty) = C.Forall (desugarTypeName <$> vars) (desugarType ty)

desugarType :: T.Type -> C.Type
desugarType (T.TyCon name) = C.TyCon (desugarTypeName name)
desugarType ty@(T.TyVar name gen) =
  case gen of
    TyVarGenerated -> error $ "Found generated type name, bad! " ++ show ty
    TyVarNotGenerated -> C.TyVar (desugarTypeName name)
desugarType (T.TyFun ty1 ty2) = C.TyFun (desugarType ty1) (desugarType ty2)

desugarTypeName :: T.TypeName -> C.TypeName
desugarTypeName (T.TypeName name id' mPrim) = C.TypeName name id' mPrim
