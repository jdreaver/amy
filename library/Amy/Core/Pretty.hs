{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.Pretty
  ( prettyModule
  , prettyExpr
  ) where

import Data.Foldable (toList)

import Amy.Core.AST
import Amy.Literal
import Amy.Pretty

prettyType :: Type -> Doc ann
prettyType (TyFun ty1 ty2) = parensIf (isTyApp ty1) (prettyType ty1) <+> "->" <+> prettyType ty2
prettyType (TyCon con) = prettyTyConName con
prettyType (TyVar var) = prettyTyVarName var
prettyType (TyRecord rows) = bracketed $ prettyTyRow <$> rows
prettyType (TyApp con args) = prettyTyConName con <+> sep (toList $ prettyArg <$> args)
 where
  prettyArg arg = parensIf (isTyApp arg) $ prettyType arg

prettyTyRow :: TyRow -> Doc ann
prettyTyRow (TyRow label ty) = prettyRowLabel label <+> "::" <+> prettyType ty

isTyApp :: Type -> Bool
isTyApp TyApp{} = True
isTyApp TyFun{} = True
isTyApp _ = False

prettyScheme' :: Scheme -> Doc ann
prettyScheme' (Forall vars ty) = prettyScheme (prettyTyVarName <$> vars) (prettyType ty)

prettyModule :: Module -> Doc ann
prettyModule (Module bindings externs typeDeclarations _) =
  vcatTwoHardLines
  $ (prettyExtern' <$> externs)
  ++ (prettyTypeDeclaration' <$> typeDeclarations)
  ++ (prettyBinding' <$> bindings)

prettyExtern' :: Extern -> Doc ann
prettyExtern' (Extern name ty) =
  prettyExtern (prettyIdent name) (prettyType ty)

prettyTypeDeclaration' :: TypeDeclaration -> Doc ann
prettyTypeDeclaration' (TypeDeclaration tyName cons) =
   prettyTypeDeclaration (prettyTyConDefinition tyName) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConDefinition conName mArg) =
    prettyDataConstructor (prettyDataConName conName) (prettyType <$> mArg)

prettyTyConDefinition :: TyConDefinition -> Doc ann
prettyTyConDefinition (TyConDefinition name args) = prettyTyConName name <> args'
 where
  args' = if null args then mempty else space <> sep (prettyTyVarName <$> args)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding ident scheme args _ body) =
  prettyBindingScheme' ident scheme <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyTypedIdent <$> args) (prettyExpr body)

prettyBindingScheme' :: IdentName -> Scheme -> Doc ann
prettyBindingScheme' ident scheme = prettyBindingScheme (prettyIdent ident) (prettyScheme' scheme)

prettyTypedIdent :: Typed IdentName -> Doc ann
prettyTypedIdent (Typed ty ident) = parens $ prettyIdent ident <+> "::" <+> prettyType ty

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit lit) = pretty $ showLiteral lit
prettyExpr (ERecord rows) = bracketed $ prettyRow <$> rows
prettyExpr (EVar var) = prettyVar var
prettyExpr (ECase (Case scrutinee (Typed _ bind) matches mDefault)) =
  prettyCase
    (prettyExpr scrutinee)
    (Just $ prettyIdent bind)
    (toList (mkMatch <$> matches) ++ defaultMatch)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
  defaultMatch =
    case mDefault of
      Nothing -> []
      Just def -> [("__DEFAULT", prettyExpr def)]
prettyExpr (ELet (Let bindings body)) =
  prettyLet (prettyBinding' <$> bindings) (prettyExpr body)
prettyExpr (EApp (App f args _)) = sep $ prettyExpr f : (prettyExpr <$> toList args)
prettyExpr (EParens expr) = parens $ prettyExpr expr

prettyRow :: Row -> Doc ann
prettyRow (Row label expr) = prettyRowLabel label <+> "=" <+> prettyExpr expr

prettyVar :: Var -> Doc ann
prettyVar (VVal (Typed _ var)) = prettyIdent var
prettyVar (VCons (Typed _ con)) = prettyDataConName con

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit lit) = pretty $ showLiteral lit
prettyPattern (PCons (PatCons con mArg _)) =
  prettyDataConName con
  <> maybe mempty (\(Typed ty arg) -> space <> parens (prettyIdent arg <+> "::" <+> prettyType ty)) mArg
