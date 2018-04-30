{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.Pretty
  ( prettyModule
  , prettyExpr
  ) where

import Data.Foldable (toList)

import Amy.Core.AST
import Amy.Literal
import Amy.Pretty

mkPrettyType :: Type -> PrettyType ann
mkPrettyType (TyCon name) = PTyDoc $ prettyTyConInfo name
mkPrettyType (TyVar name) = PTyDoc $ prettyTyVarInfo name
mkPrettyType (TyFun ty1 ty2) = PTyFun (mkPrettyType ty1) (mkPrettyType ty2)

prettyTyConInfo :: TyConInfo -> Doc ann
prettyTyConInfo (TyConInfo name _ _) = pretty name

prettyTyVarInfo :: TyVarInfo -> Doc ann
prettyTyVarInfo (TyVarInfo name _) = pretty name

mkPrettyScheme :: Scheme -> PrettyScheme ann
mkPrettyScheme (Forall vars ty) = PForall (prettyTyVarInfo <$> vars) (mkPrettyType ty)

prettyModule :: Module -> Doc ann
prettyModule (Module bindings externs typeDeclarations) =
  vcatTwoHardLines
  $ (prettyExtern' <$> externs)
  ++ (prettyTypeDeclaration' <$> typeDeclarations)
  ++ (prettyBinding' <$> bindings)

prettyExtern' :: Extern -> Doc ann
prettyExtern' (Extern name ty) =
  prettyExtern (prettyIdent name) (mkPrettyType ty)

prettyTypeDeclaration' :: TypeDeclaration -> Doc ann
prettyTypeDeclaration' (TypeDeclaration tyName dataCon tyArg) =
  prettyTypeDeclaration (prettyTyConInfo tyName) (prettyIdent dataCon) (prettyTyConInfo tyArg)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding ident scheme args _ body) =
  prettyScheme' ident scheme <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyTypedIdent <$> args) (prettyExpr body)

prettyScheme' :: Ident -> Scheme -> Doc ann
prettyScheme' ident scheme = prettyBindingScheme (prettyIdent ident) (mkPrettyScheme scheme)

prettyTypedIdent :: Typed Ident -> Doc ann
prettyTypedIdent (Typed ty ident) = parens $ prettyIdent ident <+> "::" <+> prettyType (mkPrettyType ty)

prettyIdent :: Ident -> Doc ann
prettyIdent (Ident name _ _) = pretty name

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit lit) = pretty $ showLiteral lit
prettyExpr (EVar (Typed _ var)) = prettyIdent var
prettyExpr (ECase (Case scrutinee matches)) =
  prettyCase (prettyExpr scrutinee) (toList $ mkMatch <$> matches)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
prettyExpr (ELet (Let bindings body)) =
  prettyLet (prettyBinding' <$> bindings) (prettyExpr body)
prettyExpr (EApp (App f args _)) = sep $ prettyExpr f : (prettyExpr <$> toList args)
prettyExpr (EParens expr) = parens $ prettyExpr expr

prettyPattern :: Pattern -> Doc ann
prettyPattern (PatternLit lit) = pretty $ showLiteral lit
prettyPattern (PatternVar (Typed _ var)) = prettyIdent var
