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
prettyTyConInfo (TyConInfo name _) = pretty name

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
prettyTypeDeclaration' (TypeDeclaration tyName cons) =
   prettyTypeDeclaration (prettyTyConInfo tyName) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConstructor conName mArg) =
    prettyDataConstructor (prettyConstructorName conName) (prettyTyConInfo <$> mArg)

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
prettyIdent (Ident name _) = pretty name

prettyConstructorName :: ConstructorName -> Doc ann
prettyConstructorName (ConstructorName name _) = pretty name

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit lit) = pretty $ showLiteral lit
prettyExpr (EVar var) = prettyVar var
prettyExpr (ECase (Case scrutinee matches)) =
  prettyCase (prettyExpr scrutinee) (toList $ mkMatch <$> matches)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
prettyExpr (ELet (Let bindings body)) =
  prettyLet (prettyBinding' <$> bindings) (prettyExpr body)
prettyExpr (EApp (App f args _)) = sep $ prettyExpr f : (prettyExpr <$> toList args)
prettyExpr (EParens expr) = parens $ prettyExpr expr

prettyVar :: Var -> Doc ann
prettyVar (VVal (Typed _ var)) = prettyIdent var
prettyVar (VCons (Typed _ var)) = prettyConstructorName var

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit lit) = pretty $ showLiteral lit
prettyPattern (PVar (Typed _ var)) = prettyIdent var
prettyPattern (PCons (PatCons (Typed _ var) mArg _)) =
  prettyConstructorName var <> maybe mempty prettyArg mArg
 where
  prettyArg = (space <>) . prettyArg'
  prettyArg' arg@PCons{} = parens (prettyPattern arg)
  prettyArg' arg = prettyPattern arg
