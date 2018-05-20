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
mkPrettyType (TyTerm t) = PTyDoc $ prettyTypeTerm t
mkPrettyType (TyFun ty1 ty2) = PTyFun (mkPrettyType ty1) (mkPrettyType ty2)

prettyTypeTerm :: TypeTerm -> Doc ann
prettyTypeTerm (TyCon con) = prettyTyConInfo con
prettyTypeTerm (TyVar var) = prettyTyVarInfo var

prettyTyConInfo :: TyConInfo -> Doc ann
prettyTyConInfo (TyConInfo name args) = prettyTyConName name <> args'
 where
  args' = if null args then mempty else space <> sep (prettyArg <$> args)
  prettyArg arg = parensIf (isConWithArgs arg) $ prettyTypeTerm arg

isConWithArgs :: TypeTerm -> Bool
isConWithArgs (TyCon (TyConInfo _ args)) = not (null args)
isConWithArgs _ = False

prettyTyVarInfo :: TyVarInfo -> Doc ann
prettyTyVarInfo (TyVarInfo name) = prettyTyVarName name

mkPrettyScheme :: Scheme -> PrettyScheme ann
mkPrettyScheme (Forall vars ty) = PForall (prettyTyVarInfo <$> vars) (mkPrettyType ty)

prettyModule :: Module -> Doc ann
prettyModule (Module bindings externs typeDeclarations _) =
  vcatTwoHardLines
  $ (prettyExtern' <$> externs)
  ++ (prettyTypeDeclaration' <$> typeDeclarations)
  ++ (prettyBinding' <$> bindings)

prettyExtern' :: Extern -> Doc ann
prettyExtern' (Extern name ty) =
  prettyExtern (prettyIdent name) (mkPrettyType ty)

prettyTypeDeclaration' :: TypeDeclaration -> Doc ann
prettyTypeDeclaration' (TypeDeclaration tyName cons) =
   prettyTypeDeclaration (prettyTyConDefinition tyName) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConDefinition conName mArg) =
    prettyDataConstructor (prettyDataConName conName) (prettyTypeTerm <$> mArg)

prettyTyConDefinition :: TyConDefinition -> Doc ann
prettyTyConDefinition (TyConDefinition name args) = prettyTyConName name <> args'
 where
  args' = if null args then mempty else space <> sep (prettyTyVarInfo <$> args)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding ident scheme args _ body) =
  prettyScheme' ident scheme <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyTypedIdent <$> args) (prettyExpr body)

prettyScheme' :: IdentName -> Scheme -> Doc ann
prettyScheme' ident scheme = prettyBindingScheme (prettyIdent ident) (mkPrettyScheme scheme)

prettyTypedIdent :: Typed IdentName -> Doc ann
prettyTypedIdent (Typed ty ident) = parens $ prettyIdent ident <+> "::" <+> prettyType (mkPrettyType ty)

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit lit) = pretty $ showLiteral lit
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

prettyVar :: Var -> Doc ann
prettyVar (VVal (Typed _ var)) = prettyIdent var
prettyVar (VCons (Typed _ con)) = prettyDataConName con

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit lit) = pretty $ showLiteral lit
prettyPattern (PCons (PatCons con mArg _)) =
  prettyDataConName con
  <> maybe mempty (\(Typed ty arg) -> space <> parens (prettyIdent arg <+> "::" <+> prettyType (mkPrettyType ty))) mArg
