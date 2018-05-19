module Amy.Syntax.Pretty
  ( prettyModule
  , prettyDeclaration
  , prettyTypeDeclaration
  , prettyExpr
  ) where

import Data.Foldable (toList)

import Amy.Literal
import Amy.Pretty
import Amy.Syntax.AST

mkPrettyType :: Type -> PrettyType ann
mkPrettyType (TyTerm t) = PTyDoc $ prettyTypeTerm t
mkPrettyType (TyFun ty1 ty2) = PTyFun (mkPrettyType ty1) (mkPrettyType ty2)

prettyTypeTerm :: TypeTerm -> Doc ann
prettyTypeTerm (TyCon con) = prettyTyConInfo con
prettyTypeTerm (TyVar var) = prettyTyVarName (locatedValue var)
prettyTypeTerm (TyParens t) = parens (prettyTypeTerm t)

prettyTyConInfo :: TyConInfo -> Doc ann
prettyTyConInfo (TyConInfo name args _) = prettyTyConName name <> args'
 where
  args' = if null args then mempty else space <> sep (prettyTypeTerm <$> args)

mkPrettyScheme :: Scheme -> PrettyScheme ann
mkPrettyScheme (Forall vars ty) = PForall (pretty . unTyVarName . locatedValue <$> vars) (mkPrettyType ty)

prettyModule :: Module -> Doc ann
prettyModule (Module decls) = vcatTwoHardLines (prettyDeclaration <$> decls)

prettyDeclaration :: Declaration -> Doc ann
prettyDeclaration (DeclBinding binding) = prettyBinding' binding
prettyDeclaration (DeclBindingType bindingTy) = prettyBindingType' bindingTy
prettyDeclaration (DeclExtern (Extern (Located _ name) ty)) =
  prettyExtern (prettyIdent name) (mkPrettyType ty)
prettyDeclaration (DeclType (TypeDeclaration info cons)) =
  prettyTypeDeclaration (prettyTyConDefinition info) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConDefinition (Located _ conName) mArg) =
    prettyDataConstructor (prettyDataConName conName) (prettyTypeTerm <$> mArg)

prettyTyConDefinition :: TyConDefinition -> Doc ann
prettyTyConDefinition (TyConDefinition name args _) = prettyTyConName name <> args'
 where
  args' = if null args then mempty else space <> sep (prettyTyVarName . locatedValue <$> args)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding (Located _ name) args body) =
  prettyBinding (prettyIdent name) (prettyIdent . locatedValue <$> args) (prettyExpr body)

prettyBindingType' :: BindingType -> Doc ann
prettyBindingType' (BindingType (Located _ name) ty) =
  prettyBindingScheme (prettyIdent name) (mkPrettyScheme ty)

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit (Located _ lit)) = pretty $ showLiteral lit
prettyExpr (EVar var) = prettyVar var
prettyExpr (EIf (If pred' then' else')) =
  prettyIf (prettyExpr pred') (prettyExpr then') (prettyExpr else')
prettyExpr (ECase (Case scrutinee matches)) =
  prettyCase (prettyExpr scrutinee) Nothing (toList $ mkMatch <$> matches)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
prettyExpr (ELet (Let bindings body)) =
  prettyLet (prettyLetBinding <$> bindings) (prettyExpr body)
 where
  prettyLetBinding (LetBinding binding) = prettyBinding' binding
  prettyLetBinding (LetBindingType bindingTy) = prettyBindingType' bindingTy
prettyExpr (EApp (App f args)) = sep $ prettyExpr f : (prettyExpr <$> toList args)
prettyExpr (EParens expr) = parens $ prettyExpr expr

prettyVar :: Var -> Doc ann
prettyVar (VVal (Located _ var)) = prettyIdent var
prettyVar (VCons (Located _ var)) = prettyDataConName var

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit (Located _ lit)) = pretty $ showLiteral lit
prettyPattern (PVar (Located _ var)) = prettyIdent var
prettyPattern (PParens pat) = parens (prettyPattern pat)
prettyPattern (PCons (PatCons (Located _ con) mArg)) =
  prettyDataConName con <> maybe mempty prettyArg mArg
 where
  prettyArg = (space <>) . prettyArg'
  prettyArg' arg@PCons{} = parens (prettyPattern arg)
  prettyArg' arg = prettyPattern arg
