module Amy.TypeCheck.Pretty
  ( prettyModule
  , prettyExpr

  , prettyScheme
  , mkPrettyScheme
  , prettyType
  , mkPrettyType
  ) where

import Data.Foldable (toList)

import Amy.Literal
import Amy.Pretty
import Amy.TypeCheck.AST

mkPrettyType :: Type -> PrettyType ann
mkPrettyType (TyTerm t) = PTyDoc $ prettyTypeTerm t
mkPrettyType (TyFun ty1 ty2) = PTyFun (mkPrettyType ty1) (mkPrettyType ty2)

prettyTypeTerm :: TypeTerm -> Doc ann
prettyTypeTerm (TyCon con) = prettyTyConInfo con
prettyTypeTerm (TyVar var) = prettyTyVarInfo var
prettyTypeTerm (TyParens t) = parens (prettyTypeTerm t)

prettyTyConInfo :: TyConInfo -> Doc ann
prettyTyConInfo (TyConInfo name args _) = prettyTyConName name <> args'
 where
  args' = if null args then mempty else space <> sep (prettyArg <$> args)
  prettyArg arg = parensIf (isConWithArgs arg) $ prettyTypeTerm arg

isConWithArgs :: TypeTerm -> Bool
isConWithArgs (TyCon (TyConInfo _ args _)) = not (null args)
isConWithArgs _ = False

prettyTyVarInfo :: TyVarInfo -> Doc ann
prettyTyVarInfo (TyVarInfo name _ _) = prettyTyVarName name

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
prettyTyConDefinition (TyConDefinition name args _) = prettyTyConName name <> args'
 where
  args' = if null args then mempty else space <> sep (prettyTyVarInfo <$> args)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding ident scheme args _ body) =
  prettyScheme' ident scheme <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyIdent . typedValue <$> args) (prettyExpr body)

prettyScheme' :: IdentName -> Scheme -> Doc ann
prettyScheme' ident scheme = prettyBindingScheme (prettyIdent ident) (mkPrettyScheme scheme)

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit lit) = pretty $ showLiteral lit
prettyExpr (EVar var) = prettyVar var
prettyExpr (EIf (If pred' then' else')) =
  prettyIf (prettyExpr pred') (prettyExpr then') (prettyExpr else')
prettyExpr (ECase (Case scrutinee matches)) =
  prettyCase (prettyExpr scrutinee) Nothing (toList $ mkMatch <$> matches)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
prettyExpr (ELet (Let bindings body)) =
  prettyLet (prettyBinding' <$> bindings) (prettyExpr body)
prettyExpr (EApp (App f args _)) = sep $ prettyExpr f : (prettyExpr <$> toList args)
prettyExpr (EParens expr) = parens $ prettyExpr expr

prettyVar :: Var -> Doc ann
prettyVar (VVal (Typed _ var)) = prettyIdent var
prettyVar (VCons (Typed _ con)) = prettyDataConName con

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit lit) = pretty $ showLiteral lit
prettyPattern (PVar (Typed _ var)) = prettyIdent var
prettyPattern (PParens pat) = parens (prettyPattern pat)
prettyPattern (PCons (PatCons con mArg _)) =
  prettyDataConName con <> maybe mempty prettyArg mArg
 where
  prettyArg = (space <>) . prettyArg'
  prettyArg' arg@PCons{} = parens (prettyPattern arg)
  prettyArg' arg = prettyPattern arg
