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
mkPrettyType (TyCon (Located _ var)) = PTyDoc $ pretty var
mkPrettyType (TyVar (Located _ var)) = PTyDoc $ pretty var
mkPrettyType (TyFun ty1 ty2) = PTyFun (mkPrettyType ty1) (mkPrettyType ty2)

mkPrettyScheme :: Scheme -> PrettyScheme ann
mkPrettyScheme (Forall vars ty) = PForall (pretty . locatedValue <$> vars) (mkPrettyType ty)

prettyModule :: Module -> Doc ann
prettyModule (Module decls) = vcatTwoHardLines (prettyDeclaration <$> decls)

prettyDeclaration :: Declaration -> Doc ann
prettyDeclaration (DeclBinding binding) = prettyBinding' binding
prettyDeclaration (DeclBindingType bindingTy) = prettyBindingType' bindingTy
prettyDeclaration (DeclExtern (Extern (Located _ name) ty)) =
  prettyExtern (pretty name) (mkPrettyType ty)
prettyDeclaration (DeclType (TypeDeclaration (Located _ tyName) (Located _ tyCon) (Located _ arg))) =
  prettyTypeDeclaration (pretty tyName) (pretty tyCon) (pretty arg)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding (Located _ name) args body) =
  prettyBinding (pretty name) (pretty . locatedValue <$> args) (prettyExpr body)

prettyBindingType' :: BindingType -> Doc ann
prettyBindingType' (BindingType (Located _ name) ty) =
  prettyBindingScheme (pretty name) (mkPrettyScheme ty)

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit (Located _ lit)) = pretty $ showLiteral lit
prettyExpr (EVar var) = prettyVar var
prettyExpr (EIf (If pred' then' else')) =
  prettyIf (prettyExpr pred') (prettyExpr then') (prettyExpr else')
prettyExpr (ECase (Case scrutinee matches)) =
  prettyCase (prettyExpr scrutinee) (toList $ mkMatch <$> matches)
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
prettyVar (Variable (Located _ var)) = pretty var
prettyVar (DataConstructor (Located _ var)) = pretty var

prettyPattern :: Pattern -> Doc ann
prettyPattern (PatternLit (Located _ lit)) = pretty $ showLiteral lit
prettyPattern (PatternVar (Located _ var)) = pretty var
