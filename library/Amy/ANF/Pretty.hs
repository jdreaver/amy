module Amy.ANF.Pretty
  ( prettyModule
  , prettyExpr
  ) where

import Data.Foldable (toList)

import Amy.ANF.AST
import Amy.Literal
import Amy.Pretty
import Amy.Prim (showPrimitiveFunctionName)

mkPrettyType :: Type -> PrettyType ann
mkPrettyType (TyCon name) = PTyDoc $ prettyTypeName name
mkPrettyType (TyVar name) = PTyDoc $ prettyTypeName name
mkPrettyType (TyFun ty1 ty2) = PTyFun (mkPrettyType ty1) (mkPrettyType ty2)

prettyTypeName :: TypeName -> Doc ann
prettyTypeName (TypeName name _ _) = pretty name

mkPrettyScheme :: Scheme -> PrettyScheme ann
mkPrettyScheme (Forall vars ty) = PForall (pretty . typeNameText <$> vars) (mkPrettyType ty)

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
  prettyTypeDeclaration (prettyTypeName tyName) (prettyIdent dataCon) (prettyTypeName tyArg)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding ident scheme args _ body) =
  prettyBindingScheme (prettyIdent ident) (mkPrettyScheme scheme) <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyIdent . typedValue <$> args) (prettyExpr body)

prettyIdent :: Ident -> Doc ann
prettyIdent (Ident name _ _ _) = pretty name

prettyVal :: Val -> Doc ann
prettyVal (Var (Typed _ var)) = prettyIdent var
prettyVal (Lit lit) = pretty $ showLiteral lit

prettyExpr :: Expr -> Doc ann
prettyExpr (EVal val) = prettyVal val
prettyExpr (ECase (Case scrutinee matches _)) =
  prettyCase (prettyVal scrutinee) (toList $ mkMatch <$> matches)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
prettyExpr (ELet (Let bindings body)) =
  prettyLet (prettyBinding' <$> bindings) (prettyExpr body)
prettyExpr (EApp (App f args _)) = sep $ prettyIdent (typedValue f) : (prettyVal <$> args)
prettyExpr (EPrimOp (App f args _)) =
  sep $ pretty (showPrimitiveFunctionName f) : (prettyVal <$> args)

prettyPattern :: Pattern -> Doc ann
prettyPattern (PatternLit lit) = pretty $ showLiteral lit
prettyPattern (PatternVar (Typed _ var)) = prettyIdent var
