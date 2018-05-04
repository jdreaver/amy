module Amy.ANF.Pretty
  ( prettyModule
  , prettyExpr
  ) where

import Data.Foldable (toList)

import Amy.ANF.AST
import Amy.Literal
import Amy.Pretty
import Amy.Prim

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
  prettyBindingScheme (prettyIdent ident) (mkPrettyScheme scheme) <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyIdent . typedValue <$> args) (prettyExpr body)

prettyIdent :: Ident -> Doc ann
prettyIdent (Ident name _ _) = pretty name

prettyConstructorName :: ConstructorName -> Doc ann
prettyConstructorName (ConstructorName name _) = pretty name

prettyVal :: Val -> Doc ann
prettyVal (Var var) = prettyVar var
prettyVal (Lit lit) = pretty $ showLiteral lit

prettyVar :: Var -> Doc ann
prettyVar (VVal (Typed _ var)) = prettyIdent var
prettyVar (VCons (Typed _ var)) = prettyConstructorName var

prettyExpr :: Expr -> Doc ann
prettyExpr (EVal val) = prettyVal val
prettyExpr (ECase (Case scrutinee matches _)) =
  prettyCase (prettyVal scrutinee) (toList $ mkMatch <$> matches)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
prettyExpr (ELet (Let bindings body)) =
  prettyLet (prettyBinding' <$> bindings) (prettyExpr body)
prettyExpr (EApp (App f args _)) = sep $ prettyVar f : (prettyVal <$> args)
prettyExpr (EPrimOp (App (PrimitiveFunction _ name _ _) args _)) =
  sep $ pretty name : (prettyVal <$> args)

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit lit) = pretty $ showLiteral lit
prettyPattern (PVar (Typed _ var)) = prettyIdent var
prettyPattern (PCons (PatCons (Typed _ var) mArg _)) =
  prettyConstructorName var <> maybe mempty (\(Typed _ arg) -> space <> prettyIdent arg) mArg
