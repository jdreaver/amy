{-# LANGUAGE OverloadedStrings #-}

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
prettyTypeDeclaration' (TypeDeclaration tyName cons) =
   prettyTypeDeclaration (prettyTyConInfo tyName) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConstructor conName _ mArg _ _ _) =
    prettyDataConstructor (pretty conName) (prettyTyConInfo <$> mArg)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding ident scheme args _ body) =
  prettyBindingScheme (prettyIdent ident) (mkPrettyScheme scheme) <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyIdent . typedValue <$> args) (prettyExpr body)

prettyIdent :: Ident -> Doc ann
prettyIdent (Ident name _ _) = pretty name

prettyVal :: Val -> Doc ann
prettyVal (Var var) = prettyVar var
prettyVal (Lit lit) = pretty $ showLiteral lit

prettyVar :: Var -> Doc ann
prettyVar (VVal (Typed _ var)) = prettyIdent var
prettyVar (VCons (Typed _ cons)) = pretty . dataConstructorName . dataConInfoCons $ cons

prettyExpr :: Expr -> Doc ann
prettyExpr (EVal val) = prettyVal val
prettyExpr (ECase (Case scrutinee (Typed _ bind) matches mDefault _)) =
  prettyCase
    (prettyVal scrutinee)
    (Just $ prettyIdent bind)
    (toList (mkMatch <$> matches) ++ defaultMatch)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
  defaultMatch =
    case mDefault of
      Nothing -> []
      Just def -> [("__DEFAULT", prettyExpr def)]
prettyExpr (ELet (Let bindings body)) =
  prettyLet (prettyLetBinding <$> bindings) (prettyExpr body)
prettyExpr (EApp (App (Typed _ ident) args _)) = sep $ prettyIdent ident : (prettyVal <$> args)
prettyExpr (ECons (App (Typed _ info) args _)) =
  sep $ pretty (dataConstructorName $ dataConInfoCons info) : (prettyVal <$> args)
prettyExpr (EPrimOp (App (PrimitiveFunction _ name _ _) args _)) =
  sep $ pretty name : (prettyVal <$> args)

prettyLetBinding :: LetBinding -> Doc ann
prettyLetBinding (LetBinding ident scheme body) =
  prettyBindingScheme (prettyIdent ident) (mkPrettyScheme scheme) <>
  hardline <>
  prettyBinding (prettyIdent ident) [] (prettyExpr body)

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit lit) = pretty $ showLiteral lit
prettyPattern (PCons (PatCons cons mArg _)) =
  pretty (dataConstructorName $ dataConInfoCons cons) <> maybe mempty (\(Typed _ arg) -> space <> prettyIdent arg) mArg
