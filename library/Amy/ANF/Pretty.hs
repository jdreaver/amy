{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Pretty
  ( prettyModule
  , prettyExpr
  ) where

import Data.Foldable (toList)
import Data.Maybe (maybeToList)

import Amy.ANF.AST
import Amy.Literal
import Amy.Pretty
import Amy.Prim

mkPrettyType :: Type -> PrettyType ann
mkPrettyType = PTyDoc . pretty . show

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
prettyTypeDeclaration' (TypeDeclaration tyName _ cons) =
   prettyTypeDeclaration (pretty tyName) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConstructor conName _ mArg _ _ _) =
    prettyDataConstructor (pretty conName) (prettyType . mkPrettyType <$> mArg)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding ident args retTy body) =
  prettyBindingType (prettyIdent ident) (mkPrettyType $ FuncType (typedType <$> args) retTy) <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyIdent . typedValue <$> args) (prettyExpr body)

prettyIdent :: Ident -> Doc ann
prettyIdent (Ident name _ _) = pretty name

prettyVal :: Val -> Doc ann
prettyVal (Var (Typed _ ident)) = prettyIdent ident
prettyVal (Lit lit) = pretty $ showLiteral lit
prettyVal (ConEnum _ con) = pretty (dataConstructorName $ dataConInfoCons con)

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
prettyExpr (ELetVal (LetVal bindings body)) =
  prettyLetVal (prettyLetValBinding <$> bindings) (prettyExpr body)
prettyExpr (EApp (App (Typed _ ident) args _)) =
  "$call" <+> prettyIdent ident <+> list (prettyVal <$> args)
prettyExpr (EConApp (ConApp info mArg _ _)) =
  "$mkCon" <+> pretty (dataConstructorName $ dataConInfoCons info) <+> list (prettyVal <$> maybeToList mArg)
prettyExpr (EPrimOp (App (PrimitiveFunction _ name _ _) args _)) =
  "$primOp" <+> pretty name <+> list (prettyVal <$> args)

prettyLetValBinding :: LetValBinding -> Doc ann
prettyLetValBinding (LetValBinding ident ty body) =
  prettyBindingType (prettyIdent ident) (mkPrettyType ty) <>
  hardline <>
  prettyBinding (prettyIdent ident) [] (prettyExpr body)

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit lit) = pretty $ showLiteral lit
prettyPattern (PCons (PatCons cons mArg _)) =
  pretty (dataConstructorName $ dataConInfoCons cons)
  <> maybe mempty (\(Typed ty arg) -> space <> parens (prettyIdent arg <+> "::" <+> prettyType (mkPrettyType ty))) mArg
