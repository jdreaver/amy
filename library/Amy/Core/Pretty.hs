{-# LANGUAGE OverloadedStrings #-}

module Amy.Core.Pretty
  ( prettyModule
  , prettyExpr
  ) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map

import Amy.Core.AST
import Amy.Literal
import Amy.Pretty

prettyModule :: Module -> Doc ann
prettyModule (Module bindings externs typeDeclarations) =
  vcatTwoHardLines
  $ (prettyExtern' <$> externs)
  ++ (prettyTypeDeclaration' <$> typeDeclarations)
  ++ (prettyBinding' <$> concat (toList <$> bindings))

prettyExtern' :: Extern -> Doc ann
prettyExtern' (Extern name ty) =
  prettyExtern (prettyIdent name) (prettyType ty)

prettyTypeDeclaration' :: TypeDeclaration -> Doc ann
prettyTypeDeclaration' (TypeDeclaration tyName cons) =
   prettyTypeDeclaration (prettyTyConDefinition tyName) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConDefinition (Located _ conName) mArg) =
    prettyDataConstructor (prettyDataConName conName) (prettyType <$> mArg)

prettyTyConDefinition :: TyConDefinition -> Doc ann
prettyTyConDefinition (TyConDefinition (Located _ name) args) = prettyTyConName name <> args'
 where
  args' = if null args then mempty else space <> sep (prettyTyVarName . locatedValue <$> args)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding ident ty args _ body) =
  prettyBindingType' ident ty <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyTypedIdent <$> args) (prettyExpr body)

prettyBindingType' :: IdentName -> Type -> Doc ann
prettyBindingType' ident ty = prettyBindingType (prettyIdent ident) (prettyType ty)

prettyTypedIdent :: Typed IdentName -> Doc ann
prettyTypedIdent (Typed ty ident) = parens $ prettyIdent ident <+> "::" <+> prettyType ty

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit lit) = pretty $ showLiteral lit
prettyExpr (ERecord rows) = bracketed $ uncurry prettyRow <$> Map.toList (typedValue <$> rows)
prettyExpr (ERecordSelect expr field _) = prettyExpr expr <> "." <> prettyRowLabel field
prettyExpr (EVar (Typed _ ident)) = prettyIdent ident
prettyExpr (ECon (Typed _ con)) = prettyDataConName con
prettyExpr (ECase (Case scrutinee bind matches mDefault)) =
  prettyCase
    (prettyExpr scrutinee)
    (Just $ prettyTypedIdent bind)
    (toList (mkMatch <$> matches) ++ defaultMatch)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
  defaultMatch =
    case mDefault of
      Nothing -> []
      Just def -> [("__DEFAULT", prettyExpr def)]
prettyExpr (ELet (Let bindings body)) =
  prettyLet (prettyBinding' <$> toList bindings) (prettyExpr body)
prettyExpr (ELam (Lambda args body _)) = prettyLambda (prettyTypedIdent <$> toList args) (prettyExpr body)
prettyExpr (EApp (App f arg _)) = prettyExpr f <+> prettyExpr arg
prettyExpr (EParens expr) = parens $ prettyExpr expr

prettyRow :: RowLabel -> Expr -> Doc ann
prettyRow label expr = prettyRowLabel label <> ":" <+> prettyExpr expr

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit lit) = pretty $ showLiteral lit
prettyPattern (PCons (PatCons con mArg _)) =
  prettyDataConName con
  <> maybe mempty (\(Typed ty arg) -> space <> parens (prettyIdent arg <+> "::" <+> prettyType ty)) mArg
