{-# LANGUAGE OverloadedStrings #-}

module Amy.TypeCheck.Pretty
  ( prettyModule
  , prettyExpr

  , prettyType
  ) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map

import Amy.Literal
import Amy.Pretty
import Amy.TypeCheck.AST

prettyModule :: Module -> Doc ann
prettyModule (Module bindings externs typeDeclarations) =
  vcatTwoHardLines
  $ (prettyExtern' <$> externs)
  ++ (prettyTypeDeclaration' <$> typeDeclarations)
  ++ (prettyBinding' <$> concat (toList <$> bindings))

prettyExtern' :: Extern -> Doc ann
prettyExtern' (Extern (Located _ name) ty) =
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
prettyBinding' (Binding (Located _ ident) ty args _ body) =
  prettyBindingType' ident ty <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyIdent . locatedValue . typedValue <$> args) (prettyExpr body)

prettyBindingType' :: IdentName -> Type -> Doc ann
prettyBindingType' ident ty = prettyBindingType (prettyIdent ident) (prettyType ty)

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit (Located _ lit)) = pretty $ showLiteral lit
prettyExpr (ERecord rows) = bracketed $ uncurry prettyRow <$> Map.toList (Map.mapKeys locatedValue $ typedValue <$> rows)
prettyExpr (ERecordSelect expr (Located _ field) _) = prettyExpr expr <> "." <> prettyRowLabel field
prettyExpr (EVar (Typed _ (Located _ ident))) = prettyIdent ident
prettyExpr (ECon (Typed _ (Located _ con))) = prettyDataConName con
prettyExpr (EIf (If pred' then' else' _)) =
  prettyIf (prettyExpr pred') (prettyExpr then') (prettyExpr else')
prettyExpr (ECase (Case scrutinee matches _)) =
  prettyCase (prettyExpr scrutinee) Nothing (toList $ mkMatch <$> matches)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
prettyExpr (ELet (Let bindings body _)) =
  prettyLet (prettyBinding' <$> concat (toList <$> bindings)) (prettyExpr body)
prettyExpr (ELam (Lambda args body _ _)) =
  prettyLambda (prettyIdent . locatedValue . typedValue <$> toList args) (prettyExpr body)
prettyExpr (EApp (App f arg _)) = prettyExpr f <+> prettyExpr arg
prettyExpr (EParens expr) = parens $ prettyExpr expr

prettyRow :: RowLabel -> Expr -> Doc ann
prettyRow label expr = prettyRowLabel label <> ":" <+> prettyExpr expr

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit (Located _ lit)) = pretty $ showLiteral lit
prettyPattern (PVar (Typed _ (Located _ var))) = prettyIdent var
prettyPattern (PParens pat) = parens (prettyPattern pat)
prettyPattern (PCons (PatCons (Located _ con) mArg _)) =
  prettyDataConName con <> maybe mempty prettyArg mArg
 where
  prettyArg = (space <>) . prettyArg'
  prettyArg' arg@PCons{} = parens (prettyPattern arg)
  prettyArg' arg = prettyPattern arg
