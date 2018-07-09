{-# LANGUAGE OverloadedStrings #-}

module Amy.Syntax.Pretty
  ( prettyModule
  , prettyTypeDeclaration
  , prettyExpr
  , prettyType
  ) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map

import Amy.Literal
import Amy.Pretty
import Amy.Syntax.AST

prettyModule :: Module -> Doc ann
prettyModule (Module _ typeDecls externs bindings) =
  vcatTwoHardLines
  $ (prettyTypeDeclaration' <$> typeDecls)
  ++ (prettyExtern' <$> externs)
  ++ (prettyBinding' <$> bindings)

prettyTypeDeclaration' :: TypeDeclaration -> Doc ann
prettyTypeDeclaration' (TypeDeclaration info cons) =
  prettyTypeDeclaration (prettyTyConDefinition info) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConDefinition (Located _ conName) mArg) =
    prettyDataConstructor (prettyDataConName conName) (prettyType <$> mArg)

prettyTyConDefinition :: TyConDefinition -> Doc ann
prettyTyConDefinition (TyConDefinition (Located _ name) args) = prettyTyConName name <> args'
 where
  args' = if null args then mempty else space <> sep (prettyTyVarName . locatedValue <$> args)

prettyExtern' :: Extern -> Doc ann
prettyExtern' (Extern (Located _ name) ty) =
  prettyExtern (prettyIdent name) (prettyType ty)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding (Located _ name) ty args _ body) = tyDoc <> bindingDoc
 where
  tyDoc =
    case ty of
      TyUnknown -> mempty
      _ -> prettyBindingType (prettyIdent name) (prettyType ty) <> hardline
  bindingDoc =
    prettyBinding (prettyIdent name) (prettyIdent . locatedValue <$> args) (prettyExpr body)

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit (Located _ lit)) = pretty $ showLiteral lit
prettyExpr (ERecord _ rows) = bracketed $ uncurry prettyRow <$> Map.toList rows
 where
  prettyRow (Located _ label) (Typed _ expr) = prettyRowLabel label <> ":" <+> prettyExpr expr
prettyExpr (ERecordSelect expr field _) = prettyExpr expr <> "." <> prettyRowLabel (locatedValue field)
prettyExpr (EVar (Typed _ (Located _ ident))) = prettyIdent ident
prettyExpr (ECon (Typed _ (Located _ dataCon))) = prettyDataConName dataCon
prettyExpr (EIf (If pred' then' else' _)) =
  prettyIf (prettyExpr pred') (prettyExpr then') (prettyExpr else')
prettyExpr (ECase (Case scrutinee matches _)) =
  prettyCase (prettyExpr scrutinee) Nothing (toList $ mkMatch <$> matches)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
prettyExpr (ELet (Let bindings body _)) =
  prettyLet (prettyBinding' <$> bindings) (prettyExpr body)
prettyExpr (ELam (Lambda args body _ _)) = prettyLambda (prettyIdent . locatedValue <$> toList args) (prettyExpr body)
prettyExpr (EApp f arg) = prettyExpr f <+> prettyExpr arg
prettyExpr (EParens expr) = parens $ prettyExpr expr

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
