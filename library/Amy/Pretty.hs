{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for pretty printing.

module Amy.Pretty
  ( module X
  , parensIf

    -- Types
  , prettyType
  , prettyScheme

    -- Syntax AST
  , prettyModule
  , prettyDeclaration
  , prettyBinding
  , prettyBindingType
  , prettyExpr

    -- Typed AST
  , prettyTModule
  , prettyTExpr
  ) where

import Data.Foldable (toList)
import Data.Text.Prettyprint.Doc as X

import Amy.Literal
import Amy.Names
import Amy.Prim
import Amy.Syntax.AST
import Amy.Type
import Amy.TypeCheck.AST

--
-- General helpers
--

parensIf ::  Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

vcatHardLines :: [Doc ann] -> Doc ann
vcatHardLines = concatWith (\x y -> x <> hardline <> hardline <> y)

--
-- Types
--

prettyType :: (Pretty a) => Type a -> Doc ann
prettyType t =
  case t of
    TyCon ty -> pretty ty
    TyVar (TVar var) -> pretty var
    TyArr tyLeft tyRight ->
      parensIf (isArr tyLeft) (prettyType tyLeft) <+> "->" <+> prettyType tyRight
 where
  isArr TyArr{} = True
  isArr _ = False

prettyScheme :: (Pretty a) => Scheme a -> Doc ann
prettyScheme (Forall vars ty) =
  case vars of
    [] -> prettyType ty
    _ -> "forall" <+> hcat (punctuate space (pretty . unTyVar <$> vars)) <> "." <+> prettyType ty

--
-- Syntax AST
--

prettyModule :: Module -> Doc ann
prettyModule (Module decls) = vcatHardLines (prettyDeclaration <$> decls)

prettyDeclaration :: Declaration -> Doc ann
prettyDeclaration (DeclBinding binding) = prettyBinding binding
prettyDeclaration (DeclBindingType bindingType) = prettyBindingType bindingType
prettyDeclaration (DeclExtern bindingType) = "extern" <+> prettyBindingType bindingType

prettyBinding :: Binding -> Doc ann
prettyBinding (Binding (Located _ name) args body) =
  sep (pretty name : (pretty . locatedValue <$> args)) <+> "=" <+> prettyExpr body

prettyBindingType :: BindingType -> Doc ann
prettyBindingType (BindingType (Located _ name) ty) =
  pretty name <+> "::" <+> prettyType (locatedValue <$> ty)

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit (Located _ lit)) = pretty $ showLiteral lit
prettyExpr (EVar (Located _ var)) = pretty var
prettyExpr (EIf (If pred' then' else')) =
  "if" <+> prettyExpr pred' <+>
  "then" <+> prettyExpr then' <+>
  "else" <+> prettyExpr else'
prettyExpr (ELet (Let bindings body)) =
  "let" <+> vcat (prettyLetBinding <$> bindings) <+> "in" <+> prettyExpr body
 where
  prettyLetBinding (LetBinding binding) = prettyBinding binding
  prettyLetBinding (LetBindingType bindingType) = prettyBindingType bindingType
prettyExpr (EApp (App f args)) = sep $ prettyExpr f : (prettyExpr <$> toList args)
prettyExpr (EParens expr) = parens $ prettyExpr expr

--
-- Type Check AST
--

prettyTModule :: TModule -> Doc ann
prettyTModule (TModule bindings externs) =
  vcatHardLines $ (prettyTExtern <$> externs) ++ (prettyTBinding <$> bindings)

prettyTExtern :: TExtern -> Doc ann
prettyTExtern (TExtern name ty) = "extern" <+> prettyName name <+> "::" <+> prettyType (showPrimitiveType <$> ty)

prettyTBinding :: TBinding -> Doc ann
prettyTBinding (TBinding ident scheme args _ body) =
  prettyIdent ident <+> "::" <+> prettyScheme (showPrimitiveType <$> scheme) <>
  hardline <>
  sep (prettyIdent ident : (prettyName . typedValue <$> args)) <+>
  "=" <>
  hardline <>
  indent 2 (prettyTExpr body)

prettyName :: Name -> Doc ann
prettyName (PrimitiveName prim) = pretty $ show prim
prettyName (IdentName ident) = prettyIdent ident

prettyIdent :: Ident -> Doc ann
prettyIdent (Ident name _ _) = pretty name

prettyTExpr :: TExpr -> Doc ann
prettyTExpr (TELit lit) = pretty $ showLiteral lit
prettyTExpr (TEVar (Typed _ var)) = prettyName var
prettyTExpr (TEIf (TIf pred' then' else')) =
  "if" <+> prettyTExpr pred' <+>
  "then" <+> prettyTExpr then' <+>
  "else" <+> prettyTExpr else'
prettyTExpr (TELet (TLet bindings body)) =
  "let" <>
  hardline <>
  indent 2 (vcat (prettyTBinding <$> bindings)) <>
  hardline <>
  indent (-2) "in" <>
  hardline <>
  indent 2 (prettyTExpr body)
prettyTExpr (TEApp (TApp f args _)) = sep $ prettyTExpr f : (prettyTExpr <$> toList args)
