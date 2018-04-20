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
  ) where

import Data.Foldable (toList)
import Data.Text.Prettyprint.Doc as X

import Amy.Literal
import Amy.Syntax.AST
import Amy.Type

parensIf ::  Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

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
prettyModule (Module decls) = vcat (prettyDeclaration <$> decls)

prettyDeclaration :: Declaration -> Doc ann
prettyDeclaration (DeclBinding binding) = prettyBinding binding
prettyDeclaration (DeclBindingType bindingType) = prettyBindingType bindingType
prettyDeclaration (DeclExtern bindingType) = prettyBindingType bindingType

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
