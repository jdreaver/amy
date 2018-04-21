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

    -- ANF AST
  , prettyANFModule
  , prettyANFExpr
  ) where

import Data.Foldable (toList)
import Data.Text.Prettyprint.Doc as X

import Amy.ANF.AST
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

groupOrHang :: Doc ann -> Doc ann
groupOrHang doc =
  group (
    flatAlt
    (line <> indent 2 doc)
    (space <> doc)
  )

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
-- General AST Helpers
--

prettyIf :: Doc ann -> Doc ann -> Doc ann -> Doc ann
prettyIf pred' then' else' =
  align $
    vsep
    [ "if" <> groupOrHang pred'
    , "then" <> groupOrHang then'
    , "else" <> groupOrHang else'
    ]

prettyLet :: [Doc ann] -> Doc ann -> Doc ann
prettyLet bindings body =
  "let" <>
  line <>
  indent 2 (vcat bindings) <>
  line <>
  indent (-2) "in" <>
  groupOrHang body

prettyBinding' :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann
prettyBinding' name args body =
  sep (name : args) <+> "=" <> groupOrHang body

prettyBindingType' :: (Pretty a) => Doc ann -> Type a -> Doc ann
prettyBindingType' name ty = name <+> "::" <+> prettyType ty

prettyBindingScheme' :: (Pretty a) => Doc ann -> Scheme a -> Doc ann
prettyBindingScheme' name scheme = name <+> "::" <+> prettyScheme scheme

prettyExtern :: (Pretty a) => Doc ann -> Type a -> Doc ann
prettyExtern name ty = "extern" <+> prettyBindingType' name ty

--
-- Syntax AST
--

prettyModule :: Module -> Doc ann
prettyModule (Module decls) = vcatHardLines (prettyDeclaration <$> decls)

prettyDeclaration :: Declaration -> Doc ann
prettyDeclaration (DeclBinding binding) = prettyBinding binding
prettyDeclaration (DeclBindingType bindingType) = prettyBindingType bindingType
prettyDeclaration (DeclExtern (Extern (Located _ name) ty)) =
  prettyExtern (pretty name) (locatedValue <$> ty)

prettyBinding :: Binding -> Doc ann
prettyBinding (Binding (Located _ name) args body) =
  prettyBinding' (pretty name) (pretty . locatedValue <$> args) (prettyExpr body)

prettyBindingType :: BindingType -> Doc ann
prettyBindingType (BindingType (Located _ name) ty) =
  prettyBindingScheme' (pretty name) (locatedValue <$> ty)

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit (Located _ lit)) = pretty $ showLiteral lit
prettyExpr (EVar (Located _ var)) = pretty var
prettyExpr (EIf (If pred' then' else')) =
  prettyIf (prettyExpr pred') (prettyExpr then') (prettyExpr else')
prettyExpr (ELet (Let bindings body)) =
  prettyLet (prettyLetBinding <$> bindings) (prettyExpr body)
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
prettyTExtern (TExtern name ty) =
  prettyExtern (prettyName name) (showPrimitiveType <$> ty)

prettyTBinding :: TBinding -> Doc ann
prettyTBinding (TBinding ident scheme args _ body) =
  prettyBindingScheme' (prettyIdent ident) (showPrimitiveType <$> scheme) <>
  hardline <>
  prettyBinding' (prettyIdent ident) (prettyName . typedValue <$> args) (prettyTExpr body)

prettyName :: Name -> Doc ann
prettyName (PrimitiveName prim) = pretty $ showPrimitiveFunctionName prim
prettyName (IdentName ident) = prettyIdent ident

prettyIdent :: Ident -> Doc ann
prettyIdent (Ident name _ _) = pretty name

prettyTExpr :: TExpr -> Doc ann
prettyTExpr (TELit lit) = pretty $ showLiteral lit
prettyTExpr (TEVar (Typed _ var)) = prettyName var
prettyTExpr (TEIf (TIf pred' then' else')) =
  prettyIf (prettyTExpr pred') (prettyTExpr then') (prettyTExpr else')
prettyTExpr (TELet (TLet bindings body)) =
  prettyLet (prettyTBinding <$> bindings) (prettyTExpr body)
prettyTExpr (TEApp (TApp f args _)) = sep $ prettyTExpr f : (prettyTExpr <$> toList args)
prettyTExpr (TEParens expr) = parens $ prettyTExpr expr

--
-- ANF AST
--

prettyANFModule :: ANFModule -> Doc ann
prettyANFModule (ANFModule bindings externs) =
  vcatHardLines $ (prettyANFExtern <$> externs) ++ (prettyANFBinding <$> bindings)

prettyANFExtern :: ANFExtern -> Doc ann
prettyANFExtern (ANFExtern name ty) =
  prettyExtern (prettyName name) (showPrimitiveType <$> ty)

prettyANFBinding :: ANFBinding -> Doc ann
prettyANFBinding (ANFBinding ident scheme args _ body) =
  prettyBindingScheme' (prettyIdent ident) (showPrimitiveType <$> scheme) <>
  hardline <>
  prettyBinding' (prettyIdent ident) (prettyName . typedValue <$> args) (prettyANFExpr body)

prettyANFVal :: ANFVal -> Doc ann
prettyANFVal (ANFVar (Typed _ var)) = prettyName var
prettyANFVal (ANFLit lit) = pretty $ showLiteral lit

prettyANFExpr :: ANFExpr -> Doc ann
prettyANFExpr (ANFEVal val) = prettyANFVal val
prettyANFExpr (ANFEIf (ANFIf pred' then' else' _)) =
  prettyIf (prettyANFVal pred') (prettyANFExpr then') (prettyANFExpr else')
prettyANFExpr (ANFELet (ANFLet bindings body)) =
  prettyLet (prettyANFBinding <$> bindings) (prettyANFExpr body)
prettyANFExpr (ANFEApp (ANFApp f args _)) = sep $ prettyIdent (typedValue f) : (prettyANFVal <$> args)
prettyANFExpr (ANFEPrimOp (ANFApp f args _)) =
  sep $ pretty (showPrimitiveFunctionName f) : (prettyANFVal <$> args)
