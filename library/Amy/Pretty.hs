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
  , prettyTScheme
  , prettyTExpr

    -- ANF AST
  , prettyANFModule
  , prettyANFExpr
  ) where

import Data.Foldable (toList)
import Data.Text.Prettyprint.Doc as X

import Amy.ANF.AST
import Amy.Literal
import Amy.Prim
import Amy.Syntax.AST
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

data PrettyType ann
  = PTyDoc !(Doc ann)
  | PTyFun !(PrettyType ann) !(PrettyType ann)
  deriving (Show)

prettyType :: PrettyType ann -> Doc ann
prettyType t =
  case t of
    PTyDoc ty -> ty
    PTyFun tyLeft tyRight ->
      parensIf (isArr tyLeft) (prettyType tyLeft) <+> "->" <+> prettyType tyRight
 where
  isArr PTyFun{} = True
  isArr _ = False

data PrettyScheme ann = PForall [Doc ann] (PrettyType ann)
  deriving (Show)

prettyScheme :: PrettyScheme ann -> Doc ann
prettyScheme (PForall vars ty) =
  case vars of
    [] -> prettyType ty
    _ -> "forall" <+> hcat (punctuate space vars) <> "." <+> prettyType ty

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

prettyBindingType' :: Doc ann -> PrettyType ann -> Doc ann
prettyBindingType' name ty = name <+> "::" <+> prettyType ty

prettyBindingScheme' :: Doc ann -> PrettyScheme ann -> Doc ann
prettyBindingScheme' name scheme = name <+> "::" <+> prettyScheme scheme

prettyExtern :: Doc ann -> PrettyType ann -> Doc ann
prettyExtern name ty = "extern" <+> prettyBindingType' name ty

--
-- Syntax AST
--

mkPrettyType :: Type -> PrettyType ann
mkPrettyType (TyCon (Located _ var)) = PTyDoc $ pretty var
mkPrettyType (TyVar (Located _ var)) = PTyDoc $ pretty var
mkPrettyType (TyFun ty1 ty2) = PTyFun (mkPrettyType ty1) (mkPrettyType ty2)

mkPrettyScheme :: Scheme -> PrettyScheme ann
mkPrettyScheme (Forall vars ty) = PForall (pretty . locatedValue <$> vars) (mkPrettyType ty)

prettyModule :: Module -> Doc ann
prettyModule (Module decls) = vcatHardLines (prettyDeclaration <$> decls)

prettyDeclaration :: Declaration -> Doc ann
prettyDeclaration (DeclBinding binding) = prettyBinding binding
prettyDeclaration (DeclBindingType bindingType) = prettyBindingType bindingType
prettyDeclaration (DeclExtern (Extern (Located _ name) ty)) =
  prettyExtern (pretty name) (mkPrettyType ty)

prettyBinding :: Binding -> Doc ann
prettyBinding (Binding (Located _ name) args body) =
  prettyBinding' (pretty name) (pretty . locatedValue <$> args) (prettyExpr body)

prettyBindingType :: BindingType -> Doc ann
prettyBindingType (BindingType (Located _ name) ty) =
  prettyBindingScheme' (pretty name) (mkPrettyScheme ty)

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

mkPrettyTType :: TType -> PrettyType ann
mkPrettyTType (TTyCon name) = PTyDoc $ pretty $ tTypeNameText name
mkPrettyTType (TTyVar name _) = PTyDoc $ pretty $ tTypeNameText name
mkPrettyTType (TTyFun ty1 ty2) = PTyFun (mkPrettyTType ty1) (mkPrettyTType ty2)

mkPrettyTScheme :: TScheme -> PrettyScheme ann
mkPrettyTScheme (TForall vars ty) = PForall (pretty . tTypeNameText <$> vars) (mkPrettyTType ty)

prettyTModule :: TModule -> Doc ann
prettyTModule (TModule bindings externs) =
  vcatHardLines $ (prettyTExtern <$> externs) ++ (prettyTBinding <$> bindings)

prettyTExtern :: TExtern -> Doc ann
prettyTExtern (TExtern name ty) =
  prettyExtern (prettyTIdent name) (mkPrettyTType ty)

prettyTBinding :: TBinding -> Doc ann
prettyTBinding (TBinding ident scheme args _ body) =
  prettyTScheme ident scheme <>
  hardline <>
  prettyBinding' (prettyTIdent ident) (prettyTIdent . tTypedValue <$> args) (prettyTExpr body)

prettyTScheme :: TIdent -> TScheme -> Doc ann
prettyTScheme ident scheme = prettyBindingScheme' (prettyTIdent ident) (mkPrettyTScheme scheme)

prettyTIdent :: TIdent -> Doc ann
prettyTIdent (TIdent name _ _) = pretty name

prettyTExpr :: TExpr -> Doc ann
prettyTExpr (TELit lit) = pretty $ showLiteral lit
prettyTExpr (TEVar (TTyped _ var)) = prettyTIdent var
prettyTExpr (TEIf (TIf pred' then' else')) =
  prettyIf (prettyTExpr pred') (prettyTExpr then') (prettyTExpr else')
prettyTExpr (TELet (TLet bindings body)) =
  prettyLet (prettyTBinding <$> bindings) (prettyTExpr body)
prettyTExpr (TEApp (TApp f args _)) = sep $ prettyTExpr f : (prettyTExpr <$> toList args)
prettyTExpr (TEParens expr) = parens $ prettyTExpr expr

--
-- ANF AST
--

mkPrettyANFType :: ANFType -> PrettyType ann
mkPrettyANFType (ANFTyCon name) = PTyDoc $ pretty $ anfTypeNameText name
mkPrettyANFType (ANFTyVar name) = PTyDoc $ pretty $ anfTypeNameText name
mkPrettyANFType (ANFTyFun ty1 ty2) = PTyFun (mkPrettyANFType ty1) (mkPrettyANFType ty2)

mkPrettyANFScheme :: ANFScheme -> PrettyScheme ann
mkPrettyANFScheme (ANFForall vars ty) = PForall (pretty . anfTypeNameText <$> vars) (mkPrettyANFType ty)

prettyANFModule :: ANFModule -> Doc ann
prettyANFModule (ANFModule bindings externs) =
  vcatHardLines $ (prettyANFExtern <$> externs) ++ (prettyANFBinding <$> bindings)

prettyANFExtern :: ANFExtern -> Doc ann
prettyANFExtern (ANFExtern name ty) =
  prettyExtern (prettyANFIdent name) (mkPrettyANFType ty)

prettyANFBinding :: ANFBinding -> Doc ann
prettyANFBinding (ANFBinding ident scheme args _ body) =
  prettyBindingScheme' (prettyANFIdent ident) (mkPrettyANFScheme scheme) <>
  hardline <>
  prettyBinding' (prettyANFIdent ident) (prettyANFIdent . anfTypedValue <$> args) (prettyANFExpr body)

prettyANFIdent :: ANFIdent -> Doc ann
prettyANFIdent (ANFIdent name _ _ _) = pretty name

prettyANFVal :: ANFVal -> Doc ann
prettyANFVal (ANFVar (ANFTyped _ var)) = prettyANFIdent var
prettyANFVal (ANFLit lit) = pretty $ showLiteral lit

prettyANFExpr :: ANFExpr -> Doc ann
prettyANFExpr (ANFEVal val) = prettyANFVal val
prettyANFExpr (ANFEIf (ANFIf pred' then' else' _)) =
  prettyIf (prettyANFVal pred') (prettyANFExpr then') (prettyANFExpr else')
prettyANFExpr (ANFELet (ANFLet bindings body)) =
  prettyLet (prettyANFBinding <$> bindings) (prettyANFExpr body)
prettyANFExpr (ANFEApp (ANFApp f args _)) = sep $ prettyANFIdent (anfTypedValue f) : (prettyANFVal <$> args)
prettyANFExpr (ANFEPrimOp (ANFApp f args _)) =
  sep $ pretty (showPrimitiveFunctionName f) : (prettyANFVal <$> args)
