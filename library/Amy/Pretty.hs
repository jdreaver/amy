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

import Amy.ANF.AST as ANF
import Amy.Literal
import Amy.Prim
import Amy.Syntax.AST as S
import Amy.TypeCheck.AST as T

--
-- General helpers
--

parensIf ::  Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

vcatHardLines :: [Doc ann] -> Doc ann
vcatHardLines = concatWith (\x y -> x <> hardline <> y)

vcatTwoHardLines :: [Doc ann] -> Doc ann
vcatTwoHardLines = concatWith (\x y -> x <> hardline <> hardline <> y)

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

prettyCase :: Doc ann -> [(Doc ann, Doc ann)] -> Doc ann
prettyCase scrutinee matches =
  "case" <+> scrutinee <+> "of" <>
  line <>
  indent 2 (vcatHardLines matches')
 where
  prettyMatch pat body = pat <+> "->" <> groupOrHang body
  matches' = uncurry prettyMatch <$> matches

prettyLet :: [Doc ann] -> Doc ann -> Doc ann
prettyLet bindings body =
  "let" <>
  line <>
  indent 2 (vcatHardLines bindings) <>
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

mkPrettyType :: S.Type -> PrettyType ann
mkPrettyType (S.TyCon (Located _ var)) = PTyDoc $ pretty var
mkPrettyType (S.TyVar (Located _ var)) = PTyDoc $ pretty var
mkPrettyType (S.TyFun ty1 ty2) = PTyFun (mkPrettyType ty1) (mkPrettyType ty2)

mkPrettyScheme :: S.Scheme -> PrettyScheme ann
mkPrettyScheme (S.Forall vars ty) = PForall (pretty . locatedValue <$> vars) (mkPrettyType ty)

prettyModule :: S.Module -> Doc ann
prettyModule (S.Module decls) = vcatTwoHardLines (prettyDeclaration <$> decls)

prettyDeclaration :: Declaration -> Doc ann
prettyDeclaration (DeclBinding binding) = prettyBinding binding
prettyDeclaration (DeclBindingType bindingTy) = prettyBindingType bindingTy
prettyDeclaration (DeclExtern (S.Extern (Located _ name) ty)) =
  prettyExtern (pretty name) (mkPrettyType ty)

prettyBinding :: S.Binding -> Doc ann
prettyBinding (S.Binding (Located _ name) args body) =
  prettyBinding' (pretty name) (pretty . locatedValue <$> args) (prettyExpr body)

prettyBindingType :: BindingType -> Doc ann
prettyBindingType (BindingType (Located _ name) ty) =
  prettyBindingScheme' (pretty name) (mkPrettyScheme ty)

prettyExpr :: S.Expr -> Doc ann
prettyExpr (S.ELit (Located _ lit)) = pretty $ showLiteral lit
prettyExpr (S.EVar (Located _ var)) = pretty var
prettyExpr (S.EIf (S.If pred' then' else')) =
  prettyIf (prettyExpr pred') (prettyExpr then') (prettyExpr else')
prettyExpr (S.ECase (S.Case scrutinee matches)) =
  prettyCase (prettyExpr scrutinee) (toList $ mkMatch <$> matches)
 where
  mkMatch (S.Match pat body) = (prettyPattern pat, prettyExpr body)
prettyExpr (S.ELet (S.Let bindings body)) =
  prettyLet (prettyLetBinding <$> bindings) (prettyExpr body)
 where
  prettyLetBinding (LetBinding binding) = prettyBinding binding
  prettyLetBinding (LetBindingType bindingTy) = prettyBindingType bindingTy
prettyExpr (S.EApp (S.App f args)) = sep $ prettyExpr f : (prettyExpr <$> toList args)
prettyExpr (S.EParens expr) = parens $ prettyExpr expr

prettyPattern :: S.Pattern -> Doc ann
prettyPattern (S.PatternLit (Located _ lit)) = pretty $ showLiteral lit
prettyPattern (S.PatternVar (Located _ var)) = pretty var

--
-- Type Check AST
--

mkPrettyTType :: T.Type -> PrettyType ann
mkPrettyTType (T.TyCon name) = PTyDoc $ pretty $ T.typeNameText name
mkPrettyTType (T.TyVar name _) = PTyDoc $ pretty $ T.typeNameText name
mkPrettyTType (T.TyFun ty1 ty2) = PTyFun (mkPrettyTType ty1) (mkPrettyTType ty2)

mkPrettyTScheme :: T.Scheme -> PrettyScheme ann
mkPrettyTScheme (T.Forall vars ty) = PForall (pretty . T.typeNameText <$> vars) (mkPrettyTType ty)

prettyTModule :: T.Module -> Doc ann
prettyTModule (T.Module bindings externs) =
  vcatTwoHardLines $ (prettyTExtern <$> externs) ++ (prettyTBinding <$> bindings)

prettyTExtern :: T.Extern -> Doc ann
prettyTExtern (T.Extern name ty) =
  prettyExtern (prettyTIdent name) (mkPrettyTType ty)

prettyTBinding :: T.Binding -> Doc ann
prettyTBinding (T.Binding ident scheme args _ body) =
  prettyTScheme ident scheme <>
  hardline <>
  prettyBinding' (prettyTIdent ident) (prettyTIdent . T.typedValue <$> args) (prettyTExpr body)

prettyTScheme :: T.Ident -> T.Scheme -> Doc ann
prettyTScheme ident scheme = prettyBindingScheme' (prettyTIdent ident) (mkPrettyTScheme scheme)

prettyTIdent :: T.Ident -> Doc ann
prettyTIdent (T.Ident name _ _) = pretty name

prettyTExpr :: T.Expr -> Doc ann
prettyTExpr (T.ELit lit) = pretty $ showLiteral lit
prettyTExpr (T.EVar (T.Typed _ var)) = prettyTIdent var
prettyTExpr (T.EIf (T.If pred' then' else')) =
  prettyIf (prettyTExpr pred') (prettyTExpr then') (prettyTExpr else')
prettyTExpr (T.ECase (T.Case scrutinee matches)) =
  prettyCase (prettyTExpr scrutinee) (toList $ mkMatch <$> matches)
 where
  mkMatch (T.Match pat body) = (prettyTPattern pat, prettyTExpr body)
prettyTExpr (T.ELet (T.Let bindings body)) =
  prettyLet (prettyTBinding <$> bindings) (prettyTExpr body)
prettyTExpr (T.EApp (T.App f args _)) = sep $ prettyTExpr f : (prettyTExpr <$> toList args)
prettyTExpr (T.EParens expr) = parens $ prettyTExpr expr

prettyTPattern :: T.Pattern -> Doc ann
prettyTPattern (T.PatternLit lit) = pretty $ showLiteral lit
prettyTPattern (T.PatternVar (T.Typed _ var)) = prettyTIdent var

--
-- ANF AST
--

mkPrettyANFType :: ANF.Type -> PrettyType ann
mkPrettyANFType (ANF.TyCon name) = PTyDoc $ pretty $ ANF.typeNameText name
mkPrettyANFType (ANF.TyVar name) = PTyDoc $ pretty $ ANF.typeNameText name
mkPrettyANFType (ANF.TyFun ty1 ty2) = PTyFun (mkPrettyANFType ty1) (mkPrettyANFType ty2)

mkPrettyANFScheme :: ANF.Scheme -> PrettyScheme ann
mkPrettyANFScheme (ANF.Forall vars ty) = PForall (pretty . ANF.typeNameText <$> vars) (mkPrettyANFType ty)

prettyANFModule :: ANF.Module -> Doc ann
prettyANFModule (ANF.Module bindings externs) =
  vcatTwoHardLines $ (prettyANFExtern <$> externs) ++ (prettyANFBinding <$> bindings)

prettyANFExtern :: ANF.Extern -> Doc ann
prettyANFExtern (ANF.Extern name ty) =
  prettyExtern (prettyANFIdent name) (mkPrettyANFType ty)

prettyANFBinding :: ANF.Binding -> Doc ann
prettyANFBinding (ANF.Binding ident scheme args _ body) =
  prettyBindingScheme' (prettyANFIdent ident) (mkPrettyANFScheme scheme) <>
  hardline <>
  prettyBinding' (prettyANFIdent ident) (prettyANFIdent . ANF.typedValue <$> args) (prettyANFExpr body)

prettyANFIdent :: ANF.Ident -> Doc ann
prettyANFIdent (ANF.Ident name _ _ _) = pretty name

prettyANFVal :: ANF.Val -> Doc ann
prettyANFVal (ANF.Var (ANF.Typed _ var)) = prettyANFIdent var
prettyANFVal (ANF.Lit lit) = pretty $ showLiteral lit

prettyANFExpr :: ANF.Expr -> Doc ann
prettyANFExpr (ANF.EVal val) = prettyANFVal val
prettyANFExpr (ANF.ECase (ANF.Case scrutinee matches _)) =
  prettyCase (prettyANFVal scrutinee) (toList $ mkMatch <$> matches)
 where
  mkMatch (ANF.Match pat body) = (prettyANFPattern pat, prettyANFExpr body)
prettyANFExpr (ANF.ELet (ANF.Let bindings body)) =
  prettyLet (prettyANFBinding <$> bindings) (prettyANFExpr body)
prettyANFExpr (ANF.EApp (ANF.App f args _)) = sep $ prettyANFIdent (ANF.typedValue f) : (prettyANFVal <$> args)
prettyANFExpr (ANF.EPrimOp (ANF.App f args _)) =
  sep $ pretty (showPrimitiveFunctionName f) : (prettyANFVal <$> args)

prettyANFPattern :: ANF.Pattern -> Doc ann
prettyANFPattern (ANF.PatternLit lit) = pretty $ showLiteral lit
prettyANFPattern (ANF.PatternVar (ANF.Typed _ var)) = prettyANFIdent var
