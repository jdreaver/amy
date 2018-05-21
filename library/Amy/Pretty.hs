{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for pretty printing.

module Amy.Pretty
  ( module X

    -- Helpers
  , parensIf
  , vcatHardLines
  , vcatTwoHardLines
  , groupOrHang
  , list
  , bracketed

    -- Names
  , prettyIdent
  , prettyDataConName
  , prettyTyConName
  , prettyTyVarName
  , prettyRowLabel

    -- Types
  , prettyScheme

    -- General AST
  , prettyIf
  , prettyCase
  , prettyLet
  , prettyLetVal
  , prettyBinding
  , prettyBindingType
  , prettyBindingScheme
  , prettyExtern
  , prettyTypeDeclaration
  , prettyDataConstructor
  ) where

import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc as X hiding (list)

import Amy.Names

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

list :: [Doc ann] -> Doc ann
list =
  group
  . encloseSep
    (flatAlt "[ " "[")
    (flatAlt "\n]" "]")
    ", "

bracketed :: [Doc ann] -> Doc ann
bracketed =
  group
  . encloseSep
    "{ "
    (flatAlt "\n}" " }")
    ", "

--
-- Names
--
prettyIdent :: IdentName -> Doc ann
prettyIdent = pretty . unIdentName

prettyDataConName :: DataConName -> Doc ann
prettyDataConName = pretty . unDataConName

prettyTyConName :: TyConName -> Doc ann
prettyTyConName = pretty . unTyConName

prettyTyVarName :: TyVarName -> Doc ann
prettyTyVarName = pretty . unTyVarName

prettyRowLabel :: RowLabel -> Doc ann
prettyRowLabel = pretty . unRowLabel

--
-- Types
--

prettyScheme :: [Doc ann] -> Doc ann -> Doc ann
prettyScheme vars ty =
  case vars of
    [] -> ty
    _ -> "forall" <+> hcat (punctuate space vars) <> "." <+> ty

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

prettyCase :: Doc ann -> Maybe (Doc ann) -> [(Doc ann, Doc ann)] -> Doc ann
prettyCase scrutinee mBinder matches =
  "case" <+> scrutinee <+> "of" <> maybe mempty (space <>) mBinder <>
  groupOrHang (vcatHardLines matches')
 where
  prettyMatch pat body = pat <+> "->" <> groupOrHang body
  matches' = uncurry prettyMatch <$> matches

prettyLet :: [Doc ann] -> Doc ann -> Doc ann
prettyLet = prettyLet' Nothing

prettyLetVal :: [Doc ann] -> Doc ann -> Doc ann
prettyLetVal = prettyLet' (Just "val")

prettyLet' :: Maybe (Doc ann) -> [Doc ann] -> Doc ann -> Doc ann
prettyLet' mSuffix bindings body =
  "let" <> fromMaybe mempty mSuffix <>
  line <>
  indent 2 (vcatHardLines bindings) <>
  line <>
  indent (-2) "in" <>
  groupOrHang body

prettyBinding :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann
prettyBinding name args body =
  sep (name : args) <+> "=" <> groupOrHang body

prettyBindingType :: Doc ann -> Doc ann -> Doc ann
prettyBindingType name ty = name <+> "::" <> groupOrHang ty

prettyBindingScheme :: Doc ann -> Doc ann -> Doc ann
prettyBindingScheme name scheme = name <+> "::" <> groupOrHang scheme

prettyExtern :: Doc ann -> Doc ann -> Doc ann
prettyExtern name ty = "extern" <+> prettyBindingType name ty

prettyTypeDeclaration :: Doc ann -> [Doc ann] -> Doc ann
prettyTypeDeclaration tyName dataCons =
  tyName <> groupOrHang (encloseSep "= " mempty (flatAlt "| " " | ") dataCons)

prettyDataConstructor :: Doc ann -> Maybe (Doc ann) -> Doc ann
prettyDataConstructor tyConName mArg = tyConName <> maybe mempty (space <>) mArg
