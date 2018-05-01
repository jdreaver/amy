{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for pretty printing.

module Amy.Pretty
  ( module X

    -- Helpers
  , parensIf
  , vcatHardLines
  , vcatTwoHardLines
  , groupOrHang

    -- Types
  , PrettyType(..)
  , prettyType
  , PrettyScheme(..)
  , prettyScheme

    -- General AST
  , prettyIf
  , prettyCase
  , prettyLet
  , prettyBinding
  , prettyBindingType
  , prettyBindingScheme
  , prettyExtern
  , prettyTypeDeclaration
  , prettyDataConstructor
  ) where

import Data.Text.Prettyprint.Doc as X

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
  groupOrHang (vcatHardLines matches')
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

prettyBinding :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann
prettyBinding name args body =
  sep (name : args) <+> "=" <> groupOrHang body

prettyBindingType :: Doc ann -> PrettyType ann -> Doc ann
prettyBindingType name ty = name <+> "::" <+> prettyType ty

prettyBindingScheme :: Doc ann -> PrettyScheme ann -> Doc ann
prettyBindingScheme name scheme = name <+> "::" <+> prettyScheme scheme

prettyExtern :: Doc ann -> PrettyType ann -> Doc ann
prettyExtern name ty = "extern" <+> prettyBindingType name ty

prettyTypeDeclaration :: Doc ann -> Doc ann -> Doc ann
prettyTypeDeclaration tyName tyCon = tyName <+> "=" <+> tyCon

prettyDataConstructor :: Doc ann -> Maybe (Doc ann) -> Doc ann
prettyDataConstructor tyConName mArg = tyConName <> maybe mempty (space <>) mArg
