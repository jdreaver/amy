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
  , prettyTyExistVarName
  , prettyRowLabel

    -- Kinds
  , prettyKind

    -- General AST
  , prettyIf
  , prettyCase
  , prettyLet
  , prettyLetVal
  , prettyBinding
  , prettyLambda
  , prettyBindingType
  , prettyExtern
  , prettyTypeDeclaration
  , prettyDataConstructor
  , prettyTyRecord
  ) where

import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc as X hiding (list)

import Amy.Kind
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

punctuatePrefix
  :: Doc ann -- ^ Punctuation, e.g. 'comma'
  -> [Doc ann]
  -> [Doc ann]
punctuatePrefix _ [] = []
punctuatePrefix p (d:ds) = d : ((\d' -> p <> d') <$> ds)

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

prettyTyExistVarName :: TyExistVarName -> Doc ann
prettyTyExistVarName = ("$t" <>) . pretty . unTyExistVarName

prettyRowLabel :: RowLabel -> Doc ann
prettyRowLabel = pretty . unRowLabel

--
-- Kinds
--

prettyKind :: Kind -> Doc ann
prettyKind KStar = "*"
prettyKind (KUnknown i) = "k" <> pretty i
prettyKind KRow = "#row"
prettyKind (KFun k1 k2) = parensIf (isKFun k1) (prettyKind k1) <+> "->" <+> prettyKind k2

isKFun :: Kind -> Bool
isKFun KFun{} = True
isKFun _ = False

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

prettyLambda :: [Doc ann] -> Doc ann -> Doc ann
prettyLambda args body =
  "\\" <> sep args <+> "->" <> groupOrHang body

prettyBindingType :: Doc ann -> Doc ann -> Doc ann
prettyBindingType name ty = name <+> "::" <> groupOrHang ty

prettyExtern :: Doc ann -> Doc ann -> Doc ann
prettyExtern name ty = "extern" <+> prettyBindingType name ty

prettyTypeDeclaration :: Doc ann -> [Doc ann] -> Doc ann
prettyTypeDeclaration tyName dataCons =
  tyName <> groupOrHang (encloseSep "= " mempty (flatAlt "| " " | ") dataCons)

prettyDataConstructor :: Doc ann -> Maybe (Doc ann) -> Doc ann
prettyDataConstructor tyConName mArg = tyConName <> maybe mempty (space <>) mArg

prettyTyRecord :: [Doc ann] -> Maybe (Doc ann) -> Doc ann
prettyTyRecord fields mVar =
  group $ "{" <+> cat fieldsWithVar <> flatAlt "\n}" " }"
 where
  fieldsWithVar = punctuatePrefix ", " fields ++ maybe [] (\v -> [flatAlt "| " " | " <> v]) mVar
