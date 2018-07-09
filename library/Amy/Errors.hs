{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | User facing errors.

module Amy.Errors
  ( Error(..)
  , showError
  , ErrorMessage(..)
  ) where

import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Text.Megaparsec.Pos

import Amy.Kind
import Amy.Pretty
import Amy.Syntax.AST

data Error
  = Error
  { errorMessage :: !ErrorMessage
  , errorLocation :: !SourceSpan
  } deriving (Show, Eq)

showError :: Error -> String
showError (Error message (SourceSpan start _)) =
  renderString . layoutPretty defaultLayoutOptions $
    pretty (sourcePosPretty start) <> ":" <> groupOrHang (prettyErrorMessage message)

data ErrorMessage
  = UnknownVariable !IdentName
  | UnknownDataCon !DataConName
  | UnknownTypeVariable !TyVarName
  | UnknownTypeConstructor !TyConName
  | VariableShadowed !IdentName
  | DuplicateDataConstructor !DataConName
  | DuplicateTypeConstructor !TyConName
  | UnificationFail !Type !Type
  | KindUnificationFail !Kind ! Kind
  | InfiniteType !TyExistVarName !Type
  | InfiniteKind !Int !Kind
  | TooManyBindingArguments !Int !Int
  deriving (Show, Eq)

prettyErrorMessage :: ErrorMessage -> Doc ann
prettyErrorMessage = \case
  UnknownVariable ident -> "Unknown variable:" <+> prettyIdent ident
  UnknownDataCon con -> "Unknown data constructor:" <+> prettyDataConName con
  UnknownTypeVariable tyvar -> "Unknown type variable:" <+> prettyTyVarName tyvar
  UnknownTypeConstructor tycon -> "Unknown type constructor:" <+> prettyTyConName tycon
  VariableShadowed x -> "Variable shadowed:" <+> prettyIdent x
  DuplicateDataConstructor con -> "Data constructor already exists:" <+> prettyDataConName con
  DuplicateTypeConstructor con -> "Type constructor already exists:" <+> prettyTyConName con
  UnificationFail t1 t2 ->
    "Could not match type" <> hardline <> indent 2 (prettyType t1) <> hardline <> "with type" <> hardline <> indent 2 (prettyType t2)
  KindUnificationFail k1 k2 ->
    "Could not match kind" <> hardline <> indent 2 (prettyKind k1) <> hardline <> "with kind" <> hardline <> indent 2 (prettyKind k2)
  InfiniteType _ t -> "Cannot infer infinite type:" <> prettyType t
  InfiniteKind _ k -> "Cannot infer infinite kind:" <> prettyKind k
  TooManyBindingArguments expected actual ->
    "Too many arguments to binding. Declared type implies a maximum of" <+> pretty expected <+>
    "arguments, but found" <+> pretty actual <+> "arguments."
