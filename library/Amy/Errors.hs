-- | User facing errors.

module Amy.Errors
  ( ErrorMessage(..)
  , showErrorMessage
  ) where

import Data.Void (Void)
import Text.Groom
import Text.Megaparsec

import Amy.Kind
import Amy.Syntax.AST as S
import Amy.TypeCheck.AST as T

data ErrorMessage
  -- Parser
  = ParserError !(ParseError Char Void)

  -- Type checker
  | UnknownVariable !IdentName
  | UnknownDataCon !DataConName
  | UnknownTypeVariable !TyVarName
  | UnknownTypeConstructor !TyConName
  | VariableShadowed !IdentName
  | DuplicateDataConstructor !DataConName
  | DuplicateTypeConstructor !TyConName
  | UnificationFail !T.Type !T.Type
  | KindUnificationFail !Kind ! Kind
  | InfiniteType !TyExistVarName !T.Type
  | InfiniteKind !Int !Kind
  | TooManyBindingArguments !S.Binding
  deriving (Show, Eq)

showErrorMessage :: ErrorMessage -> String
showErrorMessage (ParserError err) = parseErrorPretty err
showErrorMessage err = groom err
