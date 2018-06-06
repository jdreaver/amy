-- | User facing errors.

module Amy.Errors
  ( Error(..)
  , showError
  , errorLocation
  ) where

import Data.Void (Void)
import Text.Groom
import Text.Megaparsec

import Amy.Kind
import Amy.Syntax.AST as S
import Amy.TypeCheck.AST as T

data Error
  -- Parser
  = ParserError !(ParseError Char Void)

  -- Type checker
  -- TODO: Add source spans here
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

errorLocation :: Error -> Maybe SourceSpan
errorLocation e =
  case e of
    ParserError{} -> Nothing

    UnknownVariable{} -> Nothing
    UnknownDataCon{} -> Nothing
    UnknownTypeVariable{} -> Nothing
    UnknownTypeConstructor{} -> Nothing
    VariableShadowed{} -> Nothing
    DuplicateDataConstructor{} -> Nothing
    DuplicateTypeConstructor{} -> Nothing
    UnificationFail{} -> Nothing
    KindUnificationFail{} -> Nothing
    InfiniteType{} -> Nothing
    InfiniteKind{} -> Nothing
    TooManyBindingArguments{} -> Nothing

showError :: Error -> String
showError (ParserError err) = parseErrorPretty err
showError err = groom err
