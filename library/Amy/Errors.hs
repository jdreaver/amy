-- | User facing errors.

module Amy.Errors
  ( Error(..)
  , showError
  , errorLocation
  ) where

import Data.Void (Void)
import Text.Groom
import Text.Megaparsec

import Amy.Renamer.AST as R
import Amy.Syntax.AST as S
import Amy.TypeCheck.AST as T

data Error
  -- Parser
  = ParserError !(ParseError Char Void)

  -- Renamer
  | UnknownVariable !(Located IdentName)
  | UnknownDataCon !(Located DataConName)
  | VariableShadowed !(Located IdentName) !(Located IdentName)
  | TypeConstructorDefinitionContainsTyCon !S.TyConInfo
  | DuplicateDataConstructorName !(Located DataConName) !R.DataConDefinition
  | DuplicateTypeVariable !(Located TyVarName) !(Located TyVarName)
  | TypeConstructorAlreadyExists !(Located TyConName) !R.TyConInfo
  | UnknownTypeConstructor !(Located TyConName)
  | UnknownTypeVariable !(Located TyVarName)

  -- Type checker
  -- TODO: Add source spans here
  | UnificationFail !T.Type !T.Type
  | KindMismatch !T.TyVarInfo !T.Type
  | InfiniteType !T.TyVarInfo !T.Type
  | UnboundVariable !IdentName

  -- | BindingLacksTypeSignature !RBinding
  -- | TypeMismatch !(Type PrimitiveType) !(Type PrimitiveType)
  -- | CantFindType !(Located Name)
  -- | WrongNumberOfArguments !Int !Int
  -- | ExpectedPrimitiveType !(Maybe (Located Name)) !(Type PrimitiveType)
  -- | ExpectedFunctionType !(Type PrimitiveType)
  deriving (Show, Eq)

errorLocation :: Error -> Maybe SourceSpan
errorLocation e =
  case e of
    ParserError{} -> Nothing
    UnknownVariable (Located s _) -> Just s
    UnknownDataCon (Located s _) -> Just s
    VariableShadowed (Located s _) _ -> Just s
    TypeConstructorDefinitionContainsTyCon (S.TyConInfo _ _ s) -> Just s
    DuplicateDataConstructorName (Located s _) _ -> Just s
    DuplicateTypeVariable (Located s _) _ -> Just s
    TypeConstructorAlreadyExists (Located s _) _ -> Just s
    UnknownTypeConstructor (Located s _) -> Just s
    UnknownTypeVariable (Located s _) -> Just s

    UnificationFail{} -> Nothing
    KindMismatch{} -> Nothing
    InfiniteType{} -> Nothing
    UnboundVariable{} -> Nothing

    -- BindingLacksTypeSignature bind -> Just $ locatedSpan $ rBindingName bind
    -- TypeMismatch{} -> Nothing
    -- CantFindType{} -> Nothing
    -- WrongNumberOfArguments{} -> Nothing
    -- ExpectedPrimitiveType mLocated _ -> locatedSpan <$> mLocated
    -- ExpectedFunctionType{} -> Nothing

showError :: Error -> String
showError (ParserError err) = parseErrorPretty err
showError err = groom err
