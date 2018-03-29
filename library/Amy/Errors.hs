-- | User facing errors.

module Amy.Errors
  ( Error(..)
  , showError
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

import Amy.AST
import Amy.Names
import Amy.Type

data Error
  -- Parser
  = ParserError !(ParseError Char Void)

  -- Renamer
  | TypeSignatureLacksBinding !Text
  | BindingLacksTypeSignature !Text
  | UnknownVariable !Text
  | VariableShadowed !Text !ValueName

  -- Type checker
  | TypeMismatch !Type !Type
  | UnknownTypeName !Text
  | CantFindType !ValueName
  | WrongNumberOfArguments !Int !Int
  | ExpectedPrimitiveType !(Maybe ValueName) !Type
  | ExpectedFunctionType !Type

  -- Codegen
  | CodegenUnknownTypeName !Text
  | CodegenMissingSymbol !ValueName
  | CodegenExpectedPrimitiveType !Type
  | NoCurrying !(FunctionApplication ValueName Type)
  deriving (Show, Eq)

showError :: Error -> String
showError (ParserError err) = parseErrorPretty err
showError err = show err
