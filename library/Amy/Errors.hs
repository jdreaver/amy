-- | User facing errors.

module Amy.Errors
  ( Error(..)
  , showError
  ) where

import Data.Text (Text)
import Data.Void (Void)
import LLVM.AST (Operand)
import Text.Megaparsec

import Amy.Names
import Amy.Prim
import Amy.Renamer.AST
import Amy.Type
import Amy.TypeCheck.AST

data Error
  -- Parser
  = ParserError !(ParseError Char Void)

  -- Renamer
  | UnknownVariable !Text
  | VariableShadowed !Text !ValueName

  -- Type checker
  | BindingLacksTypeSignature !RBinding
  | TypeMismatch !(Type PrimitiveType) !(Type PrimitiveType)
  | UnknownTypeName !Text
  | CantFindType !ValueName
  | WrongNumberOfArguments !Int !Int
  | ExpectedPrimitiveType !(Maybe ValueName) !(Type PrimitiveType)
  | ExpectedFunctionType !(Type PrimitiveType)

  -- Codegen
  | CodegenUnknownTypeName !Text
  | CodegenMissingSymbol !ValueName
  | CodegenExpectedPrimitiveType !(Type PrimitiveType)
  | NoCurrying !TApp
  | UnknownOperandType !Operand
  deriving (Show, Eq)

showError :: Error -> String
showError (ParserError err) = parseErrorPretty err
showError err = show err
