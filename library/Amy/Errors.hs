-- | User facing errors.

module Amy.Errors
  ( Error(..)
  , showError
  , errorLocation
  ) where

import Data.Text (Text)
import Data.Void (Void)
import LLVM.AST (Operand)
import Text.Megaparsec

import Amy.Names
import Amy.Prim
import Amy.Renamer.AST
import Amy.Syntax.Located
import Amy.Type
import Amy.TypeCheck.AST

data Error
  -- Parser
  = ParserError !(ParseError Char Void)

  -- Renamer
  | UnknownVariable !(Located Text)
  | VariableShadowed !(Located Text) !ValueName

  -- Type checker
  | BindingLacksTypeSignature !RBinding
  | TypeMismatch !(Type PrimitiveType) !(Type PrimitiveType)
  | UnknownTypeName !(Located Text)
  | CantFindType !(Located ValueName)
  | WrongNumberOfArguments !Int !Int
  | ExpectedPrimitiveType !(Maybe (Located ValueName)) !(Type PrimitiveType)
  | ExpectedFunctionType !(Type PrimitiveType)

  -- Codegen
  | CodegenMissingSymbol !ValueName
  | CodegenExpectedPrimitiveType !(Type PrimitiveType)
  | NoCurrying !TApp
  | UnknownOperandType !Operand
  deriving (Show, Eq)

errorLocation :: Error -> Maybe SourceSpan
errorLocation e =
  case e of
    ParserError{} -> Nothing
    UnknownVariable (Located s _) -> Just s
    VariableShadowed (Located s _) _ -> Just s

    BindingLacksTypeSignature bind -> Just $ locatedSpan $ rBindingName bind
    TypeMismatch{} -> Nothing
    UnknownTypeName (Located s _) -> Just s
    CantFindType{} -> Nothing
    WrongNumberOfArguments{} -> Nothing
    ExpectedPrimitiveType mLocated _ -> locatedSpan <$> mLocated
    ExpectedFunctionType{} -> Nothing

    CodegenMissingSymbol{} -> Nothing
    CodegenExpectedPrimitiveType{} -> Nothing
    NoCurrying{} -> Nothing
    UnknownOperandType{} -> Nothing

showError :: Error -> String
showError (ParserError err) = parseErrorPretty err
showError err = show err
