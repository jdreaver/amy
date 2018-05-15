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

import Amy.ANF.AST as ANF
import Amy.Renamer.AST as R
import Amy.Syntax.Located
import Amy.TypeCheck.AST as T

data Error
  -- Parser
  = ParserError !(ParseError Char Void)

  -- Renamer
  | UnknownVariable !(Located Text)
  | VariableShadowed !(Located Text) !(Located R.Ident)
  | DuplicateDataConstructorName !(Located Text) !R.DataConstructor
  | DuplicateTypeVariable !R.TyVarInfo !R.TyVarInfo
  | TypeConstructorAlreadyExists !(Located Text) !R.TyConInfo
  | UnknownTypeConstructor !(Located Text)
  | UnknownTypeVariable !(Located Text)
  | NonIdentifierName !(Located Text)

  -- Type checker
  -- TODO: Add source spans here
  | UnificationFail !T.Type !T.Type
  | KindMismatch !T.TyVarInfo !T.Type
  | InfiniteType !T.TyVarInfo !T.Type
  | UnboundVariable !T.Ident
  | UnboundConstructor !T.DataConstructor

  -- | BindingLacksTypeSignature !RBinding
  -- | TypeMismatch !(Type PrimitiveType) !(Type PrimitiveType)
  -- | CantFindType !(Located Name)
  -- | WrongNumberOfArguments !Int !Int
  -- | ExpectedPrimitiveType !(Maybe (Located Name)) !(Type PrimitiveType)
  -- | ExpectedFunctionType !(Type PrimitiveType)

  -- Codegen
  | CodegenMissingSymbol !ANF.Ident
  | NoCurrying !T.App
  | UnknownOperandType !Operand
  deriving (Show, Eq)

errorLocation :: Error -> Maybe SourceSpan
errorLocation e =
  case e of
    ParserError{} -> Nothing
    UnknownVariable (Located s _) -> Just s
    VariableShadowed (Located s _) _ -> Just s
    DuplicateDataConstructorName (Located s _) _ -> Just s
    DuplicateTypeVariable (R.TyVarInfo _ _ s) _ -> Just s
    TypeConstructorAlreadyExists (Located s _) _ -> Just s
    UnknownTypeConstructor (Located s _) -> Just s
    UnknownTypeVariable (Located s _) -> Just s
    NonIdentifierName (Located s _) -> Just s

    UnificationFail{} -> Nothing
    KindMismatch{} -> Nothing
    InfiniteType{} -> Nothing
    UnboundVariable{} -> Nothing
    UnboundConstructor{} -> Nothing

    -- BindingLacksTypeSignature bind -> Just $ locatedSpan $ rBindingName bind
    -- TypeMismatch{} -> Nothing
    -- CantFindType{} -> Nothing
    -- WrongNumberOfArguments{} -> Nothing
    -- ExpectedPrimitiveType mLocated _ -> locatedSpan <$> mLocated
    -- ExpectedFunctionType{} -> Nothing

    CodegenMissingSymbol{} -> Nothing
    NoCurrying{} -> Nothing
    UnknownOperandType{} -> Nothing

showError :: Error -> String
showError (ParserError err) = parseErrorPretty err
showError err = show err
