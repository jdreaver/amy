-- | Common AST components shared across various actual ASTs.

module Amy.Literal
  ( Literal(..)
  , literalType
  ) where

import Amy.Type

-- | A 'Literal' is any literal from the source code. This type is used in many
-- ASTs since there is no need for renaming or annotating types to a literal.
data Literal
  = LiteralInt !Int
  | LiteralDouble !Double
  | LiteralBool !Bool
  deriving (Show, Eq)

literalType :: Literal -> PrimitiveType
literalType (LiteralInt _) = IntType
literalType (LiteralDouble _) = DoubleType
literalType (LiteralBool _) = BoolType
