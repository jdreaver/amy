module Amy.ASTCommon
  ( ConstructorSpan(..)
  , ConstructorIndex(..)
  ) where

-- | The span of a data constructor is the total number of constructors for
-- that type. The span for True and False in @Bool = False | True@ is 2. The
-- span for primitive "constructors" like number literals is infinite.
newtype ConstructorSpan = ConstructorSpan { unConstructorSpan :: Int }
  deriving (Show, Eq, Ord)

-- | The index of a data constructor is the position of a constructor within
-- the declaration. For @Bool = False | True@, @False@ has position 0 and
-- @True@ has position 1. The constructor in types with only one constructor
-- have position 0 of course.
newtype ConstructorIndex = ConstructorIndex { unConstructorIndex :: Int }
  deriving (Show, Eq, Ord)
