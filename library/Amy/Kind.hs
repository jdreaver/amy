module Amy.Kind
  ( Kind(..)
  ) where

data Kind
  = KStar
  | KFun !Kind !Kind
  deriving (Show, Eq, Ord)

infixr 0 `KFun`
