module Amy.CPS.AST
  ( CPSVal(..)
  , CPSExpr(..)
  , CPSApp(..)
  , CPSFix(..)
  , CPSFixBinding(..)
  , CPSIf(..)
  , CPSPrimOp(..)
  ) where

import Amy.Literal
import Amy.Names
import Amy.Prim

data CPSVal
  = CPSVar !ValueName
  | CPSLabel !ValueName
  | CPSLit !Literal
  deriving (Show, Eq)

data CPSExpr
  = CPSEApp !CPSApp
  | CPSEFix !CPSFix
  | CPSEIf !CPSIf
  | CPSEPrimOp !CPSPrimOp
  | CPSHalt
  deriving (Show, Eq)

data CPSApp
  = CPSApp
  { cpsAppFunction :: !CPSVal -- Should be label or var only if type checked, but can't prove
  , cpsAppArgs :: ![CPSVal]
  } deriving (Show, Eq)

data CPSFix
  = CPSFix
  { cpsFixBindings :: ![CPSFixBinding]
  , cpsFixBody :: !CPSExpr
  } deriving (Show, Eq)

data CPSFixBinding
  = CPSFixBinding
  { cpsFixBindingName :: !ValueName
  , cpsFixBindingArgs :: ![ValueName]
  , cpsFixBindingBody :: !CPSExpr
  } deriving (Show, Eq)

data CPSIf
  = CPSIf
  { cpsIfPredicate :: !CPSVal
  , cpsIfThen :: !CPSExpr
  , cpsIfElse :: !CPSExpr
  } deriving (Show, Eq)

data CPSPrimOp
  = CPSPrimOp
  { cpsPrimOpFunction :: !PrimitiveFunctionName
  , cpsPrimOpArgs :: ![CPSVal]
  , cpsPrimOpOutput :: !ValueName
  , cpsPrimOpExpr :: !CPSExpr
  } deriving (Show, Eq)
