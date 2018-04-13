module Amy.CPS.AST
  ( CPSVal(..)
  , CPSExpr(..)
  , CPSApp(..)
  , CPSFix(..)
  , CPSFixBinding(..)
  , CPSIf(..)
  , CPSPrimOp(..)
  , freeCPSFixVariables
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

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
  , cpsPrimOpOutputs :: ![ValueName]
  , cpsPrimOpContinuations :: ![CPSExpr]
  } deriving (Show, Eq)

--
-- Free Variables
--

freeCPSFixVariables :: CPSFix -> Set ValueName
freeCPSFixVariables (CPSFix bindings body) =
  freeCPSExprVariables body
    `Set.union` Set.unions (freeCPSFixBindingVariables <$> bindings)
    `Set.difference` Set.fromList (cpsFixBindingName <$> bindings)

freeCPSFixBindingVariables :: CPSFixBinding -> Set ValueName
freeCPSFixBindingVariables (CPSFixBinding _ args body) =
  freeCPSExprVariables body `Set.difference` Set.fromList args

freeCPSExprVariables :: CPSExpr -> Set ValueName
freeCPSExprVariables expr =
  case expr of
    CPSEApp (CPSApp f args) -> Set.unions (freeCPSValVariables <$> (f : args))
    CPSEFix fix -> freeCPSFixVariables fix
    CPSEIf (CPSIf pred' then' else') ->
      freeCPSValVariables pred'
      `Set.union` freeCPSExprVariables then'
      `Set.union` freeCPSExprVariables else'
    CPSEPrimOp (CPSPrimOp _ args outputs conts) ->
      Set.unions (freeCPSValVariables <$> args)
      `Set.union` Set.unions (freeCPSExprVariables <$> conts)
      `Set.difference` Set.fromList outputs
    CPSHalt -> Set.empty

freeCPSValVariables :: CPSVal -> Set ValueName
freeCPSValVariables (CPSVar v) = Set.singleton v
freeCPSValVariables (CPSLabel v) = Set.singleton v
freeCPSValVariables (CPSLit _) = Set.empty
