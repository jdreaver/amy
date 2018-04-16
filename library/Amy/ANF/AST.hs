module Amy.ANF.AST
  ( ANFModule(..)
  , ANFBinding(..)
  , ANFExtern(..)
  , ANFVal(..)
  , ANFExpr(..)
  , ANFLet(..)
  , ANFIf(..)
  , ANFApp(..)
  ) where

import Amy.Literal
import Amy.Names
import Amy.Prim
import Amy.Type

data ANFModule
  = ANFModule
  { anfModuleBindings :: ![ANFBinding]
  , anfModuleExterns :: ![ANFExtern]
  } deriving (Show, Eq)

data ANFBinding
  = ANFBinding
  { anfBindingName :: !Name
  , anfBindingType :: !(Scheme PrimitiveType)
  , anfBindingArgs :: ![Typed PrimitiveType Name]
  , anfBindingReturnType :: !(Type PrimitiveType)
  , anfBindingBody :: !ANFExpr
  } deriving (Show, Eq)

data ANFExtern
  = ANFExtern
  { anfExternName :: !Name
  , anfExternType :: !(Type PrimitiveType)
  } deriving (Show, Eq)

data ANFVal
  = ANFVar !(Typed PrimitiveType Name)
  | ANFLit !Literal
  deriving (Show, Eq)

data ANFExpr
  = ANFEVal !ANFVal
  | ANFELet !ANFLet
  | ANFEIf !ANFIf
  | ANFEApp !ANFApp
  deriving (Show, Eq)

data ANFLet
  = ANFLet
  { anfLetBindings :: ![ANFBinding]
  , anfLetExpression :: !ANFExpr
  } deriving (Show, Eq)

data ANFIf
  = ANFIf
  { anfIfPredicate :: !ANFVal
  , anfIfThen :: !ANFExpr
  , anfIfElse :: !ANFExpr
  , anfIfId :: !IdentId
  , anfIfType :: !(Type PrimitiveType)
  } deriving (Show, Eq)

data ANFApp
  = ANFApp
  { anfAppFunction :: !ANFVal
  , anfAppArgs :: ![ANFVal]
  , anfAppReturnType :: !(Type PrimitiveType)
  } deriving (Show, Eq)
