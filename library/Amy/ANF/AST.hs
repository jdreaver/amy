module Amy.ANF.AST
  ( ANFModule(..)
  , ANFBinding(..)
  , ANFExtern(..)
  , ANFVal(..)
  , ANFExpr(..)
  , ANFLet(..)
  , ANFIf(..)
  , ANFApp(..)

  , ANFIdent(..)
  ) where

import Data.Text (Text)

import Amy.Literal
import Amy.Prim
import Amy.Type

data ANFModule
  = ANFModule
  { anfModuleBindings :: ![ANFBinding]
  , anfModuleExterns :: ![ANFExtern]
  } deriving (Show, Eq)

data ANFBinding
  = ANFBinding
  { anfBindingName :: !ANFIdent
  , anfBindingType :: !(Scheme PrimitiveType)
  , anfBindingArgs :: ![Typed PrimitiveType ANFIdent]
  , anfBindingReturnType :: !(Type PrimitiveType)
  , anfBindingBody :: !ANFExpr
  } deriving (Show, Eq)

data ANFExtern
  = ANFExtern
  { anfExternName :: !ANFIdent
  , anfExternType :: !(Type PrimitiveType)
  } deriving (Show, Eq)

data ANFVal
  = ANFVar !(Typed PrimitiveType ANFIdent)
  | ANFLit !Literal
  deriving (Show, Eq)

data ANFExpr
  = ANFEVal !ANFVal
  | ANFELet !ANFLet
  | ANFEIf !ANFIf
  | ANFEApp !(ANFApp (Typed PrimitiveType ANFIdent))
  | ANFEPrimOp !(ANFApp PrimitiveFunctionName)
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
  , anfIfType :: !(Type PrimitiveType)
  } deriving (Show, Eq)

data ANFApp f
  = ANFApp
  { anfAppFunction :: !f
  , anfAppArgs :: ![ANFVal]
  , anfAppReturnType :: !(Type PrimitiveType)
  } deriving (Show, Eq)

-- | An identifier from source code
data ANFIdent
  = ANFIdent
  { anfIdentText :: !Text
  , anfIdentId :: !Int
  , anfIdentPrimitiveName :: !(Maybe PrimitiveFunctionName)
  , anfIdentIsTopLevel :: !Bool
  } deriving (Show, Eq, Ord)
