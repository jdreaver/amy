{-# LANGUAGE DeriveFunctor #-}

module Amy.ANF.AST
  ( ANFModule(..)
  , ANFBinding(..)
  , ANFExtern(..)
  , ANFVal(..)
  , anfValType
  , ANFExpr(..)
  , ANFLet(..)
  , ANFIf(..)
  , ANFApp(..)

  , ANFIdent(..)
  , ANFType(..)
  , ANFTypeName(..)
  , ANFScheme(..)
  , ANFTyped(..)
  ) where

import Data.Text (Text)

import Amy.Literal
import Amy.Prim

data ANFModule
  = ANFModule
  { anfModuleBindings :: ![ANFBinding]
  , anfModuleExterns :: ![ANFExtern]
  } deriving (Show, Eq)

data ANFBinding
  = ANFBinding
  { anfBindingName :: !ANFIdent
  , anfBindingType :: !ANFScheme
  , anfBindingArgs :: ![ANFTyped ANFIdent]
  , anfBindingReturnType :: !ANFType
  , anfBindingBody :: !ANFExpr
  } deriving (Show, Eq)

data ANFExtern
  = ANFExtern
  { anfExternName :: !ANFIdent
  , anfExternType :: !ANFType
  } deriving (Show, Eq)

data ANFVal
  = ANFVar !(ANFTyped ANFIdent)
  | ANFLit !Literal
  deriving (Show, Eq)

anfValType :: ANFVal -> ANFType
anfValType (ANFVar (ANFTyped ty _)) = ty
anfValType (ANFLit lit) =
  let primTy = literalType lit
  in ANFTyCon $ ANFTypeName (showPrimitiveType primTy) (primitiveTypeId primTy) (Just primTy)

data ANFExpr
  = ANFEVal !ANFVal
  | ANFELet !ANFLet
  | ANFEIf !ANFIf
  | ANFEApp !(ANFApp (ANFTyped ANFIdent))
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
  , anfIfType :: !ANFType
  } deriving (Show, Eq)

data ANFApp f
  = ANFApp
  { anfAppFunction :: !f
  , anfAppArgs :: ![ANFVal]
  , anfAppReturnType :: !ANFType
  } deriving (Show, Eq)

-- | An identifier from source code
data ANFIdent
  = ANFIdent
  { anfIdentText :: !Text
  , anfIdentId :: !Int
  , anfIdentPrimitiveName :: !(Maybe PrimitiveFunctionName)
  , anfIdentIsTopLevel :: !Bool
  } deriving (Show, Eq, Ord)

data ANFType
  = ANFTyCon !ANFTypeName
  | ANFTyVar !ANFTypeName
  | ANFTyFun !ANFType !ANFType
  deriving (Show, Eq, Ord)

infixr 0 `ANFTyFun`

data ANFTypeName
  = ANFTypeName
  { anfTypeNameText :: !Text
  , anfTypeNameId :: !Int
  , anfTypeNamePrimitiveType :: !(Maybe PrimitiveType)
  } deriving (Show, Eq, Ord)

data ANFScheme
  = ANFForall ![ANFTypeName] ANFType
  deriving (Show, Eq)

data ANFTyped a
  = ANFTyped
  { anfTypedType :: !ANFType
  , anfTypedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
