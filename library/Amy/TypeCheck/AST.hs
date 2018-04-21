-- | Version of a renamer 'RModule' after type checking.

module Amy.TypeCheck.AST
  ( TModule(..)
  , TBinding(..)
  , TExtern(..)
  , TExpr(..)
  , TIf(..)
  , TLet(..)
  , TApp(..)
  , expressionType
  , tModuleNames

  , TName(..)
  , TIdent(..)

    -- Re-export
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Amy.Literal
import Amy.Prim
import Amy.Type

-- | A 'TModule' is an 'RModule' after renaming.
data TModule
  = TModule
  { tModuleBindings :: ![TBinding]
  , tModuleExterns :: ![TExtern]
  }
  deriving (Show, Eq)

-- | A binding after renaming. This is a combo of a 'Binding' and a
-- 'BindingType' after they've been paired together.
data TBinding
  = TBinding
  { tBindingName :: !TIdent
  , tBindingType :: !(Scheme PrimitiveType)
    -- ^ Type for whole function
  , tBindingArgs :: ![Typed PrimitiveType TName]
    -- ^ Argument names and types split out from 'tBindingType'
  , tBindingReturnType :: !(Type PrimitiveType)
    -- ^ Return type split out from 'tBindingType'
  , tBindingBody :: !TExpr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data TExtern
  = TExtern
  { tExternName :: !TIdent
  , tExternType :: !(Type PrimitiveType)
  } deriving (Show, Eq)

-- | A renamed 'Expr'
data TExpr
  = TELit !Literal
  | TEVar !(Typed PrimitiveType TName)
  | TEIf !TIf
  | TELet !TLet
  | TEApp !TApp
  | TEParens !TExpr
  deriving (Show, Eq)

data TIf
  = TIf
  { tIfPredicate :: !TExpr
  , tIfThen :: !TExpr
  , tIfElse :: !TExpr
  } deriving (Show, Eq)

data TLet
  = TLet
  { tLetBindings :: ![TBinding]
  , tLetExpression :: !TExpr
  } deriving (Show, Eq)

data TApp
  = TApp
  { tAppFunction :: !TExpr
  , tAppArgs :: !(NonEmpty TExpr)
  , tAppReturnType :: !(Type PrimitiveType)
  } deriving (Show, Eq)

expressionType :: TExpr -> Type PrimitiveType
expressionType (TELit lit) = TyCon $ literalType lit
expressionType (TEVar (Typed ty _)) = ty
expressionType (TEIf if') = expressionType (tIfThen if') -- Checker ensure "then" and "else" types match
expressionType (TELet let') = expressionType (tLetExpression let')
expressionType (TEApp app) = tAppReturnType app
expressionType (TEParens expr) = expressionType expr

-- | Get all the 'Name's in a module.
tModuleNames :: TModule -> [TName]
tModuleNames (TModule bindings externs) =
  concatMap bindingNames bindings
  ++ fmap (TIdentName . tExternName) externs

bindingNames :: TBinding -> [TName]
bindingNames binding =
  (TIdentName $ tBindingName binding)
  : (typedValue <$> tBindingArgs binding)
  ++ exprNames (tBindingBody binding)

exprNames :: TExpr -> [TName]
exprNames (TELit _) = []
exprNames (TEVar var) = [typedValue var]
exprNames (TEIf (TIf pred' then' else')) =
  exprNames pred'
  ++ exprNames then'
  ++ exprNames else'
exprNames (TELet (TLet bindings expr)) =
  concatMap bindingNames bindings
  ++ exprNames expr
exprNames (TEApp (TApp f args _)) =
  exprNames f
  ++ concatMap exprNames args
exprNames (TEParens expr) = exprNames expr

data TName
  = TPrimitiveName !PrimitiveFunctionName
  | TIdentName !TIdent
  deriving (Show, Eq, Ord)

-- | An identifier from source code
data TIdent
  = TIdent
  { tIdentText :: !Text
  , tIdentId :: !Int
  , tIdentIsTopLevel :: !Bool
  } deriving (Show, Eq, Ord)
