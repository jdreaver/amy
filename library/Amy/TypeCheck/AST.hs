{-# LANGUAGE DeriveFunctor #-}

-- | Version of a renamer 'RModule' after type checking.

module Amy.TypeCheck.AST
  ( TModule(..)
  , TBinding(..)
  , TExtern(..)
  , TExpr(..)
  , TIf(..)
  , TCase(..)
  , TMatch(..)
  , TPattern(..)
  , TLet(..)
  , TApp(..)
  , literalTType
  , expressionType
  , patternType
  , tModuleNames

  , TIdent(..)
  , TType(..)
  , TyVarGenerated(..)
  , TTypeName(..)
  , TScheme(..)
  , TTyped(..)

    -- Re-export
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import Amy.Literal
import Amy.Prim

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
  , tBindingType :: !TScheme
    -- ^ Type for whole function
  , tBindingArgs :: ![TTyped TIdent]
    -- ^ Argument names and types split out from 'tBindingType'
  , tBindingReturnType :: !TType
    -- ^ Return type split out from 'tBindingType'
  , tBindingBody :: !TExpr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data TExtern
  = TExtern
  { tExternName :: !TIdent
  , tExternType :: !TType
  } deriving (Show, Eq)

-- | A renamed 'Expr'
data TExpr
  = TELit !Literal
  | TEVar !(TTyped TIdent)
  | TEIf !TIf
  | TECase !TCase
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

data TCase
  = TCase
  { tCaseScrutinee :: !TExpr
  , tCaseAlternatives :: !(NonEmpty TMatch)
  } deriving (Show, Eq)

data TMatch
  = TMatch
  { tMatchPattern :: !TPattern
  , tMatchBody :: !TExpr
  } deriving (Show, Eq)

data TPattern
  = TPatternLit !Literal
  | TPatternVar !(TTyped TIdent)
  deriving (Show, Eq)

data TLet
  = TLet
  { tLetBindings :: ![TBinding]
  , tLetExpression :: !TExpr
  } deriving (Show, Eq)

data TApp
  = TApp
  { tAppFunction :: !TExpr
  , tAppArgs :: !(NonEmpty TExpr)
  , tAppReturnType :: !TType
  } deriving (Show, Eq)

literalTType :: Literal -> TType
literalTType lit =
  let primTy = literalType lit
  in TTyCon $ TTypeName (showPrimitiveType primTy) (primitiveTypeId primTy) (Just primTy)

expressionType :: TExpr -> TType
expressionType (TELit lit) = literalTType lit
expressionType (TEVar (TTyped ty _)) = ty
expressionType (TEIf if') = expressionType (tIfThen if') -- Checker ensure "then" and "else" types match
expressionType (TECase (TCase _ (TMatch _ expr :| _))) = expressionType expr
expressionType (TELet let') = expressionType (tLetExpression let')
expressionType (TEApp app) = tAppReturnType app
expressionType (TEParens expr) = expressionType expr

patternType :: TPattern -> TType
patternType (TPatternLit lit) = literalTType lit
patternType (TPatternVar (TTyped ty _)) = ty

-- | Get all the 'Name's in a module.
tModuleNames :: TModule -> [TIdent]
tModuleNames (TModule bindings externs) =
  concatMap bindingNames bindings
  ++ fmap tExternName externs

bindingNames :: TBinding -> [TIdent]
bindingNames binding =
  tBindingName binding
  : (tTypedValue <$> tBindingArgs binding)
  ++ exprNames (tBindingBody binding)

exprNames :: TExpr -> [TIdent]
exprNames (TELit _) = []
exprNames (TEVar var) = [tTypedValue var] -- TODO: Shouldn't we only need name bindings here?
exprNames (TEIf (TIf pred' then' else')) =
  exprNames pred'
  ++ exprNames then'
  ++ exprNames else'
exprNames (TECase (TCase scrutinee matches)) =
  exprNames scrutinee ++ concatMap matchNames matches
exprNames (TELet (TLet bindings expr)) =
  concatMap bindingNames bindings
  ++ exprNames expr
exprNames (TEApp (TApp f args _)) =
  exprNames f
  ++ concatMap exprNames args
exprNames (TEParens expr) = exprNames expr

matchNames :: TMatch -> [TIdent]
matchNames (TMatch pat body) = patternNames pat ++ exprNames body

patternNames :: TPattern -> [TIdent]
patternNames (TPatternLit _) = []
patternNames (TPatternVar (TTyped _ var)) = [var]

data TIdent
  = TIdent
  { tIdentText :: !Text
  , tIdentId :: !Int
  , tIdentPrimitiveName :: !(Maybe PrimitiveFunctionName)
  } deriving (Show, Eq, Ord)

data TType
  = TTyCon !TTypeName
  | TTyVar !TTypeName !TyVarGenerated
  | TTyFun !TType !TType
  deriving (Show, Eq, Ord)

data TyVarGenerated
  = TyVarGenerated
  | TyVarNotGenerated
  deriving (Show, Eq, Ord)

infixr 0 `TTyFun`

data TTypeName
  = TTypeName
  { tTypeNameText :: !Text
  , tTypeNameId :: !Int
  , tTypeNamePrimitiveType :: !(Maybe PrimitiveType)
  } deriving (Show, Eq, Ord)

data TScheme
  = TForall ![TTypeName] TType
  deriving (Show, Eq)

data TTyped a
  = TTyped
  { tTypedType :: !TType
  , tTypedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
