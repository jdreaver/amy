{-# LANGUAGE DeriveFunctor #-}

-- | Version of a renamer 'RModule' after type checking.

module Amy.TypeCheck.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , Expr(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , Let(..)
  , App(..)
  , literalTType
  , expressionType
  , patternType
  , moduleNames

  , Ident(..)
  , Type(..)
  , TyVarGenerated(..)
  , TypeName(..)
  , Scheme(..)
  , Typed(..)

    -- Re-export
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import Amy.Literal
import Amy.Prim

-- | A 'TModule' is an 'RModule' after renaming.
data Module
  = Module
  { moduleBindings :: ![Binding]
  , moduleExterns :: ![Extern]
  } deriving (Show, Eq)

-- | A binding after renaming. This is a combo of a 'Binding' and a
-- 'BindingType' after they've been paired together.
data Binding
  = Binding
  { bindingName :: !Ident
  , bindingType :: !Scheme
    -- ^ Type for whole function
  , bindingArgs :: ![Typed Ident]
    -- ^ Argument names and types split out from 'bindingType'
  , bindingReturnType :: !Type
    -- ^ Return type split out from 'bindingType'
  , bindingBody :: !Expr
  } deriving (Show, Eq)

-- | A renamed extern declaration.
data Extern
  = Extern
  { externName :: !Ident
  , externType :: !Type
  } deriving (Show, Eq)

-- | A renamed 'Expr'
data Expr
  = ELit !Literal
  | EVar !(Typed Ident)
  | EIf !If
  | ECase !Case
  | ELet !Let
  | EApp !App
  | EParens !Expr
  deriving (Show, Eq)

data If
  = If
  { ifPredicate :: !Expr
  , ifThen :: !Expr
  , ifElse :: !Expr
  } deriving (Show, Eq)

data Case
  = Case
  { caseScrutinee :: !Expr
  , caseAlternatives :: !(NonEmpty Match)
  } deriving (Show, Eq)

data Match
  = Match
  { matchPattern :: !Pattern
  , matchBody :: !Expr
  } deriving (Show, Eq)

data Pattern
  = PatternLit !Literal
  | PatternVar !(Typed Ident)
  deriving (Show, Eq)

data Let
  = Let
  { letBindings :: ![Binding]
  , letExpression :: !Expr
  } deriving (Show, Eq)

data App
  = App
  { appFunction :: !Expr
  , appArgs :: !(NonEmpty Expr)
  , appReturnType :: !Type
  } deriving (Show, Eq)

literalTType :: Literal -> Type
literalTType lit =
  let primTy = literalType lit
  in TyCon $ TypeName (showPrimitiveType primTy) (primitiveTypeId primTy) (Just primTy)

expressionType :: Expr -> Type
expressionType (ELit lit) = literalTType lit
expressionType (EVar (Typed ty _)) = ty
expressionType (EIf if') = expressionType (ifThen if') -- Checker ensure "then" and "else" types match
expressionType (ECase (Case _ (Match _ expr :| _))) = expressionType expr
expressionType (ELet let') = expressionType (letExpression let')
expressionType (EApp app) = appReturnType app
expressionType (EParens expr) = expressionType expr

patternType :: Pattern -> Type
patternType (PatternLit lit) = literalTType lit
patternType (PatternVar (Typed ty _)) = ty

-- | Get all the 'Name's in a module.
moduleNames :: Module -> [Ident]
moduleNames (Module bindings externs) =
  concatMap bindingNames bindings
  ++ fmap externName externs

bindingNames :: Binding -> [Ident]
bindingNames binding =
  bindingName binding
  : (typedValue <$> bindingArgs binding)
  ++ exprNames (bindingBody binding)

exprNames :: Expr -> [Ident]
exprNames (ELit _) = []
exprNames (EVar var) = [typedValue var] -- TODO: Shouldn't we only need name bindings here?
exprNames (EIf (If pred' then' else')) =
  exprNames pred'
  ++ exprNames then'
  ++ exprNames else'
exprNames (ECase (Case scrutinee matches)) =
  exprNames scrutinee ++ concatMap matchNames matches
exprNames (ELet (Let bindings expr)) =
  concatMap bindingNames bindings
  ++ exprNames expr
exprNames (EApp (App f args _)) =
  exprNames f
  ++ concatMap exprNames args
exprNames (EParens expr) = exprNames expr

matchNames :: Match -> [Ident]
matchNames (Match pat body) = patternNames pat ++ exprNames body

patternNames :: Pattern -> [Ident]
patternNames (PatternLit _) = []
patternNames (PatternVar (Typed _ var)) = [var]

data Ident
  = Ident
  { identText :: !Text
  , identId :: !Int
  , identPrimitiveName :: !(Maybe PrimitiveFunctionName)
  } deriving (Show, Eq, Ord)

data Type
  = TyCon !TypeName
  | TyVar !TypeName !TyVarGenerated
  | TyFun !Type !Type
  deriving (Show, Eq, Ord)

data TyVarGenerated
  = TyVarGenerated
  | TyVarNotGenerated
  deriving (Show, Eq, Ord)

infixr 0 `TyFun`

data TypeName
  = TypeName
  { typeNameText :: !Text
  , typeNameId :: !Int
  , typeNamePrimitiveType :: !(Maybe PrimitiveType)
  } deriving (Show, Eq, Ord)

data Scheme
  = Forall ![TypeName] Type
  deriving (Show, Eq)

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
