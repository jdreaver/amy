{-# LANGUAGE DeriveFunctor #-}

module Amy.Core.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , Expr(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , Let(..)
  , App(..)
  , expressionType
  , moduleNames

  , Ident(..)
  , Type(..)
  , TyConInfo(..)
  , TyVarInfo(..)
  , Scheme(..)
  , Typed(..)

    -- Re-export
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import Amy.Literal
import Amy.Prim

data Module
  = Module
  { moduleBindings :: ![Binding]
  , moduleExterns :: ![Extern]
  , moduleTypeDeclarations :: ![TypeDeclaration]
  } deriving (Show, Eq)

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

data Extern
  = Extern
  { externName :: !Ident
  , externType :: !Type
  } deriving (Show, Eq)

data TypeDeclaration
  = TypeDeclaration
  { typeDeclarationTypeName :: !TyConInfo
  , typeDeclarationConstructorName :: !Ident
  , typeDeclarationArgument :: !TyConInfo
  } deriving (Show, Eq)

data Expr
  = ELit !Literal
  | EVar !(Typed Ident)
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

literalType' :: Literal -> Type
literalType' lit =
  let primTy = literalType lit
  in TyCon $ TyConInfo (showPrimitiveType primTy) (primitiveTypeId primTy) (Just primTy)

expressionType :: Expr -> Type
expressionType (ELit lit) = literalType' lit
expressionType (EVar (Typed ty _)) = ty
expressionType (ECase (Case _ (Match _ expr :| _))) = expressionType expr
expressionType (ELet let') = expressionType (letExpression let')
expressionType (EApp app) = appReturnType app
expressionType (EParens expr) = expressionType expr

-- | Get all the 'Name's in a module.
moduleNames :: Module -> [Ident]
moduleNames (Module bindings externs typeDeclarations) =
  concatMap bindingNames bindings
  ++ fmap typeDeclarationConstructorName typeDeclarations
  ++ fmap externName externs

bindingNames :: Binding -> [Ident]
bindingNames binding =
  bindingName binding
  : (typedValue <$> bindingArgs binding)
  ++ exprNames (bindingBody binding)

exprNames :: Expr -> [Ident]
exprNames (ELit _) = []
exprNames (EVar var) = [typedValue var] -- TODO: Shouldn't we only need name bindings here?
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
  = TyCon !TyConInfo
  | TyVar !TyVarInfo
  | TyFun !Type !Type
  deriving (Show, Eq, Ord)

infixr 0 `TyFun`

data TyConInfo
  = TyConInfo
  { tyConInfoText :: !Text
  , tyConInfoId :: !Int
  , tyConInfoPrimitiveType :: !(Maybe PrimitiveType)
  } deriving (Show, Eq, Ord)

data TyVarInfo
  = TyVarInfo
  { tyVarInfoName :: !Text
  , tyVarInfoId :: !Int
  } deriving (Show, Eq, Ord)

data Scheme
  = Forall ![TyVarInfo] Type
  deriving (Show, Eq)

data Typed a
  = Typed
  { typedType :: !Type
  , typedValue :: !a
  } deriving (Show, Eq, Ord, Functor)
