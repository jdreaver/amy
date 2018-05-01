{-# LANGUAGE DeriveFunctor #-}

module Amy.Core.AST
  ( Module(..)
  , Binding(..)
  , Extern(..)
  , TypeDeclaration(..)
  , DataConstructor(..)
  , Expr(..)
  , Var(..)
  , If(..)
  , Case(..)
  , Match(..)
  , Pattern(..)
  , ConstructorPattern(..)
  , Let(..)
  , App(..)
  , expressionType
  , moduleNameIds

  , Ident(..)
  , ConstructorName(..)
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
  , typeDeclarationConstructor :: !DataConstructor
  } deriving (Show, Eq)

data DataConstructor
  = DataConstructor
  { dataConstructorName :: !ConstructorName
  , dataConstructorArgument :: !(Maybe TyConInfo)
  } deriving (Show, Eq)

data Expr
  = ELit !Literal
  | EVar !Var
  | ECase !Case
  | ELet !Let
  | EApp !App
  | EParens !Expr
  deriving (Show, Eq)

data Var
  = VVal !(Typed Ident)
  | VCons !(Typed ConstructorName)
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
  | PatternCons !ConstructorPattern
  deriving (Show, Eq)

data ConstructorPattern
  = ConstructorPattern
  { constructorPatternConstructor :: !(Typed ConstructorName)
  , constructorPatternArg :: !(Maybe (Typed Ident))
  , constructorPatternReturnType :: !Type
  } deriving (Show, Eq)

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
expressionType (EVar var) =
  case var of
    VVal (Typed ty _) -> ty
    VCons (Typed ty _) -> ty
expressionType (ECase (Case _ (Match _ expr :| _))) = expressionType expr
expressionType (ELet let') = expressionType (letExpression let')
expressionType (EApp app) = appReturnType app
expressionType (EParens expr) = expressionType expr

-- | Get all the 'Name's in a module.
moduleNameIds :: Module -> [Int]
moduleNameIds (Module bindings externs typeDeclarations) =
  concatMap bindingNameIds bindings
  ++ fmap (constructorNameId . dataConstructorName . typeDeclarationConstructor) typeDeclarations
  ++ fmap (identId . externName) externs

bindingNameIds :: Binding -> [Int]
bindingNameIds binding =
  identId (bindingName binding)
  : (identId . typedValue <$> bindingArgs binding)
  ++ exprNameIds (bindingBody binding)

exprNameIds :: Expr -> [Int]
exprNameIds (ELit _) = []
exprNameIds (EVar _) = []
exprNameIds (ECase (Case scrutinee matches)) =
  exprNameIds scrutinee ++ concatMap matchNames matches
exprNameIds (ELet (Let bindings expr)) =
  concatMap bindingNameIds bindings
  ++ exprNameIds expr
exprNameIds (EApp (App f args _)) =
  exprNameIds f
  ++ concatMap exprNameIds args
exprNameIds (EParens expr) = exprNameIds expr

matchNames :: Match -> [Int]
matchNames (Match pat body) = patternNameIds pat ++ exprNameIds body

patternNameIds :: Pattern -> [Int]
patternNameIds (PatternLit _) = []
patternNameIds (PatternVar (Typed _ var)) = [identId var]
patternNameIds (PatternCons (ConstructorPattern _ mArg _)) = maybe [] (\(Typed _ var) -> [identId var]) mArg

data Ident
  = Ident
  { identText :: !Text
  , identId :: !Int
  , identPrimitiveName :: !(Maybe PrimitiveFunctionName)
  } deriving (Show, Eq, Ord)

data ConstructorName
  = ConstructorName
  { constructorNameText :: !Text
  , constructorNameId :: !Int
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
