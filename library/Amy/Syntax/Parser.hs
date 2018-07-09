{-# LANGUAGE FlexibleContexts #-}

module Amy.Syntax.Parser
  ( parseModule

  , externDecl
  , parseBindingType
  , parseType
  , typeTerm
  , binding
  , expression
  , expressionParens
  , caseExpression
  , ifExpression
  , letExpression'
  , literal
  ) where

import qualified Control.Applicative.Combinators.NonEmpty as CNE
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Expr

import Amy.Syntax.AST as S
import Amy.Syntax.Lexer as L
import Amy.Syntax.Monad

parseModule :: AmyParser Module
parseModule = do
  (typeDecls, externs, bindings) <- noIndent (parseDeclarations moduleDeclaration) <* eof
  fileName <- sourceName <$> getPosition
  pure $ Module fileName typeDecls externs bindings

moduleDeclaration :: AmyParser Declaration
moduleDeclaration =
  choice
  [ DeclExtern <$> externDecl <?> "extern"
  , try (DeclBindingType <$> parseBindingType <?> "binding type")
  , DeclBinding <$> binding <?> "binding"
  , DeclType <$> typeDeclaration <?> "type declaration"
  ]

data Declaration
  = DeclBinding !Binding
  | DeclBindingType !(Located IdentName, Type)
  | DeclExtern !Extern
  | DeclType !TypeDeclaration
  deriving (Show, Eq)

declBinding :: Declaration -> Maybe Binding
declBinding (DeclBinding x) = Just x
declBinding _ = Nothing

declBindingType :: Declaration -> Maybe (Located IdentName, Type)
declBindingType (DeclBindingType x) = Just x
declBindingType _ = Nothing

declExtern :: Declaration -> Maybe Extern
declExtern (DeclExtern x) = Just x
declExtern _ = Nothing

declTypeDeclaration :: Declaration -> Maybe TypeDeclaration
declTypeDeclaration (DeclType x) = Just x
declTypeDeclaration _ = Nothing

parseDeclarations :: AmyParser Declaration -> AmyParser ([TypeDeclaration], [Extern], [Binding])
parseDeclarations parser = do
  declarations <- indentedBlock parser
  let
    typeDecls = mapMaybe declTypeDeclaration declarations
    externs = mapMaybe declExtern declarations
    bindings = mapMaybe declBinding declarations
    bindingTypes = mapMaybe declBindingType declarations
    bindingTypeMap = Map.fromList $ (\(Located _ ident', ty) -> (ident', ty)) <$> bindingTypes
    bindings' =
      fmap
      (\b -> b { bindingType = Map.findWithDefault TyUnknown (locatedValue $ bindingName b) bindingTypeMap })
      bindings

    -- TODO: Throw an error if there is a binding type without a binding

    -- TODO: Enforce that binding types must be followed by the binding

  pure (typeDecls, externs, bindings')

externDecl :: AmyParser Extern
externDecl = do
  _ <- extern
  name <- ident
  _ <- doubleColon
  ty <- parseType <?> "type"
  pure
    Extern
    { externName = name
    , externType = ty
    }

binding :: AmyParser Binding
binding = do
  name <- ident
  args <- many $ assertIndented *> ident
  _ <- equals
  body <- expression <?> "expression"
  pure
    Binding
    { bindingName = name
    , bindingType = TyUnknown
    , bindingArgs = args
    , bindingReturnType = TyUnknown
    , bindingBody = body
    }

parseBindingType :: AmyParser (Located IdentName, Type)
parseBindingType = do
  name <- ident
  _ <- doubleColon
  ty <- parseType <?> "binding type"
  pure (name, ty)

parseType :: AmyParser Type
parseType = makeExprParser term table
 where
  table =
    [ [InfixL (pure TyApp)]
    , [InfixR (TyFun <$ rArrow)]
    ]
  term = assertIndented *> typeTerm

typeTerm :: AmyParser Type
typeTerm =
  choice
  [ parens parseType <?> "type parens"
  , tyForall <?> "forall type"
  , TyVar . fromLocated <$> tyVar <?> "type variable"
  , TyCon . fromLocated <$> tyCon <?> "type constructor"
  , uncurry TyRecord <$> tyRecord <?> "record"
  ]

tyForall :: AmyParser Type
tyForall = do
  _ <- forall
  vars <- fmap fromLocated <$> CNE.some (assertIndented *> tyVar)
  _ <- assertIndented *> dot
  ty <- parseType <?> "type"
  pure $ TyForall vars ty

tyRecord :: AmyParser (Map (MaybeLocated RowLabel) Type, Maybe Type)
tyRecord =
  between lBrace rBrace $ do
    fields <- (`sepBy` comma) $ do
      label' <- fromLocated <$> (assertIndented *> L.rowLabel)
      _ <- assertIndented *> doubleColon
      ty <- parseType
      pure (label', ty)
    mTyVar <- optional $ assertIndented *> pipe *> assertIndented *> parseType
    pure (Map.fromList fields, mTyVar)

typeDeclaration :: AmyParser TypeDeclaration
typeDeclaration = do
  tyName <- tyConDefinition
  equals' <- optional equals
  constructors <-
    case equals' of
      Nothing -> pure []
      Just _ -> assertIndented *> (dataConDefinition `sepBy` pipe)
  pure
    TypeDeclaration
    { typeDeclarationTypeName = tyName
    , typeDeclarationConstructors = constructors
    }

tyConDefinition :: AmyParser TyConDefinition
tyConDefinition = do
  name <- tyCon
  args <- many $ assertIndented *> tyVar
  pure
    TyConDefinition
    { tyConDefinitionName = name
    , tyConDefinitionArgs = args
    }

dataConDefinition :: AmyParser DataConDefinition
dataConDefinition = do
  name <- dataCon
  mArg <- optional parseType
  pure
    DataConDefinition
    { dataConDefinitionName = name
    , dataConDefinitionArgument = mArg
    }

expression :: AmyParser Expr
expression = makeExprParser term table
 where
  table =
    [ [Postfix parseSelector]
    , [InfixL (pure EApp)]
    ]
  term = assertIndented *> expressionTerm

expressionTerm :: AmyParser Expr
expressionTerm =
  choice
  [ expressionParens <?> "parens"
  , ELit <$> literal <?> "literal"
  , record <?> "record"
  , EIf <$> ifExpression <?> "if expression"
  , ECase <$> caseExpression <?> "case expression"
  , ELet <$> letExpression' <?> "let expression"
  , ELam <$> lambda <?> "lambda"
  , EVar . Typed TyUnknown <$> ident <?> "identifier"
  , ECon . Typed TyUnknown <$> dataCon <?> "data constructor"
  ]

expressionParens :: AmyParser Expr
expressionParens = EParens <$> parens expression

parseSelector :: AmyParser (Expr -> Expr)
parseSelector = do
  label' <- recordSelector
  pure $ \expr -> ERecordSelect expr label' TyUnknown

literal :: AmyParser (Located Literal)
literal =
  choice
  [ fmap LiteralInt <$> int
  , fmap LiteralDouble <$> double
  , fmap LiteralText <$> text
  ]

record :: AmyParser Expr
record = do
  startSpan <- lBrace
  rows <- fmap Map.fromList $ (`sepBy` comma) $ do
    label' <- assertIndented *> L.rowLabel
    _ <- assertIndented *> colon
    expr <- expression
    pure (label', Typed TyUnknown expr)
  endSpan <- assertIndented *> rBrace
  pure $ ERecord (mergeSpans startSpan endSpan) rows

ifExpression :: AmyParser If
ifExpression = do
  startSpan <- if'
  predicate <- assertIndented *> expression
  _ <- assertIndented *> then'
  thenExpression <- expression
  _ <- assertIndented *> else'
  elseExpression <- expression
  let endSpan = expressionSpan elseExpression
  pure
    If
    { ifPredicate = predicate
    , ifThen = thenExpression
    , ifElse = elseExpression
    , ifSpan = mergeSpans startSpan endSpan
    }

caseExpression :: AmyParser Case
caseExpression = do
  startSpan <- case'
  scrutinee <- expression <?> "case scrutinee expression"
  _ <- assertIndented *> of'
  matches <- indentedBlockNonEmpty (caseMatch <?> "case match")
  let endSpan = matchSpan $ NE.last matches
  pure
    Case
    { caseScrutinee = scrutinee
    , caseAlternatives = matches
    , caseSpan = mergeSpans startSpan endSpan
    }

caseMatch :: AmyParser Match
caseMatch = do
  pat <- parsePattern <?> "pattern"
  _ <- assertIndented *> rArrow
  body <- expression <?> "expression"
  pure
    Match
    { matchPattern = pat
    , matchBody = body
    }

parsePattern :: AmyParser Pattern
parsePattern =
  choice
  [ PLit <$> literal <?> "pattern literal"
  , PVar . Typed TyUnknown <$> ident <?> "pattern variable"
  , PCons <$> patCons <?> "pattern constructor"
  , PParens <$> parens parsePattern <?> "parentheses"
  ]

patCons :: AmyParser PatCons
patCons = do
  constructor <- dataCon
  mArg <- optional $ assertIndented *> parsePattern
  pure
    PatCons
    { patConsConstructor = constructor
    , patConsArg = mArg
    , patConsType = TyUnknown
    }

letExpression' :: AmyParser Let
letExpression' = do
  startSpan <- let'
  let
    parser =
      choice
      [ try (DeclBindingType <$> parseBindingType) <?> "let binding type"
      , DeclBinding <$> binding <?> "let binding"
      ]
  (_, _, bindings) <- parseDeclarations parser
  _ <- in'
  expr <- expression
  pure
    Let
    { letBindings = bindings
    , letExpression = expr
    , letSpan = mergeSpans startSpan (expressionSpan expr)
    }

lambda :: AmyParser Lambda
lambda = do
  startSpan <- backslash
  args <- CNE.some $ assertIndented *> ident
  _ <- rArrow
  body <- expression <?> "lambda body"
  pure
    Lambda
    { lambdaArgs = args
    , lambdaBody = body
    , lambdaSpan = mergeSpans startSpan (expressionSpan body)
    , lambdaType = TyUnknown
    }
