{-# LANGUAGE FlexibleContexts #-}

module Amy.Syntax.Parser
  ( parseModule

  , declaration
  , externDecl
  , bindingType
  , parseScheme
  , parseType
  , typeTerm
  , binding
  , expression
  , expression'
  , expressionParens
  , caseExpression
  , ifExpression
  , letExpression'
  , literal
  ) where

import qualified Control.Applicative.Combinators.NonEmpty as CNE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Expr

import Amy.Syntax.AST
import Amy.Syntax.Lexer
import Amy.Syntax.Monad

parseModule :: AmyParser Module
parseModule = Module <$> do
  spaceConsumerNewlines
  noIndent (indentedBlock declaration) <* eof

declaration :: AmyParser Declaration
declaration =
  (DeclExtern <$> externDecl <?> "extern")
  <|> try (DeclBindingType <$> bindingType <?> "binding type")
  <|> (DeclBinding <$> binding <?> "binding")
  <|> (DeclType <$> typeDeclaration <?> "type declaration")

externDecl :: AmyParser Extern
externDecl = do
  extern
  name <- identifier
  doubleColon
  ty <- parseType <?> "type"
  pure
    Extern
    { externName = name
    , externType = ty
    }

bindingType :: AmyParser BindingType
bindingType = do
  name <- identifier
  doubleColon
  scheme <- parseScheme <?> "type scheme"
  pure
    BindingType
    { bindingTypeName = name
    , bindingTypeScheme = scheme
    }

parseScheme :: AmyParser Scheme
parseScheme = do
  vars <- optional parseSchemeVars
  ty <- parseType <?> "type"
  pure $ Forall (fromMaybe [] vars) ty

parseSchemeVars :: AmyParser [Located TyVarName]
parseSchemeVars = do
  forall
  vars <- many tyVarName
  dot
  pure vars

parseType :: AmyParser Type
parseType = makeExprParser term table
 where
  table = [[InfixR (TyFun <$ typeSeparatorArrow)]]
  term = parens parseType <|> (TyTerm <$> typeTerm)

typeTerm :: AmyParser TypeTerm
typeTerm = do
  con :| rest <- CNE.sepBy1 typeTerm' spaceConsumer
  process con rest
 where
  mkError = fancyFailure . Set.singleton . ErrorFail
  process (TyCon (TyConInfo con _ span')) args = pure $ TyCon $ TyConInfo con args span'
  process (TyVar var) [] = pure $ TyVar var
  process t@(TyVar _) args = mkError $ "type variable with arguments encountered (no higher-kinded types allowed): " ++ show (t, args)
  process (TyParens t) [] = pure $ TyParens t
  process t@(TyParens _) args = mkError $ "type parens with arguments encountered (no higher-kinded types allowed): " ++ show (t, args)

typeTerm' :: AmyParser TypeTerm
typeTerm' =
  (TyParens <$> parens typeTerm <?> "type parens")
  <|> (TyVar <$> tyVarName <?> "type variable")
  <|> (TyCon <$> tyConInfoNoArgs <?> "type constructor")

tyConInfoNoArgs :: AmyParser TyConInfo
tyConInfoNoArgs = do
  Located span' name <- tyConName
  pure
    TyConInfo
    { tyConInfoName = name
    , tyConInfoArgs = []
    , tyConInfoLocation = span'
    }

binding :: AmyParser Binding
binding = do
  name <- identifier <* spaceConsumerNewlines
  args <- many (identifier <* spaceConsumerNewlines)
  equals <* spaceConsumerNewlines
  body <- expression <?> "expression"
  pure
    Binding
    { bindingName = name
    , bindingArgs = args
    , bindingBody = body
    }

typeDeclaration :: AmyParser TypeDeclaration
typeDeclaration = do
  tyName <- tyConDefinition
  equals' <- optional $ equals <* spaceConsumerNewlines
  constructors <-
    case equals' of
      Nothing -> pure []
      Just _ -> dataConDefinition `sepBy` (dataConstructorSep <* spaceConsumerNewlines)
  pure
    TypeDeclaration
    { typeDeclarationTypeName = tyName
    , typeDeclarationConstructors = constructors
    }

tyConDefinition :: AmyParser TyConDefinition
tyConDefinition = do
  Located span' name <- tyConName
  args <- many tyVarName
  pure
    TyConDefinition
    { tyConDefinitionName = name
    , tyConDefinitionArgs = args
    , tyConDefinitionLocation = span'
    }

dataConDefinition :: AmyParser DataConDefinition
dataConDefinition = do
  dataCon <- dataConName
  mArg <- optional typeTerm
  pure
    DataConDefinition
    { dataConDefinitionName = dataCon
    , dataConDefinitionArgument = mArg
    }

expression :: AmyParser Expr
expression = do
  -- Parse a NonEmpty list of expressions separated by spaces.
  f :| args <- lineFold expression'

  pure $
    case NE.nonEmpty args of
      -- Just a simple expression
      Nothing -> f
      -- We must have a function application
      Just args' ->
        EApp
        App
        { appFunction = f
        , appArgs = args'
        }

-- | Parses any expression except function application. This is needed to avoid
-- left recursion. Without this distinction, f a b would be parsed as f (a b)
-- instead of (f a) b.
expression' :: AmyParser Expr
expression' =
  (expressionParens <?> "parens")
  <|> (ELit <$> literal <?> "literal")
  <|> (EIf <$> ifExpression <?> "if expression")
  <|> (ECase <$> caseExpression <?> "case expression")
  <|> (ELet <$> letExpression' <?> "let expression")
  <|> (EVar <$> variable <?> "variable")

expressionParens :: AmyParser Expr
expressionParens = EParens <$> parens expression

literal :: AmyParser (Located Literal)
literal =
  fmap (either LiteralDouble LiteralInt) <$> number

variable :: AmyParser Var
variable =
  (VVal <$> identifier)
  <|> (VCons <$> dataConName)

ifExpression :: AmyParser If
ifExpression = do
  if'
  predicate <- expression
  then'
  thenExpression <- expression
  else'
  elseExpression <- expression
  pure
    If
    { ifPredicate = predicate
    , ifThen = thenExpression
    , ifElse = elseExpression
    }

caseExpression :: AmyParser Case
caseExpression = do
  case' <* spaceConsumerNewlines
  scrutinee <- expression <?> "case scrutinee expression"
  of' <* spaceConsumerNewlines
  matches <- indentedBlockNonEmpty (caseMatch <?> "case match")
  pure
    Case
    { caseScrutinee = scrutinee
    , caseAlternatives = matches
    }

caseMatch :: AmyParser Match
caseMatch = do
  pat <- parsePattern <?> "pattern"
  rightArrow
  body <- expression <?> "expression"
  pure
    Match
    { matchPattern = pat
    , matchBody = body
    }

parsePattern :: AmyParser Pattern
parsePattern =
  try (PLit <$> literal <?> "pattern literal")
  <|> (PVar <$> identifier <?> "pattern variable")
  <|> (PCons <$> patCons <?> "pattern constructor")
  <|> (PParens <$> parens parsePattern <?> "parentheses")

patCons :: AmyParser PatCons
patCons = do
  constructor <- dataConName
  mArg <- optional parsePattern
  pure
    PatCons
    { patConsConstructor = constructor
    , patConsArg = mArg
    }

letExpression' :: AmyParser Let
letExpression' = do
  let' <* spaceConsumerNewlines
  let
    parser =
      try (LetBinding <$> binding)
      <|> (LetBindingType <$> bindingType)
  bindings <- indentedBlock parser
  in' <* spaceConsumerNewlines
  expr <- expression
  pure
    Let
    { letBindings = bindings
    , letExpression = expr
    }
