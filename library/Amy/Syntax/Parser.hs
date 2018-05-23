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
  , expressionParens
  , caseExpression
  , ifExpression
  , letExpression'
  , literal
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Expr

import Amy.Syntax.AST as S
import Amy.Syntax.Lexer as L
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
parseType = makeExprParser typeTerm table
 where
  table =
    [ [InfixL (TyApp <$ spaceConsumer)]
    , [InfixR (TyFun <$ typeSeparatorArrow)]
    ]

typeTerm :: AmyParser Type
typeTerm =
  (parens parseType <?> "type parens")
  <|> (TyVar <$> tyVarName <?> "type variable")
  <|> (TyCon <$> tyConName <?> "type constructor")
  <|> (uncurry TyRecord <$> tyRecord <?> "record")

tyRecord :: AmyParser (Map (Located RowLabel) Type, Maybe (Located TyVarName))
tyRecord =
  braces $ do
    fields <- (`sepBy` comma) $ do
      label' <- L.rowLabel
      doubleColon
      ty <- parseType
      pure (label', ty)
    mTyVar <- optional $ pipe *> tyVarName
    pure (Map.fromList fields, mTyVar)

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
      Just _ -> dataConDefinition `sepBy` (pipe <* spaceConsumerNewlines)
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
expression = makeExprParser term table
 where
  table =
    [ [InfixL (EApp <$ spaceConsumerNewlines)]
    ]
  term = assertIndented *> expressionTerm <* spaceConsumerNewlines

expressionTerm :: AmyParser Expr
expressionTerm = do
  expr <- expressionTerm'
  try (parseSelector expr <?> "record selector") <|> pure expr

expressionTerm' :: AmyParser Expr
expressionTerm' =
  (expressionParens <?> "parens")
  <|> (ELit <$> literal <?> "literal")
  <|> (ERecord <$> record <?> "record")
  <|> (EIf <$> ifExpression <?> "if expression")
  <|> (ECase <$> caseExpression <?> "case expression")
  <|> (ELet <$> letExpression' <?> "let expression")
  <|> (EVar <$> variable <?> "variable")

expressionParens :: AmyParser Expr
expressionParens = EParens <$> parens expression

parseSelector :: Expr -> AmyParser Expr
parseSelector expr = try $ ERecordSelect expr <$> (C.char '.' >> L.rowLabel)

literal :: AmyParser (Located Literal)
literal =
  fmap (either LiteralDouble LiteralInt) <$> number

record :: AmyParser (Map (Located RowLabel) Expr)
record =
  fmap Map.fromList $ braces $ (`sepBy` comma) $ do
    label' <- L.rowLabel
    equals
    expr <- expression
    pure (label', expr)

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
