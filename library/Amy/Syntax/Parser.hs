{-# LANGUAGE FlexibleContexts #-}

module Amy.Syntax.Parser
  ( parseModule

  , declaration
  , externDecl
  , bindingType
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
  _ <- extern
  name <- identifier
  _ <- doubleColon
  ty <- parseType <?> "type"
  pure
    Extern
    { externName = name
    , externType = ty
    }

bindingType :: AmyParser BindingType
bindingType = do
  name <- identifier
  _ <- doubleColon
  ty <- parseType <?> "binding type"
  pure
    BindingType
    { bindingTypeName = name
    , bindingTypeType = ty
    }

parseType :: AmyParser Type
parseType = makeExprParser term table
 where
  table =
    [ [InfixL (TyApp <$ spaceConsumerNewlines)]
    , [InfixR (TyFun <$ typeSeparatorArrow)]
    ]
  term = assertIndented *> typeTerm <* spaceConsumerNewlines

typeTerm :: AmyParser Type
typeTerm =
  (parens parseType <?> "type parens")
  <|> (tyForall <?> "forall type")
  <|> (TyVar <$> tyVarName <?> "type variable")
  <|> (TyCon <$> tyConName <?> "type constructor")
  <|> (uncurry TyRecord <$> tyRecord <?> "record")

tyForall :: AmyParser Type
tyForall = do
  _ <- forall
  vars <- CNE.some tyVarName
  _ <- dot
  ty <- parseType <?> "type"
  pure $ TyForall vars ty

tyRecord :: AmyParser (Map (Located RowLabel) Type, Maybe (Located TyVarName))
tyRecord =
  braces $ do
    fields <- (`sepBy` comma) $ do
      label' <- L.rowLabel
      _ <- doubleColon
      ty <- parseType
      pure (label', ty)
    mTyVar <- optional $ pipe *> tyVarName
    pure (Map.fromList fields, mTyVar)

binding :: AmyParser Binding
binding = do
  name <- identifier
  args <- many (identifier <* spaceConsumerNewlines)
  _ <- equals
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
  equals' <- optional equals
  constructors <-
    case equals' of
      Nothing -> pure []
      Just _ -> dataConDefinition `sepBy` pipe
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
  mArg <- optional parseType
  pure
    DataConDefinition
    { dataConDefinitionName = dataCon
    , dataConDefinitionArgument = mArg
    }

expression :: AmyParser Expr
expression = makeExprParser term table
 where
  table =
    [ [Postfix (parseSelector <* spaceConsumerNewlines)]
    , [InfixL (EApp <$ spaceConsumerNewlines)]
    ]
  term = assertIndented *> expressionTerm <* spaceConsumerNewlines

expressionTerm :: AmyParser Expr
expressionTerm =
  (expressionParens <?> "parens")
  <|> (ELit <$> literal <?> "literal")
  <|> (ERecord <$> record <?> "record")
  <|> (EIf <$> ifExpression <?> "if expression")
  <|> (ECase <$> caseExpression <?> "case expression")
  <|> (ELet <$> letExpression' <?> "let expression")
  <|> (EVar <$> variable <?> "variable")

expressionParens :: AmyParser Expr
expressionParens = EParens <$> parens expression

parseSelector :: AmyParser (Expr -> Expr)
parseSelector = do
  label' <- C.char '.' >> L.rowLabel
  pure $ \expr -> ERecordSelect expr label'

literal :: AmyParser (Located Literal)
literal =
  fmap (either LiteralDouble LiteralInt) <$> number

record :: AmyParser (Map (Located RowLabel) Expr)
record =
  fmap Map.fromList $ braces $ (`sepBy` comma) $ do
    label' <- L.rowLabel
    _ <- colon
    expr <- expression
    pure (label', expr)

variable :: AmyParser Var
variable =
  (VVal <$> identifier)
  <|> (VCons <$> dataConName)

ifExpression :: AmyParser If
ifExpression = do
  _ <- if'
  predicate <- expression
  _ <- then'
  thenExpression <- expression
  _ <- else'
  elseExpression <- expression
  pure
    If
    { ifPredicate = predicate
    , ifThen = thenExpression
    , ifElse = elseExpression
    }

caseExpression :: AmyParser Case
caseExpression = do
  _ <- case'
  scrutinee <- expression <?> "case scrutinee expression"
  _ <- of'
  matches <- indentedBlockNonEmpty (caseMatch <?> "case match")
  pure
    Case
    { caseScrutinee = scrutinee
    , caseAlternatives = matches
    }

caseMatch :: AmyParser Match
caseMatch = do
  pat <- parsePattern <?> "pattern"
  _ <- rightArrow
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
  _ <- let'
  let
    parser =
      try (LetBinding <$> binding)
      <|> (LetBindingType <$> bindingType)
  bindings <- indentedBlock parser
  _ <- in'
  expr <- expression
  pure
    Let
    { letBindings = bindings
    , letExpression = expr
    }
