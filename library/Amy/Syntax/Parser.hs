module Amy.Syntax.Parser
  ( parseModule

  , declaration
  , externDecl
  , bindingType
  , parseScheme
  , parseType
  , binding
  , expression
  , expression'
  , expressionParens
  , caseExpression
  , ifExpression
  , letExpression'
  , literal
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
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

parseSchemeVars :: AmyParser [Located Text]
parseSchemeVars = do
  forall
  vars <- many identifier
  dot
  pure vars

parseType :: AmyParser Type
parseType = makeExprParser term table
 where
  tVar = (TyCon <$> typeIdentifier) <|> (TyVar <$> identifier)
  table = [[InfixR (TyFun <$ typeSeparatorArrow)]]
  term = parens parseType <|> tVar

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
  tyName <- typeIdentifier
  equals' <- optional $ equals <* spaceConsumerNewlines
  constructors <-
    case equals' of
      Nothing -> pure []
      Just _ -> dataConstructor `sepBy` (dataConstructorSep <* spaceConsumerNewlines)
  pure
    TypeDeclaration
    { typeDeclarationTypeName = tyName
    , typeDeclarationConstructors = constructors
    }

dataConstructor :: AmyParser DataConstructor
dataConstructor = do
  dataCon <- dataConstructorName'
  mArg <- optional typeIdentifier
  pure
    DataConstructor
    { dataConstructorName = dataCon
    , dataConstructorArgument = mArg
    }

expression :: AmyParser Expr
expression = do
  -- Parse a NonEmpty list of expressions separated by spaces.
  expressions <- lineFold expression'

  pure $
    case expressions of
      -- Just a simple expression
      expr :| [] -> expr
      -- We must have a function application
      f :| args ->
        EApp
        App
        { appFunction = f
        , appArgs = NE.fromList args
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
  (fmap (either LiteralDouble LiteralInt) <$> number)
  <|> (fmap LiteralBool <$> bool)

variable :: AmyParser Var
variable =
  (VVal <$> identifier)
  <|> (VCons <$> dataConstructorName')

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
  try (PatternLit <$> literal <?> "pattern literal")
  <|> (PatternVar <$> identifier <?> "pattern variable")
  <|> (PatternCons <$> constructorPattern <?> "pattern constructor")

constructorPattern :: AmyParser ConstructorPattern
constructorPattern = do
  constructor <- dataConstructorName'
  mArg <- optional identifier
  pure
    ConstructorPattern
    { constructorPatternConstructor = constructor
    , constructorPatternArg = mArg
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
