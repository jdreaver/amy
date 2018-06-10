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
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Megaparsec
import Text.Megaparsec.Expr

import Amy.Syntax.AST as S
import Amy.Syntax.Lexer as L
import Amy.Syntax.Located
import Amy.Syntax.Monad

parseModule :: AmyParser Module
parseModule = do
  declarations <- noIndent (indentedBlock declaration) <* eof
  fileName <- sourceName <$> getPosition
  pure $ Module fileName declarations

declaration :: AmyParser Declaration
declaration =
  choice
  [ DeclExtern <$> externDecl <?> "extern"
  , try (DeclBindingType <$> bindingType <?> "binding type")
  , DeclBinding <$> binding <?> "binding"
  , DeclType <$> typeDeclaration <?> "type declaration"
  ]

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

bindingType :: AmyParser BindingType
bindingType = do
  name <- ident
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
    [ [InfixL (pure TyApp)]
    , [InfixR (TyFun <$ rArrow)]
    ]
  term = assertIndented *> typeTerm

typeTerm :: AmyParser Type
typeTerm =
  choice
  [ parens parseType <?> "type parens"
  , tyForall <?> "forall type"
  , TyVar <$> tyVar <?> "type variable"
  , TyCon <$> tyCon <?> "type constructor"
  , uncurry TyRecord <$> tyRecord <?> "record"
  ]

tyForall :: AmyParser Type
tyForall = do
  _ <- forall
  vars <- CNE.some (assertIndented *> tyVar)
  _ <- assertIndented *> dot
  ty <- parseType <?> "type"
  pure $ TyForall vars ty

tyRecord :: AmyParser (Map (Located RowLabel) Type, Maybe (Located TyVarName))
tyRecord =
  between lBrace rBrace $ do
    fields <- (`sepBy` comma) $ do
      label' <- assertIndented *> L.rowLabel
      _ <- assertIndented *> doubleColon
      ty <- parseType
      pure (label', ty)
    mTyVar <- optional $ assertIndented *> pipe *> assertIndented *> tyVar
    pure (Map.fromList fields, mTyVar)

binding :: AmyParser Binding
binding = do
  name <- ident
  args <- many $ assertIndented *> ident
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
  , EVar <$> variable <?> "variable"
  ]

expressionParens :: AmyParser Expr
expressionParens = EParens <$> parens expression

parseSelector :: AmyParser (Expr -> Expr)
parseSelector = do
  label' <- recordSelector
  pure $ \expr -> ERecordSelect expr label'

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
    pure (label', expr)
  endSpan <- assertIndented *> rBrace
  pure $ ERecord (mergeSpans startSpan endSpan) rows

variable :: AmyParser Var
variable =
  choice
  [ VVal <$> ident
  , VCons <$> dataCon
  ]

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
  , PVar <$> ident <?> "pattern variable"
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
    }

letExpression' :: AmyParser Let
letExpression' = do
  startSpan <- let'
  let
    parser =
      choice
      [ try (LetBinding <$> binding)
      , LetBindingType <$> bindingType
      ]
  bindings <- indentedBlock parser
  _ <- in'
  expr <- expression
  pure
    Let
    { letBindings = bindings
    , letExpression = expr
    , letSpan = mergeSpans startSpan (expressionSpan expr)
    }
