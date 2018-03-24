{-# LANGUAGE NamedFieldPuns #-}

module Rascal.Renamer.Renamer
  ( rename
  ) where

import Control.Monad.Except
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Exts (toList)

import Rascal.Renamer.AST
import Rascal.Renamer.Monad
import Rascal.Parser.AST

-- | Gives a unique identity to all names in the AST
rename :: ParserAST -> Either [RenamerError] RenamerAST
rename = runRenamer emptyRenamerState . rename'

rename' :: ParserAST -> Renamer RenamerAST
rename' (ParserAST declarations) = do
  -- TODO: Try to do each of these steps in such a way that we can get as many
  -- errors as possible. For example, we should be able to validate all
  -- function declarations "in parallel" so if more than one has an error we
  -- can show all the errors. Currently we fail on the first error. Maybe this
  -- should be applicative?

  -- Ensure that every function has a type declaration and vice versa
  functionDeclarationsWithTypes <- either throwError pure $ pairFunctionDeclarations (toList declarations)

  -- Add function declarations to scope and check types
  functionDeclarationsWithIds <- mapM (uncurry addFunctionDeclarationToScope) functionDeclarationsWithTypes

  -- Validate each function expression
  declarations' <- mapM renameDeclaration functionDeclarationsWithIds

  pure $ RenamerAST $ RenamerASTFunction <$> declarations'

-- | Pair function declarations with type declarations.
pairFunctionDeclarations
  :: [ParserASTDeclaration]
  -> Either [RenamerError] (NonEmpty (ParserFunctionDeclaration, NonEmpty Text))
pairFunctionDeclarations declarations =
  let
    -- Get all the functions and type declarations in separate maps by name
    functionNameMap = Map.fromList $ mapMaybe getFuncName declarations
    typeNameMap = Map.fromList $ mapMaybe getTypeName declarations

    -- Combine the two maps
    combined :: Map Text (Either RenamerError (ParserFunctionDeclaration, NonEmpty Text))
    combined =
      Map.merge
      -- Function declaration but no type declaration
      (Map.mapMissing $ \name _ -> Left $ FunctionLacksTypeSignature name)
      -- Types declaration but no function declaration
      (Map.mapMissing $ \name _ -> Left $ TypeSignatureLacksBinding name)
      -- Both a type and function declaration
      (Map.zipWithMatched $ \_ f t -> Right (f, parserFunctionTypeDeclarationTypeNames t))
      functionNameMap
      typeNameMap

    -- Split out any errors
    (errors, pairs) = partitionEithers $ Map.elems combined

  in
    if null errors
    then Right (NE.fromList pairs) -- NE.fronlist shouldn't fail because everything was paired
    else Left errors

 where
  getFuncName :: ParserASTDeclaration -> Maybe (Text, ParserFunctionDeclaration)
  getFuncName (ParserASTFunction func@ParserFunctionDeclaration{parserFunctionDeclarationName}) =
    Just (parserFunctionDeclarationName, func)
  getFuncName _ = Nothing

  getTypeName :: ParserASTDeclaration -> Maybe (Text, ParserFunctionTypeDeclaration)
  getTypeName (ParserASTFunctionType func@ParserFunctionTypeDeclaration{parserFunctionTypeDeclarationName}) =
    Just (parserFunctionTypeDeclarationName, func)
  getTypeName _ = Nothing

-- | Adds the name for a function declaration to the scope
addFunctionDeclarationToScope
  :: ParserFunctionDeclaration
  -> NonEmpty Text
  -> Renamer (IdName, ParserFunctionDeclaration, NonEmpty IdName) -- TODO: Better type than a tuple?
addFunctionDeclarationToScope declaration typeNames = do
  -- Add function name to scope
  idName <- addValueToScope (parserFunctionDeclarationName declaration)

  -- Make sure all the types exist
  typeIds <- ensureTypesExist typeNames

  pure (idName, declaration, typeIds)

ensureTypesExist :: NonEmpty Text -> Renamer (NonEmpty IdName)
ensureTypesExist typeNames = do
  let lookupTypeWithError typeName = maybe (Left $ UnknownType typeName) Right <$> lookupTypeInScope typeName
  eTypeIds <- mapM lookupTypeWithError (toList typeNames)
  case partitionEithers eTypeIds of
    ([], ids) -> pure (NE.fromList ids) -- NE.fromList shouldn't fail since all IDs exist
    (errors, _) -> throwError errors

renameDeclaration
  :: (IdName, ParserFunctionDeclaration, NonEmpty IdName)
  -> Renamer RenamerFunctionDeclaration
renameDeclaration (idName, declaration, typeIds) = withNewScope $ do -- Begin new scope
  -- Check that number of arguments matches types minus 1
  let
    numFuncTypeArgs = NE.length typeIds - 1
    numFuncArgs = length (parserFunctionDeclarationArgs declaration)
  when (numFuncTypeArgs /= numFuncArgs) $
    throwError [FunctionArgumentMismatch (idNameText idName) numFuncTypeArgs numFuncArgs]

  -- Add function arguments to expression
  args <- mapM addValueToScope (parserFunctionDeclarationArgs declaration)

  -- Run renamer on expression
  expression <- renameExpression (parserFunctionDeclarationBody declaration)

  pure
    RenamerFunctionDeclaration
    { renamerFunctionDeclarationName = idName
    , renamerFunctionDeclarationArgs = args
    , renamerFunctionDeclarationTypeNames = typeIds
    , renamerFunctionDeclarationBody = expression
    }

renameExpression :: ParserASTExpression -> Renamer RenamerASTExpression
renameExpression (ParserASTLiteral lit) = pure $ RenamerASTLiteral lit
renameExpression (ParserASTVariable var) =
  lookupValueInScope var >>= maybe (throwError [UnknownVariable var]) (pure . RenamerASTVariable)
renameExpression (ParserASTFunctionApplication app) = do
  let funcName = parserFunctionApplicationFunctionName app
  funcNameId <- maybe (throwError [UnknownVariable funcName]) pure =<< lookupValueInScope funcName
  expressions <- mapM renameExpression (parserFunctionApplicationArgs app)
  pure $
    RenamerASTFunctionApplication
    RenamerFunctionApplication
    { renamerFunctionApplicationFunctionName = funcNameId
    , renamerFunctionApplicationArgs = expressions
    }
renameExpression (ParserASTExpressionParens expr) = RenamerASTExpressionParens <$> renameExpression expr
