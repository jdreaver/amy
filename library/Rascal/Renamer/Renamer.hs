{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Rascal.Renamer.Renamer
  ( rename
  ) where

import Control.Monad (void)
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
rename ast = runRenamer emptyRenamerState $ setupDefaultEnvironment >> rename' ast

-- | Add known type definitions
setupDefaultEnvironment :: Renamer ()
setupDefaultEnvironment = do
  void $ addTypeToScope "Int"
  void $ addTypeToScope "Double"

rename' :: ParserAST -> Renamer RenamerAST
rename' (ParserAST declarations) = do
  -- TODO: Try to do each of these steps in such a way that we can get as many
  -- errors as possible. For example, we should be able to validate all binding
  -- declarations "in parallel" so if more than one has an error we can show
  -- all the errors. Currently we fail on the first error. Maybe this should be
  -- applicative?

  -- Ensure that every binding has a type declaration and vice versa
  bindingDeclarationsWithTypes <- either throwError pure $ pairBindingDeclarations (toList declarations)

  -- Add binding declarations to scope and check types
  bindingDeclarationsWithIds <- mapM (uncurry addBindingDeclarationToScope) bindingDeclarationsWithTypes

  -- Validate each binding expression
  declarations' <- mapM renameDeclaration bindingDeclarationsWithIds

  pure $ RenamerAST $ RenamerASTBinding <$> declarations'

-- | Pair binding declarations with type declarations.
pairBindingDeclarations
  :: [ParserASTDeclaration]
  -> Either [RenamerError] (NonEmpty (ParserBindingDeclaration, NonEmpty Text))
pairBindingDeclarations declarations =
  let
    -- Get all the bindings and type declarations in separate maps by name
    bindingNameMap = Map.fromList $ mapMaybe getFuncName declarations
    typeNameMap = Map.fromList $ mapMaybe getTypeName declarations

    -- Combine the two maps
    combined :: Map Text (Either RenamerError (ParserBindingDeclaration, NonEmpty Text))
    combined =
      Map.merge
      -- Binding declaration but no type declaration
      (Map.mapMissing $ \name _ -> Left $ BindingLacksTypeSignature name)
      -- Types declaration but no binding declaration
      (Map.mapMissing $ \name _ -> Left $ TypeSignatureLacksBinding name)
      -- Both a type and binding declaration
      (Map.zipWithMatched $ \_ f t -> Right (f, parserBindingTypeDeclarationTypeNames t))
      bindingNameMap
      typeNameMap

    -- Split out any errors
    (errors, pairs) = partitionEithers $ Map.elems combined

  in
    if null errors
    then Right (NE.fromList pairs) -- NE.fronlist shouldn't fail because everything was paired
    else Left errors

 where
  getFuncName :: ParserASTDeclaration -> Maybe (Text, ParserBindingDeclaration)
  getFuncName (ParserASTBinding func@ParserBindingDeclaration{parserBindingDeclarationName}) =
    Just (parserBindingDeclarationName, func)
  getFuncName _ = Nothing

  getTypeName :: ParserASTDeclaration -> Maybe (Text, ParserBindingTypeDeclaration)
  getTypeName (ParserASTBindingType func@ParserBindingTypeDeclaration{parserBindingTypeDeclarationName}) =
    Just (parserBindingTypeDeclarationName, func)
  getTypeName _ = Nothing

-- | Adds the name for a binding declaration to the scope
addBindingDeclarationToScope
  :: ParserBindingDeclaration
  -> NonEmpty Text
  -> Renamer (IdName, ParserBindingDeclaration, NonEmpty IdName) -- TODO: Better type than a tuple?
addBindingDeclarationToScope declaration typeNames = do
  -- Add binding name to scope
  idName <- addValueToScope TopLevelDefinition (parserBindingDeclarationName declaration)

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
  :: (IdName, ParserBindingDeclaration, NonEmpty IdName)
  -> Renamer RenamerBindingDeclaration
renameDeclaration (idName, declaration, typeIds) = withNewScope $ do -- Begin new scope
  -- Check that number of arguments matches types minus 1
  let
    numFuncTypeArgs = NE.length typeIds - 1
    numFuncArgs = length (parserBindingDeclarationArgs declaration)
  when (numFuncTypeArgs /= numFuncArgs) $
    throwError [FunctionArgumentMismatch (idNameText idName) numFuncTypeArgs numFuncArgs]

  -- Add binding arguments to expression
  args <- mapM (addValueToScope LocalDefinition) (parserBindingDeclarationArgs declaration)

  -- Run renamer on expression
  expression <- renameExpression (parserBindingDeclarationBody declaration)

  pure
    RenamerBindingDeclaration
    { renamerBindingDeclarationName = idName
    , renamerBindingDeclarationArgs = args
    , renamerBindingDeclarationTypeNames = typeIds
    , renamerBindingDeclarationBody = expression
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
