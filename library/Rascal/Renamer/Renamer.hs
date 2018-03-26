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

import Rascal.AST
import Rascal.Renamer.AST
import Rascal.Renamer.Monad

-- | Gives a unique identity to all names in the AST
rename :: AST Text -> Either [RenamerError] RenamerAST
rename ast = runRenamer emptyRenamerState $ setupDefaultEnvironment >> rename' ast

-- | Add known type definitions
setupDefaultEnvironment :: Renamer ()
setupDefaultEnvironment = do
  void $ addTypeToScope "Int"
  void $ addTypeToScope "Double"

rename' :: AST Text -> Renamer RenamerAST
rename' (AST declarations) = do
  -- TODO: Try to do each of these steps in such a way that we can get as many
  -- errors as possible. For example, we should be able to validate all binding
  -- declarations "in parallel" so if more than one has an error we can show
  -- all the errors. Currently we fail on the first error. Maybe this should be
  -- applicative?

  -- Handle extern declarations
  let
    getExtern (TopLevelExternType dec) = Just dec
    getExtern _ = Nothing
  externDeclarations <-
    fmap (fmap RenamerASTExtern) $
    mapM renameExternDeclaration $
    mapMaybe getExtern (toList declarations)

  -- Ensure that every binding has a type declaration and vice versa
  bindingDeclarationsWithTypes <- either throwError pure $ pairBindingDeclarations (toList declarations)

  -- Add binding declarations to scope and check types
  bindingDeclarationsWithIds <- mapM (uncurry addBindingDeclarationToScope) bindingDeclarationsWithTypes

  -- Validate each binding expression
  bindingDeclarations <- fmap RenamerASTBinding <$> mapM renameBindingDeclaration bindingDeclarationsWithIds

  pure $ RenamerAST $ externDeclarations ++ bindingDeclarations

-- | Pair binding declarations with type declarations.
pairBindingDeclarations
  :: [TopLevel Text]
  -> Either [RenamerError] [(BindingValue Text, NonEmpty Text)]
pairBindingDeclarations declarations =
  let
    -- Get all the bindings and type declarations in separate maps by name
    bindingNameMap = Map.fromList $ mapMaybe getFuncName declarations
    typeNameMap = Map.fromList $ mapMaybe getTypeName declarations

    -- Combine the two maps
    combined :: Map Text (Either RenamerError (BindingValue Text, NonEmpty Text))
    combined =
      Map.merge
      -- Binding declaration but no type declaration
      (Map.mapMissing $ \name _ -> Left $ BindingLacksTypeSignature name)
      -- Types declaration but no binding declaration
      (Map.mapMissing $ \name _ -> Left $ TypeSignatureLacksBinding name)
      -- Both a type and binding declaration
      (Map.zipWithMatched $ \_ f t -> Right (f, bindingTypeType t))
      bindingNameMap
      typeNameMap

    -- Split out any errors
    (errors, pairs) = partitionEithers $ Map.elems combined

  in
    if null errors
    then Right pairs
    else Left errors

 where
  getFuncName :: TopLevel a -> Maybe (a, BindingValue a)
  getFuncName (TopLevelBindingValue bind@BindingValue{bindingValueName}) = Just (bindingValueName, bind)
  getFuncName _ = Nothing

  getTypeName :: TopLevel a -> Maybe (a, BindingType a)
  getTypeName (TopLevelBindingType bind@BindingType{bindingTypeName}) = Just (bindingTypeName, bind)
  getTypeName _ = Nothing

-- | Adds the name for a binding declaration to the scope
addBindingDeclarationToScope
  :: BindingValue Text
  -> NonEmpty Text
  -> Renamer (IdName, BindingValue Text, NonEmpty IdName) -- TODO: Better type than a tuple?
addBindingDeclarationToScope declaration typeNames = do
  -- Add binding name to scope
  idName <- addValueToScope TopLevelDefinition (bindingValueName declaration)

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

renameExternDeclaration
  :: BindingType Text
  -> Renamer RenamerExternDeclaration
renameExternDeclaration declaration = do
  -- Add extern name to scope
  idName <- addValueToScope TopLevelDefinition $ bindingTypeName declaration

  -- Add types to scope
  typeIds <- ensureTypesExist $ bindingTypeType declaration

  pure
    RenamerExternDeclaration
    { renamerExternDeclarationName = idName
    , renamerExternDeclarationTypeNames = typeIds
    }

renameBindingDeclaration
  :: (IdName, BindingValue Text, NonEmpty IdName)
  -> Renamer RenamerBindingDeclaration
renameBindingDeclaration (idName, declaration, typeIds) = withNewScope $ do -- Begin new scope
  -- Check that number of arguments matches types minus 1
  let
    numFuncTypeArgs = NE.length typeIds - 1
    numFuncArgs = length (bindingValueArgs declaration)
  when (numFuncTypeArgs /= numFuncArgs) $
    throwError [FunctionArgumentMismatch (idNameText idName) numFuncTypeArgs numFuncArgs]

  -- Add binding arguments to scope
  args <- mapM (addValueToScope LocalDefinition) (bindingValueArgs declaration)

  -- Run renamer on expression
  expression <- renameExpression (bindingValueBody declaration)

  pure
    RenamerBindingDeclaration
    { renamerBindingDeclarationName = idName
    , renamerBindingDeclarationArgs = args
    , renamerBindingDeclarationTypeNames = typeIds
    , renamerBindingDeclarationBody = expression
    }

renameExpression :: Expression Text -> Renamer RenamerASTExpression
renameExpression (ExpressionLiteral lit) = pure $ RenamerASTLiteral lit
renameExpression (ExpressionVariable var) =
  lookupValueInScope var >>= maybe (throwError [UnknownVariable var]) (pure . RenamerASTVariable)
renameExpression (ExpressionFunctionApplication app) = do
  let funcName = functionApplicationFunctionName app
  funcNameId <- maybe (throwError [UnknownVariable funcName]) pure =<< lookupValueInScope funcName
  expressions <- mapM renameExpression (functionApplicationArgs app)
  pure $
    RenamerASTFunctionApplication
    RenamerFunctionApplication
    { renamerFunctionApplicationFunctionName = funcNameId
    , renamerFunctionApplicationArgs = expressions
    }
renameExpression (ExpressionParens expr) = RenamerASTExpressionParens <$> renameExpression expr
