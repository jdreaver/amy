{-# LANGUAGE NamedFieldPuns #-}

module Amy.Renamer.Renamer
  ( rename
  ) where

import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Validation

import Amy.Errors
import Amy.Renamer.AST
import Amy.Renamer.Monad
import Amy.Prim
import Amy.Syntax.AST

-- | Gives a unique identity to all names in the AST
rename :: Module -> Either [Error] RModule
rename ast = toEither . runRenamer emptyRenamerState $ rename' ast

rename' :: Module -> Renamer (Validation [Error] RModule)
rename' (Module declarations) = do
  -- Rename extern declarations
  rModuleExterns <- traverse renameExtern (mapMaybe declExtern declarations)

  -- Rename binding value declarations
  let
    bindings = mapMaybe declBinding declarations
    bindingTypeMap = bindingTypesMap $ mapMaybe declBindingType declarations
  traverse_ (addValueToScope True) (bindingName <$> bindings)
  rModuleBindings <- traverse (renameBinding bindingTypeMap) bindings

  pure
    $ RModule
    <$> sequenceA rModuleBindings
    <*> sequenceA rModuleExterns

bindingTypesMap :: [BindingType] -> Map Text Scheme
bindingTypesMap = Map.fromList . fmap (\(BindingType (Located _ name) ts) -> (name, ts))

renameExtern :: Extern -> Renamer (Validation [Error] RExtern)
renameExtern extern = do
  rExternName <- addValueToScope True (externName extern)
  rExternType <- renameType (externType extern)
  pure $
    RExtern
      <$> rExternName
      <*> rExternType

-- renameType :: Located Text -> Validation [Error] (Located PrimitiveType)
-- renameType name =
--   maybe (Failure [UnknownTypeName name]) Success $ traverse readPrimitiveType name

renameScheme :: Scheme -> Renamer (Validation [Error] RScheme)
renameScheme (Forall vars ty) = do
  -- TODO: Actually add these type variables into some scope properly. They can
  -- be used in the current type definition and any other type definitions
  -- (like for let bindings) inside the binding body.
  let vars' = (\(Located span' name) -> RTypeName name span' (-1) Nothing) <$> vars
  ty' <- renameType ty
  pure $ RForall vars' <$> ty'

renameType :: Type -> Renamer (Validation [Error] RType)
renameType (TyCon name@(Located span' name')) =
  let primName = readPrimitiveTyCon name
  in traverse (\prim -> pure $ RTyCon $ RTypeName name' span' (primitiveTypeId prim) (Just prim)) primName
renameType (TyVar name) =
  -- TODO: Actually look up TyVar names
  pure $ Failure [UnknownTypeName name]
renameType (TyFun ty1 ty2) = do
  ty1' <- renameType ty1
  ty2' <- renameType ty2
  pure $
    RTyFun
    <$> ty1'
    <*> ty2'

readPrimitiveTyCon :: Located Text -> Validation [Error] PrimitiveType
readPrimitiveTyCon name@(Located _ name') = maybe (Failure [UnknownTypeName name]) Success $ readPrimitiveType name'

renameBinding :: Map Text Scheme -> Binding -> Renamer (Validation [Error] RBinding)
renameBinding typeMap binding = withNewScope $ do -- Begin new scope
  name <- lookupValueInScopeOrError (bindingName binding)
  let
    rBindingName =
      case name of
        (Success (Located l ident)) -> pure (Located l ident)
        Failure f -> Failure f
  rBindingType <- traverse renameScheme $ Map.lookup (locatedValue $ bindingName binding) typeMap
  rBindingArgs <- traverse (addValueToScope False) (bindingArgs binding)
  rBindingBody <- renameExpression (bindingBody binding)
  pure $
    RBinding
    <$> rBindingName
    <*> sequenceA rBindingType
    <*> sequenceA rBindingArgs
    <*> rBindingBody

renameExpression :: Expr -> Renamer (Validation [Error] RExpr)
renameExpression (ELit lit) = pure $ Success $ RELit lit
renameExpression (EVar var) = fmap REVar <$> lookupValueInScopeOrError var
renameExpression (EIf (If predicate thenExpression elseExpression)) = do
  rIfPredicate <- renameExpression predicate
  rIfThen <- renameExpression thenExpression
  rIfElse <- renameExpression elseExpression
  pure
    $ fmap REIf
    $ RIf
    <$> rIfPredicate
    <*> rIfThen
    <*> rIfElse
renameExpression (ELet (Let bindings expression)) =
  withNewScope $ do
    let
      bindings' = mapMaybe letBinding bindings
      bindingTypeMap = bindingTypesMap $ mapMaybe letBindingType bindings
    traverse_ (addValueToScope False) (bindingName <$> bindings')
    rLetBindings <- traverse (renameBinding bindingTypeMap) bindings'
    rLetExpression <- renameExpression expression
    pure
      $ fmap RELet
      $ RLet
      <$> sequenceA rLetBindings
      <*> rLetExpression
renameExpression (EApp app) = do
  rAppFunction <- renameExpression (appFunction app)
  rAppArgs <- traverse renameExpression (appArgs app)
  pure
    $ fmap REApp
    $ RApp
    <$> rAppFunction
    <*> sequenceA rAppArgs
renameExpression (EParens expr) = fmap REParens <$> renameExpression expr
