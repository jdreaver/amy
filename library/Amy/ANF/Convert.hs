{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Convert
  ( normalizeModule
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)

import Amy.ANF.AST as ANF
import Amy.ANF.Monad
import Amy.Core.AST as C
import Amy.Prim

normalizeModule :: C.Module -> ANF.Module
normalizeModule (C.Module bindingGroups externs typeDeclarations) =
  let
    bindings = concatMap NE.toList bindingGroups

    -- Record top-level names
    topLevelNames =
      (C.bindingName <$> bindings)
      ++ (C.externName <$> externs)

    -- Actual conversion
    convertRead = anfConvertRead topLevelNames typeDeclarations
  in runANFConvert convertRead $ do
    typeDeclarations' <- traverse convertTypeDeclaration typeDeclarations
    externs' <- traverse convertExtern externs
    bindings' <- traverse (normalizeBinding (Just "res")) bindings
    textPointers <- getTextPointers
    pure $ ANF.Module bindings' externs' typeDeclarations' textPointers

convertExtern :: C.Extern -> ANFConvert ANF.Extern
convertExtern (C.Extern name ty) = ANF.Extern name <$> convertType ty

convertTypeDeclaration :: C.TypeDeclaration -> ANFConvert ANF.TypeDeclaration
convertTypeDeclaration (C.TypeDeclaration tyConDef con) = do
  ty <- getTyConDefinitionType tyConDef
  con' <- traverse convertDataConDefinition con
  pure $ ANF.TypeDeclaration (C.tyConDefinitionName tyConDef) ty con'

convertDataConDefinition :: C.DataConDefinition -> ANFConvert ANF.DataConDefinition
convertDataConDefinition (C.DataConDefinition conName mTyArg) = do
  mTyArg' <- traverse convertType mTyArg
  pure
    ANF.DataConDefinition
    { ANF.dataConDefinitionName = conName
    , ANF.dataConDefinitionArgument = mTyArg'
    }

convertDataCon :: DataConName -> ANFConvert ANF.DataCon
convertDataCon con = do
  (ty, index) <- getDataConInfo con
  pure
    ANF.DataCon
    { ANF.dataConName = con
    , ANF.dataConType = ty
    , ANF.dataConIndex = index
    }

convertType :: C.Type -> ANFConvert ANF.Type
convertType ty = go (typeToNonEmpty ty)
 where
  go :: NonEmpty C.Type -> ANFConvert ANF.Type
  go (ty' :| []) =
    case ty' of
      C.TyCon con -> getTyConType con
      C.TyVar _ -> pure OpaquePointerType
      app@C.TyApp{} ->
        case unfoldTyApp app of
          TyCon con :| _ -> getTyConType con
          _ -> error $ "Can't convert non-TyCon TyApp yet " ++ show ty'
      -- N.B. ANF/LLVM doesn't care about polymorphic records
      C.TyRecord rows _ -> mkRecordType rows
      C.TyFun{} -> mkFunctionType ty
      C.TyForall _ ty'' -> convertType ty''
  go _ = mkFunctionType ty

mkRecordType :: Map RowLabel C.Type -> ANFConvert ANF.Type
mkRecordType rows = do
  rows' <- for (Map.toAscList rows) $ \(label, ty) -> do
    ty' <- convertType ty
    pure (label, ty')
  pure $ RecordType rows'

mkFunctionType :: C.Type -> ANFConvert ANF.Type
mkFunctionType ty = do
  args <- traverse convertType (NE.init ts)
  returnType <- convertType $ NE.last ts
  pure $ FuncType args returnType
 where
  ts = typeToNonEmpty ty

typeToNonEmpty :: C.Type -> NonEmpty C.Type
typeToNonEmpty (t1 `C.TyFun` t2) = NE.cons t1 (typeToNonEmpty t2)
typeToNonEmpty ty = ty :| []

convertTypedIdent :: C.Typed IdentName -> ANFConvert (ANF.Typed IdentName)
convertTypedIdent (C.Typed ty arg) = do
  ty' <- convertType ty
  pure $ ANF.Typed ty' arg

normalizeExpr
  :: Text -- ^ Base name for generated variables
  -> C.Expr -- ^ Expression to normalize
  -> ANFConvert ANF.Expr
normalizeExpr _ (C.ELit lit) = ANF.EVal . ANF.Lit <$> normalizeLiteral lit
normalizeExpr _ (C.ERecord rows) =
  normalizeRows (Map.toList rows) $ \rows' ->
    pure $ ANF.ERecord $ Map.fromList rows'
normalizeExpr name (C.ERecordSelect expr label ty) =
  normalizeName name expr $ \val ->
    ANF.ERecordSelect val label <$> convertType ty
normalizeExpr name var@C.EVar{} = normalizeName name var (pure . ANF.EVal)
normalizeExpr name expr@(C.ECase (C.Case scrutinee bind matches defaultExpr)) =
  normalizeName name scrutinee $ \scrutineeVal -> do
    bind' <- convertTypedIdent bind
    matches' <- traverse (normalizeMatch name) matches
    defaultExpr' <- traverse (normalizeExpr name) defaultExpr
    ty <- convertType $ expressionType expr
    pure $ ANF.ECase (ANF.Case scrutineeVal bind' matches' defaultExpr' ty)
normalizeExpr name (C.ELet (C.Let bindingGroups expr)) = do
  let bindings = concatMap NE.toList bindingGroups
  bindings' <- traverse normalizeLetBinding bindings
  expr' <- normalizeExpr name expr
  pure $ ANF.ELetVal $ collapseLetVals $ ANF.LetVal bindings' expr'
normalizeExpr name (C.EApp app@(C.App _ _ retTy)) = do
  -- TODO: More robust arity checking besides just unfolding App nodes.
  let func :| args = C.unfoldApp app
  normalizeList (normalizeName name) (toList args) $ \argVals -> do
    retTy' <- convertType retTy
    case func of
      C.EVar (C.VCons (C.Typed _ con)) -> do
        con' <- convertDataCon con
        let
          mArg =
            case argVals of
              [] -> Nothing
              [x] -> Just x
              _ -> error $ "Tried to create ConApp node, but incorrect number of args " ++ show func
        case retTy' of
          TaggedUnionType structName intBits -> pure $ ANF.EConApp $ ANF.ConApp con' mArg structName intBits
          _ -> error $ "Invalid type for ConApp " ++ show retTy'
      _ ->
        normalizeName name func $ \funcVal ->
          case funcVal of
            ANF.Lit lit -> error $ "Encountered lit function application " ++ show lit
            ANF.ConEnum _ con -> error $ "Encountered con enum function application " ++ show con
            ANF.Var (tyIdent@(ANF.Typed _ ident)) _ ->
              -- TODO: We need something more robust besides looking up by name.
              -- The Renamer should maybe handle resolving this name, or when we
              -- have modules we should make sure we are looking at the Prim
              -- module.
              case Map.lookup ident primitiveFunctionsByName of
                -- Primitive operation
                Just prim -> pure $ ANF.EPrimOp $ ANF.App prim argVals retTy'
                -- Default, just a function call
                Nothing -> pure $ ANF.EApp $ ANF.App tyIdent argVals retTy'
normalizeExpr name (C.EParens expr) = normalizeExpr name expr

normalizeLiteral :: C.Literal -> ANFConvert ANF.Literal
normalizeLiteral (C.LiteralInt i) = pure $ ANF.LiteralInt i
normalizeLiteral (C.LiteralDouble d) = pure $ ANF.LiteralDouble d
normalizeLiteral (C.LiteralText t) = ANF.LiteralTextPointer <$> makeTextPointer t

normalizeName :: Text -> C.Expr -> (ANF.Val -> ANFConvert ANF.Expr) -> ANFConvert ANF.Expr
normalizeName _ (C.ELit lit) c = c =<< ANF.Lit <$> normalizeLiteral lit
normalizeName name (C.EVar var) c =
  case var of
    C.VVal (C.Typed ty ident) -> do
      ty' <- convertType ty
      isTopLevel <- isIdentTopLevel ident
      let ident' = ANF.Typed ty' ident
      if isTopLevel
        -- Top-level values need to be first called as functions
        then
          case ty' of
            FuncType{} -> c $ ANF.Var ident' isTopLevel
            _ -> mkNormalizeLet name (ANF.EApp $ ANF.App ident' [] ty') ty' c
        -- Not a top-level value, just return
        else c $ ANF.Var ident' isTopLevel
    C.VCons (C.Typed ty con) -> do
      con' <- convertDataCon con
      ty' <- convertType ty
      case ty' of
        EnumType intBits -> c $ ConEnum intBits con'
        TaggedUnionType structName intBits ->
          mkNormalizeLet name (ANF.EConApp $ ANF.ConApp con' Nothing structName intBits) ty' c
        _ -> error $ "Invalid type for constructor in normalizeName " ++ show ty'
normalizeName name expr c = do
  expr' <- normalizeExpr name expr
  exprType <- convertType $ expressionType expr
  mkNormalizeLet name expr' exprType c

mkNormalizeLet :: Text -> ANF.Expr -> ANF.Type -> (ANF.Val -> ANFConvert ANF.Expr) -> ANFConvert ANF.Expr
mkNormalizeLet name expr exprType c = do
  newIdent <- freshIdent name
  body <- c $ ANF.Var (ANF.Typed exprType newIdent) False
  pure $ ANF.ELetVal $ collapseLetVals $ ANF.LetVal [ANF.LetValBinding newIdent exprType expr] body

normalizeBinding :: Maybe Text -> C.Binding -> ANFConvert ANF.Binding
normalizeBinding mName (C.Binding ident _ args retTy body) = do
  -- If we are given a base name, then use it. Otherwise use the binding name
  -- as the base name for all sub expressions.
  let subName = fromMaybe (unIdentName ident) mName
  body' <- normalizeExpr subName body
  args' <- traverse convertTypedIdent args
  retTy' <- convertType retTy
  pure $ ANF.Binding ident args' retTy' body'

normalizeLetBinding :: C.Binding -> ANFConvert ANF.LetValBinding
normalizeLetBinding (C.Binding ident ty [] _ body) = do
  body' <- normalizeExpr (unIdentName ident) body
  ty' <- convertType ty
  pure $ ANF.LetValBinding ident ty' body'
normalizeLetBinding bind@C.Binding{} =
  error $ "Encountered let binding with arguments. Functions not allowed in ANF. " ++ show bind

normalizeMatch :: Text -> C.Match -> ANFConvert ANF.Match
normalizeMatch name (C.Match pat body) = do
  pat' <- convertPattern pat
  body' <- normalizeExpr name body
  pure $ ANF.Match pat' body'

convertPattern :: C.Pattern -> ANFConvert ANF.Pattern
convertPattern (C.PLit lit) = ANF.PLit <$> normalizeLiteral lit
convertPattern (C.PCons (C.PatCons cons mArg retTy)) = do
  cons' <- convertDataCon cons
  mArg' <- traverse convertTypedIdent mArg
  retTy' <- convertType retTy
  pure $ ANF.PCons $ ANF.PatCons cons' mArg' retTy'

-- | Helper for normalizing lists of things
normalizeList :: (Monad m) => (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
normalizeList _ [] c = c []
normalizeList norm (x:xs) c =
  norm x $ \v -> normalizeList norm xs $ \vs -> c (v:vs)

normalizeRows :: [(RowLabel, C.Typed C.Expr)] -> ([(RowLabel, ANF.Typed ANF.Val)] -> ANFConvert ANF.Expr) -> ANFConvert ANF.Expr
normalizeRows [] c = c []
normalizeRows ((RowLabel label, C.Typed ty x):xs) c = do
  ty' <- convertType ty
  normalizeName label x $ \v -> normalizeRows xs $ \vs -> c ((RowLabel label, ANF.Typed ty' v):vs)

-- | ANF conversion produces a lot of nested and adjacent letval expressions.
-- This function cleans them up into a single letval. Note that bindings in a
-- single letval expression are non-recursive, and order matters, so there
-- isn't a risk of accidentally introducing recursion by reordering them, as
-- long as they are in the correct order.
--
-- Nested letvals get turned "inside out":
--
-- @
--    letval
--      x2 =
--        letval
--          x1 = 2
--        in x1 + 2
--      ...
-- @
--
-- @
--    letval
--      x1 = 2
--      x2 = x1 + x2
--      ...
-- @
--
-- Adjacent letvals get concatenated:
--
-- @
--    letval
--      x1 = 1
--    in
--      letval
--        x2 = 2
--        x3 = 3
--      in x1 + x2 + x3
-- @
--
-- @
--    letval
--      x1 = 1
--      x2 = 2
--      x3 = 3
--    in x1 + x2 + x3
-- @
collapseLetVals :: ANF.LetVal -> ANF.LetVal
collapseLetVals (ANF.LetVal bindings body) =
  let
    bindings' = concatMap collapseLetValBinding bindings
  in
    case body of
      ANF.ELetVal (ANF.LetVal bodyBindings bodyExpr) ->
        ANF.LetVal (bindings' ++ bodyBindings) bodyExpr
      _ -> ANF.LetVal bindings' body

-- | Utility function for 'collapseLetVals' that does the "turning inside out"
-- on bindings.
collapseLetValBinding :: ANF.LetValBinding -> [ANF.LetValBinding]
collapseLetValBinding (ANF.LetValBinding ident ty (ANF.ELetVal (ANF.LetVal bodyBindings bodyExpr))) =
  bodyBindings ++ [ANF.LetValBinding ident ty bodyExpr]
collapseLetValBinding binding = [binding]
