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
import Amy.Core.LambdaLift
import Amy.Prim

normalizeModule :: C.Module -> ANF.Module
normalizeModule mod' =
  let
    -- First do lambda lifting
    (C.Module bindingGroups externs typeDeclarations) = lambdaLifting mod'
    bindings = concatMap NE.toList bindingGroups

    -- Record top-level names
    bindingTys = (\b -> (C.bindingName b, (C.typedType <$> C.bindingArgs b, C.bindingReturnType b))) <$> bindings
    externTys = (\e -> (C.externName e, mkExternType (C.externType e))) <$> externs
    topLevelTys = bindingTys ++ externTys

    -- Actual conversion
    convertRead = anfConvertRead topLevelTys typeDeclarations
  in runANFConvert convertRead $ do
    typeDeclarations' <- traverse convertTypeDeclaration typeDeclarations
    externs' <- traverse convertExtern externs
    bindings' <- traverse (normalizeBinding (Just "res")) bindings
    textPointers <- getTextPointers
    closureWrappers <- getClosureWrappers
    pure $ ANF.Module bindings' externs' typeDeclarations' textPointers closureWrappers

convertExtern :: C.Extern -> ANFConvert ANF.Extern
convertExtern (C.Extern name ty) = do
  let (argTys, retTy) = mkExternType ty
  argTys' <- traverse convertType argTys
  retTy' <- convertType retTy
  pure $ ANF.Extern name argTys' retTy'

mkExternType :: C.Type -> ([C.Type], C.Type)
mkExternType ty =
  let tyNE = typeToNonEmpty ty
  in (NE.init tyNE, NE.last tyNE)

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
      C.TyFun{} -> pure ClosureType
      C.TyForall _ ty'' -> convertType ty''
  go _ = pure ClosureType

mkRecordType :: Map RowLabel C.Type -> ANFConvert ANF.Type
mkRecordType rows = do
  rows' <- for (Map.toAscList rows) $ \(label, ty) -> do
    ty' <- convertType ty
    pure (label, ty')
  pure $ RecordType rows'

typeToNonEmpty :: C.Type -> NonEmpty C.Type
typeToNonEmpty (t1 `C.TyFun` t2) = NE.cons t1 (typeToNonEmpty t2)
typeToNonEmpty ty = ty :| []

convertTypedIdent :: C.Typed IdentName -> ANFConvert (ANF.Typed IdentName)
convertTypedIdent (C.Typed ty ident) = do
  ty' <- convertType ty
  pure $ ANF.Typed ty' ident

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
normalizeExpr name (C.ELet (C.Let bindings expr)) = do
  bindings' <- traverse normalizeLetBinding bindings
  expr' <- normalizeExpr name expr
  pure $ ANF.ELetVal $ collapseLetVals $ ANF.LetVal (NE.toList bindings') expr'
normalizeExpr name (C.EApp app@(C.App _ _ retTy)) = do
  let func :| args = C.unfoldApp app
  normalizeList (normalizeName name) (toList args) $ \rawArgVals ->
   normalizeList maybeCreateClosure rawArgVals $ \argVals -> do
    retTy' <- convertType retTy
    case func of
      C.EVar (C.VVal (C.Typed _ ident)) -> do
        mFuncType <- getKnownFuncType ident
        case (Map.lookup ident primitiveFunctionsByName, mFuncType) of
          -- Primitive operation
          (Just prim, _) -> pure $ ANF.EPrimOp $ ANF.App prim argVals retTy'
          -- Known function
          (_, Just (funcArgTys, funcRetTy)) -> do
            funcArgTys' <- traverse convertType funcArgTys
            funcRetTy' <- convertType funcRetTy
            if length argVals == length funcArgTys
            -- Known function call
            then pure $ ANF.EKnownFuncApp $ KnownFuncApp ident argVals funcArgTys' retTy' funcRetTy'
            -- Too few or too many args, fall back to using a closure and call it
            else
              createClosure ident funcArgTys' funcRetTy' [] $ \closureVal ->
                pure $ ECallClosure $ CallClosure closureVal argVals retTy'
          -- Unknown function, must be a closure
          (_, Nothing) -> pure $ ECallClosure $ CallClosure (ANF.Var (ANF.Typed ClosureType ident)) argVals retTy'
      -- Data constructor
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
          pure $ ECallClosure $ CallClosure funcVal argVals retTy'

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
      mFuncType <- getKnownFuncType ident
      ty' <- convertType ty
      case mFuncType of
        -- Top-level values need to be first called as functions
        Just ([], _) -> mkNormalizeLet name (ANF.EKnownFuncApp $ ANF.KnownFuncApp ident [] [] ty' ty') ty' c
        _ -> c $ ANF.Var $ ANF.Typed ty' ident
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
  body <- c $ ANF.Var (ANF.Typed exprType newIdent)
  pure $ ANF.ELetVal $ collapseLetVals $ ANF.LetVal [ANF.LetValBinding newIdent exprType expr] body

maybeCreateClosure :: ANF.Val -> (ANF.Val -> ANFConvert ANF.Expr) -> ANFConvert ANF.Expr
maybeCreateClosure v@(ANF.Var (ANF.Typed _ ident)) c = do
  mFuncType <- getKnownFuncType ident
  case mFuncType of
    Just (argTys, retTy) -> do
      argTys' <- traverse convertType argTys
      retTy' <- convertType retTy
      createClosure ident argTys' retTy' [] c
    _ -> c v
maybeCreateClosure v c = c v

createClosure :: ANF.IdentName -> [ANF.Type] -> ANF.Type -> [ANF.Val] -> (ANF.Val -> ANFConvert ANF.Expr) -> ANFConvert ANF.Expr
createClosure ident argTys retTy args c = do
  wrapperName <- putClosureWrapper ident argTys retTy
  let arity = length argTys
  mkNormalizeLet (unIdentName ident <> "_closure") (ECreateClosure $ CreateClosure wrapperName arity args) ClosureType c

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
