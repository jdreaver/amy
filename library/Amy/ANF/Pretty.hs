{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Pretty
  ( prettyModule
  , prettyExpr
  ) where

import Data.Foldable (toList)
import Data.Maybe (maybeToList)
import qualified Data.Map.Strict as Map

import Amy.ANF.AST
import Amy.Pretty hiding (prettyType)
import Amy.Prim

prettyType :: Type -> Doc ann
prettyType PrimIntType = "PrimInt"
prettyType PrimDoubleType = "PrimDouble"
prettyType PrimTextType = "PrimText"
prettyType (PointerType ty) = "Pointer" <+> parens (prettyType ty)
prettyType OpaquePointerType = "OpaquePointer"
prettyType ClosureType = "ClosureType"
prettyType (EnumType bits) = "Enum" <+> pretty bits
prettyType (TaggedUnionType (TyConName name) bits) = "TaggedUnion" <+> pretty name <+> pretty bits
prettyType (RecordType rows) =
  "RecordType" <> groupOrHang (bracketed ((\(RowLabel label, ty) -> pretty label <+> "::" <> groupOrHang (prettyType ty)) <$> rows))

prettyFunctionType :: [Type] -> Type -> Doc ann
prettyFunctionType args retTy = "Func" <> groupOrHang (tupled (prettyType <$> args) <+> "=>" <+> prettyType retTy)

prettyModule :: Module -> Doc ann
prettyModule (Module bindings externs typeDeclarations _ _ closureWrappers) =
  vcatTwoHardLines
  $ (prettyExtern' <$> externs)
  ++ (prettyTypeDeclaration' <$> typeDeclarations)
  ++ (prettyClosureWrapper <$> closureWrappers)
  ++ (prettyBinding' <$> bindings)

prettyExtern' :: Extern -> Doc ann
prettyExtern' (Extern name argTys retTy) =
  prettyExtern (prettyIdent name) (prettyFunctionType argTys retTy)

prettyTypeDeclaration' :: TypeDeclaration -> Doc ann
prettyTypeDeclaration' (TypeDeclaration tyName _ cons) =
   prettyTypeDeclaration (prettyTyConName tyName) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConDefinition conName mArg) =
    prettyDataConstructor (prettyDataConName conName) (prettyType <$> mArg)

prettyClosureWrapper :: ClosureWrapper -> Doc ann
prettyClosureWrapper (ClosureWrapper wrapperName originalName argTys returnTy) =
  prettyIdent wrapperName <+> parens (prettyIdent originalName) <+> list (prettyType <$> argTys) <+> "=>" <+> prettyType returnTy

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding ident args retTy body) =
  prettyBindingType (prettyIdent ident) (prettyFunctionType (typedType <$> args) retTy) <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyIdent . typedValue <$> args) (prettyExpr body)

prettyVal :: Val -> Doc ann
prettyVal (Var (Typed _ ident)) = prettyIdent ident
prettyVal (Lit lit) = pretty $ show lit
prettyVal (ConEnum _ con) = prettyDataConName (dataConName con)

prettyExpr :: Expr -> Doc ann
prettyExpr (EVal val) = prettyVal val
prettyExpr (ERecord rows) = bracketed $ uncurry prettyRow <$> Map.toList (typedValue <$> rows)
prettyExpr (ERecordSelect val field _) = prettyVal val <> "." <> prettyRowLabel field
prettyExpr (ECase (Case scrutinee (Typed _ bind) matches mDefault _)) =
  prettyCase
    (prettyVal scrutinee)
    (Just $ prettyIdent bind)
    (toList (mkMatch <$> matches) ++ defaultMatch)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
  defaultMatch =
    case mDefault of
      Nothing -> []
      Just def -> [("__DEFAULT", prettyExpr def)]
prettyExpr (ECreateClosure (CreateClosure f arity)) =
  "$createClosure" <+> prettyIdent f <+> pretty arity
prettyExpr (ECallClosure (CallClosure f args retTy)) =
  "$callClosure" <+> prettyVal f <+> list (prettyVal <$> args) <+> "::" <+> prettyType retTy
prettyExpr (ELetVal (LetVal bindings body)) =
  prettyLetVal (prettyLetValBinding <$> bindings) (prettyExpr body)
prettyExpr (EKnownFuncApp (KnownFuncApp ident args _ _ _)) =
  "$call" <+> prettyIdent ident <+> list (prettyVal <$> args)
prettyExpr (EConApp (ConApp info mArg _ _)) =
  "$mkCon" <+> prettyDataConName (dataConName info) <+> list (prettyVal <$> maybeToList mArg)
prettyExpr (EPrimOp (App (PrimitiveFunction _ name _) args _)) =
  "$primOp" <+> prettyIdent name <+> list (prettyVal <$> args)

prettyRow :: RowLabel -> Val -> Doc ann
prettyRow label val = prettyRowLabel label <> ":" <> groupOrHang (prettyVal val)

prettyLetValBinding :: LetValBinding -> Doc ann
prettyLetValBinding (LetValBinding ident ty body) =
  prettyBindingType (prettyIdent ident) (prettyType ty) <>
  hardline <>
  prettyBinding (prettyIdent ident) [] (prettyExpr body)

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit lit) = pretty $ show lit
prettyPattern (PCons (PatCons con mArg _)) =
  prettyDataConName (dataConName con)
  <> maybe mempty (\(Typed ty arg) -> space <> parens (prettyIdent arg <+> "::" <+> prettyType ty)) mArg
