{-# LANGUAGE OverloadedStrings #-}

module Amy.ANF.Pretty
  ( prettyModule
  , prettyExpr
  ) where

import Data.Foldable (toList)
import Data.Maybe (maybeToList)

import Amy.ANF.AST
import Amy.Literal
import Amy.Pretty
import Amy.Prim

prettyType :: Type -> Doc ann
prettyType = pretty . show

prettyModule :: Module -> Doc ann
prettyModule (Module bindings externs typeDeclarations) =
  vcatTwoHardLines
  $ (prettyExtern' <$> externs)
  ++ (prettyTypeDeclaration' <$> typeDeclarations)
  ++ (prettyBinding' <$> bindings)

prettyExtern' :: Extern -> Doc ann
prettyExtern' (Extern name ty) =
  prettyExtern (prettyIdent name) (prettyType ty)

prettyTypeDeclaration' :: TypeDeclaration -> Doc ann
prettyTypeDeclaration' (TypeDeclaration tyName _ cons) =
   prettyTypeDeclaration (prettyTyConName tyName) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConDefinition conName mArg) =
    prettyDataConstructor (prettyDataConName conName) (prettyType <$> mArg)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding ident args retTy body) =
  prettyBindingType (prettyIdent ident) (prettyType $ FuncType (typedType <$> args) retTy) <>
  hardline <>
  prettyBinding (prettyIdent ident) (prettyIdent . typedValue <$> args) (prettyExpr body)

prettyVal :: Val -> Doc ann
prettyVal (Var (Typed _ ident) _) = prettyIdent ident
prettyVal (Lit lit) = pretty $ showLiteral lit
prettyVal (ConEnum _ con) = prettyDataConName (dataConName con)

prettyExpr :: Expr -> Doc ann
prettyExpr (EVal val) = prettyVal val
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
prettyExpr (ELetVal (LetVal bindings body)) =
  prettyLetVal (prettyLetValBinding <$> bindings) (prettyExpr body)
prettyExpr (EApp (App (Typed _ ident) args _)) =
  "$call" <+> prettyIdent ident <+> list (prettyVal <$> args)
prettyExpr (EConApp (ConApp info mArg _ _)) =
  "$mkCon" <+> prettyDataConName (dataConName info) <+> list (prettyVal <$> maybeToList mArg)
prettyExpr (EPrimOp (App (PrimitiveFunction _ name _) args _)) =
  "$primOp" <+> prettyIdent name <+> list (prettyVal <$> args)

prettyLetValBinding :: LetValBinding -> Doc ann
prettyLetValBinding (LetValBinding ident ty body) =
  prettyBindingType (prettyIdent ident) (prettyType ty) <>
  hardline <>
  prettyBinding (prettyIdent ident) [] (prettyExpr body)

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit lit) = pretty $ showLiteral lit
prettyPattern (PCons (PatCons con mArg _)) =
  prettyDataConName (dataConName con)
  <> maybe mempty (\(Typed ty arg) -> space <> parens (prettyIdent arg <+> "::" <+> prettyType ty)) mArg
