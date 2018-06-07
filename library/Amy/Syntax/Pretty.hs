{-# LANGUAGE OverloadedStrings #-}

module Amy.Syntax.Pretty
  ( prettyModule
  , prettyDeclaration
  , prettyTypeDeclaration
  , prettyExpr
  , prettyType
  ) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map

import Amy.Literal
import Amy.Pretty
import Amy.Syntax.AST

prettyType :: Type -> Doc ann
prettyType (TyFun ty1 ty2) = parensIf (isTyFun ty1) (prettyType ty1) <+> "->" <+> prettyType ty2
prettyType (TyCon con) = prettyTyConName (locatedValue con)
prettyType (TyVar var) = prettyTyVarName (locatedValue var)
prettyType (TyRecord rows mVar) = prettyTyRecord (uncurry prettyTyRow <$> Map.toList rows) (prettyTyVarName . locatedValue <$> mVar)
prettyType (TyApp f arg) = prettyType f <+> parensIf (isTyApp arg) (prettyType arg)
prettyType (TyForall vars ty) = "forall" <+> hcat (punctuate space $ prettyTyVarName . locatedValue <$> toList vars) <> "." <+> prettyType ty

prettyTyRow :: Located RowLabel -> Type -> Doc ann
prettyTyRow (Located _ label) ty = prettyRowLabel label <+> "::" <+> prettyType ty

isTyApp :: Type -> Bool
isTyApp TyApp{} = True
isTyApp TyFun{} = True -- Technically -> is an infix app
isTyApp _ = False

isTyFun :: Type -> Bool
isTyFun TyFun{} = True
isTyFun _ = False

prettyModule :: Module -> Doc ann
prettyModule (Module decls) = vcatTwoHardLines (prettyDeclaration <$> decls)

prettyDeclaration :: Declaration -> Doc ann
prettyDeclaration (DeclBinding binding) = prettyBinding' binding
prettyDeclaration (DeclBindingType bindingTy) = prettyBindingType' bindingTy
prettyDeclaration (DeclExtern (Extern (Located _ name) ty)) =
  prettyExtern (prettyIdent name) (prettyType ty)
prettyDeclaration (DeclType (TypeDeclaration info cons)) =
  prettyTypeDeclaration (prettyTyConDefinition info) (prettyConstructor <$> cons)
 where
  prettyConstructor (DataConDefinition (Located _ conName) mArg) =
    prettyDataConstructor (prettyDataConName conName) (prettyType <$> mArg)

prettyTyConDefinition :: TyConDefinition -> Doc ann
prettyTyConDefinition (TyConDefinition name args _) = prettyTyConName name <> args'
 where
  args' = if null args then mempty else space <> sep (prettyTyVarName . locatedValue <$> args)

prettyBinding' :: Binding -> Doc ann
prettyBinding' (Binding (Located _ name) args body) =
  prettyBinding (prettyIdent name) (prettyIdent . locatedValue <$> args) (prettyExpr body)

prettyBindingType' :: BindingType -> Doc ann
prettyBindingType' (BindingType (Located _ name) ty) =
  prettyBindingType (prettyIdent name) (prettyType ty)

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit (Located _ lit)) = pretty $ showLiteral lit
prettyExpr (ERecord rows) = bracketed $ uncurry prettyRow <$> Map.toList rows
prettyExpr (ERecordSelect expr field) = prettyExpr expr <> "." <> prettyRowLabel (locatedValue field)
prettyExpr (EVar var) = prettyVar var
prettyExpr (EIf (If pred' then' else' _)) =
  prettyIf (prettyExpr pred') (prettyExpr then') (prettyExpr else')
prettyExpr (ECase (Case scrutinee matches _)) =
  prettyCase (prettyExpr scrutinee) Nothing (toList $ mkMatch <$> matches)
 where
  mkMatch (Match pat body) = (prettyPattern pat, prettyExpr body)
prettyExpr (ELet (Let bindings body _)) =
  prettyLet (prettyLetBinding <$> bindings) (prettyExpr body)
 where
  prettyLetBinding (LetBinding binding) = prettyBinding' binding
  prettyLetBinding (LetBindingType bindingTy) = prettyBindingType' bindingTy
prettyExpr (EApp f arg) = prettyExpr f <+> prettyExpr arg
prettyExpr (EParens expr) = parens $ prettyExpr expr

prettyRow :: Located RowLabel -> Expr -> Doc ann
prettyRow (Located _ label) expr = prettyRowLabel label <> ":" <+> prettyExpr expr

prettyVar :: Var -> Doc ann
prettyVar (VVal (Located _ var)) = prettyIdent var
prettyVar (VCons (Located _ var)) = prettyDataConName var

prettyPattern :: Pattern -> Doc ann
prettyPattern (PLit (Located _ lit)) = pretty $ showLiteral lit
prettyPattern (PVar (Located _ var)) = prettyIdent var
prettyPattern (PParens pat) = parens (prettyPattern pat)
prettyPattern (PCons (PatCons (Located _ con) mArg)) =
  prettyDataConName con <> maybe mempty prettyArg mArg
 where
  prettyArg = (space <>) . prettyArg'
  prettyArg' arg@PCons{} = parens (prettyPattern arg)
  prettyArg' arg = prettyPattern arg
