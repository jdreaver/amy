module Amy.Syntax.BindingGroups
  ( bindingGroups
  ) where

import Data.Foldable (toList)
import Data.Graph
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Amy.Syntax.AST

--
-- Binding Group
--

bindingGroups :: [Binding] -> [NonEmpty Binding]
bindingGroups bindings =
  let
    bindingNames = Set.fromList $ locatedValue . bindingName <$> bindings
    mkNode binding =
      ( binding
      , locatedValue (bindingName binding)
      , Set.toList $ freeBindingVars binding `Set.intersection` bindingNames
      )
    nodes = mkNode <$> bindings
    components' = stronglyConnComp nodes
  in makeBindingGroup <$> components'

makeBindingGroup :: SCC Binding -> NonEmpty Binding
makeBindingGroup (AcyclicSCC binding) = binding :| []
makeBindingGroup (CyclicSCC bindings) =
  fromMaybe (error "Found empty CyclicSCC while computing binding groups")
  $ NE.nonEmpty bindings

--
-- Free variables
--

freeBindingVars :: Binding -> Set IdentName
freeBindingVars (Binding (Located _ name) args body) =
  freeExprVars body `Set.difference` Set.fromList (name : (locatedValue <$> args))

freeExprVars :: Expr -> Set IdentName
freeExprVars ELit{} = Set.empty
freeExprVars (ERecord _ rows) = Set.unions $ freeExprVars <$> Map.elems rows
freeExprVars (ERecordSelect expr _) = freeExprVars expr
freeExprVars (EVar (VVal (Located _ ident))) = Set.singleton ident
freeExprVars (EVar VCons{}) = Set.empty
freeExprVars (EIf (If pred' then' else' _)) = freeExprVars pred' `Set.union` freeExprVars then' `Set.union` freeExprVars else'
freeExprVars (ECase (Case scrutinee matches _)) = Set.unions (freeExprVars scrutinee : toList (freeMatchVars <$> matches))
 where
  freeMatchVars (Match pat expr) = freeExprVars expr `Set.difference` patternVars pat
freeExprVars (ELam (Lambda args body _)) =
  freeExprVars body `Set.difference` Set.fromList (toList $ locatedValue <$> args)
freeExprVars (ELet (Let bindings expr _)) =
  let bindings' = mapMaybe letBinding bindings
  in Set.unions (freeExprVars expr : (freeBindingVars <$> bindings'))
freeExprVars (EApp f arg) = freeExprVars f `Set.union` freeExprVars arg
freeExprVars (EParens expr) = freeExprVars expr

patternVars :: Pattern -> Set IdentName
patternVars PLit{} = Set.empty
patternVars (PVar (Located _ ident)) = Set.singleton ident
patternVars (PCons (PatCons _ mPat)) = maybe Set.empty patternVars mPat
patternVars (PParens pat) = patternVars pat
