module Amy.Syntax.BindingGroups
  ( bindingGroups
  ) where

import Data.Graph
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Amy.Syntax.AST

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
