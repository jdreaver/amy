{-# LANGUAGE OverloadedStrings #-}

module Amy.TypeCheck.Inference
  ( runInference
  , inferType
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import Amy.Literal
import Amy.Names
import Amy.Prim
import Amy.Renamer.AST
import Amy.Syntax.Located
import Amy.Type
import Amy.TypeCheck.Inference.ConstraintCollection
import Amy.TypeCheck.Inference.ConstraintSolving

-- TODO: Split this function up between inference and constraint solving monads
inferType :: RBinding -> Inference (Subst, Scheme PrimitiveType)
inferType b = do
  (cs, t) <- head <$> inferBindings [b]
  --let cs' = [ExpInstConst t s | (x, s) <- Env.toList env, t <- As.lookup x as]
  subst <- solve cs --(cs ++ cs')
  pure (subst, normalize $ substituteType subst t)

--
-- Misc
--

_l :: a -> Located a
_l = Located (SourceSpan "" 1 1 1 1)

_b1 :: RBinding
_b1 =
  RBinding
  x
  Nothing
  [y, z]
  ( REApp (RApp (REVar y) ((REVar z) :| [RELit (_l $ LiteralBool False)]))
  )
 where
  y = _l $ ValueName "y" (NameIntId 1)
  z = _l $ ValueName "z" (NameIntId 2)
  x = _l $ ValueName "x" (NameIntId 3)

_e1 :: RExpr
_e1 =
  RELet $
  RLet
  [RBinding x Nothing [y] (RELit (_l $ LiteralBool True))]
  (REApp (RApp (REVar x) (RELit (_l $ LiteralInt 1) :| [])))
 where
  y = _l $ ValueName "y" (NameIntId 1)
  x = _l $ ValueName "x" (NameIntId 2)

_e2 :: RExpr
_e2 =
  RELet $
  RLet
  [RBinding x Nothing [y, z] (RELit (_l $ LiteralBool True))]
  --(REVar x)
  (REApp (RApp (REVar x) (RELit (_l $ LiteralInt 1) :| [])))
 where
  y = _l $ ValueName "y" (NameIntId 1)
  z = _l $ ValueName "z" (NameIntId 2)
  x = _l $ ValueName "x" (NameIntId 3)

_e3 :: RExpr
_e3 =
  RELet $
  RLet
  [ RBinding x Nothing [y, z] (RELit (_l $ LiteralBool True))
  , RBinding a Nothing [] (RELit (_l $ LiteralBool False))
  ]
  --(REVar x)
  --(REApp (RApp (REVar x) (RELit (l $ LiteralInt 1) :| [])))
  (REApp (RApp (REVar x) ((REVar a) :| [RELit (_l $ LiteralBool False)])))
 where
  y = _l $ ValueName "y" (NameIntId 1)
  z = _l $ ValueName "z" (NameIntId 2)
  x = _l $ ValueName "x" (NameIntId 3)
  a = _l $ ValueName "a" (NameIntId 3)
