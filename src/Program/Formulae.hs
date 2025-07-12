{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Program.Formulae
  ( Formula (..),
    ConcreteFormula,
    MetaFormula,
    match,
  )
where

import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.Map as M
import Defs
import Defs.Formulae

--
type Context a b = M.Map Identifier b

data QuantifiedVariables

data FormulaVariables

type EqState = State (Context FormulaVariables ConcreteFormula) Bool

-- | match a concrete formula to a metaformula,
-- and return the result and generated context of constraints
-- TODO: return Maybe/custom ADT to signify error
match :: MetaFormula -> ConcreteFormula -> (Bool, Context FormulaVariables ConcreteFormula)
-- match a concrete formula to a metaformula (axiom schema)
-- both MetaFormula and ConcreteFormula share the same ADT
-- and only have a type-level distinction (since Formula is a phantom type)
match lhs rhs =
  runState (helper M.empty lhs rhs) M.empty
  where
    helper :: Context QuantifiedVariables ConcreteFormula -> MetaFormula -> ConcreteFormula -> EqState
    helper _ Void Void = pure True
    helper qctx (Variable x) y =
      case M.lookup x qctx of
        Nothing -> pure True
        Just z -> pure $ y == z
    helper qctx (PropVariable x) y = do
      ctx <- get
      case M.lookup x ctx of
        Nothing -> do
          modify $ M.insert x y
          pure True
        Just z -> pure $ y == z
    helper qctx (Negation lhs) (Negation rhs) = helper qctx lhs rhs
    helper qctx (lhs1 :&: rhs1) (lhs2 :&: rhs2) = do
      res1 <- helper qctx lhs1 lhs2
      res2 <- helper qctx rhs1 rhs2
      pure $ res1 && res2
    helper qctx (lhs1 :|: rhs1) (lhs2 :|: rhs2) = do
      res1 <- helper qctx lhs1 lhs2
      res2 <- helper qctx rhs1 rhs2
      pure $ res1 && res2
    helper qctx (lhs1 :->: rhs1) (lhs2 :->: rhs2) = do
      res1 <- helper qctx lhs1 lhs2
      res2 <- helper qctx rhs1 rhs2
      pure $ res1 && res2
    helper qctx (Forall x lhs) (Forall y rhs) =
      helper (M.insert x (Variable y) qctx) lhs rhs
    helper qctx (Exists x lhs) (Exists y rhs) =
      helper (M.insert x (Variable y) qctx) lhs rhs
    helper qctx (Predicate p1@(PropVariable x) lhs) (Predicate p2 rhs) = do
      res1 <- helper qctx' p1 p2
      res2 <- mapM (\(x, y) -> helper qctx' x y) $ lhs `zip` rhs
      pure $ length lhs == length rhs && res1 && and res2
      where
        qctx' = M.insert x p2 qctx
    helper _ _ _ = pure False
