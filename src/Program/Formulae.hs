{-# LANGUAGE InstanceSigs #-}

module Program.Formulae where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

type Identifier = String

type Context a b = M.Map Identifier b

data Substitution a = String :~> Formula a
  deriving (Show, Eq, Ord)

data Formula a
  = Void
  | Variable String
  | Negation (Formula a)
  | (Formula a) :&: (Formula a)
  | (Formula a) :|: (Formula a)
  | (Formula a) :->: (Formula a)
  | Forall String (Formula a)
  | Exists String (Formula a)
  | (Formula a) `With` (Substitution a)
  deriving (Show, Eq, Ord)

infixl 6 :&:

infixl 6 :|:

infixr 6 :->:

data Concrete

data Meta

type ConcreteFormula = Formula Concrete

type MetaFormula = Formula Meta

type EqState = State (Context FormulaVariables ConcreteFormula) Bool

match :: MetaFormula -> ConcreteFormula -> Bool
-- apparently equivalence relations are NOT symmetric 'round here :)
-- TODO: potentially just pattern match the opposite case? helper qctx x (Variable y)?
-- UPDATE: it won't work since it matters which term contains the metavariables (or the most general types)
-- UPDATE 2: equality for formulas, as i have defined them, CANNOT be symmetric...
-- the reason for that being that any variable WILL BE matched to every single axiom...
-- for example,
-- A -> B -> A == A ---> obviously doesn't work
-- BUT
-- A == A -> B -> A ---> will work since A is a metavariable :)
match lhs rhs = evalState (helper M.empty lhs rhs) M.empty
  where
    -- implementing by always replacing the lhs with the rhs
    helper :: Context QuantifiedVariables String -> MetaFormula -> ConcreteFormula -> EqState
    helper _ Void Void = pure True
    helper qctx (Variable x) y =
      case M.lookup x qctx of
        Just z -> pure $ y == Variable z
        Nothing -> do
          fctx <- get
          case M.lookup x fctx of
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
      helper (M.insert x y qctx) lhs rhs
    helper qctx (Exists x lhs) (Exists y rhs) =
      helper (M.insert x y qctx) lhs rhs
    helper qctx (lhs `With` lsub) (rhs `With` rsub) = error "TODO"
    helper _ _ _ = pure False

data Bound

data Free

type Variable a = String

data QuantifiedVariables

data FormulaVariables

split :: Formula a -> (S.Set (Variable Bound), S.Set (Variable Free))
split = helper S.empty S.empty
  where
    helper :: S.Set (Variable Bound) -> S.Set (Variable Free) -> Formula a -> (S.Set (Variable Bound), S.Set (Variable Free))
    helper _ _ Void = (S.empty, S.empty)
    helper bv fv (Variable x)
      | S.member x bv = (S.insert x bv, fv)
      | otherwise = (bv, S.insert x fv)
    helper bv fv (Negation f) = helper bv fv f
    helper bv fv (lhs :&: rhs) = (S.union lbv rbv, S.union lfv rfv)
      where
        (lbv, lfv) = helper bv fv lhs
        (rbv, rfv) = helper bv fv rhs
    helper bv fv (lhs :|: rhs) = (S.union lbv rbv, S.union lfv rfv)
      where
        (lbv, lfv) = helper bv fv lhs
        (rbv, rfv) = helper bv fv rhs
    helper bv fv (lhs :->: rhs) = (S.union lbv rbv, S.union lfv rfv)
      where
        (lbv, lfv) = helper bv fv lhs
        (rbv, rfv) = helper bv fv rhs
    helper bv fv (Forall x f) = helper (S.insert x bv) fv f
    helper bv fv (Exists x f) = helper (S.insert x bv) fv f
    -- TODO actually think about this one lol
    helper bv fv (f `With` (var :~> subst)) = (S.union fbv sbv, S.union ffv sfv)
      where
        (fbv, ffv) = helper bv fv f
        (sbv, sfv) = helper bv fv subst
