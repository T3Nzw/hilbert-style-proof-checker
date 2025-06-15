{-# LANGUAGE InstanceSigs #-}

module Program.Formulae where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

data Bound

data Free

type Variable a = String

type Identifier = String

data QuantifiedVariables

data FormulaVariables

type Context a b = M.Map Identifier b

data Op = And | Or | Implies

data Substitution = String :~> Formula
  deriving (Show)

data Formula
  = Void
  | Variable String
  | Negation Formula
  | Formula :&: Formula
  | Formula :|: Formula
  | Formula :->: Formula
  | Forall String Formula
  | Exists String Formula
  | Formula `With` Substitution
  deriving (Show)

infixl 6 :&:

infixl 6 :|:

infixr 6 :->:

type EqState = State (Context FormulaVariables Formula) Bool

-- the credit goes to ChatGPT for instructing me to implement
-- syntactic equality! i was recursively calling (==) in my
-- (==) implementation and it was leading to weird stuf...
-- (the weird stuff being an inconsistent axiomatic system)
syntacticEq :: Formula -> Formula -> Bool
syntacticEq Void Void = True
syntacticEq (Variable x) (Variable y) = x == y
syntacticEq (Negation lhs) (Negation rhs) = syntacticEq lhs rhs
syntacticEq (lhs1 :&: rhs1) (lhs2 :&: rhs2) =
  syntacticEq lhs1 lhs2 && syntacticEq rhs1 rhs2
syntacticEq (lhs1 :|: rhs1) (lhs2 :|: rhs2) =
  syntacticEq lhs1 lhs2 && syntacticEq rhs1 rhs2
syntacticEq (lhs1 :->: rhs1) (lhs2 :->: rhs2) =
  syntacticEq lhs1 lhs2 && syntacticEq rhs1 rhs2
syntacticEq (Forall x lhs) (Forall y rhs) =
  x == y && syntacticEq lhs rhs
syntacticEq (Exists x lhs) (Exists y rhs) =
  x == y && syntacticEq lhs rhs
syntacticEq (lhs `With` (x :~> lsub)) (rhs `With` (y :~> rsub)) =
  syntacticEq lhs rhs && x == y && syntacticEq lsub rsub
syntacticEq _ _ = False

instance Eq Formula where
  (==) :: Formula -> Formula -> Bool
  -- apparently equivalence relations are NOT symmetric 'round here :)
  -- TODO: potentially just pattern match the opposite case? helper qctx x (Variable y)?
  -- UPDATE: it won't work since it matters which term contains the metavariables (or the most general types)
  lhs == rhs = evalState (helper M.empty lhs rhs) M.empty || evalState (helper M.empty rhs lhs) M.empty
    where
      -- implementing by always replacing the lhs with the rhs
      helper :: Context QuantifiedVariables String -> Formula -> Formula -> EqState
      helper _ Void Void = pure True
      helper qctx (Variable x) y =
        case M.lookup x qctx of
          Just z -> pure $ syntacticEq y (Variable z)
          Nothing -> do
            fctx <- get
            case M.lookup x fctx of
              Nothing -> do
                modify $ M.insert x y
                pure True
              Just z -> pure $ syntacticEq y z
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

split :: Formula -> (S.Set (Variable Bound), S.Set (Variable Free))
split = helper S.empty S.empty
  where
    helper :: S.Set (Variable Bound) -> S.Set (Variable Free) -> Formula -> (S.Set (Variable Bound), S.Set (Variable Free))
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
