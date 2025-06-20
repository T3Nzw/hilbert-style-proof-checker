{-# LANGUAGE OverloadedRecordDot #-}

module Utils
  ( elemBy,
    tokenise,
    split,
    validQuantified,
  )
where

import qualified Data.Set as S
import Program.Formulae

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy _ _ [] = False
elemBy cmp x (y : ys) = x `cmp` y || elemBy cmp x ys

tokenise :: String -> [String]
tokenise = words

data Bound

data Free

type Variable a = String

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

-- i believe this is more appropriate for axioms,
-- since pattern matching should only be top-level
-- also, it's much easier to make a top-level function
-- recurse over something than the opposite :D

-- | only does top-level comparisons
validQuantified :: ConcreteFormula -> Bool
validQuantified (Forall x _ :->: (_ `With` sub)) = x == sub._var
validQuantified (Forall x _ :->: (_ :->: (Forall y _))) = x == y
validQuantified (_ `With` sub :->: (Exists x _)) = sub._var == x
validQuantified (Forall x _ :->: (Exists y _)) = x == y
validQuantified _ = True
