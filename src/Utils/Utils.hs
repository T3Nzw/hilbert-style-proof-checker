{-# LANGUAGE OverloadedRecordDot #-}

module Utils
  ( elemBy,
    tokenise,
    validQuantified,
    isFreeIn,
    validFree,
  )
where

import Program.Formulae

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy _ _ [] = False
elemBy cmp x (y : ys) = x `cmp` y || elemBy cmp x ys

tokenise :: String -> [String]
tokenise = words

type Identifier = String

isFreeIn :: Identifier -> ConcreteFormula -> Bool
isFreeIn x (Negation f) = isFreeIn x f
isFreeIn x (lhs :&: rhs) = isFreeIn x lhs && isFreeIn x rhs
isFreeIn x (lhs :|: rhs) = isFreeIn x lhs && isFreeIn x rhs
isFreeIn x (lhs :->: rhs) = isFreeIn x lhs && isFreeIn x rhs
isFreeIn x (Forall y f) = x /= y && isFreeIn x f
isFreeIn x (Exists y f) = x /= y && isFreeIn x f
isFreeIn x (Predicate p v) = isFreeIn x p && all (isFreeIn x) v
isFreeIn _ _ = True

validQuantified :: ConcreteFormula -> Bool
validQuantified (Negation f) = validQuantified f
validQuantified (lhs :&: rhs) = validQuantified lhs && validQuantified rhs
validQuantified (lhs :|: rhs) = validQuantified lhs && validQuantified rhs
validQuantified (lhs :->: rhs) = validQuantified lhs && validQuantified rhs
validQuantified (Forall x f) = x `isFreeIn` f && validQuantified f
validQuantified (Exists x f) = x `isFreeIn` f && validQuantified f
validQuantified (Predicate p v) = validQuantified p && all validQuantified v
validQuantified _ = True

-- TODO check if correctly implemented

-- | check if free variable constraints
-- are satisfied in accordance with certain axioms
validFree :: ConcreteFormula -> Bool
validFree (Forall x (fb :->: _) :->: (_ :->: Forall _ _)) =
  not $ isFreeIn x fb -- should be ok since the axiom schema needs to be matched first:)
validFree (Forall x (_ :->: fb) :->: (Exists _ _ :->: _)) =
  not $ isFreeIn x fb
validFree _ = True
