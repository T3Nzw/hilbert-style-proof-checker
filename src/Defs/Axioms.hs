module Defs.Axioms where

data Position = Lhs | Rhs
  deriving (Show, Eq, Ord)

data Axiom
  = S
  | K
  | CONJ_ELIM Position
  | CONJ_INTRO
  | DISJ_INTRO Position
  | DISJ_ELIM
  | UNIV_WITNESS
  | UNIV_IMPLIES
  | SUB_EXIST
  | EX_IMPLIES
  | EX_FALSO
  | STAB
  deriving (Show, Eq, Ord)
