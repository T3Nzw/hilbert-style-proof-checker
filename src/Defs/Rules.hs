module Defs.Rules where

import Defs.Axioms

data Rule
  = AS -- assumption
  | AX Axiom -- axiom
  | MP -- modus ponens
  | GEN -- generalisation
  | TH String -- previously proved theorem
  deriving (Show, Eq, Ord)
