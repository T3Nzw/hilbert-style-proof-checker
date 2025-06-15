module Program.Rules where

import qualified Program.Axioms as Axiom

data Rule
  = AS -- assumption
  | AX Axiom.Axiom -- just beautiful; axiom
  | MP -- modus ponens
  | GEN -- generalisation
  deriving (Show)
