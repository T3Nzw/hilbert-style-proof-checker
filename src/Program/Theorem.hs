module Program.Theorem where

import Control.Monad.State
import qualified Data.Map as M
import qualified Program.Axioms as Axiom
import Program.Formulae
import qualified Program.ProofStatement as PS
import qualified Program.Rules as Rule

data Theorem = Theorem Identifier PS.Goal PS.Context PS.ProofStatements
