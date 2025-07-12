module Defs.Theorem where

import Data.Map.Ordered (OMap)
import Defs
import qualified Defs.ProofStatement as PS

data Theorem = Theorem Identifier PS.Goal PS.Context PS.ProofStatements

-- TODO make it prettier
instance Show Theorem where
  show (Theorem iden goal ctx pss) =
    "Theorem \""
      ++ iden
      ++ "\"\nGoal: "
      ++ show goal
      ++ "\nInitial context:\n"
      ++ show ctx
      ++ "\n"
      ++ show pss

type ProvedTheorems = OMap String Theorem
