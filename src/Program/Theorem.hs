module Program.Theorem where

import Control.Monad.State
import qualified Data.Set as S
import Program.Formulae
import Program.ProofStatement (ProofStatement)
import qualified Program.ProofStatement as PS

data Theorem = Theorem Identifier PS.Goal PS.Context PS.ProofStatements

prove :: Theorem -> Either String (Maybe ProofStatement)
prove (Theorem iden (PS.Goal goal) initctx pss) =
  case success of
    Nothing -> if S.member goal ctx then Right Nothing else Left "goal not derived"
    Just fail -> Right $ Just fail
  where
    (success, PS.Context ctx) = runState (PS.proofcheck pss) initctx

-- TODO pretty print proof process (expand axioms and substitutions)
