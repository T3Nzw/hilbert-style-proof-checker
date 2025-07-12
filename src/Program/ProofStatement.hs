module Program.ProofStatement where

import Control.Monad.State (MonadState (get), State, modify)
import qualified Data.Map.Ordered as Ord
import qualified Data.Set as S
import Defs.ProofStatement
import Defs.Rules
import Defs.Theorem
import Program.Axioms (matchToMetaFormula)
import qualified Program.Formulae as Formula
import Unsafe.Coerce (unsafeCoerce)
import Utils (isFreeIn)
import qualified Utils

-- | validate a single proof statement
proofcheck' :: ProvedTheorems -> Context -> ProofStatement -> Bool
proofcheck' _ (Context ctx) (f `By` AS) = S.member f ctx
proofcheck' _ _ (f `By` AX axiom) = fst (Formula.match axiomFormula f) && Utils.validQuantified f && Utils.validFree f
  where
    axiomFormula = matchToMetaFormula axiom
proofcheck' _ (Context ctx) (f `By` MP) = match ctx (S.toList ctx) f
  where
    match :: S.Set Formula.ConcreteFormula -> [Formula.ConcreteFormula] -> Formula.ConcreteFormula -> Bool
    match _ [] _ = False
    match og (f : fs) b = S.member (f Formula.:->: b) og || match og fs b
proofcheck' _ (Context ctx) (Formula.Forall x f `By` GEN) = S.member f ctx && notfree
  where
    notfree = not $ all (x `isFreeIn`) ctx
proofcheck' ts _ (f `By` TH iden) =
  case Ord.lookup iden ts of
    Nothing -> False
    Just (Theorem _ (Goal goal) _ _) -> fst $ Formula.match (unsafeCoerce goal :: Formula.MetaFormula) f -- this is safe, i promise. actually im not so sure. dunno what it does.
proofcheck' _ _ _ = False

type ProofState = State Context (Maybe ProofStatement)

-- | validate a series of proof statements
proofcheck :: ProvedTheorems -> ProofStatements -> ProofState
proofcheck _ [] = pure Nothing
proofcheck ts (x : xs) = do
  ctx <- get
  if proofcheck' ts ctx x
    then do
      -- TODO: handle oof case
      let (f `By` _) = x
      modify $ \(Context c) -> Context $ S.insert f c
      proofcheck ts xs
    else
      pure $ Just x
