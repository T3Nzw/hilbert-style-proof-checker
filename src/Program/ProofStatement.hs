module Program.ProofStatement where

import Control.Monad.State (MonadState (get), State, modify)
import qualified Data.Map.Ordered as Ord
import qualified Data.Set as S
import Debug.Trace
import Defs
import Defs.ProofStatement
import qualified Defs.ProofStatement as PS
import Defs.Rules
import Defs.Theorem
import Program.Axioms (matchToMetaFormula)
import qualified Program.Formulae as Formula
import Unsafe.Coerce (unsafeCoerce)
import Utils (isFreeIn)
import qualified Utils

{-
basic algorithm layout:
1. pattern match the rule in the proof statement:
  1.1. AS -> look up the formula in the context
  1.2. AX -> pattern match the argument of the rule (axiom name) -> return the correct axiom formula
  1.3. MP -> for the formula B in the proof statement, try to identify a formula A s.t. A and A -> B are both in the context
  1.4. GEN -> for the formula \forall x. A in the proof statement, look up the term A in the context -> fail if x is free in the context
2. if 1 fails, report an error for said proof statement
3. if 1 succeeds:
  3.1. AS -> ok
  3.2. AX -> compare the formula in the proof statement with the axiom formula
  3.3. MP -> ok
  3.4. ok
  add the formula to the context.
note that AX doesn't require that the context has any assumptions! so an initially "empty" context
will not cause the algorithm to fail
-}

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

-- not necessary to return the context, but might be useful for future improvements :)
-- i think the state monad is my favourite, it's so fucking cool...
type ProofState = State Context (Maybe ProofStatement)

-- this one is a bit backwards - you would expect to return Just ...
-- if the function succeeds, and return Nothing if it fails :D
-- returns a value iff the proofcheck has failed, and if so, where - thus the Just value

-- | validate a series of proof statements
proofcheck :: ProvedTheorems -> ProofStatements -> ProofState
-- foldr is for losers anyway
proofcheck _ [] = pure Nothing
proofcheck ts (x : xs) = do
  ctx <- get
  if proofcheck' ts ctx x
    then do
      let (f `By` _) = x
      modify $ \(Context c) -> Context $ S.insert f c
      proofcheck ts xs
    else
      pure $ Just x
