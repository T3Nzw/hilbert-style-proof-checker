module Program.ProofStatement where

import Control.Monad.State (MonadState (get), State, modify)
import Program.Axioms (matchToFormula)
import Program.Formulae (Formula (..), split, syntacticEq)
import qualified Program.Formulae as Formula
import Program.Rules

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy _ _ [] = False
elemBy cmp x (y : ys) = x `cmp` y || elemBy cmp x ys

data ProofStatement = Formula.Formula `By` Rule
  deriving (Show)

type ProofStatements = [ProofStatement]

newtype Goal = Goal Formula.Formula
  deriving (Show)

-- tradeoff of complexity for sanity (Data.Set requires the ADT to implement Ord
-- but i have a custom Eq implementation)
newtype Context = Context [Formula]
  deriving (Show)

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
will not cause the algorithm to fail; although the context is never really empty, it always contains bottom (Void) - well should it?
-}
proofcheck' :: Context -> ProofStatement -> Bool
-- SYNTACTIC EQUALITY.
proofcheck' (Context ctx) (f `By` AS) = elemBy syntacticEq f ctx
proofcheck' _ (f `By` AX axiom) = f == axiomFormula
  where
    axiomFormula = matchToFormula axiom
proofcheck' (Context ctx) (f `By` MP) = match ctx ctx f
  where
    -- O(n^2) duh
    match :: [Formula] -> [Formula] -> Formula -> Bool
    match _ [] _ = False
    match og (f : fs) b = elemBy syntacticEq (f :->: b) og || match og fs b
-- assuming split works correctly
proofcheck' (Context ctx) (Formula.Forall x f `By` GEN) = elemBy syntacticEq f ctx && notfree
  where
    notfree = all ((x `notElem`) . snd . split) ctx
proofcheck' _ _ = False

-- not necessary to return the context, but might be useful for future improvements :)
-- i think the state monad is my favourite, it's so fucking cool...
type ProofState = State Context (Maybe ProofStatement)

-- this one is a bit backwards - you would expect to return Just ...
-- if the function succeeds, and return Nothing if it fails :D
-- returns a value iff the proofcheck has failed, and if so, where - thus the Just value
proofcheck :: ProofStatements -> ProofState
-- foldr is for losers anyway
proofcheck [] = pure Nothing
proofcheck (x : xs) = do
  ctx <- get
  if proofcheck' ctx x
    then do
      let (f `By` _) = x
      modify $ \(Context c) -> Context $ c ++ [f]
      proofcheck xs
    else
      pure $ Just x
