module Program.Axioms where

import Program.Formulae

data Position = Lhs | Rhs
  deriving (Show, Eq, Ord)

data Axiom
  = S
  | K
  | CONJ_ELIM Position
  | CONJ_INTRO
  | DISJ_INTRO Position
  | DISJ_ELIM
  | UNIV_WITNESS String
  | UNIV_IMPLIES
  | SUB_EXIST String
  | EX_IMPLIES
  | EX_FALSO
  | STAB
  deriving (Show, Eq, Ord)

-- really, i've represented A, B, and C as ordinary variables, but they're supposed to be metavariables :)
-- i don't think that breaks the equality algorithm? at least i hope not

-- axiom schemas in:

-- minimal logic

s :: Formula
s = (Variable "A" :->: Variable "B" :->: Variable "C") :->: (Variable "B" :->: Variable "C") :->: Variable "A" :->: Variable "C"

k :: Formula
k = Variable "A" :->: Variable "B" :->: Variable "A"

conjElimLeft :: Formula
conjElimLeft = (Variable "A" :&: Variable "B") :->: Variable "B"

conjElimRight :: Formula
conjElimRight = (Variable "A" :&: Variable "B") :->: Variable "A"

conjIntro :: Formula
conjIntro = Variable "A" :->: Variable "B" :->: (Variable "A" :&: Variable "B")

disjIntroLeft :: Formula
disjIntroLeft = Variable "B" :->: (Variable "A" :|: Variable "B")

disjIntroRight :: Formula
disjIntroRight = Variable "A" :->: (Variable "A" :|: Variable "B")

disjElim :: Formula
disjElim = (Variable "A" :->: Variable "C") :->: (Variable "B" :->: Variable "C") :->: (Variable "A" :|: Variable "B") :->: Variable "C"

univWitness :: Formula
univWitness = Forall "x" (Variable "A") :->: Variable "A" `With` ("x" :~> Variable "t")

univImplies :: Formula -- where x not in FV(B)
univImplies = Forall "x" (Variable "B" :->: Variable "A") :->: (Variable "B" :->: Forall "x" (Variable "A"))

subExist :: Formula -- where x not in FV(B)
subExist = Variable "A" `With` ("x" :~> Variable "t") :->: Exists "x" (Variable "A")

exImplies :: Formula
exImplies = Forall "x" (Variable "A" :->: Variable "B") :->: (Exists "x" (Variable "A") :->: Variable "B")

-- intuitionistic logic

exFalso :: Formula
exFalso = Void :->: Variable "A"

-- classical logic

stab :: Formula
stab = Negation (Negation (Variable "A")) :->: Variable "A"

matchToFormula :: Axiom -> Formula
matchToFormula S = s
matchToFormula K = k
matchToFormula (CONJ_ELIM Lhs) = conjElimLeft
matchToFormula (CONJ_ELIM Rhs) = conjElimRight
matchToFormula CONJ_INTRO = conjIntro
matchToFormula (DISJ_INTRO Lhs) = disjIntroLeft
matchToFormula (DISJ_INTRO Rhs) = disjIntroRight
matchToFormula DISJ_ELIM = disjElim
-- TODO potentially apply some substitution on the bound variable!
matchToFormula (UNIV_WITNESS subst) = univWitness
matchToFormula UNIV_IMPLIES = univImplies
-- TODO same as above
matchToFormula (SUB_EXIST subst) = subExist
matchToFormula EX_IMPLIES = exImplies
matchToFormula EX_FALSO = exFalso
matchToFormula STAB = stab
