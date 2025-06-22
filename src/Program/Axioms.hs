{-# LANGUAGE DataKinds #-}

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
  | UNIV_WITNESS
  | UNIV_IMPLIES
  | SUB_EXIST
  | EX_IMPLIES
  | EX_FALSO
  | STAB
  deriving (Show, Eq, Ord)

-- axiom schemas in:

-- minimal logic

s :: MetaFormula
s = (Variable "A" :->: Variable "B" :->: Variable "C") :->: (Variable "A" :->: Variable "B") :->: Variable "A" :->: Variable "C"

k :: MetaFormula
k = Variable "A" :->: Variable "B" :->: Variable "A"

conjElimLeft :: MetaFormula
conjElimLeft = (Variable "A" :&: Variable "B") :->: Variable "B"

conjElimRight :: MetaFormula
conjElimRight = (Variable "A" :&: Variable "B") :->: Variable "A"

conjIntro :: MetaFormula
conjIntro = Variable "A" :->: Variable "B" :->: (Variable "A" :&: Variable "B")

disjIntroLeft :: MetaFormula
disjIntroLeft = Variable "B" :->: (Variable "A" :|: Variable "B")

disjIntroRight :: MetaFormula
disjIntroRight = Variable "A" :->: (Variable "A" :|: Variable "B")

disjElim :: MetaFormula
disjElim = (Variable "A" :->: Variable "C") :->: (Variable "B" :->: Variable "C") :->: (Variable "A" :|: Variable "B") :->: Variable "C"

univWitness :: MetaFormula
univWitness = Forall "x" (Variable "A") :->: Variable "A" `With` ("x" :~> Variable "t")

univImplies :: MetaFormula -- where x not in FV(B)
univImplies = Forall "x" (Variable "B" :->: Variable "A") :->: (Variable "B" :->: Forall "x" (Variable "A"))

subExist :: MetaFormula -- where x not in FV(B)
subExist = Variable "A" `With` ("x" :~> Variable "t") :->: Exists "x" (Variable "A")

exImplies :: MetaFormula
exImplies = Forall "x" (Variable "A" :->: Variable "B") :->: (Exists "x" (Variable "A") :->: Variable "B")

-- intuitionistic logic

exFalso :: MetaFormula
exFalso = Void :->: Variable "A"

-- classical logic

stab :: MetaFormula
stab = Negation (Negation (Variable "A")) :->: Variable "A"

matchToMetaFormula :: Axiom -> MetaFormula
matchToMetaFormula S = s
matchToMetaFormula K = k
matchToMetaFormula (CONJ_ELIM Lhs) = conjElimLeft
matchToMetaFormula (CONJ_ELIM Rhs) = conjElimRight
matchToMetaFormula CONJ_INTRO = conjIntro
matchToMetaFormula (DISJ_INTRO Lhs) = disjIntroLeft
matchToMetaFormula (DISJ_INTRO Rhs) = disjIntroRight
matchToMetaFormula DISJ_ELIM = disjElim
matchToMetaFormula UNIV_WITNESS = univWitness
matchToMetaFormula UNIV_IMPLIES = univImplies
matchToMetaFormula SUB_EXIST = subExist
matchToMetaFormula EX_IMPLIES = exImplies
matchToMetaFormula EX_FALSO = exFalso
matchToMetaFormula STAB = stab
