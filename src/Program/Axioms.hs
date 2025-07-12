{-# LANGUAGE DataKinds #-}

module Program.Axioms where

import Defs.Axioms
import Defs.Formulae

-- axiom schemas in:

-- minimal logic

s :: MetaFormula
s = (PropVariable "A" :->: PropVariable "B" :->: PropVariable "C") :->: (PropVariable "A" :->: PropVariable "B") :->: PropVariable "A" :->: PropVariable "C"

k :: MetaFormula
k = PropVariable "A" :->: PropVariable "B" :->: PropVariable "A"

conjElimLeft :: MetaFormula
conjElimLeft = (PropVariable "A" :&: PropVariable "B") :->: PropVariable "B"

conjElimRight :: MetaFormula
conjElimRight = (PropVariable "A" :&: PropVariable "B") :->: PropVariable "A"

conjIntro :: MetaFormula
conjIntro = PropVariable "A" :->: PropVariable "B" :->: (PropVariable "A" :&: PropVariable "B")

disjIntroLeft :: MetaFormula
disjIntroLeft = PropVariable "B" :->: (PropVariable "A" :|: PropVariable "B")

disjIntroRight :: MetaFormula
disjIntroRight = PropVariable "A" :->: (PropVariable "A" :|: PropVariable "B")

disjElim :: MetaFormula
disjElim = (PropVariable "A" :->: PropVariable "C") :->: (PropVariable "B" :->: PropVariable "C") :->: (PropVariable "A" :|: PropVariable "B") :->: PropVariable "C"

univWitness :: MetaFormula
univWitness = Forall "x" (Predicate (PropVariable "A") [Variable "x"]) :->: Predicate (PropVariable "A") [Variable "t"]

univImplies :: MetaFormula -- where x not in FV(B)
univImplies = Forall "x" (Variable "B" :->: Predicate (PropVariable "A") [Variable "x"]) :->: (Variable "B" :->: Forall "x" (Predicate (PropVariable "A") [Variable "x"]))

subExist :: MetaFormula -- where x not in FV(B)
subExist = Predicate (PropVariable "A") [Variable "t"] :->: Exists "x" (Predicate (PropVariable "A") [Variable "x"])

exImplies :: MetaFormula
exImplies = Forall "x" (Predicate (PropVariable "A") [Variable "x"] :->: Variable "B") :->: (Exists "x" (Predicate (PropVariable "A") [Variable "x"]) :->: Variable "B")

-- intuitionistic logic

exFalso :: MetaFormula
exFalso = Void :->: PropVariable "A"

-- classical logic

stab :: MetaFormula
stab = Negation (Negation (PropVariable "A")) :->: PropVariable "A"

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
