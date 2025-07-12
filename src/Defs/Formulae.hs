module Defs.Formulae where

data Formula a
  = Void
  | Variable String
  | PropVariable String
  | Negation (Formula a)
  | (Formula a) :&: (Formula a)
  | (Formula a) :|: (Formula a)
  | (Formula a) :->: (Formula a)
  | Forall String (Formula a)
  | Exists String (Formula a)
  | Predicate (Formula a) [Formula a]
  deriving (Eq, Ord)

isComplexFormula :: Formula a -> Bool
isComplexFormula (_ :&: _) = True
isComplexFormula (_ :|: _) = True
isComplexFormula (_ :->: _) = True
isComplexFormula (Forall _ _) = True
isComplexFormula (Exists _ _) = True
isComplexFormula _ = False

parenthesiseFormula :: Formula a -> String
parenthesiseFormula f =
  if isComplexFormula f
    then "(" ++ show f ++ ")"
    else show f

instance Show (Formula a) where
  show Void = "⊥"
  show (Variable x) = x
  show (PropVariable x) = x
  show (Negation f) = "¬" ++ parenthesiseFormula f
  show (lhs :&: rhs) = show lhs ++ " ∧ " ++ parenthesiseFormula rhs
  show (lhs :|: rhs) = show lhs ++ " ∨ " ++ parenthesiseFormula rhs
  show (lhs :->: rhs) = parenthesiseFormula lhs ++ " → " ++ show rhs
  show (Forall x f) = "∀" ++ x ++ ", " ++ parenthesiseFormula f
  show (Exists x f) = "∃" ++ x ++ ", " ++ parenthesiseFormula f
  show (Predicate p v) = parenthesiseFormula p ++ "(" ++ showArgs v ++ ")"
    where
      showArgs :: [Formula a] -> String
      showArgs [] = ""
      showArgs [x] = show x
      showArgs (x : xs) = show x ++ ", " ++ showArgs xs

infixl 9 :&:

infixl 8 :|:

infixr 7 :->:

data Concrete

data Meta

type ConcreteFormula = Formula Concrete

type MetaFormula = Formula Meta
