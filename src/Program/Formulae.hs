{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Program.Formulae
  ( Formula (..),
    ConcreteSubstitution,
    ConcreteFormula,
    MetaFormula,
    Substitution (..),
    match,
  )
where

import Control.Monad.State
import qualified Data.Map as M

type Identifier = String

-- | phantom type for a map with string keys that is polymorphic over its values
type Context a b = M.Map Identifier b

data Substitution a = (:~>) {_var :: String, _formula :: Formula a}
  deriving (Eq, Ord)

-- because of disgusting record syntax default show implementation
instance Show (Substitution a) where
  show :: Substitution a -> String
  show (var :~> f) = "(" ++ var ++ " :~> " ++ show f ++ ")"

data Formula a
  = Void
  | Variable String
  | Negation (Formula a)
  | (Formula a) :&: (Formula a)
  | (Formula a) :|: (Formula a)
  | (Formula a) :->: (Formula a)
  | Forall String (Formula a)
  | Exists String (Formula a)
  | (Formula a) `With` (Substitution a)
  deriving (Show, Eq, Ord)

infixl 6 :&:

infixl 6 :|:

infixr 6 :->:

data Concrete

data Meta

type ConcreteSubstitution = Substitution Concrete

type ConcreteFormula = Formula Concrete

type MetaFormula = Formula Meta

--

data QuantifiedVariables

data FormulaVariables

type EqState = State (Context FormulaVariables ConcreteFormula) Bool

-- | match a concrete formula to a metaformula,
-- and return the result and generated context of constraints
match :: MetaFormula -> ConcreteFormula -> (Bool, Context FormulaVariables ConcreteFormula)
-- match a concrete formula to a metaformula (axiom schema)
-- both MetaFormula and ConcreteFormula share the same ADT
-- and only have a type-level distinction (since Formula is a phantom type)
match lhs rhs =
  runState (helper M.empty lhs rhs) M.empty
  where
    helper :: Context QuantifiedVariables String -> MetaFormula -> ConcreteFormula -> EqState
    helper _ Void Void = pure True
    helper qctx (Variable x) y =
      case M.lookup x qctx of
        Just z -> pure $ y == Variable z
        Nothing -> do
          fctx <- get
          case M.lookup x fctx of
            Nothing -> do
              modify $ M.insert x y
              pure True
            Just z -> pure $ y == z
    helper qctx (Negation lhs) (Negation rhs) = helper qctx lhs rhs
    helper qctx (lhs1 :&: rhs1) (lhs2 :&: rhs2) = do
      res1 <- helper qctx lhs1 lhs2
      res2 <- helper qctx rhs1 rhs2
      pure $ res1 && res2
    helper qctx (lhs1 :|: rhs1) (lhs2 :|: rhs2) = do
      res1 <- helper qctx lhs1 lhs2
      res2 <- helper qctx rhs1 rhs2
      pure $ res1 && res2
    helper qctx (lhs1 :->: rhs1) (lhs2 :->: rhs2) = do
      res1 <- helper qctx lhs1 lhs2
      res2 <- helper qctx rhs1 rhs2
      pure $ res1 && res2
    helper qctx (Forall x lhs) (Forall y rhs) =
      helper (M.insert x y qctx) lhs rhs
    helper qctx (Exists x lhs) (Exists y rhs) =
      helper (M.insert x y qctx) lhs rhs
    helper qctx (lhs `With` lsub) (rhs `With` rsub) = do
      let qctx' = M.insert lsub._var rsub._var qctx
      res1 <- helper qctx' lhs rhs
      res2 <- helper qctx' lsub._formula rsub._formula
      pure $ res1 && res2
    helper _ _ _ = pure False
