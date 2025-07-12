module Defs.ProofStatement where

import qualified Data.Set as S
import Defs.Formulae
import Defs.Rules

data ProofStatement = ConcreteFormula `By` Rule | Oof
  deriving (Show, Eq, Ord)

type ProofStatements = [ProofStatement]

newtype Goal = Goal ConcreteFormula
  deriving (Show, Eq, Ord)

newtype Context = Context {_ctx :: S.Set ConcreteFormula}

instance Show Context where
  show (Context ctx) = '{' : helper (S.toList ctx)
    where
      helper [] = "}"
      helper (x : xs@(_ : _)) = "  " ++ show x ++ "\n, " ++ helper xs
      helper (x : xs) = "  " ++ show x ++ "\n" ++ helper xs
