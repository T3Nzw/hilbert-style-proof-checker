module HMIC.Parser where

-- TODO spaces/spaces1 parsers should also parse newlines and tabs

import Control.Applicative ((<|>))
import qualified Data.Set as S
import Parser
import qualified Program.Axioms as Axiom
import qualified Program.Formulae as Formula
import Program.ProofStatement
import qualified Program.Rules as Rule
import Program.Theorem

keywords :: [String]
keywords = ["theorem", ":=", "assume", "begin", "by", "qed"]

-- could've potentially used maps
rules :: [String]
rules = ["AS", "AX", "MP", "GEN"]

axioms :: [String]
axioms =
  [ "S",
    "K",
    "CONJ_ELIM",
    "CONJ_INTRO",
    "DISJ_INTRO",
    "DISJ_ELIM",
    "UNIV_WITNESS",
    "UNIV_IMPLIES",
    "SUB_EXIST",
    "EX_IMPLIES",
    "EX_FALSO",
    "STAB"
  ]

parseIdentifier :: Parser String
parseIdentifier = upper >>= \x -> (x :) <$> many' (lower <|> digit <|> char '_')

parseFormula :: Parser Formula.ConcreteFormula
parseFormula = do
  return Formula.Void

parseGoal :: Parser Goal
parseGoal = do
  _ <- string "goal"
  _ <- spaces1
  f <- parseFormula
  _ <- spaces
  _ <- char '.'
  return $ Goal f

parseAssumptions :: Parser Program.ProofStatement.Context
parseAssumptions = do
  return $ Context S.empty

parsePosition :: Parser Axiom.Position
parsePosition = do
  pos <- oneOfS ["left", "right"]
  return $ case pos of
    "left" -> Axiom.Lhs
    "right" -> Axiom.Rhs
    _ -> error "this can never happen"

-- TODO create a map bc writing these by hand is waaaay
-- too error-prone
parseAxiom :: Parser Axiom.Axiom
parseAxiom = do
  axiom <- oneOfS axioms
  case axiom of
    "S" -> return Axiom.S
    "K" -> return Axiom.K
    "CONJ_ELIM" -> Axiom.CONJ_ELIM <$> parsePosition
    "CONJ_INTRO" -> return Axiom.CONJ_INTRO
    "DISJ_INTRO" -> Axiom.DISJ_INTRO <$> parsePosition
    "DISJ_ELIM" -> return Axiom.DISJ_ELIM
    "UNIV_WITNESS" -> Axiom.UNIV_WITNESS <$> parseFormula
    "UNIV_IMPLIES" -> return Axiom.UNIV_IMPLIES
    "SUB_EXIST" -> Axiom.SUB_EXIST <$> parseFormula
    "EX_IMPLIES" -> return Axiom.EX_IMPLIES
    "EX_FALSO" -> return Axiom.EX_FALSO
    "STAB" -> return Axiom.STAB
    _ -> error "this can never happen"

parseRule :: Parser Rule.Rule
parseRule = do
  rule <- oneOfS rules
  case rule of
    "AS" -> return Rule.AS
    "AX" -> Rule.AX <$> parseAxiom
    "MP" -> return Rule.MP
    "GEN" -> return Rule.GEN
    _ -> error "this can never happen"

parseProofStatement :: Parser ProofStatement
parseProofStatement = do
  statement <- parseFormula
  _ <- spaces1
  _ <- string "by"
  _ <- spaces1
  rule <- parseRule
  _ <- spaces
  _ <- char '.'
  return $ statement `By` rule

parseProofStatements :: Parser ProofStatements
parseProofStatements = do many' parseProofStatement

parseBegin :: Parser ()
parseBegin = do
  _ <- spaces
  _ <- string "begin"
  _ <- spaces
  _ <- char '.'
  return ()

parseQed :: Parser ()
parseQed = do
  _ <- spaces
  _ <- string "qed"
  _ <- spaces
  _ <- char '.'
  return ()

parseTheorem :: Parser Theorem
parseTheorem = do
  _ <- string "theorem"
  _ <- spaces1
  iden <- parseIdentifier
  _ <- spaces
  _ <- string ":="
  _ <- spaces
  ctx <- parseAssumptions
  _ <- spaces
  goal <- parseGoal
  _ <- spaces
  _ <- parseBegin
  _ <- spaces
  pss <- parseProofStatements
  _ <- spaces
  _ <- parseQed

  return $ Theorem iden goal ctx pss
