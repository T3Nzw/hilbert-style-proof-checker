{-# LANGUAGE InstanceSigs #-}

-- TODO fix consuming of whitespaces/tabs/newlines

module HMIC.Parser where

import Control.Applicative ((<|>))
import qualified Data.Set as S
import HMIC.FormulaParser (itoken)
import Parser
import qualified Program.Axioms as Axiom
import qualified Program.Formulae as Formula
import Program.ProofStatement
import qualified Program.Rules as Rule
import Program.Theorem

-- could've potentially used maps
rules :: [String]
rules = ["AS", "AX", "MP", "GEN"]

axioms :: [String]
axioms =
  [ "K",
    "CONJ_ELIM",
    "CONJ_INTRO",
    "DISJ_INTRO",
    "DISJ_ELIM",
    "UNIV_WITNESS",
    "UNIV_IMPLIES",
    "SUB_EXIST",
    "EX_IMPLIES",
    "EX_FALSO",
    "STAB",
    "S"
  ]

parseIdentifier :: Parser String
parseIdentifier = label "invalid identifier name" $ upper >>= \x -> (x :) <$> many' (letter <|> digit <|> char '_')

parseFormula :: Parser Formula.ConcreteFormula
parseFormula = parser

parseGoal :: Parser Goal
parseGoal = do
  _ <- label "missing keyword goal" $ string "goal"
  _ <- intervals1
  f <- itoken parseFormula
  _ <- label "missing end of statement delimiter \".\" after goal" $ char '.'
  return $ Goal f

parseAssumptions :: Parser Program.ProofStatement.Context
parseAssumptions = do
  _ <- label "missing keyword assume" $ string "assume"
  label "invalid assumptions syntax" $ emptyCtx <|> parseAssumptions'
  where
    parseAssumptions' :: Parser Program.ProofStatement.Context
    parseAssumptions' = do
      let commaP = many' $ token parseFormula <* char ','
      let dotP = many1 $ token parseFormula <* char '.'
      Context . S.fromList <$> (intervals1 >> (commaP >>= \x -> (x ++) <$> dotP))
    emptyCtx :: Parser Program.ProofStatement.Context
    emptyCtx = intervals >> char '.' >> pure (Context S.empty)

parsePosition :: Parser Axiom.Position
parsePosition = do
  pos <- label "invalid axiom position" $ oneOfS ["left", "right"]
  return $ case pos of
    "left" -> Axiom.Lhs
    "right" -> Axiom.Rhs
    _ -> error "this can never happen"

-- TODO create a map bc writing these by hand is waaaay
-- too error-prone
parseAxiom :: Parser Axiom.Axiom
parseAxiom = do
  axiom <- label "invalid axiom name" $ oneOfS axioms
  case axiom of
    "S" -> return Axiom.S
    "K" -> return Axiom.K
    "CONJ_ELIM" -> Axiom.CONJ_ELIM <$> (intervals1 >> parsePosition)
    "CONJ_INTRO" -> return Axiom.CONJ_INTRO
    "DISJ_INTRO" -> Axiom.DISJ_INTRO <$> (intervals1 >> parsePosition)
    "DISJ_ELIM" -> return Axiom.DISJ_ELIM
    "UNIV_WITNESS" -> return Axiom.UNIV_WITNESS
    "UNIV_IMPLIES" -> return Axiom.UNIV_IMPLIES
    "SUB_EXIST" -> return Axiom.SUB_EXIST
    "EX_IMPLIES" -> return Axiom.EX_IMPLIES
    "EX_FALSO" -> return Axiom.EX_FALSO
    "STAB" -> return Axiom.STAB
    _ -> error "this can never happen"

parseRule :: Parser Rule.Rule
parseRule = do
  rule <- label "invalid rule name" $ oneOfS rules
  case rule of
    "AS" -> return Rule.AS
    "AX" -> Rule.AX <$> (intervals1 >> parseAxiom)
    "MP" -> return Rule.MP
    "GEN" -> return Rule.GEN
    _ -> error "this can never happen"

parseProofStatement :: Parser ProofStatement
parseProofStatement = do
  _ <- intervals
  statement <- parseFormula
  _ <- intervals
  _ <- label "missing keyword by" $ string "by"
  _ <- intervals1
  rule <- itoken parseRule
  _ <- label "missing end of statement delimiter \".\" after rule" $ char '.'
  return $ statement `By` rule

parseOof :: Parser ProofStatement
parseOof = string "oof" >> intervals >> char '.' >> pure Oof

parseProofStatements :: Parser ProofStatements
parseProofStatements = many' (parseProofStatement <|> parseOof)

parseBegin :: Parser ()
parseBegin = do
  _ <- itoken $ label "missing keyword begin" $ string "begin"
  _ <- label "missing end of statement delimiter \".\" after begin" $ char '.'
  return ()

parseQed :: Parser ()
parseQed = do
  _ <- itoken $ label "missing keyword qed" $ string "qed"
  _ <- label "missing end of statement delimiter \".\" after qed" $ char '.'
  return ()

parseTheorem :: Parser Theorem
parseTheorem = do
  _ <- itoken $ label "missing keyword theorem" $ string "theorem"
  iden <- parseIdentifier
  _ <- itoken $ label "missing symbol :=" $ string ":="
  ctx <- itoken parseAssumptions
  goal <- itoken parseGoal
  _ <- itoken parseBegin
  pss <- itoken parseProofStatements
  _ <- itoken parseQed
  return $ Theorem iden goal ctx pss

-- TODO pretty print proofs :)
parsePrint :: Parser String
parsePrint = do
  _ <- string "print"
  _ <- intervals1
  iden <- parseIdentifier
  _ <- intervals
  _ <- char '.'
  pure iden

newtype Theorems = Theorems [Theorem]

parseTheorems :: Parser Theorems
parseTheorems = Theorems <$> all' parseTheorem

instance Parseable Theorems where
  parser :: Parser Theorems
  parser = parseTheorems
