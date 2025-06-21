{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}

module HMIC.Parser where

import Control.Applicative ((<|>))
import Control.Monad (void)
import qualified Data.Set as S
import Parser
import qualified Program.Axioms as Axiom
import qualified Program.Formulae as Formula
import Program.ProofStatement
import qualified Program.Rules as Rule
import Program.Theorem
import qualified Utils

intervals :: Parser ()
intervals = void $ many' (char ' ' <|> char '\t' <|> char '\n')

intervals1 :: Parser ()
intervals1 = void $ many1 (char ' ' <|> char '\t' <|> char '\n')

-- not too sure what i would need them for:)
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
parseIdentifier = label "invalid identifier name" $ upper >>= \x -> (x :) <$> many' (lower <|> digit <|> char '_')

parseFormula :: Parser Formula.ConcreteFormula
parseFormula = do
  -- consume all characters until "by" is encountered,
  -- meaning everything that comprises the formula in a statement
  -- is parsed and then tokenised for further precedence parsing
  -- lowkey doesn't work since a formula might also be followed by ',' or '.'
  -- tokens <- consumeUntil "by" >>= \s -> pure $ Utils.tokenise s
  -- TODO precedence parsing...
  _ <- string "Void"
  return Formula.Void

parseGoal :: Parser Goal
parseGoal = do
  _ <- label "missing keyword goal" $ string "goal"
  _ <- intervals1
  f <- parseFormula
  _ <- intervals
  _ <- label "missing end of statement delimiter (.) after goal" $ char '.'
  return $ Goal f

-- TODO fix
parseAssumptions :: Parser Program.ProofStatement.Context
parseAssumptions = do
  _ <- label "missing keyword assume" $ string "assume"
  _ <- intervals1
  ctx <- many' (parseFormula <* (char ',' <|> char '.'))
  return $ Context $ S.fromList ctx

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
  rule <- parseRule
  _ <- intervals
  _ <- label "missing end of statement delimiter (.) after rule" $ char '.'
  return $ statement `By` rule

parseProofStatements :: Parser ProofStatements
parseProofStatements = many' parseProofStatement

parseBegin :: Parser ()
parseBegin = do
  _ <- intervals
  _ <- label "missing keyword begin" $ string "begin"
  _ <- intervals
  _ <- label "missing end of statement delimiter (.) after begin" $ char '.'
  return ()

parseQed :: Parser ()
parseQed = do
  _ <- intervals
  _ <- label "missing keyword qed" $ string "qed"
  _ <- intervals
  _ <- label "missing end of statement delimiter (.) after qed" $ char '.'
  return ()

-- add labels to the smaller parsers:)
parseTheorem :: Parser Theorem
parseTheorem = do
  _ <- intervals
  _ <- label "missing keyword theorem" $ string "theorem"
  _ <- intervals1
  iden <- parseIdentifier
  _ <- intervals
  _ <- label "missing symbol :=" $ string ":="
  _ <- intervals
  ctx <- parseAssumptions
  _ <- intervals
  goal <- parseGoal
  _ <- intervals
  _ <- parseBegin
  _ <- intervals
  pss <- parseProofStatements
  _ <- intervals
  _ <- parseQed
  _ <- intervals

  return $ Theorem iden goal ctx pss

newtype Theorems = Theorems [Theorem]

parseTheorems :: Parser Theorems
parseTheorems = Theorems <$> all' parseTheorem

instance Parseable Theorems where
  parser :: Parser Theorems
  parser = parseTheorems
