{-# LANGUAGE InstanceSigs #-}

-- TODO fix consuming of whitespaces/tabs/newlines

module HMIC.Parser where

import Control.Applicative ((<|>))
import Control.Monad (void)
import qualified Data.Set as S
import qualified Defs.Axioms as Axiom
import qualified Defs.Formulae as Formula
import Defs.ProofStatement
import qualified Defs.Rules as Rule
import Defs.Theorem
import HMIC.FormulaParser (itoken)
import Parser

-- could've potentially used maps
rules :: [String]
rules = ["AS", "AX", "MP", "GEN", "TH"]

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

parseComment :: Parser ()
parseComment = void $ intervals >> string ";" >> many' (sat (\x -> x /= '\n' && x /= '\r')) >> optional (char '\n')

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

parseAssumptions :: Parser Context
parseAssumptions = do
  _ <- label "missing keyword assume" $ string "assume"
  label "invalid assumptions syntax" $ emptyCtx <|> parseAssumptions'
  where
    parseAssumptions' :: Parser Context
    parseAssumptions' = do
      let commaP = many' $ token parseFormula <* char ','
      let dotP = many1 $ token parseFormula <* char '.'
      Context . S.fromList <$> (intervals1 >> (commaP >>= \x -> (x ++) <$> dotP))
    emptyCtx :: Parser Context
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
    "TH" -> Rule.TH <$> (intervals1 >> parseIdentifier)
    _ -> error "this can never happen"

parseProofStatement :: Parser ProofStatement
parseProofStatement = do
  _ <- many' parseComment
  statement <- parseFormula
  _ <- intervals
  _ <- label "missing keyword by" $ string "by"
  _ <- intervals1
  rule <- itoken parseRule
  _ <- label "missing end of statement delimiter \".\" after rule" $ char '.'
  _ <- many' parseComment
  return $ statement `By` rule

parseOof :: Parser ProofStatement
parseOof = parseComment *> (string "oof" >> intervals >> char '.' >> pure Oof) <* parseComment

parseProofStatements :: Parser ProofStatements
parseProofStatements = many' (itoken parseOof <|> itoken parseProofStatement)

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
  _ <- many' parseComment
  _ <- itoken $ label "missing keyword theorem" $ string "theorem"
  iden <- parseIdentifier
  _ <- itoken $ label "missing symbol :=" $ string ":="
  _ <- many' parseComment
  ctx <- itoken parseAssumptions
  _ <- many' parseComment
  goal <- itoken parseGoal
  _ <- many' parseComment
  _ <- itoken parseBegin
  _ <- many' parseComment
  pss <- itoken parseProofStatements
  _ <- many' parseComment
  _ <- itoken parseQed
  _ <- many' parseComment
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
  deriving (Show)

parseTheorems :: Parser Theorems
parseTheorems = Theorems <$> all' parseTheorem

instance Parseable Theorems where
  parser :: Parser Theorems
  parser = parseTheorems
