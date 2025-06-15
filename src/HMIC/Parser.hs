module HMIC.Parser where

import Control.Applicative ((<|>))
import Parser
import qualified Program.Formulae as Formula
import Program.ProofStatement
import qualified Program.Rules as Rule
import Program.Theorem

keywords :: [String]
keywords = ["theorem", ":=", "assume", "begin", "by", "qed"]

parseIdentifier :: Parser String
parseIdentifier = upper >>= \x -> (x :) <$> many' (lower <|> digit <|> char '_')

parseFormula :: Parser Formula.Formula
parseFormula = do
  return Formula.Void

parseAssumptions :: Parser Program.ProofStatement.Context
parseAssumptions = do
  return $ Context []

parseProofStatement :: Parser ProofStatement
parseProofStatement = do
  return $ Formula.Void `By` Rule.AS

parseProofStatements :: Parser ProofStatements
parseProofStatements = do many' parseProofStatement

parseTheorem :: Parser Theorem
parseTheorem = do
  _ <- string "theorem"
  _ <- spaces1
  iden <- parseIdentifier

  return $ Theorem iden (Goal Formula.Void) (Context []) []
