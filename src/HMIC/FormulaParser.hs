{-# LANGUAGE InstanceSigs #-}

module HMIC.FormulaParser where

-- not as abstract as possible, but i'm already too confused on how to do it
-- very sloppily implemented, but hey, as long as it works. (it doesn't)

import Control.Applicative ((<|>))
import qualified Data.Map as M
import Parser
import Program.Formulae

type BindingPower = (Int, Int)

type Constructor a = a -> a -> a

data OpInfo a = OpInfo BindingPower (Constructor (Formula a))

binaryOps :: M.Map String (OpInfo a)
binaryOps =
  M.fromList
    [ ("&", OpInfo (30, 31) (:&:)),
      ("|", OpInfo (20, 21) (:|:)),
      ("->", OpInfo (11, 10) (:->:))
    ]

itoken :: Parser a -> Parser a
itoken p = intervals *> p <* intervals

parseVariable :: Parser (Formula a)
parseVariable = Variable <$> itoken (upper >>= \x -> (x :) <$> many' letter)

parseNegation :: Parser (Formula a)
parseNegation = Negation <$> (char '!' >> parseAtom)

parseQuantifier :: Parser (Formula a)
parseQuantifier = do
  q <- itoken (string "\\forall" <|> string "\\exists")
  var <- itoken (many' lower)
  _ <- char ','
  f <- itoken $ parseFormula 0
  pure $
    if q == "\\forall"
      then Forall var f
      else Exists var f

parseParentheses :: Parser (Formula a)
parseParentheses = do
  _ <- char '('
  f <- itoken $ parseFormula 0
  _ <- char ')'
  pure f

parseSubstitution :: Parser (Substitution a)
parseSubstitution = do
  _ <- char '['
  var <- itoken (many1 lower)
  f <- itoken $ parseFormula 0
  _ <- char ']'
  pure $ var :~> f

parseOperation :: Parser String
parseOperation = string "&" <|> string "|" <|> string "->"

parseAtom :: Parser (Formula a)
parseAtom = do
  atom <- parseVariable <|> parseNegation <|> parseQuantifier <|> parseParentheses
  subst <- many' parseSubstitution
  pure $ foldl With atom subst

parseFormula :: Int -> Parser (Formula a)
parseFormula curRbp = do
  lhs <- itoken parseAtom
  parseRhs curRbp lhs
  where
    parseRhs :: Int -> Formula a -> Parser (Formula a)
    parseRhs curRbp lhs = do
      operation <- optional $ itoken (lookahead parseOperation) -- the functional peek
      case operation of
        Nothing -> pure lhs
        Just op -> case M.lookup op binaryOps of
          Nothing -> zero
          Just (OpInfo (lbp, rbp) ctor) -> do
            if curRbp < lbp
              then do
                _ <- itoken parseOperation
                rhs <- parseFormula rbp
                parseRhs curRbp $ lhs `ctor` rhs
              else
                pure lhs

instance Parseable (Formula a) where
  parser :: Parser (Formula a)
  parser = parseFormula 0
