{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

-- resources used:
-- https://people.cs.nott.ac.uk/pszgmh/monparsing.pdf

module Parser where

import Control.Applicative
import Control.Monad
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)

newtype Parser a = Parser {_run :: String -> [(a, String)]}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser (\input -> [(f a, res) | (a, res) <- p input])

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = Parser (\input -> [(v, input)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> (Parser p) = Parser $ \input -> [(f x, res) | (f, int) <- pf input, (x, res) <- p int]

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p) >>= f = Parser $ \input ->
    concatMap
      ( \(x, res) ->
          let (Parser p') = f x in p' res
      )
      (p input)

instance Alternative Parser where
  empty :: Parser a
  empty = zero

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser $ \input -> case _run (p `plus` q) input of
    [] -> []
    (x : _) -> [x]

result :: a -> Parser a
result = return

zero :: Parser a
zero = Parser $ const []

item :: Parser Char
item = Parser $ \case
  [] -> []
  (x : xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \input -> if p input then result input else zero

plus :: Parser a -> Parser a -> Parser a
plus p q = Parser $ \input -> _run p input ++ _run q input

char :: Char -> Parser Char
char x = sat (== x)

lower :: Parser Char
lower = sat isAsciiLower

upper :: Parser Char
upper = sat isAsciiUpper

letter :: Parser Char
letter = lower <|> upper

digit :: Parser Char
digit = sat isDigit

alphanum :: Parser Char
alphanum = letter <|> digit

ascii :: Parser Char
ascii = sat isAscii

space :: Parser Char
space = sat (== ' ')

tab :: Parser Char
tab = sat (== '\t')

newline :: Parser Char
newline = sat (== '\n')

interval :: Parser Char
interval = space <|> tab

string :: String -> Parser String
string "" = result ""
string (x : xs) = char x >> string xs >> return (x : xs)

many' :: Parser a -> Parser [a]
many' p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many' p <|> return []
  return (x : xs)

zeroOrOne :: Parser a -> Parser [a]
zeroOrOne p = (: []) <$> p <|> return []

repeatP :: Int -> Parser a -> Parser [a]
repeatP 0 _ = pure []
repeatP n p = p >>= \x -> (x :) <$> repeatP (n - 1) p

-- TODO potentially make it somewhat polymorphic
oneOf :: [Char] -> Parser Char
oneOf = foldr (\x xs -> char x <|> xs) zero

oneOfS :: [String] -> Parser String
oneOfS = foldr (\x xs -> string x <|> xs) zero

spaces :: Parser ()
spaces = void $ many' space

spaces1 :: Parser ()
spaces1 = void $ many1 space

token :: Parser a -> Parser a
token p = spaces *> p <* spaces

eof :: Parser ()
eof = Parser $ \input -> ([((), "") | null input])

all :: Parser String
all = many' item

branch :: Bool -> Parser a -> Parser a -> Parser a
branch b t f = Parser $ \input -> if b then _run t input else _run f input

tokeniseBy :: Parser Char -> Parser [String]
tokeniseBy p = many' (many1 p <* spaces)

tokenise :: Parser [String]
tokenise = tokeniseBy notInterval
  where
    notInterval = sat (\x -> x /= ' ' && x /= '\n' && x /= '\t')

-- a lookahead parser...
consumeUntil :: String -> Parser String
consumeUntil s = Parser $ \input -> _run (repeatP (index 0 input s) item) input -- eww
  where
    index :: Int -> String -> String -> Int
    index n [] _ = n
    index n str substr =
      if take len str == substr
        then n
        else index (n + 1) (tail str) substr
      where
        len = length substr

class Parseable a where
  parse :: String -> Either String a
