{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

-- TODO make parse errors in various combinators more descriptive

-- resources used:
-- https://people.cs.nott.ac.uk/pszgmh/monparsing.pdf
-- modified to be deterministic (unlike the implementation
-- described in the paper above)
-- using the Either monad :)
-- the previous one was technically also deteministic
-- since it would always return the head of the list
-- of all the parsed terms, but it didn't allow for
-- descriptive error messages

module Parser where

import Control.Applicative
import Control.Monad
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)

newtype ParseError = ParseError {_desc :: String}

instance Show ParseError where
  show :: ParseError -> String
  show (ParseError desc) =
    ">>> Parse error occured:"
      ++ "\n>>> Description: "
      ++ desc

newtype Parser a = Parser {_run :: String -> Either ParseError (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (res, rest) <- p input
    Right (f res, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = Parser $ \input -> Right (v, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> (Parser p) = Parser $ \input -> do
    (f, int) <- pf input
    (x, res) <- p int
    Right (f x, res)

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input
    let Parser p' = f x
    p' rest

instance Alternative Parser where
  empty :: Parser a
  empty = zero

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p) <|> (Parser q) = Parser $ \input ->
    case p input of
      Right res -> Right res
      Left err1 -> case q input of
        Right res -> Right res
        Left err2 -> Left $ ParseError $ "all alternatives failed: " ++ show err1 ++ "---\n" ++ show err2

result :: a -> Parser a
result = return

zero :: Parser a
zero = Parser $ \input -> Left $ ParseError ""

item :: Parser Char
item = Parser $ \case
  [] -> Left $ ParseError "no input to consume"
  (x : xs) -> Right (x, xs)

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \input -> if p input then result input else zero

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

-- | attempts to parse everything; otherwise it fails.
-- return the first parse error encountered, if one occurs.
all' :: Parser a -> Parser [a]
all' p = Parser $ \input -> do
  case _run (many' p) input of
    Left _ -> Left $ ParseError "this will never happen"
    Right (x, xs) -> case xs of
      [] -> Right (x, xs)
      _ -> case _run p xs of
        Left err -> Left err
        Right _ -> Left $ ParseError "how come it wouldn't have parsed it the first time?"

repeatP :: Int -> Parser a -> Parser [a]
repeatP 0 _ = pure []
repeatP n p = p >>= \x -> (x :) <$> repeatP (n - 1) p

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- TODO potentially make it somewhat polymorphic
oneOf :: [Char] -> Parser Char
oneOf = foldr (\x xs -> char x <|> xs) zero

oneOfS :: [String] -> Parser String
oneOfS = foldr (\x xs -> string x <|> xs) zero

spaces :: Parser ()
spaces = void $ many' space

spaces1 :: Parser ()
spaces1 = void $ many1 space

intervals :: Parser ()
intervals = void $ many' (char ' ' <|> char '\t' <|> char '\n')

intervals1 :: Parser ()
intervals1 = void $ many1 (char ' ' <|> char '\t' <|> char '\n')

token :: Parser a -> Parser a
token p = spaces *> p <* spaces

eof :: Parser ()
eof = Parser $ \case
  [] -> Right ((), "")
  _ -> Left $ ParseError "input not exhausted"

branch :: Bool -> Parser a -> Parser a -> Parser a
branch b t f = Parser $ \input -> if b then _run t input else _run f input

tokeniseBy :: Parser Char -> Parser [String]
tokeniseBy p = many' (many1 p <* spaces)

tokenise :: Parser [String]
tokenise = tokeniseBy notInterval
  where
    notInterval = sat (\x -> x /= ' ' && x /= '\n' && x /= '\t')

lookahead :: Parser a -> Parser a
lookahead p = Parser $ \input -> do
  case _run p input of
    Left err -> Left err
    Right (res, _) -> Right (res, input)

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

label :: String -> Parser a -> Parser a
label desc (Parser p) = Parser $ \input -> do
  case p input of
    Left _ -> Left $ ParseError desc
    Right res -> Right res

class Parseable a where
  parser :: Parser a
  evalP :: String -> Either ParseError a

  default parser :: (Read a) => Parser a
  parser = Parser $ \input -> do
    case reads input of
      [(x, rest)] -> Right (x, rest)
      _ -> Left $ ParseError "default parser implementation failed"

  default evalP :: String -> Either ParseError a
  evalP input = fst <$> _run parser input

  {-# MINIMAL parser #-}
