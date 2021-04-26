module Parser where

import Control.Applicative
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List
import Text.Read

newtype Parser a t = Parser {runParser :: [a] -> Maybe (t, [a])}

satisfy :: (Eq a) => (a -> Bool) -> Parser a a
satisfy predicate = Parser func
  where
    func [] = Nothing -- fail on empty input
    func (x : xs)
      | predicate x = Just (x, xs)
      | otherwise = Nothing

prefix :: Eq a => [a] -> Parser a [a]
prefix str = Parser func
  where
    func x = if str `isPrefixOf` x then Just (take (length str) x, drop (length str) x) else Nothing

notPrefix :: Eq a => [a] -> Parser a [a]
notPrefix str = Parser func
  where
    func x = if not $ str `isPrefixOf` x then Just (take 1 x, drop 1 x) else Nothing

letter :: Parser Char Char
letter = satisfy isAlpha

digit :: Parser Char Char
digit = satisfy isDigit

char :: Char -> Parser Char Char
char = satisfy . (==)

times :: Integer -> Parser t a -> Parser t [a]
times x p = if x <= 1 then (: []) <$> p else (:) <$> p <*> times (x - 1) p

anyOf :: (Eq a, Foldable t) => t a -> Parser a a
anyOf chars = satisfy $ flip elem chars

anythingBut :: (Eq a, Show a) => a -> Parser a a
anythingBut = satisfy . (/=)

nothing :: Parser a ()
nothing = Parser $ \tokens -> if null tokens then Just ((), tokens) else Nothing

first :: (a -> b) -> Parser t a -> Parser t b
first mapper (Parser runParserInstance1) = Parser runParserInstance2
  where
    runParserInstance2 inputString = case runParserInstance1 inputString of
      Nothing -> Nothing
      Just (x, xs) -> Just (mapper x, xs)

instance Functor (Parser a) where
  fmap = first

instance Alternative (Parser a) where
  empty = Parser (const Nothing)
  (<|>) (Parser f1) (Parser f2) = Parser f3
    where
      f3 inputString = f1 inputString <|> f2 inputString

pureParser :: a -> Parser t a
pureParser val = Parser (\x -> Just (val, x))

parserApply :: (a -> b -> c) -> Parser t a -> Parser t b -> Parser t c
parserApply mapper (Parser f1) (Parser f2) = Parser f3
  where
    f3 inputString = do
      (val1, rest1) <- f1 inputString
      (val2, rest2) <- f2 rest1
      pure (mapper val1 val2, rest2)

instance Applicative (Parser a) where
  pure = pureParser
  (<*>) = parserApply id

zeroOrMore :: Parser t a -> Parser t [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser t a -> Parser t [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

maybeOne :: Parser a1 a2 -> Parser a1 [a2]
maybeOne p = Parser func
  where
    func inp = case runParser p inp of
      Nothing -> Just ([], inp)
      Just (x, xs) -> Just ([x], xs)

eatWhitespace :: Parser Char String
eatWhitespace = zeroOrMore (satisfy isSpace)

space :: Parser Char [Char]
space = oneOrMore (satisfy isSpace)

intParser = Parser f
  where
    f inputString
      | null numberlikeString = Nothing
      | otherwise = Just (numberlikeString, rest)
      where
        (numberlikeString, rest) = case inputString of
          '-' : xs -> let (r, rs) = span isDigit xs in ('-' : r, rs)
          xs -> span isDigit xs

posIntParser = Parser f
  where
    f inputString
      | null numberlikeString = Nothing
      | otherwise = Just (numberlikeString, rest)
      where
        (numberlikeString, rest) = span isDigit inputString
