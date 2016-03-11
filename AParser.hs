{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

-- Exercise 1

-- | apply function to first element of a pair
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

-- fmap :: (a -> b) -> f a -> f b
instance Functor Parser where
  fmap f p = Parser (\s -> case runParser p s of
                       Nothing     -> Nothing
                       Just (a, r) -> Just (f a, r))

-- Exercise 2
instance Applicative Parser where
  pure a             = Parser $ \s -> Just (a, s)
  (Parser fp) <*> xp = Parser $ \s -> case fp s of
                                 Nothing     -> Nothing
                                 Just (a, r) -> runParser (a <$> xp) r


-- Exercise 3

skipParse :: Parser a -> Parser ()
skipParse = fmap (const ())

-- | parse 'a' and 'b' and return ('a', 'b')
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- | parse 'a' and 'b' and return ()
abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

-- | parse two integers separated by a space
intPair :: Parser [Integer]
intPair = (\a _ b -> [a,b]) <$> posInt <*> satisfy isSpace <*> posInt


-- Exercise 4
instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser fp <|> xp = Parser $ \s -> fp s <|> runParser xp s


-- Exercise 5
-- | parse either an integer or an uppercase character
intOrUppercase :: Parser ()
intOrUppercase = skipParse posInt <|> skipParse (satisfy isUpper)
