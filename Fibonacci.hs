{-# OPTIONS_GHC -Wall #-}
{-
  Fibonacci numbers
  CIS 194 (Spring 2013), Assignment 6

  by Jason Yamada-Hanff
-}

-- | straightforward recursive definition
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


-- | infinite list of Fibonacci numbers, naive recursive definition
fibs1 :: [Integer]
fibs1 = map fib [0..]


-- | iterative version
fibs2 :: [Integer]
fibs2 = map fst $ iterate fibIter (0, 1)
  where fibIter (m, n) = (n, m+n)


-- | stream datatype, a stream is a value followed by a stream
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : streamToList as

instance Show a => Show (Stream a) where
  show s = (concat . map ((++",") . show) . take 20 . streamToList) s ++ "..."

-- | apply a function to every element of a stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

-- | generate a Stream from an initial value and a function that gives the next value
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- | return an infinite list of identical elements
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- | interleave two streams (a,a -> b,b -> a,b,a,b,...)
streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons a as) bs = Cons a (streamInterleave bs as)

-- | natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 1

-- | ruler function (highest power of 2 that evenly divides given number n)
--   implementation taken from Haskell mailing list
ruler :: Stream Integer
--ruler = foldr1 streamInterleave (map streamRepeat [0..])
ruler = nthStream 0
  where nthStream n = streamInterleave (streamRepeat n) (nthStream (n+1))
