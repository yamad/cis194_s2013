{-# OPTIONS_GHC -Wall #-}
-- CIS 194 (Spring 2013) Homework 04
-- written by Jason Yamada-Hanff
import Data.List ((\\))

-- Exercise 1, reimplement functions in 'wholemeal' style

-- | original fun1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- | reimplement fun1
fun1' :: [Integer] -> Integer
fun1' = product . map (+(-2)) . filter even


-- | original fun2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- | reimplement fun2
--
-- the key insight is to note that fun2 sums the even results of
-- repeated application of a function `next`
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate next
  where next n | even n    = n `div` 2
               | otherwise = 3*n + 1


-- Exercise 2, create balanced tree with a fold

-- | AVL tree. Each node stores its height
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- | height of tree
getHeight :: Tree a -> Integer
getHeight Leaf           = 0
getHeight (Node h _ _ _) = h

-- | calculate height of tree
calcNewHeight :: Tree a -> Tree a -> Integer
calcNewHeight left right = 1 + max (getHeight left) (getHeight right)


-- | return an AVL balanced tree with all elements in a list
foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insert Leaf


-- | insert element into an AVL tree
insert :: (Ord a) => a -> Tree a -> Tree a
insert a Leaf = Node 1 Leaf a Leaf
insert a (Node _ left b right) = balance $ Node newH newL b newR
  where cmp  = a `compare` b
        newL = if cmp == LT then insert a left  else left
        newR = if cmp == GT then insert a right else right
        newH = calcNewHeight newL newR

-- | balance an AVL tree
balance :: (Ord a) => Tree a -> Tree a
balance Leaf = Leaf
balance t@(Node _ left _ right) = newT
  where heightDiff = getHeight left - getHeight right
        newT
          | heightDiff >   1  = rightRotate t
          | heightDiff < (-1) = leftRotate  t
          | otherwise         = t

-- | rotate tree left (right subtree root becomes new root)
leftRotate :: Tree a -> Tree a
leftRotate Leaf                = Leaf
leftRotate t@(Node _ _ _ Leaf) = t
leftRotate (Node _ a b (Node _ c d e)) = Node newH newL d e
  where newL  = Node (calcNewHeight a c) a b c
        newH = calcNewHeight newL e

-- | rotate tree right (left subtree root becomes new root)
rightRotate :: Tree a -> Tree a
rightRotate Leaf                = Leaf
rightRotate t@(Node _ Leaf _ _) = t
rightRotate (Node _ (Node _ a b c) d e) = Node newH a b newR
  where newR = Node (calcNewHeight c e) c d e
        newH = calcNewHeight a newR


-- Exercise 3, more folds

-- | xor, as a fold
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x == True then not acc else acc) False

-- | map, as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []


-- Exercise 4, finding primes with a Sieve of Sundaram

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map toPrimes . (\\ sieveNums) $ [1..n]
  where sieveNums = takeWhile (<=n) . map numForm . ijPairs $ n
        ijPairs k        = cartProd [1..k] [1..(k `div` 2)]
        numForm (i, j) = i + j + 2 * i * j
        toPrimes k     = 2*k + 1

-- | Cartesian product (all possible pairs)
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]


main :: IO ()
main = do
  print $ fun1 [-5..5] == fun1' [-5..5]
  print $ fun1 [0..10] == fun1' [0..10]
  print $ fun1 [0,3..10] == fun1' [0,3..10]
  print $ foldTree ['A'..'J']
  print $ xor [True, False, True, True, False]
