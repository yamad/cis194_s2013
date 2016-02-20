{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List (group, sort)

-- Exercise 1 - hopscotch

-- | list of lists where the nth-element is the input list with every nth-element
skips :: [a] -> [[a]]
skips xs = map (map (xs!!) . countByK n) [1..n]
  where n = length xs

-- | return a list from k to n, counting by k
countByK :: Int -> Int -> [Int]
countByK n k = map (+(-1)) (enumFromThenTo k (k*2) n)


-- Exercise 2 - local maxima

-- | return all local maxima (elements greater than immediate neighbors) of a list
localMaxima :: [Integer] -> [Integer]
localMaxima xs = map fst . filter (uncurry (==)) $ zip mids maxes
  where ws    = window 3 xs
        maxes = map maximum ws
        mids  = map (!!1) ws

window :: Int -> [a] -> [[a]]
window _ [] = []
window n xs@(_:xs')
  | length xs < n = []
  | otherwise     = take n xs : window n xs'


-- Exercise 3 - histogram

-- | print a histogram that displays the occurrences of each digit (0-9) in a list
histogram :: [Integer] -> String
histogram xs = unlines . reverse $ axis ++ graph
  where axis  = ["0123456789", replicate 10 '=']
        graph = (histogramLines . counts0to9) xs

-- | count number of occurrences of numbers 0 through 9
--
-- to ensure that each number is represented in the count, ([0..9]++)
-- adds at least one instance of each number (0-9) to the list. these
-- extra instances are removed at the end with `decrementToZero`.
counts0to9 :: [Integer] -> [Int]
counts0to9 = map (decrementToZero . length) . group . sort
             . ([0..9]++) . filter (\x -> 0 <= x && x <= 9)

-- | format points in a histogram
--
-- lines are generated from the 'bottom up' on the y-axis (lowest
-- point on graph is generated first)
histogramLines :: [Int] -> [String]
histogramLines xs
  | sum xs == 0 = []
  | otherwise   = formatLine xs : histogramLines (map decrementToZero xs)
  where formatLine = map putPoint
        putPoint 0 = ' '
        putPoint _ = '*'

-- | decrement over the natural numbers (only to 0)
decrementToZero :: Integral a => a -> a
decrementToZero a
  | a <= 0    = 0
  | otherwise = a - 1


-- Tests
main :: IO ()
main = do
  print $ skips "ABCD"             == ["ABCD", "BD", "C", "D"]
  print $ skips "hello!"           == ["hello!", "el!", "l!", "l", "o", "!"]
  print $ skips ([1] :: [Integer]) == [[1]]
  print $ skips [True, False]      == [[True, False], [False]]
  print $ skips ([] :: [Bool])     == []
  print $ localMaxima [2,9,5,6,1]  == [9,6]
  print $ localMaxima [2,3,4,1,5]  == [4]
  print $ localMaxima [1,2,3,4,5]  == []
  putStr $ histogram [1,1,1,5]
  putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
