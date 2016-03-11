{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-
  Join-Lists, from CIS 194 (Spring 2013), Assignment 7
-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m


-- | append two join lists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b


-- Exercise 2

treeSize :: (Sized m, Monoid m) => JoinList m a -> Int
treeSize = getSize . size . tag

-- | finds the JoinList at a given index
--
-- (indexJ i jl) == (jlToList jl !!? i)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty               = Nothing
indexJ 0 (Single _ a)        = Just a
indexJ _ (Single _ _)        = Nothing
indexJ 0 (Append _ (Single _ a) _)  = Just a
indexJ i (Append _ (Single _ _) l2) = indexJ (i-1) l2
indexJ i jl@(Append _ l1 l2)
  | i < 0 || i > treeSize jl = Nothing
  | i < treeSize l1          = indexJ i l1
  | otherwise                = indexJ (i - treeSize l1) l2


dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty        = Empty
dropJ 0 jl           = jl
dropJ _ (Single _ _) = Empty
dropJ i jl@(Append _ l1 l2)
  | i < 0            = jl
  | i >  treeSize jl = Empty
  | i >= treeSize l1 = dropJ (i - treeSize l1) l2
  | otherwise        = dropJ i l1 +++ l2

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty            = Empty
takeJ 0 _                = Empty
takeJ _ jl@(Single _ _)  = jl
takeJ i jl@(Append _ l1 l2)
  | i <  0               = Empty
  | i >  treeSize jl     = jl
  | i <= treeSize l1     = takeJ i l1
  | otherwise            = l1 +++ takeJ (i - treeSize l1) l2

instance Buffer (JoinList (Score, Size) String) where
 toString             = unlines . jlToList
 fromString           = makeJ makeSingleScoreSize
 line                 = indexJ
 replaceLine i new jl = takeJ i jl +++ fromString new +++ dropJ (i+1) jl
 numLines             = getSize . snd . tag
 value                = getScore . fst . tag

makeSingleScoreSize :: String -> JoinList (Score, Size) String
makeSingleScoreSize s = Single (scoreString s, Size 1) s

makeJ :: (Monoid b) => (String -> JoinList b String) -> String -> JoinList b String
makeJ felem s
  | n == 1    = felem s
  | otherwise = (joinJ . map (makeJ felem)) ls
  where ls = lines s
        n  = length ls

-- | reduce a list of JoinLists into a single JoinList. helper for makeJ
--
-- works by iterative pairing of adjacent values to build a balanced
-- tree (e.g. [a,b,c,d,e] --> [ab,cd,e] --> [[ab,cd],e] --> [ab,cd],e).
joinJ :: (Monoid b) => [JoinList b a] -> JoinList b a
joinJ []  = Empty
joinJ [x] = x
joinJ xs  = joinJ (joinPairs xs)
  where joinPairs []       = []
        joinPairs [x]      = [x]
        joinPairs (x:y:ys) = (x +++ y) : joinPairs ys

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)
