{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

{-
  Party
  CIS 194 (Spring 2013), Homework 8
  by Jason Yamada-Hanff
-}

module Party where

import Data.Tree
import Employee

-- | add an employee to the guest list
--   no checking to validate updated fun score
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)

-- | GuestList is a Monoid
instance Monoid GuestList where
  mempty  = GL [] 0
  mappend (GL as af) (GL bs bf) = GL (mappend as bs) (af + bf)

-- | return guest list with highest fun score
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max


-- | fold for Data.Tree
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a []) = f a []
treeFold f (Node a xs) = f a (map (treeFold f) xs)


-- | calculate best guest list with and without employee
--
-- takes a boss and a list of subtree results and returns a pair with
-- the best guest list with the given boss and without the boss
--
-- if a boss is on the guest list, his immediate subordinates will
-- have no fun.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp ts = (withBoss, withoutBoss)
  where withoutBoss = (mconcat . map fst) ts
        withBoss    = glCons emp $ (mconcat . map snd) ts

-- | return fun-maximizing guest list from a company heirarchy
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- | format guest list for display
formatGL :: GuestList -> String
formatGL (GL guests fun) = unlines (("Total fun: " ++ show fun) : names)
  where names = map empName guests

-- | generate an employee tree from a file containing results of show
-- on a Tree Employee
readTree :: String -> Tree Employee
readTree = read

-- tests
main :: IO ()
main = print (moreFun (GL [] 0) (GL [] 1))
       >> print (glCons (Emp "Jason" 100) mempty)
       >> (formatGL . maxFun . readTree) <$> readFile "company.txt"
       >>= putStrLn
