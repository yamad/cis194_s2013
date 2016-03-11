{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (reverse, sort)
import Control.Applicative
import Control.Monad (when, replicateM, filterM)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


-- Exercise 2

-- | simulate a Risk battle
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = battleUpdate bf <$> rolln (attackers bf) <*> rolln (defenders bf)

-- | run battle based on given set of die rolls for attackers/defenders
battleUpdate :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
battleUpdate bf aRolls dRolls = Battlefield (attackers bf - aLosses) (defenders bf - dLosses)
  where dLosses  = countInstances GT $ getZipList cmps
        aLosses  = (length (getZipList cmps)) - dLosses
        cmps     = compare <$> descendZL aRolls <*> descendZL dRolls

-- Exercise 3

-- | simulate a Risk invasion (battle until one side wins)
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = if canBattle bf then battle bf >>= invade else return bf
  where canBattle bf' = (defenders bf') > 0 && (attackers bf') >= 2


-- Exercise 4

-- | calculate Monte Carlo success probability for attacking army, for 1000 trials
successProb :: Battlefield -> Rand StdGen Double
successProb = successProbN 1000

-- | calculate Monte Carlo success probability for attacking army, for n trials
successProbN :: Int -> Battlefield -> Rand StdGen Double
successProbN n bf = fracPN (== True) n <$> map isWin <$> (replicateM n . invade) bf
  where isWin = (0 ==) . defenders


-- Helper functions

-- | randomly roll N dice
rolln :: Int -> Rand StdGen [DieValue]
rolln n  = sequence . replicate n $ die

-- | construct a ZipList with elements in descending order
descendZL :: (Ord a) => [a] -> ZipList a
descendZL = ZipList . reverse . sort

-- | count occurrences of an element in a list
countInstances :: (Eq a) => a -> [a] -> Int
countInstances a = length . filter (== a)

-- | count instances of list where predicate holds
countP :: (Ord a) => (a -> Bool) -> [a] -> Int
countP p = length . filter p

-- | calculate fraction of list where predicate holds
fracP :: (Ord a) => (a -> Bool) -> [a] -> Double
fracP p xs = fracPN p (length xs) xs

-- | calculate fraction of list where predicate holds, specify N directly
fracPN :: (Ord a) => (a -> Bool) -> Int -> [a] -> Double
fracPN p n = (/ (fromIntegral n)) . fromIntegral . countP p
