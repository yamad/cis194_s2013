{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-
  Scrabble scoring
  CIS 194 (Spring 2013), Assignment 7
-}
module Scrabble where

import Data.Monoid
import Data.Char (toUpper)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score c = Score (charValue . toUpper $ c)
  where charValue d
          | d `elem` "AEILNORSTU" = 1
          | d `elem` "DG"         = 2
          | d `elem` "BCMP"       = 3
          | d `elem` "FHVWY"      = 4
          | d `elem` "K"          = 5
          | d `elem` "JX"         = 8
          | d `elem` "QZ"         = 10
          | otherwise             = 0

scoreString :: String -> Score
scoreString = mconcat . map score
