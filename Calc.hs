{-# OPTIONS_GHC -Wall #-}

{- |
  CIS 194 (Spring 2013), Homework 5
  by Jason Yamada-Hanff

  Integer Calculator
-}

import ExprT  -- ^ expressions definition
import Parser -- ^ parser for strings to expressions

-- | evaluate an expression
eval :: ExprT -> Integer
eval (Lit a)     = a
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

-- | evaluate a string representation of an expression
evalStr :: String -> Maybe Integer
evalStr =  fmap eval . parseExp Lit Add Mul


-- | expression typeclass
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit a = Lit a
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a
    | a <= 0    = False
    | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Ord MinMax where
  compare (MinMax a) (MinMax b) = compare a b

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)
