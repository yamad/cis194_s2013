{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

{- |
  CIS 194 (Spring 2013), Homework 5
  by Jason Yamada-Hanff

  Integer Calculator
-}

import ExprT   -- ^ expressions definition
import Parser  -- ^ parser for strings to expressions
import StackVM -- ^ stack-based virtual machine
import qualified Data.Map.Strict as M
import Control.Applicative ((<$>), (<*>))

-- | evaluate an expression
eval :: ExprT -> Integer
eval (ExprT.Lit a)     = a
eval (ExprT.Add e1 e2) = (eval e1) + (eval e2)
eval (ExprT.Mul e1 e2) = (eval e1) * (eval e2)

-- | evaluate a string representation of an expression
evalStr :: String -> Maybe Integer
evalStr =  fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul


-- | expression typeclass
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- | original ExprT is an expression (Expr)
instance Expr ExprT where
  lit a = ExprT.Lit a
  add = ExprT.Add
  mul = ExprT.Mul

-- | integer arithmetic counts as an expression
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

-- | boolean expression semantics
--
-- values 0 and below are false, 1 and above are true
-- add/mul are logical or/and
instance Expr Bool where
  lit a
    | a <= 0    = False
    | otherwise = True
  add = (||)
  mul = (&&)


-- | min/max operations in expressions (+ means max, * means min)
newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Ord MinMax where
  compare (MinMax a) (MinMax b) = compare a b

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

-- | modulo 7 arithmetic expressions
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)


-- Exercise 5, compiler for expressions on top of hardware VM

-- in a program, an add/mul works by pushing each integer onto the
-- stack and then issuing an Add/Mul command.
--
-- a Program is a list of stack commands, and the arguments provided
-- to add/mul are what result from evaluating `lit`, namely, a program
-- consisting of a single integer push. thus, bringing these values
-- together into a single list with a final Add/Mul command makes a
-- complete program fragment for addition/multiplication.
instance Expr (Program) where
  lit a = [PushI a]
  add a b = concat [a, b, [StackVM.Add]]
  mul a b = concat [a, b, [StackVM.Mul]]


-- Exercise 6, implement named variables

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VVar String
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              deriving (Eq, Show)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- for add/mul, arguments will either be a constant function (`lit`)
-- or a variable lookup function. add/mul here result in applying
-- add/mul to Maybe Integer values, which then dispatch to the Expr
-- (Maybe Integer) instance.
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = (\_ -> Just a)        -- "lift" a literal into a constant
                                -- function, probably a more idiomatic
                                -- way to do this.
  add a b = add <$> a <*> b
  mul a b = mul <$> a <*> b

-- apply add/mul to Maybe Integers, by lifting add into the Maybe
-- context. uses applicative style. needed for implementation of Expr
-- (M.Map ...) to work. probably there is a clean way of double
-- application of add, but I don't know it so this seemed like a
-- straightforward way to fix it.
instance Expr (Maybe Integer) where
  lit = Just
  add a b = add <$> a <*> b
  mul a b = mul <$> a <*> b



-- provided test function for expressions with variables
withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs expr = expr $ M.fromList vs

-- test expression for instances of Expr
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

main :: IO ()
main = do
  print $ Just (-7)       == (testExp :: Maybe Integer)
  print $ Just True       == (testExp :: Maybe Bool)
  print $ Just (MinMax 5) == (testExp :: Maybe MinMax)
  print $ Just (Mod7 0)   == (testExp :: Maybe Mod7)
  print $ Just 9          == withVars [("x", 6)] (add (lit 3) (var "x"))
  print $ Nothing         == withVars [("x", 6)] (add (lit 3) (var "y"))
  print $ Just 54         == withVars [("x", 6), ("y", 3)]
                                (mul (var "x") (add (var "y") (var "x")))
