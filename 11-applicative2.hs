module Main where

import Control.Applicative (liftA2)

(*>*) :: Applicative f => f a -> f b -> f b
(*>*) = liftA2 (\_ b -> b)

mapA' :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA' f = sequenceA' . map f

-- had the idea right, but had to look in LYAH for use liftA2 and pure.
sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

replicateA' :: Applicative f => Int -> f a -> f [a]
replicateA' n = sequenceA' . replicate n

main :: IO ()
main = undefined
