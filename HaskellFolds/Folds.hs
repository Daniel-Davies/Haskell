module Fold where
import Prelude hiding (sum, product, length, and, or, all, any, filter)

sum :: [Integer] -> Integer
sum = foldr (+) 0

product :: [Integer] -> Integer
product= foldr (*) 1

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

all :: (a -> Bool) -> [a] -> Bool
all p = and . (map p)

any :: (a -> Bool) -> [a] -> Bool
any p = or . (map p)

length :: [a] -> Integer
--length = sum . (map (const 1))
length = foldr add1 0
  where
    add1 :: a -> Integer -> Integer
    add1 a x = x + 1

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (f p) []
  where
    f :: (a -> Bool) -> a -> [a] -> [a]
    f p v filtered = if p v then v : filtered else filtered
--group :: Eq a => [a] -> [[a]]

group :: Eq a => [a] -> [[a]]
group xs = foldr (f) [] xs
  where
    f x [] = [[x]]
    f x ((y:ys):yss)
      | x == y = (x:y:ys):yss
      | otherwise = [x]:((y:ys):yss)

transpose :: [[a]] -> [[a]]
transpose xs = foldr (f) [] xs
  where
    f :: [a] -> [[a]] -> [[a]]
    f xs [] = 
