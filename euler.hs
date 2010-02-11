module Euler
       (factors
       ,fermat
	,lcm
       ,primes) where

import qualified Data.List as List
import qualified Data.Map as Map

isqrt :: Integer -> Integer
isqrt = ceiling . sqrt . fromIntegral

fermat :: Integer -> [Integer]
fermat n = fermat' n [isqrt n..n]

fermat' :: Integer -> [Integer] -> [Integer]
fermat' _ [] = []
fermat' n (x:xs)
  | y^2 /= x^2 - n = fermat' n xs
  | x - y == 1     = [ x + y ]
  | otherwise      = concatMap fermat [ x - y, x + y ]
    where y = isqrt $ x^2 - n
          
factors :: (Integer -> [Integer]) -> Integer -> [Integer]
factors f n
  | even n = 2 : factors f (n `div` 2)
  | otherwise = f n

primes = turnerSieve [2..]
turnerSieve (p : xs) = p : turnerSieve [x | x <- xs, x `mod` p > 0]

lcm :: [Integer] -> Integer
lcm x
  | null x    = 0
  | otherwise = Map.foldWithKey (\k d bcc -> bcc * k ^ d) 1 primeCounts
    where
      allFactors = [ factors (fermat) y | y <- x ]
      groups = concat [ List.group (List.sort b) | b <- allFactors ]
      primeCounts = List.foldl (\acc c -> Map.insertWith (max) (head c) (List.genericLength c) acc) Map.empty groups
