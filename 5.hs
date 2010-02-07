-- Project Euler Problem 5
-- What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Factor as Factor

findLCM :: (Integral a) => [a] -> a
findLCM x
  | null x    = 0
  | otherwise = Map.foldWithKey (\k d bcc -> bcc * k ^ d) 1 primeCounts
    where
      factors = [ Factor.primeFactor y | y <- x ]
      groups = concat [ List.group (List.sort b) | b <- factors ]
      insertGreatestPrimes map group = Map.insertWith (max) (head group) (List.genericLength group) map
      primeCounts = List.foldl (insertGreatestPrimes) Map.empty groups

problem5 = findLCM [1..20]
