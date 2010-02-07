-- Project Euler Problem 2
-- Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million

fibonacci :: Int -> Int -> [Int]
fibonacci x y
  | y > 4000000 = [x]
  | otherwise   = x:fibonacci y (x + y)

problem2 = sum [ x | x <- fibonacci 1 2, x `mod` 2 == 0]
