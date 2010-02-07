-- Project Euler Problem 4
-- Find the largest palindrome made from the product of two 3-digit numbers

subtractions :: Int -> [(Int, Int)]
subtractions x = subtractions' 0 x

subtractions' :: Int -> Int -> [(Int, Int)]
subtractions' x y
  | x > y     = []
  | otherwise = (x,y):(subtractions' (x + 1) (y - 1))

palindrome :: Int -> Bool
palindrome x = str == (reverse str)
  where str = show x

findPalindrome :: Int -> (Int, Int, Int)
findPalindrome x = findPalindrome' x 0

findPalindrome' :: Int -> Int -> (Int, Int, Int)
findPalindrome' x y
  | y >= x              = (0, 0, 0)
  | result == (0, 0, 0) = findPalindrome' x (y + 1)
  | otherwise           = result
  where result = testSet (subtractions y) x

testSet :: [(Int,Int)] -> Int -> (Int, Int, Int)
testSet ((x,y):tail) m
  | isPalindrome = (possibility, m-x, m-y)
  | tail == []   = (0, 0, 0)
  | otherwise    = testSet tail m
  where possibility  = (m-x) * (m-y)
        isPalindrome = palindrome possibility

problem4 = findPalindrome 999
