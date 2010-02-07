-- Project Euler Problem 3
-- Find the largest prime factor of the number 600851475143

findPrime :: Integer -> Integer -> [Integer]
findPrime 1 upper = findPrime 2 upper
findPrime lower upper
  | upper == lower         = [lower]
  | upper `mod` lower == 0 = lower:findPrime lower (upper `div` lower)
  | otherwise              = findPrime (lower + 1) upper

problem3 = last (findPrime 1 600851475143)
