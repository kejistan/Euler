module Factor
       ( primeFactor ) where

fermat :: (Integral a) => a -> (a,a)
fermat x
  | even x = (2, x `div` 2)
  | otherwise      = fermat' (floor (sqrt (fromIntegral x))) 0 x

fermat' :: (Integral a) => a -> a -> a -> (a,a)
fermat' x y n
  | diff x y == n = (x - y,x + y)
  | diff x y < n  = fermat' (x + 1) 0 n
  | otherwise     = fermat' x (y + 1) n
    where
      diff a b = a^2 - b^2
      
primeFermat :: (Integral a) => a -> [a] -> [a]
primeFermat 1 xs = xs
primeFermat y xs = primeFermat' $ fermat y
  where
    primeFermat' (1, d) = d:xs
    primeFermat' (c, 1) = c:xs
    primeFermat' (c, d) = primeFermat c [] ++ primeFermat d []

primeFactor :: (Integral a) => a -> [a]
primeFactor x = primeFermat x []
