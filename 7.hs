-- Project Euler Problem 7
-- What is the 10001st prime number?

import qualified Euler as Euler

problem7 = head $ drop 10000 Euler.primes
