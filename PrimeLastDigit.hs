module PrimeLastDigit where

-- compile with:
-- ghc --make PrimeLastDigit.hs -O2 -main-is PrimeLastDigit.main

import Data.List (foldl')

primes :: [Integer]
primes = 2 : [ c | c <- [3,5..], all (\p -> c `mod` p > 0) (takeWhile (\p -> p*p <= c) primes) ]

lastDigits = map (flip mod 10) primes

czero :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
czero = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
count 0 (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = (1 + c0, c1, c2, c3, c4, c5, c6, c7, c8, c9)
count 1 (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = (c0, 1 + c1, c2, c3, c4, c5, c6, c7, c8, c9)
count 2 (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = (c0, c1, 1 + c2, c3, c4, c5, c6, c7, c8, c9)
count 3 (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = (c0, c1, c2, 1 + c3, c4, c5, c6, c7, c8, c9)
count 4 (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = (c0, c1, c2, c3, 1 + c4, c5, c6, c7, c8, c9)
count 5 (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = (c0, c1, c2, c3, c4, 1 + c5, c6, c7, c8, c9)
count 6 (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = (c0, c1, c2, c3, c4, c5, 1 + c6, c7, c8, c9)
count 7 (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = (c0, c1, c2, c3, c4, c5, c6, 1 + c7, c8, c9)
count 8 (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = (c0, c1, c2, c3, c4, c5, c6, c7, 1 + c8, c9)
count 9 (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = (c0, c1, c2, c3, c4, c5, c6, c7, c8, 1 + c9)

countLastDigitsUpTo n = foldl' (flip count) czero (take n lastDigits)

-- main loop does:
-- read an integer, N
-- count the last digits of the primes up to N
-- print the distribution
-- back to start
main = interact (unlines . map (show . countLastDigitsUpTo . read) . lines)
