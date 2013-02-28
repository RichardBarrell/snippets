module Euler12 where

triangles = scanl (+) 1 [2..]
primes = 2 : 3 : 5 : [ x | x <- [7,9..], all (\p -> x `rem` p /= 0) (takeWhile (\p -> p*p <= x) primes) ]

factorise n = fact n primes where
  fact 1 ps     = []
  fact n (p:ps) = power : fact n' ps where
    (power, n') = reachRemainder (0, n)
    reachRemainder (power, n) = case n `quotRem` p of
      (n', 0) -> reachRemainder ((power+1), n')
      _       -> (power, n)

displayFactors = filter ((>0) . snd) . zip primes . factorise

{-
-- Thought I needed these, turns out I don't.
-- 
countPermutations n r = product [n,n-1..n-r+1]
countOrderings r = product [r,r-1..1]
countCombinations n r = (countPermutations n r) `div` countOrderings r
-}

countDivisors n = product [ f + 1 | f <- factorise n ]
slowDivisors n = length [ x | x <- [1..n], n `rem` x == 0 ]

augSnd f x = (x, f x)

-- ghc --make Euler12 -O2 -main-is Euler12.euler12
euler12 = putStrLn . show . head . dropWhile ((<500) . snd) . map (augSnd countDivisors) $ triangles
