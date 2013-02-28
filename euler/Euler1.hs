module Euler1 where

brute = sum [ x | x <- [1..1000-1], (x `rem` 3 == 0) || (x `rem` 5 == 0) ]

gauss n = (n * (n+1)) `div` 2
hand = 3*(gauss (999 `div` 3)) + 5*(gauss (999 `div` 5)) - 15*(gauss (999 `div` 15))
