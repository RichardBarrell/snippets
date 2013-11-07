module Power where

import Test.QuickCheck (quickCheck, (==>))
import Data.List ((!!))

power :: (t -> t -> t) -> t -> t -> Integer -> t
power mul one n0 p = powerRec p where
  powerRec 0 = one
  powerRec p | odd p = (\n -> mul n0 n) $ powerRec (p - 1)
             | True  = (\n -> mul n n)  $ powerRec (p `div` 2)

test0 = quickCheck (\x y -> (y>=0) ==> ((x ^ y)::Integer) == (power (*) 1 x y))


powerMod modulus = power (\a b -> mod (a * b) modulus) 1

primes = 2 : 3 : [ c | c <- [5,7..], all (\p -> mod c p > 0) (takeWhile (\p -> (p*p) <= c) primes) ]

fermatLittle = quickCheck $ \pindex a -> let p = (primes !! pindex) in (pindex>=0 && pindex < 8192 && a>0 && a<p) ==> powerMod p a p == a
