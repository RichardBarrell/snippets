module EEA where

-- Apparently this implements the "extended Euclidean algorithm" for finding
-- inverses in modulo n arithmetic? I just found this lying around in my home
-- directory, dated from July 2016. I have vague memories of transcribing a
-- wikipedia article directly into this Haskell file.

durr iter stop s0 s1 = if stop s1 then [s0] else s0 : durr iter stop s1 (iter s0 s1)

ea :: Integer -> Integer -> [Integer]
ea a b = durr (\rim1 ri -> rim1 `mod` ri) (== 0) a b

eea a b = durr iter stop (a, 1, 0) (b, 0, 1) where
  stop (ri, _, _) = ri == 0
  iter (r0, s0, t0) (r1, s1, t1) = (r0 - q*r1, s0 - q*s1, t0 - q*t1) where
     q = r0 `div` r1

inverse_mod_n a n = let (r, x, y) = last (eea a n) in if r == 1 then Just x else Nothing

primes = 2 : 3 : [ c | c <- [5, 7..], is_prime_1 c]

is_prime_1 c = all (\p -> c `mod` p > 0) (takeWhile (\p -> p*p <= c) primes)
is_prime_2 c = c `elem` (takeWhile (<=c) primes)

measureMaybe Nothing = 0
measureMaybe (Just _) = 1
