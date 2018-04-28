{-# LANGUAGE BangPatterns #-}
module PermutationsCombinations where

-- I was reading http://home.pipeline.com/~hbaker1/ThermoGC.html at the time I wrote this and trying to convince myself
--  that negative temperature is a thing by demonstrating that (adiff 1e-4 (\x -> bCombineEnergies 4000 [x]) y) is
-- negative for y > 2000. (Spoilers: it indeed is.)

import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafeDupablePerformIO)

foreign import ccall unsafe "lgamma_r" lgamma_r_ :: Double -> Ptr CInt -> IO Double
foreign import ccall unsafe "lgammaf_r" lgammaf_r_ :: Float -> Ptr CInt -> IO Float

lgamma :: Double -> (Double, Int)
lgamma x = unsafeDupablePerformIO . alloca $ \signp -> do
    output <- lgamma_r_ x signp
    sign <- peek signp
    return (output, fromEnum sign)

lgammaf :: Float -> (Float, Int)
lgammaf x = unsafeDupablePerformIO . alloca $ \signp -> do
    output <- lgammaf_r_ x signp
    sign <- peek signp
    return (output, fromEnum sign)

lgamp :: Double -> Double
lgamp x | x < 0 = error "tried to ignore sign of lgamma on negative value"
lgamp x | otherwise = fst (lgamma x)

-- permutations n p = number of different sequences of length p that can be drawn from a set of n distinct elements
permutations n p = exp (lpermutations n p)

-- log(permutations n p)
lpermutations n p = lgamp (1+n) - lgamp (1+n-p)
-- log2(permutations n p)
bpermutations n p = (lpermutations n p) / (log 2)

-- combinations n p = number of different sets of size p that can be drawn from a set of n distinct elements
combinations n p = exp (lcombinations n p)

-- log(combinations n p)
lcombinations n p = lgamp (1+n) - lgamp (1+p) - lgamp (1+n-p)
-- log2(combinations n p)
bcombinations n p = (lcombinations n p) / (log 2)

-- binary logarithm of number of possible states that a system can be in (i.e. entropy of that system in bits):
--  + when there are `n` particles, 
--  + and `n - sum eLevels` of them are in the ground state
--  + and [eLevels !! 0] are in state 1, [eLevels !! 1] are in state 2, and so on.
--
-- At least I am almost sure that's what this calculates, anyway.
--
bCombineEnergies n eLevels = (lCombineEnergies n eLevels) / (log 2)

-- same as bCombineEnergies but in units of e rather than bits
lCombineEnergies n eLevels = go n 0 eLevels where
    go !n !ac eLevels | n < 0  = error "ran out of pigeonholes!"
    go !n !ac []               = ac
    go !n !ac (eLevel:eLevels) = go (n - eLevel) (ac + lcombinations n eLevel) eLevels

-- (crappy) approximate differential of `f` at point `x`, using an interval of `h` to measure it
adiff h f x = (f (x + h) - f x) / h
