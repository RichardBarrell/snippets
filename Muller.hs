module Muller where

import Data.Ratio
import Control.Arrow

fromRat :: Rational -> Double
fromRat = (\a -> (a, a)) >>> (numerator *** denominator) >>> (fromIntegral *** fromIntegral) >>> (uncurry (/))

muller :: (Fractional a) => [a]
muller = 4 : (17/4) : zipWith (\x_n1 x_n0 -> (108 - (815 - 1500 / x_n1) / x_n0)) muller (tail muller)

mullerf :: [Double]
mullerf = 4 : (17/4) : zipWith (\x_n1 x_n0 -> (108 - (815 - 1500 / x_n1) / x_n0)) mullerf (tail mullerf)

mullerr :: [Rational]
mullerr = 4 : (17/4) : zipWith (\x_n1 x_n0 -> (108 - (815 - 1500 / x_n1) / x_n0)) mullerr (tail mullerr)
