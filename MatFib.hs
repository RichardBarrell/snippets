-- Playing with maths. Compute Fibonacci numbers by computing powers
-- of a matrix using exponentiation by squaring.

module MatFib where

-- |a b| * |d e| = |x y|
-- |b c|   |e f|   |y z|
fm (a, b, c) (d, e, f) = (x, y, z) where
  x = a*d + b*e
  y = a*e + b*f
  z = e*b + c*f

fib1 = (1,1,0)
finc x = fm fib1 x
fsqr x = fm x x

fextr (x, _, _) = x

fmat 0 = (1,0,1)
fmat n | odd n = finc (fmat (n - 1))
       | True  = fsqr (fmat (n `div` 2))

fib = fextr . fmat
