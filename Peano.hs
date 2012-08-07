module Peano where

-- This was an abortive attempt to put Peano numbers
-- into Haskell that I scribbled down a few years back. 
-- It's not correct.

-- Of course, using data type "Peano" to include "expression that
-- results in a Peano number" instead of just "a Peano number"
-- is the major dumb bit.

import Debug.Trace (trace)

data Peano = Z | S Peano | Mul Peano Peano | Add Peano Peano

seven = S(S(S(S(S(S(S(Z)))))))
three = S(S(S(Z)))

ugh :: Peano -> Peano
ugh Z               = Z
ugh (S x)           = S (ugh x)
ugh (Add x Z)       = ugh x
ugh (Add x (S y))   = S (ugh (Add x y))
ugh (Add x y)       = ugh (Add x (ugh y))
ugh (Mul x Z)     = Z
ugh (Mul x (S y)) = ugh (Add x (ugh (Mul x y)))
ugh (Mul x y)     = ugh (Mul x (ugh y))

instance Eq Peano where
         Z == Z       = True
         _ == Z       = False
         Z == _       = False
         S(x) == S(y) = x == y
         x == y       = ugh x == ugh y

-- easier on the eyes when debugging a multiplier :)
--instance Show Peano where
--         showsPrec _ num = if mono num then showsPrec 0 (count num) else horrorShow num
--count Z = 0
--count (S x) = 1 + count x
--mono Z = True
--mono (S x) = mono x
--mono _ = False

instance Show Peano where
         showsPrec _ x = horrorShow x

horrorShow Z = ("0"++)
horrorShow (S x) = ("S("++) . (showsPrec 0 x) . (")"++)
horrorShow (Mul x y) = ("("++) . (showsPrec 0 x) . (" × "++) . (showsPrec 0 y) . (")"++)
horrorShow (Add x y) = ("("++) . (showsPrec 0 x) . (" + "++) . (showsPrec 0 y) . (")"++)

newtype Emitter spark x = E { unE :: [spark] -> (x,[spark]) }

instance Monad (Emitter spark) where
         (E runner) >>= f = E (\sparks -> let (v,sparks') = runner sparks in unE (f v) sparks')
         return v = E (\sparks -> (v,sparks))

runE :: Emitter spark v -> (v,[spark])
runE (E f) = f []

fire :: Emitter spark spark -> [spark]
fire = reverse . (\(a,as) -> a:as) . runE

bang :: Peano -> IO ()
bang = putStr . unlines . map show . fire . step

shoot :: spark -> Emitter spark spark
shoot x = E (\sparks -> (x, x:sparks))

shootmap :: (spark -> spark) -> Emitter spark spark -> Emitter spark spark
shootmap f (E nextrunner) = E (\oldsparks -> let (v, newsparks) = nextrunner [] in (f v, map f newsparks ++ oldsparks))

gunlist [] =     return ()
gunlist (a:as) = shoot a >> gunlist as

step :: Peano -> Emitter Peano Peano
step Z = return Z
step (S x) = shootmap S (step x)
step (Add x y) = do (Add _ y') <- shootmap (Add x)       (step y)
                    (Add x' _) <- shootmap (flip Add y') (step x)
                    mono_add x' y'
step (Mul x y) = do (Mul _ y') <- shootmap (Mul x)       (step y)
                    (Mul x' _) <- shootmap (flip Mul y') (step x)
                    mono_mul x' y'

mono_mul x Z     = return Z
mono_mul x (S y) = do let out = Add x (Mul x y)
                      shoot out
                      step out

mono_add x Z        = shoot (Add x Z) >> return x
mono_add x y@(S y') = shoot (Add x y) >> mono_add (S x) y'
mono_add x y = trace ("wat "++show x++" and "++show y) undefined
