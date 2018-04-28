module PS2Gun where

import GHC.Float (int2Double)
import Data.List (intercalate)
import Text.Printf (printf)

-- I think I was trying to work out, in PlanetSide 2, how many hits it takes
-- to drop someone if you were using HVA vs if you were using SPA, based on
-- how those were reworked to work about four years back.
-- They probably changed it again since, tbqh

inf :: Double
inf = 1.0 / 0.0

type GunSpec = (Double, Double, Double, Double)

killRanges :: GunSpec -> [(Int, Double)]
killRanges (top, near, bot, far) =
  let best = ceiling (1000.0 / top)
      wrst = ceiling (1000.0 / bot)
      range :: Int -> Double
      range k | k == wrst = inf
      range k | otherwise = damRange damToDo where
        damToDo :: Double
        damToDo = int2Double (ceiling (1000.0 / int2Double k))
        damRange :: Double -> Double
        damRange dam = near + (top-dam)*(far-near)/(top-bot)
        -- dam = top - (top-bot)*((range-near)/(far-near))
        -- dam - top = -(top-bot)*((range-near)/(far-near))
        -- top - dam = (top-bot)*((range-near)/(far-near))
        -- (top-dam)/(top-bot) = (range-near)/(far-near)
        -- (top-dam)/(top-bot)*(far-near) = range-near
        -- (top-dam)*(far-near)/(top-bot) + near = range
  in [(k, range k) | k <- [best..wrst]]

hva (top, near, bot, far) = (top, near+20, bot, far-2)
spa (top, near, bot, far) = (top, near+5, bot, far)

shkr gs = (intercalate "\n") $
  [ printf "%d at %.2f (%.2f SPA) (%.2f HVA)" k rangek spaRangeK hvaRangeK
  | ((k, rangek), (_, spaRangeK), (_, hvaRangeK)) <-
      zip3 (killRanges gs) (killRanges (spa gs)) (killRanges (hva gs))]

