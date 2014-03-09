-- I can't wrap my brain around the peg puzzle at http://www.prequeladventure.com/2014/03/aggy-extrapolate/
-- I've encoded the rules into a Haskell file to let my CPU brute-force it.
-- The game board looks like:
--     .
--    . .
--   . 0 .
--  . . . .
-- . . . . .
--
-- The puzzle's rules are:
-- You win when there is one peg on the board.
-- You may pick up a peg, jump it over an adjacent peg, and land it in
--   an empty spot in the same direction on the other side of the peg
--   that you jumped over. You (must) then remove the peg that you
--   jumped over.
--
-- A ghost will berate you to keep trying until you win.
--
-- ghc --make -O2 PrequelPeg -main-is PrequelPeg.main
-- ./PrequelPeg
--
-- It will print a sequence of winning moves, one per line, like:
-- M (Tix 5 2) (Tix 4 2) (Tix 3 2)
-- to tell you to jump the peg at (5,2) over the peg at (4,2) to land at (3,2).
-- The coordinates go (row, column). rows count from 1 to 5, with 1 at the top.
-- The column numbers go from 1 to the row number, with 1 being the leftmost
--   peg in a given column.

module PrequelPeg where

import Data.Array (array, (!), Array, Ix, range, index, inRange, rangeSize, elems, assocs)
import Control.Applicative (pure, (<$>), (<*>), (*>), (<*), Applicative)
import Control.Monad (guard)
import Data.Maybe (catMaybes)

type Path = [Move]
data Move = M !Tix !Tix !Tix deriving Show
type State = Array Tix Bool
data Tix = Tix !Int !Int deriving Show

mkTix r c = if r >= c && r > 0 && c > 0 && r < 6 then Just (Tix r c) else Nothing
tix_to_i (Tix row col) = (((row-1)*row)`div`2) + (col - 1)
on_pair f (a, b) = (f a, f b)
tt_to_ii = on_pair tix_to_i

instance Eq Tix where
  ta == tb = tix_to_i ta == tix_to_i tb
instance Ord Tix where
  compare ta tb = compare (tix_to_i ta) (tix_to_i tb)
instance Ix Tix where
  index ab c = index (tt_to_ii ab) (tix_to_i c)
  inRange ab c = inRange (tt_to_ii ab) (tix_to_i c)
  rangeSize ab = rangeSize (tt_to_ii ab)
  range (t0@(Tix r0 c0), tx@(Tix rx cx)) | t0 == tx = [t0]
                                         | t0 >  tx = []
                                         | c0 == r0 = let t1 = Tix (r0 + 1) 1  in t0 : range (t1, tx)
                                         | c0 <  r0 = let t1 = Tix r0 (c0 + 1) in t0 : range (t1, tx)

legal_bounds = (Tix 1 1, Tix 5 5)
legal_positions = range legal_bounds
initial_state = array legal_bounds [(t, t /= (Tix 3 2)) | t <- legal_positions]

is_hole :: State -> Tix -> Bool
is_hole s t@(Tix r c) | c < 1 = False
                      | c > r = False
                      | r < 1 = False
                      | r > 5 = False
                      | otherwise = not (s ! t)

is_peg :: State -> Tix -> Bool
is_peg s t@(Tix r c) | c < 1 = False
                     | c > r = False
                     | r < 1 = False
                     | r > 5 = False
                     | otherwise = s ! t

directions t@(Tix r c) = catMaybes [ M <$> mkTix (r+1) (c+0) <*> pure t <*> mkTix (r-1) (c+0) -- :)
                                   , M <$> mkTix (r+1) (c+1) <*> pure t <*> mkTix (r-1) (c-1) -- :)
                                   , M <$> mkTix (r+0) (c-1) <*> pure t <*> mkTix (r+0) (c+1) -- :|
                                   , M <$> mkTix (r+0) (c+1) <*> pure t <*> mkTix (r+0) (c-1) -- :|
                                   , M <$> mkTix (r-1) (c+0) <*> pure t <*> mkTix (r+1) (c+0) -- :)
                                   , M <$> mkTix (r-1) (c-1) <*> pure t <*> mkTix (r+1) (c+1) -- :)
                                   ]

is_legal :: State -> Move -> Bool
is_legal s (M ts tm tt) = is_peg s ts && is_peg s tm && is_hole s tt

legal_moves :: State -> [Move]
legal_moves s = do position <- legal_positions
                   direction <- directions position
                   guard (is_legal s direction)
                   return direction

apply :: Move -> State -> State
apply (M ts tm tt) s = array legal_bounds [(t, fill t) | t <- legal_positions] where
  fill t | t == ts = False
         | t == tm = False
         | t == tt = True
         | otherwise = s ! t

have_won :: State -> Bool
have_won = (1 >=) . sum . map fromEnum . elems

play s = if have_won s
  then return []
  else do m <- legal_moves s
          let s1 = apply m s
          ms <- play s1
          return (m : ms)

main = mapM_ putStrLn . map show . head . play $ initial_state
