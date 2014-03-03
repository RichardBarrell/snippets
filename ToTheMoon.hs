module ToTheMoon where

import Data.List (sort)

data Moves = M Bool [Bool] [Bool] deriving Show
type Board = [[Bool]]

instance Eq Moves where
  (M d0 h0 v0) == (M d1 h1 v1) = d0 == d1 &&
                                 h0 == h1 &&
                                 v0 == v1

instance Bounded Moves where
  minBound = M False empties empties
  maxBound = M True (replicate 5 True) (replicate 5 True)

instance Ord Moves where
  compare m0 m1 = chainCmp move_bits_set move_naive where
    chainCmp f g = case cmp f of EQ -> cmp g
                                 x  -> x
    cmp f = compare (f m0) (f m1)

all_moves = sort . takeMax . iterate move_succ $ move_zero where
  takeMax (m:ms) | m /= maxBound = m : takeMax ms
                 | otherwise     = m : []

move_bits_set :: Moves -> Int
move_bits_set (M d h v) = fromEnum d + (sum . map fromEnum) (h ++ v)
move_naive :: Moves -> [Bool]
move_naive (M d h v) = d : h ++ v

empties = replicate 5 False
move_zero = M False empties empties
move_succ (M False h v) = M True h v
move_succ (M True  h v) = hvsucc h v (M False)
hvsucc h v k = if all id h then k empties (lsucc v) else k (lsucc h) v
lsucc (False : as) = True : as
lsucc (True : as) = False : lsucc as

board_win :: Board -> Bool
board_win = all (all not)

show_board :: Board -> String
show_board = unlines . map (map b2c) where
  b2c False = ' '
  b2c True  = '1'
read_board :: String -> Board
read_board = map (map c2b) . (split ';') where
  c2b '0' = False
  c2b ' ' = False
  c2b '*' = True
  c2b '1' = True
  split :: (Eq a) => a -> [a] -> [[a]]
  split x [] = []
  split x l = let (h, t) = break (== x) l in h : split x (ttail t)
  ttail (a:as) = as
  ttail []     = []

win :: Board -> Moves
win b = head . filter (board_win . (`apply` b)) $ all_moves

apply :: Moves -> Board -> Board
apply (M d h v) = apply_diag d . apply_horz h . apply_vert v

apply_diag :: Bool -> Board -> Board
apply_diag False board = board
apply_diag True  board = reverse . zipWith invert_i [0..] . reverse $ board

-- invert i'th element of a row
invert_i :: Int -> [Bool] -> [Bool]
invert_i 0 (b:bs) = not b : bs
invert_i i (b:bs) = b : invert_i (i-1) bs
invert_i _ []     = []

apply_horz :: [Bool] -> Board -> Board
apply_horz h_moves board = zipWith cond_invert_h h_moves board

cond_invert_h False row = row
cond_invert_h True  row = map not row

apply_vert :: [Bool] -> Board -> Board
apply_vert v_moves board = transpose . apply_horz v_moves . transpose $ board

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose (row:[])   = map (:[]) row
transpose (row:rows) = zipWith (:) row (transpose rows)

main = interact (unlines . map (show . win . read_board) . lines)
