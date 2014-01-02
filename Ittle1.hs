-- This (approximately, ignoring the position of Ittle herself)
-- solves one of the fire sword puzzles in Ittle Dew that I got stuck on.
-- Usage: $ ghci Ittle1.hs
--        > winGame box0

module Ittle1 where

import Debug.Trace (trace)
import Data.Array
import Data.List (delete, sort, elem, nub)
import Control.Monad (guard)

data Tile = W | S | A | T deriving (Show, Eq)
type Loc = (Int, Int)

mkGrid :: [[e]] -> Array Loc e
mkGrid ll = array ((0, 0), (length ll - 1, length (head ll) - 1)) $ do
    (rowIx, rowList) <- zip [0..] ll
    (colIx, element) <- zip [0..] rowList
    return ((rowIx, colIx), element)

room :: Array Loc Tile
room = mkGrid [[W, W, W, W, W, W, W, W, W, W, W, W],
               [W, W, W, S, S, A, A, A, A, A, W, W],
               [W, T, S, S, S, A, A, A, A, A, A, W],
               [W, S, W, S, W, A, A, A, A, A, A, W],
               [W, S, S, S, S, A, A, A, A, A, A, W],
               [W, W, W, S, S, A, A, A, A, A, W, W],
               [W, W, W, W, W, W, W, W, W, W, W, W]]

-- lookup what is at a given tile, given gamestate
masked :: [Loc] -> Loc -> Tile
masked boxs loc = if elem loc boxs then W else room ! loc

data Move = Slice Loc | Push Loc Direction deriving (Show, Eq, Ord)
data Direction = GoUp | GoRight | GoDown | GoLeft deriving (Show, Eq, Ord)
data GS = GS [Move] [Loc]

push :: Loc -> Loc -> Move
push box dew = Push box $ case box `minusL` dew of
    ( -1,  0) -> GoUp
    (  1,  0) -> GoDown
    (  0, -1) -> GoLeft
    (  0,  1) -> GoRight

adjacent :: Loc -> [Loc]
adjacent (r, c) = [(r - 1, c    ),
                   (r + 1, c    ),
                   (r    , c - 1),
                   (r    , c + 1)]

box0 :: GS
box0 = GS [] [(3, 8), (4, 8), (3, 7), (4, 7)]

sliceMove :: GS -> [GS]
sliceMove (GS ms boxs) = do
    box <- boxs
    let dews = adjacent box
    guard (any (\dew -> masked boxs dew == A) dews)
    return $ GS (Slice box : ms) (delete box boxs)

minusL (ar, ac) (br, bc) = (ar - br, ac - bc)
plusL  (ar, ac) (br, bc) = (ar + br, ac + bc)

goThatWay :: Loc -> Loc -> [Loc]
goThatWay direction start = iterate (plusL direction) start

-- t = seq
-- t = trace

pushMove :: GS -> [GS]
pushMove (GS ms boxs) = do
    box <- boxs
    dew <- adjacent box
    -- t ("box="++show box) (return ())
    guard (masked boxs dew == A)
    -- t ("dew="++show dew) (return ())
    let dir = box `minusL` dew
    let next = box `plusL` dir
    guard (masked boxs next /= W)
    -- t ("dir="++show dir) (return ())
    -- t ("next="++show next) (return ())
    let newBox = last . takeWhile ((/= W) . masked boxs) . goThatWay dir $ next
    return $ GS (push box dew : ms) (newBox : delete box boxs)

win :: GS -> Bool
win (GS ms boxs) = elem (2, 1) boxs

playGame :: GS -> [GS]
playGame boxs = if win boxs then [boxs] else
    (pushMove boxs ++ sliceMove boxs) >>= playGame

getSolution :: GS -> [Move]
getSolution (GS ms boxs) = reverse ms

winGame :: GS -> IO ()
winGame = mapM_ (putStrLn . show . getSolution) . take 1 . playGame

countWins = length . nub . map getSolution . playGame
