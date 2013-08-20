module Main where

import System.Environment (getArgs)

main = do args <- getArgs
          let width = parseWidth args
          interact (unlines . map (take width) . lines)

parseWidth [] = 120
parseWidth (a:_) = case reads a of [(n, _)] -> n
                                   _         -> 120
