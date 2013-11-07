module Main where
-- Cut wide lines at 120 (or whatever length) chars.
-- ghc --make WideLines
-- ./foo | ./WideLines 80

import System.Environment (getArgs)
import System.IO (stdin, stdout, hSetEncoding, char8)

main = do args <- getArgs
          let width = parseWidth args
          hSetEncoding stdin char8
          hSetEncoding stdout char8
          interact (unlines . map (take width) . lines)

parseWidth [] = 120
parseWidth (a:_) = case reads a of [(n, _)] -> n
                                   _        -> 120
