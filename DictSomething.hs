module DictSomething where

-- compile with: ghc --make DictSomething.hs -main-is DictSomething.main
-- run with ./DictSomething [/usr/share/dict/words]
-- on each line, type a number and set of letters, like "5 aabcno"
-- this program will print back the set of words that fit.

import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Data.Set as DS
import System.Environment (getArgs)
import Data.Char (toLower)
import System.IO

type Dct = [String]
type Sig = DM.Map Char Int

sig :: String -> Sig
sig = DL.foldl' (\s c -> DM.insertWith (+) c 1 s) DM.empty

possible :: Sig -> Sig -> Bool
possible constraint candidate = all there $ DM.toList candidate where
    there (char, count) = constrain char >= count
    constrain char = maybe 0 id $ DM.lookup char constraint

attest :: Dct -> String -> Int -> String
attest dct word len = unwords . filter sigMatch . filter lenMatch $ dct where
    sigMatch = possible (sig word) . sig
    lenMatch = (== len) . length

search :: Dct -> String -> String
search dct line = attest dct word len where
    (slen, word) = break (==' ') line
    len = read slen

readDct :: [String] -> IO Dct
readDct [] = readDct ["/usr/share/dict/words"]
readDct dicts = fmap (DS.toList . DS.fromList . concat) $ mapM input $ dicts where
    input fn = do h <- openFile fn ReadMode
                  hSetEncoding h utf8
                  fmap (lines . map toLower) (hGetContents h)

main = getArgs >>= readDct >>= \dict -> interact (unlines . map (search dict) . lines)
