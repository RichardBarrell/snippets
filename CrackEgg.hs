module CrackEgg where

-- unpack zipped egg files into directories
-- compile:
-- ghc --make CrackEgg -main-is CrackEgg.main
-- run:
-- find ~/eggs/ -maxdepth 1 -type f -name '*.egg' -exec ./CrackEgg {} +

import System.Environment (getArgs)
import System.Process
import System.Directory
import System.IO
import Text.Printf
import Control.Exception
import System.IO.Error
import System.Exit

import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr

foreign import ccall unsafe "mkdtemp" mkdtemp_ :: Ptr CUChar -> IO (Ptr CUChar)

mkdtemp :: FilePath -> IO FilePath
mkdtemp fp = allocaBytes (1 + length fp) $ \p -> do
               pokeArray0 0 p $ map (toEnum . (`mod` 256) . fromEnum) fp
               rp <- mkdtemp_ p
               if rp == nullPtr then throwIO (userError "mkdtemp") else return ()
               rs <- peekArray0 0 rp
               return . map (toEnum . fromEnum) $ rs

mkdtempX = mkdtemp . (++"XXXXXX")

main :: IO ()
main = getArgs >>= mapM_ ((>>= crackEgg) . canonicalizePath)

crackEgg :: String -> IO ()
crackEgg eggname = dir_t eggname where
    dir_t = test doesDirectoryExist (printf "%s is a directory.\n") file_t
    file_t = test doesFileExist goAhead (printf "%s does not exist.\n")
    goAhead eggname = do tmp <- mkdtempX "/tmp/egg"
                         runProcTmp tmp "unzip" [eggname]
                         runProcTmp tmp "mv" [eggname, "/tmp"]
                         runProcTmp tmp "mv" [tmp, eggname]
    runProcTmp tmp prog args = do let cm = (proc prog args){ cwd = Just tmp }
                                  (_, _, _, ph) <- createProcess cm
                                  ExitSuccess <- waitForProcess ph
                                  return ()
    test ioPred kYes kNo param = do yn <- ioPred param
                                    (if yn then kYes else kNo) param

