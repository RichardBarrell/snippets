module Main where

-- This thing works very much like "netcat", but it understands just
-- enough of RFC 1459 to be able to respond to PING messages on your
-- behalf, because that's the most tedious part of using IRC by hand.

import Data.List
import System.IO
import Network
import Data.Char
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment
import System.Exit

main = withSocketsDo $ do
   [hostname, port, logfile] <- getArgs
   handle <- connectTo hostname (PortNumber . toEnum . read $ port)
   logchan <- newChan
   loghandle <- openFile logfile AppendMode
   hSetBuffering handle LineBuffering
   hSetBuffering loghandle LineBuffering
   forkIO $ bouncy handle logchan
   forkIO $ dumpy loghandle logchan
   runaround handle logchan

dumpy loghandle logchan = do
   line <- readChan logchan
   hPutStrLn loghandle line
   dumpy loghandle logchan

bouncy handle logchan = do
   line <- catch (hGetLine handle) (\_ -> exitWith ExitSuccess)
   if "ping " `isPrefixOf` (map toLower line)
      then do
         hPutStrLn handle $ "PONG " ++ (drop 5 line)
         bouncy handle logchan
      else do
         hPutStrLn stdout line
         writeChan logchan line
         bouncy handle logchan

runaround handle logchan = do
   line <- catch (hGetLine stdin) (\_ -> exitWith ExitSuccess)
   hPutStrLn handle line
   writeChan logchan line
   runaround handle logchan
