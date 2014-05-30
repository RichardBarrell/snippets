module Main where

-- compile: ghc --make -O2 -threaded SlowProxy
-- usage: ./SlowProxy [remote_host] [remote_port]
-- listens on 0.0.0.0:1050 and proxies to the port you specify
-- by default, delays for 100ms
-- by default, proxies to localhost, port 20000
-- if you type a number on stdin, delays for that many milliseconds

-- TODO: this is full of state machines running in different threads
-- that don't have a principled way of dealing with errors. Rewrite
-- it with explicit state machines.

import Prelude hiding (catch)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import qualified Data.ByteString as B
import System.IO
import System.Environment

import Network.BSD
import Network.Socket
import qualified Network.Socket.ByteString as NB


main = withSocketsDo $ do
  args <- getArgs
  let server_name = if length args >= 1 then args !! 0 else "localhost"
  let port_string = if length args >= 2 then args !! 1 else "20000"
  let server_port = toEnum . read $ port_string
  server_addr <- fmap hostAddress (getHostByName server_name)
  let server_sockaddr = (SockAddrInet server_port server_addr)

  acc_sock <- socket AF_INET Stream 0
  bindSocket acc_sock (SockAddrInet (toEnum 1050) iNADDR_ANY)
  listen acc_sock 8
  time <- newMVar 100

  let forever a = a >> forever a

  forkIO . forever $ do s <- hGetLine stdin
                        case reads s of
                          ((v, _):_) -> do swapMVar time v
                                           putStrLn $ "Wait "++(show v)++"ms"
                          _ -> putStrLn $ "Not a number: "++(show s)
  forever $ do (client, _) <- accept acc_sock
               forkIO (bidi client server_sockaddr time)


-- per-client, set one thread to copy bytes in each direction
bidi :: Socket -> SockAddr -> MVar Int -> IO ()
bidi client server_sockaddr time = do
  donei <- newEmptyMVar
  doneo <- newEmptyMVar
  server <- socket AF_INET Stream 0
  let stop = do sClose client
                sClose server
  (connect server server_sockaddr) `onException` stop
  t1 <- forkIO (run client server donei time)
  t2 <- forkIO (run server client doneo time)
  takeMVar donei
  takeMVar doneo
  stop


-- Swallow exceptions. Evil.
-- (this is overkill, I'm just using it below to ignore shutdown ENOTCONN)
meh :: IO () -> IO ()
meh a = catch (a) (\(SomeException e) -> return ())


-- moves bytes (delayed) from hin to hout.
-- reads `time` to decide how long to delay
-- fills `mv` when done
run :: Socket -> Socket -> MVar () -> MVar Int -> IO ()
run hin hout mv time = do
  preDelay <- atomically $ newTChan
  postDelay <- atomically $ newTChan
  outstanding <- atomically $ newTVar 0
  -- Each time "inpt" gets a blob, put it onto preDelay and fork a "push".
  -- Each "push" thread waits, then moves an item from preDelay to postDelay.
  -- The "otpt" thread reads from postDelay and pushes to the network.
  let inpt = do block <- NB.recv hin 1024
                atomically $ writeTChan preDelay block
                forkIO push
                if not (B.null block) then inpt else return ()
      push = do delayTime <- readMVar time
                threadDelay (delayTime*1000)
                atomically $ readTChan preDelay >>= writeTChan postDelay
                return ()
      otpt = do block <- atomically $ readTChan postDelay
                NB.sendAll hout block
                if not (B.null block) then otpt else done
      done = do meh (shutdown hin ShutdownReceive)
                meh (shutdown hout ShutdownSend)
  forkIO inpt
  otpt
  putMVar mv ()
