module DupChanExample where

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)

client_thread name input output = do
  value <- readChan input
  writeChan output ("Thread "++name++" saw value "++value++".")

-- infinite loop
forever m = m >> forever m

-- this will print one or other of:
-- "Thread unicast left saw value ham.", or:
-- "Thread unicast right saw value ham."
example_unicast = do
  c <- newChan
  o <- newChan
  forkIO (client_thread "unicast left" c o)
  forkIO (client_thread "unicast right" c o)
  writeChan c "ham"
  forever $ do
    value <- readChan o
    putStrLn value

-- this will print, in arbitrary order, both of:
-- "Thread broadcast left saw value spam.", and:
-- "Thread broadcast right saw value spam."
example_broadcast = do
  c <- newChan
  c_left <- dupChan c
  c_right <- dupChan c
  o <- newChan
  forkIO (client_thread "broadcast left" c_left o)
  forkIO (client_thread "broadcast right" c_right o)
  writeChan c "spam"
  forever $ do
    value <- readChan o
    putStrLn value

