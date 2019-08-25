module Wkik.RecentQueueLength (
  newRecentQueueLength,
  tickleRecentQueueLength,
  RecentQueueLength
) where

import System.Clock (Clock(Monotonic), TimeSpec, getTime, diffTimeSpec, fromNanoSecs)
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Data.Strict.Tuple (Pair, (:!:))
import Data.Word (Word64)

data RecentQueueLength = RQL { timeout :: TimeSpec, mutRef :: IORef (TimeSpec :!: Word64)) }

newRecentQueueLenght :: Integer -> RecentQueueLength
newRecentQueueLength timeoutMilliSecs = do
  now <- getTime Monotonic
  ref <- newIORef (now :!: 0)
  let timeout = fromNanoSecs (timeoutMilliSecs * 1000 * 1000)
  return $ RQL timeout ref
  
tickleRecentQueueLength :: Word64 -> RecentQueueLength -> IO Word64
tickleRecentQueueLength value (RQL timeout mutRef) = do
  now <- getTime Monotonic
  let update prev@(prevTime, prevValue) =
    if ((now `diffTimeSpec` prevTime) > timeout) || (value > prevValue)
    then (now :!: value)
    else prev
  highValue <- atomicModifyIORef mutRef (\a -> let next@(_, :!: nextValue) = update a in (next, nextValue))
