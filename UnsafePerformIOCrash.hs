import Data.IORef
import GHC.IO.Unsafe (unsafePerformIO)

-- Worked example of how unsafePerformIO pokes a hole in GHC's type system.

transmuteIO :: a -> IO b
transmuteIO a = do
  -- This 'let' binding has type (forall x. IORef x)
  let ref = unsafePerformIO (newIORef undefined) :: IORef x
  -- (writeIORef ref a) succeeds in unifying type x with type a
  writeIORef ref a
  -- (readIORef ref) succeeds in unifying type x with type b
  readIORef ref

{-
transmuteIO2 :: a -> IO b
transmuteIO2 a = do
  -- This doesn't work without unsafePerformIO because the type of 'ref' will
  -- later get unified with 'a' at the writeIORef line, and also with 'b' at
  -- the readIORef line, but 'a' and 'b' may be different from each other so
  -- the type check fails.
  ref <- (newIORef undefined) :: IO (IORef x)
  writeIORef ref a
  readIORef ref
-}

main = do
  b <- (transmuteIO :: Int -> IO (Bool -> String)) 1
  -- b <- (transmuteIO2 :: Int -> IO (Bool -> String)) 1
  putStrLn (b False)
