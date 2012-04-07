module RC4 where

-- Straightforward but possibly not awfully idiomatic implementation of
-- RC4, abusing the C FFI a lot. The `main` routine assumes that the first
-- 256 bytes of stdin are the key and everything following that is plaintext.

-- Caveat: my reason for writing this was to play around with raw
-- memory primitives for fun, not for security. I haven't even checked
-- whether or not it implements RC4 correctly. It might even have a
-- segfault bug. Don't use it in production.

-- Compile with: ghc -O2 --make RC4.hs -main-is RC4.main

import Foreign
import Foreign.C.Types
import Data.Word

import Data.ByteString (index, ByteString, hGet, hPut, length, null)
import Data.ByteString.Internal (create, unsafeCreate)

import System.IO (stdin, stdout)
import Data.Bits (xor)

newtype RC4state = RC4state (ForeignPtr Word8)

-- Like peek/pokeElemOff but with less fromEnum.
peekW :: Ptr Word8 -> Word8 -> IO Word8
peekW s i = peekElemOff s (fromEnum i)
pokeW :: Ptr Word8 -> Word8 -> Word8 -> IO ()
pokeW s i v = pokeElemOff s (fromEnum i) v

mkRC4state :: ByteString -> IO RC4state
mkRC4state key = do
  let keylen = Data.ByteString.length key
  -- 256 bytes for the state array, plus I'm storing the values of i and j
  -- just after the state array bytes because I'm too lazy to type
  -- "import IORef" and to change "newtype" above to "data"... ;)
  s <- mallocBytes 258
  flip mapM_ [0..255] (\i -> pokeW s i i)
  let swapLoop i j = do iv <- peekW s i
                        let j' = j + iv + index key (mod (fromEnum i) keylen)
                        jv <- peekW s j'
                        pokeW s i  jv
                        pokeW s j' iv
                        if i == 255 then return () else swapLoop (i+1) j'
  swapLoop 0 0
  pokeElemOff s 256 0
  pokeElemOff s 257 0
  fp <- newForeignPtr finalizerFree s
  return (RC4state fp)

rc4Bytes :: RC4state -> ByteString -> IO ByteString
rc4Bytes (RC4state fp) input = withForeignPtr fp encrypt where
  nbytes = Data.ByteString.length input
  encrypt s = create nbytes $ \o -> do
    i0 <- peekElemOff s 256
    j0 <- peekElemOff s 257
    let swapLoop i j n | n == nbytes = do pokeElemOff s 256 i
                                          pokeElemOff s 257 j
                       | otherwise = do
          let i' = i + 1
          iv <- peekW s i'
          let j' = j + iv
          jv <- peekW s j'
          pokeW s i' jv
          pokeW s j' iv
          k <- peekW s (i' + j')
          let k' = (index input n) `xor` k
          pokeElemOff o n k'
          swapLoop i' j' (n+1)
    swapLoop i0 j0 0

main = do key <- hGet stdin 256
          rc4 <- mkRC4state key
          -- Discard 768 bytes of cipher output. Don't tell Santa that I'm
          -- abusing unsafeCreate, lest I wind up on the naughty list.
          rc4Bytes rc4 (unsafeCreate 768 (const $ return ()))
          let encrypt = do
              input <- hGet stdin (1024 * 64)
              crypt <- rc4Bytes rc4 input
              hPut stdout crypt
              if Data.ByteString.null input then return () else encrypt
          encrypt
