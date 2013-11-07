module Main where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.Exit

foreign import ccall unsafe execvp :: CString -> Ptr CString -> IO Int

main = do pwd <- newCString "pwd"
          args <- mallocBytes (sizeOf pwd * 2)
          pokeElemOff args 0 pwd
          pokeElemOff args 1 nullPtr
          did_we_fail <- execvp pwd args
          exitWith (ExitFailure did_we_fail)
