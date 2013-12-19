module Tut01 where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as UT
import System.IO

import Data.IORef (newIORef, readIORef, writeIORef)

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall unsafe float_mono_time :: IO Double
-- foreign import ccall unsafe glXGetCurrentDisplay :: IO (Ptr Evil)
-- foreign import ccall unsafe glXGetCurrentDrawable :: IO (Ptr Evil)
-- foreign import ccall unsafe glXSwapIntervalEXT :: Ptr Evil -> Ptr Evil -> CInt -> IO ()

-- data Evil

main = do (argv0, args) <- UT.getArgsAndInitialize
          UT.initialWindowSize $= (UT.Size 800 600)
          UT.initialDisplayMode $= [UT.RGBAMode,
                                    UT.WithDepthBuffer,
                                    UT.DoubleBuffered]

          -- evil1 <- glXGetCurrentDisplay
          -- evil2 <- glXGetCurrentDrawable
          -- glXSwapIntervalEXT evil1 evil2 1

          get UT.displayModePossible >>= (putStrLn . show)
          w <- UT.createWindow "Bacon"
          putStrLn (show w)
          time_begin <- float_mono_time
          stateR <- newIORef (0, time_begin)
          
          [frag] <- genObjectNames 1
          shaderSource frag $= [unlines [
            "#version 330",
            "out vec4 outputColor;",
            "void main()",
            "{",
            "   outputColor = vec4(1.0f, 1.0f, 1.0f, 1.0f);",
            "}",
            ""]]
          compileShader frag
          get (shaderInfoLog frag) >>= putStrLn
          True <- get (compileStatus frag)
          
          [vert] <- genObjectNames 1
          shaderSource vert $= [unlines [
            "#version 330",
            "layout(location = 0) in vec4 position;",
            "void main()",
            "{",
            "   gl_Position = position;",
            "}",
            ""]]
          compileShader vert
          get (shaderInfoLog vert) >>= putStrLn
          True <- get (compileStatus vert)

          [prog] <- genObjectNames 1
          attachedShaders prog $= ([vert], [frag])
          linkProgram prog
          get (programInfoLog prog) >>= putStrLn
          True <- get (linkStatus prog)

          UT.displayCallback $= (disp stateR prog)
          -- UT.idleCallback $= Just (disp stateR)
          UT.mainLoop

clamp x | x >= 0 && x <= 1 = x
        | x > 1 = 1
        | otherwise = 0

fmod p bound | p < bound = p
             | otherwise = fmod (p - bound) bound

disp stateR prog = do
  (phase_then, time_then) <- readIORef stateR
  time_now  <- float_mono_time
  let phase_delta = time_now - time_then
  let phase_now = (phase_then + phase_delta) `fmod` (2*pi)
  writeIORef stateR (phase_now, time_now)

  let i = realToFrac $ (1 + sin phase_now) / 2
  clearColor $= (Color4 i i i 1)
  clear [ColorBuffer]

  

  putStrLn (show (1000 * phase_delta))
  UT.swapBuffers
  UT.postRedisplay Nothing
