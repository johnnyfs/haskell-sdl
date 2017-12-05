module OpenGl where

import Foreign.C.Types
import Foreign.Ptr

foreign import ccall glClear :: Int -> IO ()
foreign import ccall glClearColor :: Float -> Float -> Float -> Float -> IO ()
foreign import ccall glShaderSource :: Int -> Int -> Ptr (Ptr (CChar)) -> Ptr CInt -> IO ()
foreign import ccall glCompileShader :: Int -> IO ()
foreign import ccall glFinish :: IO ()

foreign import ccall "color_buffer_bit" colorBufferBit :: Int
