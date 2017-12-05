module Shader where

import System.IO
import Foreign.C.String

import Graphics.Rendering.OpenGL.GL.Shaders

import OpenGl

shaderLoad :: Int -> String -> Int
shaderLoad type' path = do
  sh <- createShader type'
  withFile (FilePath path) ReadMode (\h ->
    hGetContents h `withCStringLen` (\(s, len) -> do
      s' <- newStablePtr s
      shaderSource sh 1 s' nullPtr
      compileShader sh
    )
  )
  return sh
