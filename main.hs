{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

import Control.Concurrent
import System.IO

import Graphics.Rendering.OpenGL.GL.FlushFinish

import OpenGl
import Sdl

import Color
import Scene

windowDetails = WindowDetails {
  title = "Title",
  pos   = (centerX, centerY),
  size  = (640, 480),
  attrs = [Shown, WithOpenGl]
}

scene = Scene {
  skyColor = Just halfGrey,
  objects = [ Cube 1.0 ],
  lights  = [ Ambient white ] 
}

main =
  initialize [VideoSubsystem] >>= \case
    Left err -> fail $ "Could not initialize SDL: " ++ err
    Right _  -> do
      w <- createWindow windowDetails
      c <- createGlContext w
      prep <- prepare scene
      prerender prep
      renderScene w prep
      withEventContext eventLoop
      deleteGlContext c
      destroyWindow w
      deinitialize

renderScene w prep = do
  render prep
  finish
  swapGlWindow w

eventLoop ctx = 
  pollEvent ctx >>= \case
    Just Quit -> return ()
    _         -> eventLoop ctx
