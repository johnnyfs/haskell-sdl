{-# LANGUAGE LambdaCase #-}

module Scene (
  Light(..),
  Object(..),
  Scene(..),
  Preparation(..),
  UndoPrerender(..),
  prepare,
  prerender,
  render,
  undoPrerender) where

import OpenGl

import Data.Maybe
import Foreign.Ptr

import Color
import Fluency

data Light  = Ambient RGBA
data Object = Cube Float

data Scene = Scene {
  skyColor :: Maybe RGBA,
  objects :: [Object],
  lights  :: [Light]
}

data Preparation = Preparation {
  bgColor :: Maybe RGBA
}

type UndoPrerender = Maybe (IO ())

prepare :: Scene -> IO Preparation
prepare Scene {
    skyColor = sc
  } = do
    return $ Preparation {
      bgColor = sc
    }

prerender :: Preparation -> IO UndoPrerender
prerender Preparation {
    bgColor = bgc 
  } = do
    case bgc of
      Just (r, g, b, a) -> do
        glClearColor r g b a;
        return $ Just $ glClearColor 0 0 0 0
      _                 ->
        return $ Just $ return ()

render :: Preparation -> IO ()
render Preparation {
      bgColor = bgc
    } = do
  glClear $ bgc `whenExists` colorBufferBit `orElse` 0 

undoPrerender :: UndoPrerender -> IO (Maybe())
undoPrerender u = sequence u
