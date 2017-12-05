{-# LANGUAGE ForeignFunctionInterface #-}

module Sdl (
  Subsystem(..),
  deinitialize,
  getError,
  initialize,

  Window,
  WindowDetails(..),
  WindowAttr(..),
  centerX,
  centerY,
  createWindow,
  destroyWindow,

  GlContext,
  createGlContext,
  deleteGlContext,
  swapGlWindow,

  Event(..),
  pollEvent,
  withEventContext
) where

import Data.Bits
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

bits :: (a -> Int) -> [a] -> Int
bits conv = foldl (\b a -> b .|. conv a) 0

--------------------
-- Initialization

data Subsystem =
  VideoSubsystem

fromSubsystem' VideoSubsystem = videoSubsystem

foreign import ccall "SDL_Init" initialize' :: Int -> IO Int
foreign import ccall "SDL_Quit" deinitialize :: IO ()
foreign import ccall "SDL_GetError" getError' :: IO CString
foreign import ccall "video_subsystem" videoSubsystem :: Int

getError :: IO String
getError = getError' >>= peekCString

initialize :: [Subsystem] -> IO (Either String ())
initialize ss = do
  result <- initialize' $ bits fromSubsystem' ss
  if result < 0
    then fmap Left getError
    else return $ Right ()

-------------
-- Windows

data Window

data WindowAttr =
  Shown
  | WithOpenGl

data WindowDetails = WindowDetails {
  title :: String,
  pos   :: (Int, Int),
  size  :: (Int, Int),
  attrs :: [WindowAttr]
}

foreign import ccall "window_center_pos" centerPos :: Int
foreign import ccall "window_opengl" withOpenGl :: Int
foreign import ccall "window_shown" shown :: Int

centerX = centerPos
centerY = centerPos

fromWindowAttr' Shown = shown
fromWindowAttr' WithOpenGl = withOpenGl

foreign import ccall "SDL_CreateWindow" createWindow' :: CString -> Int -> Int -> Int -> Int -> Int -> IO (Ptr Window)
foreign import ccall "SDL_DestroyWindow" destroyWindow :: Ptr Window -> IO ()

createWindow :: WindowDetails -> IO (Ptr Window)
createWindow (WindowDetails t (x, y) (w, h) as) =
  withCString t (\t' -> createWindow' t' x y w h f)
    where f = bits fromWindowAttr' as

---------------------
-- OpenGl Contexts

data GlContext

foreign import ccall "SDL_GL_CreateContext" createGlContext :: Ptr Window -> IO (Ptr GlContext)
foreign import ccall "SDL_GL_DeleteContext" deleteGlContext :: Ptr GlContext -> IO ()
foreign import ccall "SDL_GL_SwapWindow" swapGlWindow :: Ptr Window -> IO ()

------------
-- Events

data EventContext

foreign import ccall "event_size" eventSize' :: Int
foreign import ccall "event_type_keydown" keyDown' :: Int
foreign import ccall "event_type_quit" quit' :: Int

foreign import ccall "SDL_PollEvent" pollEvent' :: Ptr EventContext -> IO Int
foreign import ccall "event_type" eventType' :: Ptr EventContext -> Int

data Event =
  Quit
  | KeyDown
  | UnknownEvent
  deriving Eq

eventType'' e
  | e' == keyDown' = KeyDown
  | e' == quit'    = Quit
  | otherwise      = UnknownEvent
  where e' = eventType' e

pollEvent :: Ptr EventContext -> IO (Maybe Event)
pollEvent e = do
  pending <- pollEvent' e
  return $ if pending == 0
    then Nothing
    else Just $ eventType'' e

withEventContext = allocaBytes eventSize'
