module GLUTRenderer
  ( module Renderer
  , GLUTRenderer
  ) where

import Data.IORef
import Data.Word
import Drawable
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT (($=))
import Renderer
import Types

data GLUTRenderer = GLUTRenderer
  { window :: GL.Window
  , closeRequested :: IORef Bool
  , events :: IORef [Event]
  }

color :: Word8 -> Word8 -> Word8 -> IO ()
color r g b =
  GL.color $
  GL.Color3
    (fromIntegral r / 255)
    (fromIntegral g / 255)
    ((fromIntegral b / 255) :: GL.GLfloat)

vertex :: Float -> Float -> Float -> IO ()
vertex x y z = do
  GL.vertex $
    GL.Vertex3 (realToFrac x) (realToFrac y) ((realToFrac z) :: GL.GLfloat)

instance Renderer GLUTRenderer where
  create title (w, h) = do
    _ <- GL.getArgsAndInitialize
    GL.initialWindowSize $= GL.Size (fromIntegral w) (fromIntegral h)
    win <- GL.createWindow title
    cr <- newIORef False
    GL.closeCallback $= Just (cr $= True)
    es <- newIORef []
    GL.keyboardMouseCallback $= Just (kc es)
    return $ GLUTRenderer {window = win, closeRequested = cr, events = es}
    where
      ks GL.Up = KeyUp
      ks GL.Down = KeyDown
      kc es (GL.Char c) keystate _modifiers (GL.Position x y) = do
        modifyIORef
          es
          (Event (Key c) (ks keystate) (fromIntegral x, fromIntegral y) :)
      kc es _ _ _ _ = return ()
  render re (DrawShape (Color r g b) (Rect (Bounds x0 y0 x1 y1))) = do
    GL.currentWindow $= Just (window re)
    GL.preservingMatrix $ do
      GL.Size w h <- GL.get GL.windowSize
      GL.translate $ GL.Vector3 (-1) 1 (0 :: GL.GLfloat)
      GL.scale (2 / fromIntegral w) (-2 / fromIntegral h) (1 :: GL.GLfloat)
      GL.renderPrimitive GL.Quads $ do
        color r g b
        vertex x0 y0 0
        vertex x1 y0 0
        vertex x1 y1 0
        vertex x0 y1 0
  render re (DrawShape (Color r g b) (Text s (Coords x y) size)) = do
    GL.currentWindow $= Just (window re)
    GL.preservingMatrix $ do
      let f = GL.Roman
      GL.Size w h <- GL.get GL.windowSize
      GL.translate $ GL.Vector3 (-1) 1 (0 :: GL.GLfloat)
      GL.scale (2 / fromIntegral w) (-2 / fromIntegral h) (1 :: GL.GLfloat)
      GL.translate $ GL.Vector3 (realToFrac x) (realToFrac y) (0 :: GL.GLfloat)
      fh <- GL.fontHeight f
      GL.scale (size / fh) (-size / fh) (1 :: GL.GLfloat)
      GL.renderString f s
  pollEvents r = do
    es <- readIORef $ events r
    writeIORef (events r) []
    return es
  clear r = do
    GL.mainLoopEvent
    GL.currentWindow $= Just (window r)
    GL.clear [GL.ColorBuffer]
  swapBuffers r = do
    GL.currentWindow $= Just (window r)
    GL.swapBuffers
  getSize r = do
    GL.currentWindow $= Just (window r)
    GL.Size w h <- GL.get GL.windowSize
    return (fromIntegral w, fromIntegral h)
  closing r = do
    GL.currentWindow $= Just (window r)
    GL.get (closeRequested r)
