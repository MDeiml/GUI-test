module GLFWRenderer
    ( module Renderer
    , GLFWRenderer
    ) where

import Data.IORef
import Data.Word
import Drawable
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as G
import Input
import Renderer
import Types

data GLFWRenderer = GLFWRenderer
    { window :: G.Window
    , events :: IORef [Event]
    }

color :: Word8 -> Word8 -> Word8 -> IO ()
color r g b =
    GL.color
        (GL.Color3 (realToFrac r) (realToFrac g) (realToFrac b) :: GL.Color3 GL.GLdouble)

vertex :: Float -> Float -> Float -> IO ()
vertex x y z =
    GL.vertex
        (GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z) :: GL.Vertex3 GL.GLdouble)

instance Renderer GLFWRenderer where
    create title (w, h) = do
        _ <- G.init
        win <- G.createWindow w h title Nothing Nothing
        maybe
            (error "Window could not be crated")
            (\win' -> do
                 es <- newIORef []
                 G.setKeyCallback win' $ Just $ kc es
                 G.setMouseButtonCallback win' $ Just $ mc es
                 G.setCursorPosCallback win' $ Just $ mmc es
                 return GLFWRenderer {window = win', events = es})
            win
      where
        kc es _win key _code ks _mod = do
            let ks' =
                    case ks of
                        G.KeyState'Pressed -> KeyDown
                        G.KeyState'Released -> KeyUp
                        G.KeyState'Repeating -> KeyRepeat
                event = KeyEvent key ks'
            modifyIORef es (event :)
        mc es win button bs _mod = do
            (x, y) <- G.getCursorPos win
            let bs' =
                    case bs of
                        G.MouseButtonState'Pressed -> ButtonDown
                        G.MouseButtonState'Released -> ButtonUp
                button' = fromEnum button
                event =
                    MouseEvent
                        button'
                        bs'
                        (Coords (realToFrac x) (realToFrac y))
            modifyIORef es (event :)
        mmc es win x y =
            let event = MouseMoveEvent (Coords (realToFrac x) (realToFrac y))
            in modifyIORef es (event :)
    render re (DrawShape (Color r g b) (Rect (Bounds x0 y0 x1 y1))) = do
        (w, h) <- G.getFramebufferSize $ window re
        G.makeContextCurrent $ Just $ window re
        GL.loadIdentity
        GL.ortho 0 (fromIntegral w) (fromIntegral h) 0 1 (-1)
        GL.renderPrimitive GL.Quads $ do
            color r g b
            vertex x0 y0 0
            vertex x1 y0 0
            vertex x1 y1 0
            vertex x0 y1 0
    clear r = do
        G.makeContextCurrent $ Just $ window r
        GL.clear [GL.ColorBuffer]
    swapBuffers r = G.swapBuffers $ window r
    getSize r = do
        (w, h) <- G.getFramebufferSize $ window r
        return (fromIntegral w, fromIntegral h)
    closing r = G.windowShouldClose $ window r
    pollEvents r = do
        G.makeContextCurrent $ Just $ window r
        G.pollEvents
        es <- readIORef $ events r
        writeIORef (events r) []
        return es
