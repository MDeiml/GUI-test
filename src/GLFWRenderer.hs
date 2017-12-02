{-# LANGUAGE MultiParamTypeClasses #-}

module GLFWRenderer
    ( module Renderer
    , GLFWRenderer
    ) where

import Control.Arrow ((***))
import Control.Monad
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map as M
import Data.Word
import Drawable
import Font
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as G
import Input
import Renderer
import Resources
import Types

data GLFWRenderer = GLFWRenderer
    { window :: G.Window
    , events :: IORef [Event]
    , defaultShader :: GL.Program
    , fontShader :: GL.Program
    }

color :: Word8 -> Word8 -> Word8 -> IO ()
color r g b =
    GL.color
        (GL.Color4
             (realToFrac r / 255)
             (realToFrac g / 255)
             (realToFrac b / 255)
             1 :: GL.Color4 GL.GLdouble)

vertex :: Float -> Float -> Float -> IO ()
vertex x y z =
    GL.vertex
        (GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z) :: GL.Vertex3 GL.GLdouble)

texCoord :: Float -> Float -> IO ()
texCoord x y =
    GL.texCoord
        (GL.TexCoord2 (realToFrac x) (realToFrac y) :: GL.TexCoord2 GL.GLdouble)

loadShader :: FilePath -> GL.ShaderType -> IO (Either String GL.Shader)
loadShader fp st = do
    shader <- GL.createShader st
    source <- BS.readFile fp
    GL.shaderSourceBS shader $= source
    GL.compileShader shader
    status <- GL.get $ GL.compileStatus shader
    if status
        then return $ Right shader
        else Left <$> GL.get (GL.shaderInfoLog shader)

createProgram :: (FilePath, FilePath) -> IO GL.Program
createProgram (fileV, fileF) = do
    vert <- either error id <$> loadShader fileV GL.VertexShader
    frag <- either error id <$> loadShader fileF GL.FragmentShader
    prog <- GL.createProgram
    GL.attachShader prog vert
    GL.attachShader prog frag
    GL.linkProgram prog
    status <- GL.get $ GL.linkStatus prog
    if status
        then return prog
        else GL.get (GL.programInfoLog prog) >>= error

renderInit :: GLFWRenderer -> GL.Program -> IO (Int, Int)
renderInit re shader = do
    G.makeContextCurrent $ Just $ window re
    (w, h) <- G.getFramebufferSize $ window re
    GL.loadIdentity
    GL.ortho 0 (fromIntegral w) (fromIntegral h) 0 1 (-1)
    GL.currentProgram $= Just shader
    return (w, h)

instance Renderer GLFWRenderer GL.TextureObject where
    create title (w, h) = do
        _ <- G.init
        win <- G.createWindow w h title Nothing Nothing
        maybe
            (error "Window could not be crated")
            (\win' -> do
                 G.makeContextCurrent $ Just win'
                 GL.texture GL.Texture2D $= GL.Enabled
                 es <- newIORef []
                 fs <- newIORef []
                 G.setKeyCallback win' $ Just $ kc es
                 G.setMouseButtonCallback win' $ Just $ mc es
                 G.setCursorPosCallback win' $ Just $ mmc es
                 fontS <- createProgram ("default.vert", "fontShader.frag")
                 defS <- createProgram ("default.vert", "default.frag")
                 GL.blend $= GL.Enabled
                 GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
                 return
                     GLFWRenderer
                     { window = win'
                     , events = es
                     , defaultShader = defS
                     , fontShader = fontS
                     })
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
        (w, h) <- renderInit re (defaultShader re)
        GL.renderPrimitive GL.Quads $ do
            color r g b
            vertex x0 y0 0
            vertex x1 y0 0
            vertex x1 y1 0
            vertex x0 y1 0
    render re (DrawShape (Color r g b) (Text text (Coords x y) f)) = do
        (w, h) <- renderInit re (fontShader re)
        GL.renderPrimitive GL.Quads $ do
            color r g b
            foldM_
                (\(x', y') c ->
                     if c == '\n'
                         then return
                                  ( x
                                  , y' +
                                    fromIntegral
                                        (ascent f - descent f +
                                         round (fromIntegral (fontsize f) * 0.2)))
                         else do
                             let c' = fromIntegral $ fromEnum c
                                 (x0, y0, x1, y1) = charCoords f c'
                                 fi = fromIntegral
                                 (w, h, bx, by, a) =
                                     let (a1, a2, a3, a4, a5) = fontMetrics f c'
                                     in (fi a1, fi a2, fi a3, fi a4, fi a5)
                                 y'' = y' + fromIntegral (ascent f) - by
                                 x'' = x' + bx
                             texCoord x0 y0
                             vertex x'' y'' 0
                             texCoord x1 y0
                             vertex (x'' + w) y'' 0
                             texCoord x1 y1
                             vertex (x'' + w) (y'' + h) 0
                             texCoord x0 y1
                             vertex x'' (y'' + h) 0
                             return (x' + a, y'))
                (x, y)
                text
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
    loadResource re (ResF size fontname) = do
        G.makeContextCurrent $ Just $ window re
        RFont <$> generateAtlas fontname size
