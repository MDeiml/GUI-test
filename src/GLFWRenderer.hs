{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module GLFWRenderer
    ( module Renderer
    , GLFWRenderer
    ) where

import qualified Codec.Picture as P
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.Vector.Storable (unsafeWith)
import Data.Word
import Drawable
import Font
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as G
import Input
import Renderer
import Resources
import Texture
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

vertex :: Float -> Float -> IO ()
vertex x y =
    GL.vertex
        (GL.Vertex3 (realToFrac x) (realToFrac y) 0 :: GL.Vertex3 GL.GLdouble)

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

renderInit :: GLFWRenderer -> GL.Program -> IO ()
renderInit re shader = do
    G.makeContextCurrent $ Just $ window re
    (w, h) <- G.getFramebufferSize $ window re
    GL.loadIdentity
    GL.ortho 0 (fromIntegral w) (fromIntegral h) 0 1 (-1)
    GL.currentProgram $= Just shader

instance Renderer GLFWRenderer Texture where
    create title (w, h) = do
        print 2
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
        renderInit re (defaultShader re)
        GL.textureBinding GL.Texture2D $= Nothing
        GL.renderPrimitive GL.Quads $ do
            color r g b
            vertex x0 y0
            vertex x1 y0
            vertex x1 y1
            vertex x0 y1
    render re (Image (Texture (tex, u0, v0, u1, v1)) (Bounds x0 y0 x1 y1)) = do
        renderInit re $ defaultShader re
        GL.textureBinding GL.Texture2D $= Just tex
        GL.renderPrimitive GL.Quads $ do
            color 0 0 0
            texCoord u0 v0
            vertex x0 y0
            texCoord u1 v0
            vertex x1 y0
            texCoord u1 v1
            vertex x1 y1
            texCoord u0 v1
            vertex x0 y1
    render re (NinePatch (NP (Sprite (Texture (tex, u0, v0, u1, v1)) _ _) (us', vs', ue', ve')) (Bounds x0 y0 x1 y1) (Bounds xs ys xe ye)) = do
        renderInit re $ defaultShader re
        GL.textureBinding GL.Texture2D $= Just tex
        let w = u1 - u0
            h = v1 - v0
            us = u0 + us' * w
            ue = u0 + ue' * w
            vs = v0 + vs' * h
            ve = v0 + ve' * h
        GL.renderPrimitive GL.Quads $ do
            color 0 0 0
            texCoord u0 v0 -- Left Bottom
            vertex x0 y0
            texCoord us v0
            vertex xs y0
            texCoord us vs
            vertex xs ys
            texCoord u0 vs
            vertex x0 ys
            texCoord us v0 -- Bottom
            vertex xs y0
            texCoord ue v0
            vertex xe y0
            texCoord ue vs
            vertex xe ys
            texCoord us vs
            vertex xs ys
            texCoord ue v0 -- Bottom Right
            vertex xe y0
            texCoord u1 v0
            vertex x1 y0
            texCoord u1 vs
            vertex x1 ys
            texCoord ue vs
            vertex xe ys
            texCoord u0 vs -- Left
            vertex x0 ys
            texCoord us vs
            vertex xs ys
            texCoord us ve
            vertex xs ye
            texCoord u0 ve
            vertex x0 ye
            texCoord us vs -- Center
            vertex xs ys
            texCoord ue vs
            vertex xe ys
            texCoord ue ve
            vertex xe ye
            texCoord us ve
            vertex xs ye
            texCoord ue vs -- Right
            vertex xe ys
            texCoord u1 vs
            vertex x1 ys
            texCoord u1 ve
            vertex x1 ye
            texCoord ue ve
            vertex xe ye
            texCoord u0 ve -- Left Top
            vertex x0 ye
            texCoord us ve
            vertex xs ye
            texCoord us v1
            vertex xs y1
            texCoord u0 v1
            vertex x0 y1
            texCoord us ve -- Top
            vertex xs ye
            texCoord ue ve
            vertex xe ye
            texCoord ue v1
            vertex xe y1
            texCoord us v1
            vertex xs y1
            texCoord ue ve -- Top Right
            vertex xe ye
            texCoord u1 ve
            vertex x1 ye
            texCoord u1 v1
            vertex x1 y1
            texCoord ue v1
            vertex xe y1
    render re (DrawShape (Color r g b) (Text text (Coords x y) f)) = do
        renderInit re (fontShader re)
        let Texture (tex, _, _, _, _) =
                glyphs f $ fromIntegral $ fromEnum $ head text
        GL.textureBinding GL.Texture2D $= Just tex
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
                                 Texture (_, x0, y0, x1, y1) = glyphs f c'
                                 fi = fromIntegral
                                 (w, h, bx, by, a) =
                                     let (a1, a2, a3, a4, a5) = fontMetrics f c'
                                     in (fi a1, fi a2, fi a3, fi a4, fi a5)
                                 y'' =
                                     fromIntegral $
                                     round $ y' + fromIntegral (ascent f) - by
                                 x'' = fromIntegral $ round $ x' + bx
                             texCoord x0 y0
                             vertex x'' y''
                             texCoord x1 y0
                             vertex (x'' + w) y''
                             texCoord x1 y1
                             vertex (x'' + w) (y'' + h)
                             texCoord x0 y1
                             vertex x'' (y'' + h)
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
    loadResource re (ResN fp) =
        maybe (Error "") RNin <$> runMaybeT (loadNinpatch re fp)
    loadResource re (ResS fp) = do
        img <- P.readImage fp
        case img of
            Left s -> return $ Error s
            Right img' ->
                case img' of
                    P.ImageRGBA8 i ->
                        loadImage i GL.RGBA8 GL.RGBA GL.UnsignedByte
                    P.ImageRGB8 i -> loadImage i GL.RGB8 GL.RGB GL.UnsignedByte
      where
        loadImage img internal format datatype =
            unsafeWith (P.imageData img) $ \buf -> do
                tex <- GL.genObjectName
                let w = P.imageWidth img
                    h = P.imageHeight img
                GL.textureBinding GL.Texture2D $= Just tex
                GL.texImage2D
                    GL.Texture2D
                    GL.NoProxy
                    0
                    internal
                    (GL.TextureSize2D (fromIntegral w) (fromIntegral h))
                    0
                    (GL.PixelData format datatype buf)
                GL.textureFilter GL.Texture2D $=
                    ((GL.Linear', Nothing), GL.Linear')
                GL.textureWrapMode GL.Texture2D GL.S $=
                    (GL.Repeated, GL.ClampToEdge)
                GL.textureWrapMode GL.Texture2D GL.T $=
                    (GL.Repeated, GL.ClampToEdge)
                return $ RSpr $ Sprite (Texture (tex, 0, 0, 1, 1)) w h
