module Texture
    ( Texture(..)
    ) where

import qualified Graphics.Rendering.OpenGL as GL

newtype Texture =
    Texture (GL.TextureObject, Float, Float, Float, Float)
