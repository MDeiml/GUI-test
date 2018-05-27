module Texture
    ( Texture(..)
    ) where

import qualified Graphics.Rendering.OpenGL as GL
import Types

data Texture =
    Texture GL.TextureObject Bounds
