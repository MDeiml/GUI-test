module Drawable
    ( Drawable(..)
    , Color(..)
    , Shape(..)
    ) where

import           Data.Text (Text)
import           Data.Word (Word8)
import           Resources
import           Types

data Drawable t
    = DrawShape Color
                (Shape t)
    | Image t
            Bounds
    | NinePatch (NinePatch t)
                Bounds
                Bounds
    deriving (Show, Eq)

data Color =
    Color Word8
          Word8
          Word8
    deriving (Show, Eq)

data Shape t
    = Rect Bounds
    | Line Coords
           Coords
    | DrawText Text
               Coords
               (Font t)
    deriving (Show, Eq)
