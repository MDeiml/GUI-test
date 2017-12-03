module Drawable
    ( Drawable(..)
    , Color(..)
    , Shape(..)
    ) where

import Data.Word (Word8)
import Resources
import Types

data Drawable t
    = DrawShape Color
                (Shape t)
    | Image t
            Bounds
    | NinePatch t
                (Float, Float, Float, Float)
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
    | Text String
           Coords
           (Font t)
    deriving (Show, Eq)
