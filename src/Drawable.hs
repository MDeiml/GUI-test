module Drawable
    ( Drawable(..)
    , Color(..)
    , Shape(..)
    ) where

import Data.Word (Word8)
import Resources
import Types

data Drawable t =
    DrawShape Color
              (Shape t)
    deriving (Show, Eq)

data Color =
    Color Word8
          Word8
          Word8
    deriving (Show, Eq)

data Shape t
    = Rect { rectBounds :: Bounds }
    | Line { p0 :: Coords
           , p1 :: Coords }
    | Text { text :: String
           , p0 :: Coords
           , font :: Font t }
    deriving (Show, Eq)
