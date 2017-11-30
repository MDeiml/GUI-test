module Drawable
    ( Drawable(..)
    , Color(..)
    , Shape(..)
    ) where

import Data.Word (Word8)
import Types

data Drawable =
    DrawShape Color
              Shape
    deriving (Show, Eq)

data Color =
    Color Word8
          Word8
          Word8
    deriving (Show, Eq)

data Shape
    = Rect { rectBounds :: Bounds }
    | Line { p0 :: Coords
           , p1 :: Coords }
    | Text { text :: String
           , fontname :: String
           , p0 :: Coords
           , size :: Int }
    deriving (Show, Eq)
