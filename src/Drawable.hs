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
  deriving (Show)

data Color =
  Color Word8
        Word8
        Word8
  deriving (Show)

data Shape
  = Rect { rectBounds :: Bounds }
  | Line { p0 :: Coords
         , p1 :: Coords }
  | Text { text :: String
         , p0 :: Coords
         , size :: Float }
  deriving (Show)
