module Types
    ( Bounds(..)
    , Coords(..)
    , inside
    ) where

data Bounds =
    Bounds Float
           Float
           Float
           Float
    deriving (Show, Eq)

data Coords =
    Coords Float
           Float
    deriving (Show, Eq)

inside :: Coords -> Bounds -> Bool
inside (Coords x y) (Bounds x0 y0 x1 y1) =
    x >= x0 && x < x1 && y >= y0 && y < y1
