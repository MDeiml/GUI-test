module Types
  ( Bounds(..)
  , Coords(..)
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
