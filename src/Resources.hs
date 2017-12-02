module Resources
    ( Resource(..)
    , ResourceId(..)
    , Resources
    , Font(..)
    ) where

import qualified Data.Map as M

newtype Resource t =
    RFont (Font t)

data Font t = Font
    { fontTex :: t
    , charCoords :: Integer -> (Float, Float, Float, Float)
    , fontMetrics :: Integer -> (Int, Int, Int, Int, Int)
    , ascent :: Int
    , descent :: Int
    , fontname :: String
    , fontsize :: Int
    }

data ResourceId =
    ResF Int
         String
    deriving (Eq, Ord, Show)

type Resources t = M.Map ResourceId (Resource t)

instance Show (Font t) where
    show f = "Font " ++ show (fontname f) ++ " " ++ show (fontsize f)

instance Eq (Font t) where
    (==) f1 f2 = fontname f1 == fontname f2 && fontsize f1 == fontsize f2
