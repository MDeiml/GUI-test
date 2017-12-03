module Resources
    ( Resource(..)
    , ResourceId(..)
    , Resources
    , Font(..)
    , Sprite(..)
    ) where

import qualified Data.Map as M

data Resource t
    = RFont (Font t)
    | RSpr (Sprite t)
    | Error String

data Sprite t =
    Sprite t
           Int
           Int

data Font t = Font
    { glyphs :: Integer -> t
    , fontMetrics :: Integer -> (Int, Int, Int, Int, Int)
    , ascent :: Int
    , descent :: Int
    , fontname :: String
    , fontsize :: Int
    }

data ResourceId
    = ResF Int
           String
    | ResS FilePath
    deriving (Eq, Ord, Show)

type Resources t = M.Map ResourceId (Resource t)

instance Show (Font t) where
    show f = "Font " ++ show (fontname f) ++ " " ++ show (fontsize f)

instance Eq (Font t) where
    (==) f1 f2 = fontname f1 == fontname f2 && fontsize f1 == fontsize f2
