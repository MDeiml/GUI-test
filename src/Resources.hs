{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Resources
    ( ResourceId(..)
    , Font(..)
    , Sprite(..)
    , NinePatch(..)
    , Resources(..)
    , resLookup
    , resInsert
    , resEmpty
    ) where

import qualified Data.Map as M
import Data.Monoid

data Sprite t =
    Sprite t
           Int
           Int
    deriving (Show, Eq)

data Font t = Font
    { glyphs :: Integer -> t
    , fontMetrics :: Integer -> (Int, Int, Int, Int, Int)
    , ascent :: Int
    , descent :: Int
    , fontname :: String
    , fontsize :: Int
    }

data NinePatch t =
    NP (Sprite t)
       (Float, Float, Float, Float)
    deriving (Show, Eq)

data ResourceId a where
    ResF :: Int -> String -> ResourceId Font
    ResS :: FilePath -> ResourceId Sprite
    ResN :: FilePath -> ResourceId NinePatch

deriving instance Eq (ResourceId a)

deriving instance Show (ResourceId a)

instance Ord (ResourceId Font) where
    compare (ResF s1 n1) (ResF s2 n2) = compare s1 s2 <> compare n1 n2

instance Ord (ResourceId Sprite) where
    compare (ResS f1) (ResS f2) = compare f1 f2

instance Ord (ResourceId NinePatch) where
    compare (ResN f1) (ResN f2) = compare f1 f2

type ResourceMap a t = M.Map (ResourceId a) (a t)

data Resources t = Resources
    { resourceFonts :: ResourceMap Font t
    , resourceSprites :: ResourceMap Sprite t
    , resourceNinePatches :: ResourceMap NinePatch t
    }

resEmpty = Resources M.empty M.empty M.empty

resLookup :: ResourceId a -> Resources t -> Maybe (a t)
resLookup i@(ResF _ _) = M.lookup i . resourceFonts
resLookup i@(ResS _) = M.lookup i . resourceSprites
resLookup i@(ResN _) = M.lookup i . resourceNinePatches

resInsert :: ResourceId a -> a t -> Resources t -> Resources t
resInsert i@(ResF _ _) r res =
    res {resourceFonts = M.insert i r $ resourceFonts res}
resInsert i@(ResS _) r res =
    res {resourceSprites = M.insert i r $ resourceSprites res}
resInsert i@(ResN _) r res =
    res {resourceNinePatches = M.insert i r $ resourceNinePatches res}

instance Show (Font t) where
    show f = "Font " ++ show (fontname f) ++ " " ++ show (fontsize f)

instance Eq (Font t) where
    (==) f1 f2 = fontname f1 == fontname f2 && fontsize f1 == fontsize f2
