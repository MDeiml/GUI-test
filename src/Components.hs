module Components
    ( label
    , label'
    , background
    ) where

import Control.Arrow
import Drawable
import Layout
import Types
import Widget

background :: Color -> Widget g [Drawable] LayoutParam () ()
background c =
    widgetOutput stdParams {pWeightX = Just 0, pWeightY = Just 0} <<<
    arr (\_ bs -> [DrawShape c $ Rect bs])

label ::
       LayoutParam
    -> String
    -> Int
    -> Color
    -> Widget a [Drawable] LayoutParam String ()
label p font size c =
    widgetOutput p <<<
    arr (\s (Bounds x y _ _) -> [DrawShape c $ Text s font (Coords x y) size])

label' :: LayoutParam -> Int -> Widget a [Drawable] LayoutParam String ()
label' p size =
    label
        p
        "/usr/share/fonts/truetype/ubuntu-font-family/Ubuntu-R.ttf"
        size
        (Color 0 0 0)
