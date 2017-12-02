module Components
    ( label
    , label'
    , background
    , time
    , initial
    , evLast
    , evLast'
    ) where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Drawable
import Input
import Layout
import Types
import Widget

background :: Color -> Widget g [Drawable] LayoutParam () ()
background c =
    widgetOutput <<<
    arr
        (const
             ( stdParams {pWeightX = Just 0, pWeightY = Just 0}
             , (: []) . DrawShape c . Rect))

label ::
       LayoutParam
    -> String
    -> Int
    -> Color
    -> Widget a [Drawable] LayoutParam String ()
label p font size c =
    widgetOutput <<<
    arr
        (\s ->
             ( p
             , \(Bounds x y _ _) ->
                   [DrawShape c $ Text s font (Coords x y) size]))

label' :: LayoutParam -> Int -> Widget a [Drawable] LayoutParam String ()
label' p size =
    label
        p
        "/usr/share/fonts/truetype/ubuntu-font-family/Ubuntu-R.ttf"
        size
        (Color 0 0 0)

globals :: Widget g r p () g
globals = buildWidget' $ \g _ -> (g, globals)

initial :: Widget g r p a a
initial = buildWidget' $ \_ i -> (i, arr $ const i)

evLast :: Widget g r p (Maybe a) (Maybe a)
evLast = evLast1 Nothing
  where
    evLast1 s =
        buildWidget' $ \_ i ->
            let new = i <|> s
            in (new, evLast1 new)

evLast' :: a -> Widget g r p (Maybe a) a
evLast' x =
    buildWidget' $ \_ i ->
        let new = fromMaybe x i
        in (new, evLast' new)

time :: Widget Globals r p () Integer
time = arr gTime <<< globals
