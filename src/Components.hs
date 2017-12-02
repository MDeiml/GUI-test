{-# LANGUAGE Arrows #-}
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
import Renderer
import Resources
import Types
import Widget
import qualified Data.Map as M

background :: Color -> Widget g (Cmd t) LayoutParam () ()
background c =
    widgetOutput <<<
    arr
        (const
             ( stdParams {pWeightX = Just 0, pWeightY = Just 0}
             , (: []) . Render . DrawShape c . Rect))

label ::
       Color -> Widget a (Cmd t) LayoutParam (Font t, String) ()
label c = proc (f, s) -> do
    let w = fromIntegral $ sum $ map ((\(_,_,_,_,x) -> x) . fontMetrics f . fromIntegral . fromEnum) s
        h = fromIntegral (ascent f - descent f) + 1.2 * count '\n' s
    widgetOutput -< ( stdParams { pHeight = h, pWidth = w }
             , \(Bounds x y _ _) ->
                   [Render $ DrawShape c $ Text s (Coords x y) f])
    where
        count _ [] = 0
        count a (x:xs)
            | a == x = 1 + count a xs
            | otherwise = count a xs

label' :: LayoutParam -> Int -> Widget' t String ()
label' p size = proc s -> do
    r <- resource -< ResF size "/usr/share/fonts/truetype/ubuntu-font-family/Ubuntu-R.ttf"
    case r of
      (RFont f) -> label (Color 0 0 0) -< (f, s)

resource :: Widget (Globals t) (Cmd t) p ResourceId (Resource t)
resource = proc i -> do
    widgetOutput' -< [LoadResource i]
    g <- globals -< ()
    returnA -< fromJust $ M.lookup i $ gResources g

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

time :: Widget (Globals t) r p () Integer
time = arr gTime <<< globals
