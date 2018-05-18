{-# LANGUAGE Arrows #-}
module Components
    ( label
    , label'
    , background
    , time
    , initial
    , evLast
    , evLast'
    , debug
    , globals
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

debug :: Widget' t String ()
debug = widgetOutput' <<< arr ((:[]) . Debug)

background :: Color -> Widget' t () ()
background c =
    widgetOutput <<<
    arr
        (const
             ( (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
             , (: []) . Render . DrawShape c . Rect))

label ::
       Color -> Widget' t (Font t, String) ()
label c = proc (f, s) -> do
    s' <- shift Nothing -< Just s
    let w = fromIntegral $ sum $ map ((\(_,_,_,_,x) -> x) . fontMetrics f . fromIntegral . fromEnum) s
        h = fromIntegral (ascent f - descent f) + 1.2 * count '\n' s
    widgetOutput -< ( (stdParams { pHeight = h, pWidth = w }, Just s /= s') -- TODO
             , \(Bounds x y _ _) ->
                   [Render $ DrawShape c $ Text s (Coords x y) f])
    where
        count _ [] = 0
        count a (x:xs)
            | a == x = 1 + count a xs
            | otherwise = count a xs

label' :: LayoutParam -> Int -> Widget' t String ()
label' p size = proc s -> do
    r <- resource -< ResF size "/usr/share/fonts/truetype/ubuntu/Ubuntu-R.ttf"
    case r of
      Just (RFont f) -> label (Color 0 0 0) -< (f, s)
      _ -> returnA -< ()

resource :: Widget' t ResourceId (Maybe (Resource t))
resource = proc i -> do
    widgetOutput' -< [LoadResource i]
    g <- globals -< ()
    returnA -< M.lookup i $ gResources g

image :: Widget' t (Sprite t) ()
image = proc (Sprite t w h) -> widgetOutput -< ((stdParams { pWidth = fromIntegral w, pHeight = fromIntegral h}, True), \bs -> [Render $ Image t bs]) -- TODO

-- textfield :: (Weight, Weight) -> Widget' t String String
-- textfield (wx, wy) = stackLayout (0,0,0,0) (AlignCenter, AlignCenter) (wx, wy) $ proc content -> do
--     RNin np <- resource -< ResN "textfield.json"
--     let NP (Sprite _ w h) (xs, ys, xe, ye) = np
--         x0 = fromIntegral w * xs
--         y0 = fromIntegral h * ys
--         x1 = fromIntegral w * (1 - xe)
--         y1 = fromIntegral h * (1 - ye)
--     stackLayout (x0,y0,x1,y1) (AlignCenter, AlignCenter) (wx, wy) $ label' -< content
--     widgetOutput -< (stdParams {pWeightX = wx, pWeightY = wy}, \b@(Bounds x0' y0' x1' y1') -> [Render $ NinePatch np b $ Bounds (x0' + x0) (y0' + y0) (x1' - x1) (y1' - y1)])
--     returnA -< content

globals :: Widget' t () (Globals t)
globals = buildWidget' $ \g _ -> return (g, globals)

initial :: Widget' t a a
initial = buildWidget' $ \_ i -> return (i, arr $ const i)

evLast :: Widget' t (Maybe a) (Maybe a)
evLast = evLast1 Nothing
  where
    evLast1 s =
        buildWidget' $ \_ i ->
            let new = i <|> s
             in return (new, evLast1 new)

evLast' :: a -> Widget' t (Maybe a) a
evLast' x =
    buildWidget' $ \_ i ->
        let new = fromMaybe x i
         in return (new, evLast' new)

time :: Widget' t () Integer
time = arr gTime <<< globals
