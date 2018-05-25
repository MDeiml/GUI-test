{-# LANGUAGE Arrows #-}

module Components
    ( background
    , initial
    , evLast
    , debug
    , image
    , module Textcomponent
    ) where

import Control.Arrow
import Data.Maybe
import Drawable
import Input
import Layout
import Resources
import Textcomponent
import Widget

background :: Color -> Widget' t () ()
background c =
    widgetOutput <<<
    arr
        (const
             ( (stdParams {pWeightX = Just 1, pWeightY = Just 1}, False)
             , (: []) . DrawShape c . Rect))

image :: Widget' t (Sprite t) ()
image =
    proc (Sprite t w h) ->
  do dim <- shift Nothing -< Just (w, h)
     widgetOutput -<
       ((stdParams{pWidth = fromIntegral w, pHeight = fromIntegral h},
         dim /= Just (w, h)),
        \ bs -> [Image t bs])

initial :: Widget' t a a
initial = buildWidget' $ \i -> return (i, arr $ const i)

evLast :: a -> Widget' t (Maybe a) a
evLast = evLast1
  where
    evLast1 s =
        buildWidget' $ \i ->
            let new = fromMaybe s i
            in return (new, evLast1 new)
