{-# LANGUAGE Arrows #-}

module Components
    ( background
    , initial
    , evLast
    , evLast'
    , debug
    , once
    , image
    , module Textcomponent
    ) where

import Control.Applicative
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
             ( (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
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

once :: a -> Widget' t () (Maybe a)
once x = buildWidget' $ const $ return (Just x, arr $ const Nothing)

evLast :: Widget' t (Maybe a) (Maybe a)
evLast = evLast1 Nothing
  where
    evLast1 s =
        buildWidget' $ \i ->
            let new = i <|> s
            in return (new, evLast1 new)

evLast' :: a -> Widget' t (Maybe a) a
evLast' x =
    buildWidget' $ \i ->
        let new = fromMaybe x i
        in return (new, evLast' new)
