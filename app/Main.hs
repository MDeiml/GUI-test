{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow
import GLFWRenderer
import Lib
import Font
import Components

test1 :: App
test1 = proc _ -> do
    widgetOutput  -< (stdParams { pHeight = 150, pWidth = 100 }, \b -> [DrawShape (Color 255 0 0) $ Rect b])
    widgetOutput  -< (stdParams { pWeightY = Just 1, pWeightX = Just 1 }, \b -> [DrawShape (Color 0 255 0) $ Rect b])
    widgetOutput  -< (stdParams { pWeightY = Just 2, pWeightX = Just 1 }, \b -> [DrawShape (Color 0 0 255) $ Rect b])
    widgetOutput  -< (stdParams { pHeight = 50, pWidth = 40 }, \b -> [DrawShape (Color 255 255 255) $ Rect b])

test2 :: App
test2 = tableLayout 2 (Nothing, Just 1) test1

test2' :: App
test2' = linearLayout Vertical (Nothing, Just 1) test1

focusWidget :: Widget' () Bool
focusWidget = stackLayout (0,0,0,0) (AlignCenter, AlignCenter) (Just 1, Just 1) $ proc _ -> do
    f <- focusListener -< ()
    fOld <- shift False -< f
    let c = if f then Color 255 0 0 else Color 255 255 255
    widgetOutput -< (stdParams {pWeightX = Just 1, pWeightY = Just 1}, \b -> [DrawShape c $ Rect b])
    returnA -< f

test3 :: App
test3 = tableLayout 2 (Just 1, Just 1) $ proc _ -> do
    focusWidget -< ()
    focusWidget -< ()
    focusWidget -< ()
    focusWidget -< ()
    returnA -< ()

test4 :: App
test4 = stackLayout (0, 0, 0, 0) (AlignCenter, AlignCenter) (Just 1, Just 1) $ proc _ -> do
    t <- time -< ()
    t0 <- initial -< t
    label' stdParams { pHeight = 50, pWidth = 40} 60 -< show $ (t - t0) `quot` 1000
    background (Color 255 255 255) -< ()

test :: IO ()
test = do
    r <- create "Test" (800,600) :: IO GLFWRenderer
    mainLoop r 15 test4

main = test
