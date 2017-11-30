{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow
import GLFWRenderer
import Lib
import Font

test1 :: Widget Event [Drawable] LayoutParam () ()
test1 = proc _ -> do
    widgetOutput stdParams { pHeight = 150, pWidth = 100 } -< (\b -> [DrawShape (Color 255 0 0) $ Rect b])
    widgetOutput stdParams { pWeightY = Just 1, pWeightX = Just 1 } -< (\b -> [DrawShape (Color 0 255 0) $ Rect b])
    widgetOutput stdParams { pWeightY = Just 2, pWeightX = Just 1 } -< (\b -> [DrawShape (Color 0 0 255) $ Rect b])
    widgetOutput stdParams { pHeight = 50, pWidth = 40 } -< (\b -> [DrawShape (Color 255 255 255) $ Rect b])

test2 :: Widget Event [Drawable] LayoutParam () ()
test2 = widgetLayout (tableLayout 2 (Nothing, Just 1)) test1

test2' :: Widget Event [Drawable] LayoutParam () ()
test2' = widgetLayout (linearLayout Vertical (Nothing, Just 1)) test1

focusWidget :: Widget Event [Drawable] LayoutParam () Bool
focusWidget = widgetLayout (stackLayout (0,0,0,0) (AlignCenter, AlignCenter) (Just 1, Just 1)) $ proc _ -> do
    f <- focusListener -< ()
    fOld <- shift False -< f
    let c = if f then Color 255 0 0 else Color 255 255 255
    widgetOutput (stdParams {pWeightX = Just 1, pWeightY = Just 1}) -< (\b -> [DrawShape c $ Rect b])
    returnA -< f

test3 :: Widget Event [Drawable] LayoutParam () ()
test3 = widgetLayout (tableLayout 2 (Just 1, Just 1)) $ proc _ -> do
    focusWidget -< ()
    focusWidget -< ()
    focusWidget -< ()
    focusWidget -< ()
    returnA -< ()

test4 :: Widget Event [Drawable] LayoutParam () ()
test4 = widgetOutput stdParams { pHeight = 50, pWidth = 40 } <<< arr (const  (\(Bounds x y _ _) -> [DrawShape (Color 1 1 1) $ Text "test, der" "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" (Coords x y) 60]))

test :: IO ()
test = do
    r <- create "Test" (800,600) :: IO GLFWRenderer
    mainLoop r test4

main = test
