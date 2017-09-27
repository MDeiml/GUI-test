{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow (returnA)
import GLFWRenderer
import Lib

test1 :: Widget Event (LayoutItem LayoutParam) () ()
test1 = proc _ -> do
    widgetOutput -< (\b -> LI { layoutDrawables = [DrawShape (Color 255 0 0) $ Rect b], layoutParam = stdParams { pHeight = 150, pWidth = 100 }})
    widgetOutput -< (\b -> LI { layoutDrawables = [DrawShape (Color 0 255 0) $ Rect b], layoutParam = stdParams { pWeightY = Just 1, pWeightX = Just 1 }})
    widgetOutput -< (\b -> LI { layoutDrawables = [DrawShape (Color 0 0 255) $ Rect b], layoutParam = stdParams { pWeightY = Just 2, pWeightX = Just 1 }})
    widgetOutput -< (\b -> LI { layoutDrawables = [DrawShape (Color 255 255 255) $ Rect b], layoutParam = stdParams { pHeight = 50, pWidth = 40 }})
    -- widgetOutput -< (\b -> LI { layoutDrawables = [DrawShape (Color 255 255 255) $ Text "Test" (Coords 0 0) 20], layoutParam = stdParams { pHeight = 50, pWidth = 40 }})

test2 :: Widget Event (LayoutItem LayoutParam) () ()
test2 = widgetLayout (tableLayout 2 (Wrap, Fill)) test1

test2' :: Widget Event (LayoutItem LayoutParam) () ()
test2' = widgetLayout (linearLayout Vertical (Wrap, Fill)) test1

focusWidget :: Widget Event (LayoutItem LayoutParam) () Bool
focusWidget = widgetLayout (stackLayout (AlignCenter, AlignCenter) (Fill, Fill)) $ proc _ -> do
    f <- focusListener -< ()
    let c = if f then Color 255 0 0 else Color 255 255 255
    widgetOutput -< (\b -> LI { layoutParam = stdParams { pWidth = 100, pHeight = 200 }, layoutDrawables = [DrawShape c $ Rect b] })
    returnA -< f

test3 :: Widget Event (LayoutItem LayoutParam) () ()
test3 = widgetLayout (tableLayout 2 (Fill, Fill)) $ proc _ -> do
    focusWidget -< ()
    focusWidget -< ()
    focusWidget -< ()
    focusWidget -< ()
    returnA -< ()

main :: IO ()
main = do
    r <- (create "Test" (800,600) :: IO GLFWRenderer)
    mainLoop r test3
