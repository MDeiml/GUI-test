{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow
import GLFWRenderer
import Lib

test1 :: App t
test1 = proc _ -> do
    widgetOutput  -< ((stdParams { pHeight = 150, pWidth = 100 }, False), \b -> [Render $ DrawShape (Color 255 0 0) $ Rect b])
    widgetOutput  -< ((stdParams { pWeightY = Just 1, pWeightX = Just 1 }, False), \b -> [Render $ DrawShape (Color 0 255 0) $ Rect b])
    widgetOutput  -< ((stdParams { pWeightY = Just 2, pWeightX = Just 1 }, False), \b -> [Render $ DrawShape (Color 0 0 255) $ Rect b])
    widgetOutput  -< ((stdParams { pHeight = 50, pWidth = 40 }, False), \b -> [Render $ DrawShape (Color 255 255 255) $ Rect b])

test2 :: App t
test2 = tableLayout 2 (Nothing, Just 1) test1

test2' :: App t
test2' = linearLayout Vertical (Nothing, Just 1) test1

focusWidget :: Widget' t () Bool
focusWidget = stackLayout (0,0,0,0) (AlignCenter, AlignCenter) (Just 1, Just 1) $ proc _ -> do
    f <- focusListener -< ()
    fOld <- shift False -< f
    let c = if f then Color 255 0 0 else Color 255 255 255
    widgetOutput -< ((stdParams {pWeightX = Just 1, pWeightY = Just 1}, False), \b -> [Render $ DrawShape c $ Rect b])
    returnA -< f

test3 :: App t
test3 = tableLayout 2 (Just 1, Just 1) $ proc _ -> do
    focusWidget -< ()
    focusWidget -< ()
    focusWidget -< ()
    focusWidget -< ()
    returnA -< ()

test4 :: App t
test4 = stackLayout (0, 0, 0, 0) (AlignCenter, AlignCenter) (Just 1, Just 1) $ proc _ -> do
    t <- time -< ()
    t0 <- initial -< t
    let secs = (t - t0) `quot` 1000
    label' stdParams { pHeight = 50, pWidth = 40} 60 -< show secs
    widgetOutput -< ((stdParams, False), (:[]) . Render . DrawShape (Color 255 0 0) . Rect)
    background (Color 255 255 255) -< ()

-- test5 :: App t
-- test5 = stackLayout (0, 0, 0, 0) (AlignCenter, AlignCenter) (Just 1, Just 1) $ proc _ -> do
--     _ <- textfield stdParams { pHeight = 30, pWidth = 100 } -< ()
--     background (Color 0 255 255) -< ()
--     returnA -< ()

test :: IO ()
test = do
    r <- create "Test" (800,600) :: IO GLFWRenderer
    mainLoop r 15 test3

main = test
