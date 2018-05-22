{-# LANGUAGE Arrows #-}

module Main where

import Control.Arrow
import Lib
import SDLRenderer

test1 :: App t
test1 =
    proc _ ->
  do widgetOutput -<
       ((stdParams{pHeight = 150, pWidth = 100}, False),
        \ b -> [DrawShape (Color 255 0 0) $ Rect b])
     widgetOutput -<
       ((stdParams{pWeightY = Just 1, pWeightX = Just 1}, False),
        \ b -> [DrawShape (Color 0 255 0) $ Rect b])
     widgetOutput -<
       ((stdParams{pWeightY = Just 2, pWeightX = Just 1}, False),
        \ b -> [DrawShape (Color 0 0 255) $ Rect b])
     widgetOutput -<
       ((stdParams{pHeight = 50, pWidth = 40}, False),
        \ b -> [DrawShape (Color 255 255 255) $ Rect b])

test2 :: App t
test2 = tableLayout 2 (Nothing, Just 1) test1

test2' :: App t
test2' = linearLayout Vertical (Nothing, Just 1) test1

focusWidget :: Widget' t () Bool
focusWidget =
    stackLayout (0, 0, 0, 0) (AlignCenter, AlignCenter) (Just 1, Just 1) $
    proc _ ->
  do f <- focusListener -< ()
     let c = if f then Color 255 0 0 else Color 255 255 255
     widgetOutput -<
       ((stdParams{pWeightX = Just 1, pWeightY = Just 1}, False),
        \ b -> [DrawShape c $ Rect b])
     returnA -< f

test3 :: App t
test3 =
    tableLayout 2 (Just 1, Just 1) $
    proc _ ->
  do focusWidget -< ()
     focusWidget -< ()
     focusWidget -< ()
     focusWidget -< ()
     returnA -< ()

test4 :: App t
test4 =
    stackLayout (0, 0, 0, 0) (AlignCenter, AlignCenter) (Just 1, Just 1) $
    proc _ ->
  do t <- time -< ()
     t0 <- initial -< t
     let secs = (t - t0) `quot` 1000
     label' 60 -< show secs
     widgetOutput -<
       ((stdParams, False), (: []) . DrawShape (Color 255 0 0) . Rect)
     background (Color 255 255 255) -< ()

test5 :: App t
test5 = stackLayout (0, 0, 0, 0) (AlignCenter, AlignCenter) (Just 1, Just 1) $
    proc _ -> do
        a -< ()
        background (Color 255 255 0) -< ()
  where
    inp =
        linearLayout Horizontal (Nothing, Nothing) $
        proc _ -> do
     i1 <- textfield stdParams{pHeight = 30, pWidth = 100} <<<
             once "2"
             -< ()
     label' 20 -< "+"
     i2 <- textfield stdParams{pHeight = 30, pWidth = 100} <<<
             once "3"
             -< ()
     let i1'
           = case reads i1 of
               [(x,_)] -> Just x
               _ -> Nothing
     let i2'
           = case reads i2 of
               [(x,_)] -> Just x
               _ -> Nothing
     returnA -< (+) <$> i1' <*> (i2' :: Maybe Integer)
    a =
        linearLayout Vertical (Just 0, Just 0) $
        proc _ -> do
     n <- evLast' 5 <<< inp -< ()
     label' 20 -< show n

test :: IO ()
test = do
    r <- create "Test" (800, 600) :: IO SDLRenderer
    mainLoop r 2 test5

main = test
