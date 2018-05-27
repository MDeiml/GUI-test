{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import qualified Data.Text     as Text
import           Lib
import           SDLRenderer
import           Text.Read     (readMaybe)

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
     label -<
       stdLabelConfig{labelConfigFont = ResF 60 "Ubuntu",
                      labelConfigText = Text.pack $ show secs}
     widgetOutput -<
       ((stdParams, False), (: []) . DrawShape (Color 255 0 0) . Rect)
     background (Color 255 255 255) -< ()

test5 :: App t
test5 =
    stackLayout (0, 0, 0, 0) (AlignCenter, AlignCenter) (Just 1, Just 1) $
    proc _ -> do
     () >- linearLayout Vertical (Nothing, Nothing) $
           proc _ -> do
              n <- evLast 5 <<< inp -< ()
              label' -< Text.pack $ show n
     background (Color 255 255 0) -< ()
  where
    inp =
        linearLayout Horizontal (Nothing, Nothing) $
        proc _ -> do
     i1 <- textfield' "2" -< Nothing
     label' -< "+"
     i2 <- textfield' "3" -< Nothing
     let i1' = readMaybe $ Text.unpack i1
         i2' = readMaybe $ Text.unpack i2
     returnA -< (+) <$> i1' <*> (i2' :: Maybe Integer)

test :: IO ()
test = do
    r <- create "Test" (400, 300) :: IO SDLRenderer
    st <- initState test5
    st' <- packWindow r st
    mainLoop r 2 st'

main = test
