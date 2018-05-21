{-# LANGUAGE Arrows #-}

module Textfield
    ( textfield
    , label
    , label'
    ) where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.List (findIndex)
import Data.Maybe
import Drawable
import GUI
import Input
import Layout
import Resources
import SDL.Input.Keyboard.Codes
import SDL.Internal.Numbered (toNumber)
import Types
import Widget

type TextInputState = (String, Int)

keyMap :: [(Key, Modifiers -> TextInputState -> TextInputState)]
keyMap =
    [ (f KeycodeBackspace, \_ (s, i) -> (take (i - 1) s ++ drop i s, i - 1))
    , (f KeycodeHome, \_ (s, i) -> (s, 0)) -- TODO
    , (f KeycodeDelete, \_ (s, i) -> (take i s ++ drop (i + 1) s, i))
    , (f KeycodeEnd, \_ (s, i) -> (s, length s)) -- TODO
    , (f KeycodeRight, \_ (s, i) -> (s, min (length s) (i + 1))) -- TODO !!!
    , (f KeycodeLeft, \_ (s, i) -> (s, max 0 (i - 1))) -- TODO !!! up down
    ]
  where
    f = fromIntegral . toNumber

keyMap' k = fromMaybe (const id) $ lookup k keyMap

label :: Color -> Widget' t (Font t, String) ()
label c =
    proc (f, s) ->
  do s' <- shift Nothing -< Just s
     let w = fromIntegral $
               sum $
                 map
                   ((\ (_, _, _, _, x) -> x) .
                      fontMetrics f . fromIntegral . fromEnum)
                   s
         h = fromIntegral (ascent f - descent f) + 1.2 * count '\n' s
     widgetOutput -<
       ((stdParams{pHeight = h, pWidth = w}, Just s /= s'),
        \ ~(Bounds x y _ _) -> [DrawShape c $ Text s (Coords x y) f])
  where
    count _ [] = 0
    count a (x:xs)
        | a == x = 1 + count a xs
        | otherwise = count a xs

label' :: Int -> Widget' t String ()
label' size =
    proc s ->
  do r <- resource -<
            ResF size "/usr/share/fonts/truetype/ubuntu/Ubuntu-R.ttf"
     case r of
         RFont f -> label (Color 0 0 0) -< (f, s)
         RError e -> debug -< e
         _ -> debug -< "Resource is not a font"

textfield :: LayoutParam -> Widget' t (Maybe String) String
textfield p =
    stackLayout (0, 0, 0, 0) (AlignCenter, AlignCenter) (pWeightX p, pWeightY p) $
    widgetState ("", 0) $
    proc ((content, caret), ev) ->
  do let (content', caret')
           = maybe (content, caret) (\ s -> (s, length s)) ev
     RNin np <- resource -< ResN "textfield.json"
     let NP (Sprite _ w h) (xs, ys, xe, ye) = np
         x0 = fromIntegral w * xs
         y0 = fromIntegral h * ys
         x1 = fromIntegral w * (1 - xe)
         y1 = fromIntegral h * (1 - ye)
     bs <- bounds -< (p, False)
     let ~(Bounds x0' y0' x1' y1') = bs
     g <- globals -< ()
     focus <- focusListener -< ()
     RFont f <- resource -<
                  ResF 20 "/usr/share/fonts/truetype/ubuntu/Ubuntu-R.ttf"
     let ws
           = map
               ((\ (_, _, _, _, x) -> x) .
                  fontMetrics f . fromIntegral . fromEnum)
               content'
     let (content'', caret'')
           = if focus then keyChars g ws (x0' + x0) bs (content', caret') else
               (content', caret')
     let ws'
           = map
               ((\ (_, _, _, _, x) -> x) .
                  fontMetrics f . fromIntegral . fromEnum)
               content''
     let cx = x0 + fromIntegral (sum $ take caret'' ws')
     t <- time -< ()
     let blink = ((t `quot` 1000) `mod` 2) == 0 && focus
     constLayout' $ stackLayout' (label (Color 0 0 0)) -<
       (((f, content''),
         ((x0, y0, x1, y1), (AlignStart, AlignCenter), (Nothing, Nothing))),
        bs)
     widgetOutput' -<
       [DrawShape (Color 0 0 0)
          (Line (Coords (x0' + cx) (y0' + y0))
             (Coords (x0' + cx) (y1' - y1)))
        | blink]
     widgetOutput' -<
       [NinePatch np bs $
          Bounds (x0' + x0) (y0' + y0) (x1' - x1) (y1' - y1)]
     returnA -< ((content'', caret''), content'')
  where
    keyChars g ws x0 bs (c, i) = foldr f (c, i) evs
      where
        ws' = scanl (+) 0 ws
        ws'' = zipWith (\a b -> (a + b) `quot` 2) ws' (tail ws')
        evs = gEvents g
        f (KeyEvent m k KeyDown) x = keyMap' k m x
        f (CharEvent c) (s, j) = (take j s ++ [c] ++ drop j s, j + 1)
        f (MouseEvent 0 ButtonDown c@(Coords mx _my)) (s, j) =
            if c `inside` bs
                then ( s
                     , fromMaybe (length s) $
                       findIndex (floor (mx - x0) <=) ws'')
                else (s, j)
        f _ (s, j) = (s, j)
