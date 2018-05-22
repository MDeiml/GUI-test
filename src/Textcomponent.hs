{-# LANGUAGE Arrows #-}

module Textcomponent
    ( textfield
    , label
    , label'
    ) where

import Control.Arrow
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

type TextInputState = (String, Int, Int)

keyMovement ::
       ((String, Int) -> Int) -> Modifiers -> TextInputState -> TextInputState
keyMovement move m (s, i, j)
    | mAlt m || mCtrl m = (s, i, j)
    | mShift m = (s, i', j)
    | otherwise = (s, i', i')
  where
    i' = max 0 $ min (length s) $ move (s, i)

keyMap :: [(Key, Modifiers -> TextInputState -> TextInputState)]
keyMap =
    [ ( f KeycodeBackspace
      , \_ (s, i, j) -> (take (i - 1) s ++ drop i s, i - 1, j - 1))
    , (f KeycodeHome, keyMovement (const 0)) -- TODO
    , (f KeycodeDelete, \_ (s, i, j) -> (take i s ++ drop (i + 1) s, i, j))
    , (f KeycodeEnd, keyMovement (\(s, _) -> length s)) -- TODO
    , (f KeycodeRight, keyMovement (\(_, i) -> i + 1)) -- TODO !!!
    , (f KeycodeLeft, keyMovement (\(_, i) -> i - 1)) -- TODO !!! up down
    ]
  where
    f = fromIntegral . toNumber

keyMap' k = fromMaybe (const id) $ lookup k keyMap

textInputIO :: Widget' t Bool ()
textInputIO = widgetState False w
  where
    w =
        buildWidget' $ \(f', f) -> do
            guiStartTextInput (f && not f')
            guiStopTextInput (f' && not f)
            return ((f, ()), w)

label :: Color -> Widget' t (Font t, String) ()
label c =
    proc ~(f, s) ->
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
    widgetState ("", 0, 0) $
    proc ~(~(content, caret, caret0), ev) ->
  do let ~(content', caret', caret0')
           = maybe (content, caret, caret0) (\ s -> (s, length s, length s))
               ev
     RNin np <- resource -< ResN "textfield.json"
     let NP (Sprite _ w h) (xs, ys, xe, ye) = np
         x0 = fromIntegral w * xs
         y0 = fromIntegral h * ys
         x1 = fromIntegral w * (1 - xe)
         y1 = fromIntegral h * (1 - ye)
     widgetOutput -< ((p, False), const [])
     es <- mouseListener -< ()
     focus <- focusListener -< ()
     textInputIO -< focus
     RFont f <- resource -<
                  ResF 20 "/usr/share/fonts/truetype/ubuntu/Ubuntu-R.ttf"
     let ws
           = map
               ((\ (_, _, _, _, x) -> x) .
                  fontMetrics f . fromIntegral . fromEnum)
               content'
     let ~(content'', caret'', caret0'')
           = if focus then keyChars es ws x0 (content', caret', caret0') else
               (content', caret', caret0')
     let ws'
           = map
               ((\ (_, _, _, _, x) -> x) .
                  fontMetrics f . fromIntegral . fromEnum)
               content''
     let cx = x0 + fromIntegral (sum $ take caret'' ws')
     let cx' = x0 + fromIntegral (sum $ take caret0'' ws')
     t <- time -< ()
     let blink = ((t `quot` 1000) `mod` 2) == 0 && focus
     stackLayout' (label (Color 0 0 0)) -<
       ((f, content''),
        ((x0, y0, x1, y1), (AlignStart, AlignCenter), (Nothing, Nothing)))
     widgetOutput -<
       ((stdParams, False),
        \ (Bounds x0' y0' _x1' y1') ->
          [DrawShape (Color 0 0 0)
             (Line (Coords (x0' + cx) (y0' + y0))
                (Coords (x0' + cx) (y1' - y1)))
           | caret'' == caret0'', blink]
            ++
            [DrawShape (Color 200 200 200)
               (Rect
                  (Bounds (x0' + min cx cx') (y0' + y0) (x0' + max cx cx')
                     (y1' - y1)))
             | caret'' /= caret0''])
     widgetOutput -<
       ((stdParams, False),
        \ bs@(Bounds x0' y0' x1' y1') ->
          [NinePatch np bs $
             Bounds (x0' + x0) (y0' + y0) (x1' - x1) (y1' - y1)])
     returnA -< ((content'', caret'', caret0''), content'')
  where
    keyChars evs ws x0 s = foldr f s evs
      where
        ws' = scanl (+) 0 ws
        ws'' = zipWith (\a b -> (a + b) `quot` 2) ws' (tail ws')
        f (KeyEvent m k KeyDown) x = keyMap' k m x
        f (CharEvent c) (s, i, j) =
            ( take (min i j) s ++ [c] ++ drop (max i j) s
            , min i j + 1
            , min i j + 1)
        f (MouseEvent 1 ButtonDown (Coords mx _my)) (s, _, _) =
            let i' = fromMaybe (length s) $ findIndex (floor (mx - x0) <=) ws''
            in (s, i', i')
        f _ x = x
