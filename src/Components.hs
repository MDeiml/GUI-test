{-# LANGUAGE Arrows #-}

module Components
    ( label
    , label'
    , background
    , initial
    , evLast
    , evLast'
    , debug
    , textfield
    , once
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Drawable
import GUI
import Graphics.UI.GLFW (Key(..))
import Input
import Layout
import Resources
import Types
import Widget

debug :: Widget' t String ()
debug =
    buildWidget' $ \s -> do
        liftIO $ putStrLn s
        return ((), debug)

background :: Color -> Widget' t () ()
background c =
    widgetOutput <<<
    arr
        (const
             ( (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
             , (: []) . DrawShape c . Rect))

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

resource :: Widget' t ResourceId (Resource t)
resource =
    buildWidget' $ \i -> do
        g <- guiGlobals
        res <- liftIO $ gResources g i
        return (res, resource)

image :: Widget' t (Sprite t) ()
image =
    proc (Sprite t w h) ->
  do dim <- shift Nothing -< Just (w, h)
     widgetOutput -<
       ((stdParams{pWidth = fromIntegral w, pHeight = fromIntegral h},
         dim /= Just (w, h)),
        \ bs -> [Image t bs])

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
     widgetOutput -< ((p, False), const [])
     g <- globals -< ()
     let (content'', caret'') = keyChars g (content', caret')
     RFont f <- resource -<
                  ResF 20 "/usr/share/fonts/truetype/ubuntu/Ubuntu-R.ttf"
     let cx
           = x0 +
               fromIntegral
                 (sum $
                    map
                      ((\ (_, _, _, _, x) -> x) .
                         fontMetrics f . fromIntegral . fromEnum)
                      (take caret'' content''))
     t <- time -< ()
     let blink = ((t `quot` 1000) `mod` 2) == 0
     stackLayout' (label (Color 0 0 0)) -<
       ((f, content''),
        ((x0, y0, x1, y1), (AlignStart, AlignCenter), (Nothing, Nothing)))
     widgetOutput -<
       ((p, False),
        \ ~(Bounds x0' y0' x1' y1') ->
          [DrawShape (Color 0 0 0)
             (Line (Coords (x0' + cx) (y0' + y0))
                (Coords (x0' + cx) (y1' - y1)))
           | blink])
     widgetOutput -<
       ((p, False),
        \ b@ ~(Bounds x0' y0' x1' y1') ->
          [NinePatch np b $
             Bounds (x0' + x0) (y0' + y0) (x1' - x1) (y1' - y1)])
     returnA -< ((content'', caret''), content'')
  where
    keyChars g (c, i) = foldr f (c, i) evs
      where
        evs = gEvents g
        f (KeyEvent k KeyDown) x = f (KeyEvent k KeyRepeat) x
        f (KeyEvent Key'Left KeyRepeat) (s, j) = (s, max 0 (j - 1))
        f (KeyEvent Key'Right KeyRepeat) (s, j) = (s, min (length s) (j + 1))
        f (KeyEvent Key'Backspace KeyRepeat) (s, j) =
            if j == 0
                then (s, 0)
                else (take (j - 1) s ++ drop j s, j - 1)
        f (CharEvent c) (s, j) = (take j s ++ [c] ++ drop j s, j + 1)
        f _ (s, j) = (s, j)

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
