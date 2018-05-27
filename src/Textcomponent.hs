{-# LANGUAGE Arrows            #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Textcomponent
    ( textfield
    , textfield'
    , label
    , label'
    , stdFont
    , LabelConfig(..)
    , stdLabelConfig
    , TextfieldConfig(..)
    , stdTextfieldConfig
    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.List                (findIndex)
import           Data.Maybe
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Drawable
import           GUI
import           Input
import           Layout
import           Resources
import           SDL.Input.Keyboard.Codes
import           SDL.Internal.Numbered    (toNumber)
import           Types
import           Widget

type TextInputState = (Text, Int, Int)

keyMovement ::
       ((Text, Int) -> Int) -> Modifiers -> TextInputState -> TextInputState
keyMovement move m (s, i, j)
    | mAlt m || mCtrl m = (s, i, j)
    | mShift m = (s, i', j)
    | otherwise = (s, i', i')
  where
    i' = max 0 $ min (Text.length s) $ move (s, i)

keyMap :: [(Key, Modifiers -> TextInputState -> TextInputState)]
keyMap =
    [ ( f KeycodeBackspace
      , \_ (s, i, j) -> (Text.take (i - 1) s <> Text.drop i s, i - 1, j - 1))
    , (f KeycodeHome, keyMovement (const 0)) -- TODO
    , (f KeycodeDelete, \_ (s, i, j) -> (Text.take i s <> Text.drop (i + 1) s, i, j))
    , (f KeycodeEnd, keyMovement (\(s, _) -> Text.length s)) -- TODO
    , (f KeycodeRight, keyMovement (\(_, i) -> i + 1)) -- TODO !!!
    , (f KeycodeLeft, keyMovement (\(_, i) -> i - 1)) -- TODO !!! up down
    ]
  where
    f = fromIntegral . toNumber

keyMap' k = fromMaybe (const id) $ lookup k keyMap

textInputIO :: Widget' t Bool ()
textInputIO =
    widgetState False $
    buildWidgetStatic' $ \(f', f) -> do
        when (f && not f') guiStartTextInput
        when (f' && not f) guiStopTextInput
        return (f, ())

data LabelConfig = LabelConfig
    { labelConfigFont  :: ResourceId Font
    , labelConfigColor :: Color
    , labelConfigText  :: Text
    }

stdLabelConfig :: LabelConfig
stdLabelConfig = LabelConfig
    { labelConfigFont = stdFont
    , labelConfigColor = Color 0 0 0
    , labelConfigText = ""
    }

stdFont :: ResourceId Font
stdFont = ResF 20 "Ubuntu"

label :: Widget' t LabelConfig ()
label = proc config ->
  do let s = labelConfigText config
         fId = labelConfigFont config
         c = labelConfigColor config
     Right f <- resource -< fId
     s' <- shift Nothing -< Just s
     let w = fromIntegral $
               sum $
                 map
                   ((\ (_, _, _, _, x) -> x) .
                       fontMetrics f . fromIntegral . fromEnum) $
                   Text.unpack s
         nls = count '\n' $ Text.unpack s
         h = fromIntegral (ascent f - descent f) + 1.2 * fromIntegral nls
     widgetOutput -<
       ((stdParams{pHeight = h, pWidth = w}, Just s /= s'),
        \ ~(Bounds x y _ _) -> [DrawShape c $ DrawText s (Coords x y) f])
  where
      count a = length . filter (==a)

label' :: Widget' t Text ()
label' = proc s -> label -< stdLabelConfig{labelConfigText = s}

data TextfieldConfig = TextfieldConfig
    { textfieldConfigText        :: Text
    , textfieldConfigTextUpdate  :: Maybe Text
    , textfieldConfigLayoutParam :: LayoutParam
    , textfieldConfigFont        :: ResourceId Font
    , textfieldConfigNinepatch   :: ResourceId NinePatch
    , textfieldConfigTextColor   :: Color
    , textfieldConfigSelectColor :: Color
    }

stdTextfieldConfig :: TextfieldConfig
stdTextfieldConfig = TextfieldConfig
    { textfieldConfigText = ""
    , textfieldConfigTextUpdate = Nothing
    , textfieldConfigLayoutParam = stdParams {pWidth = 80, pHeight = 30}
    , textfieldConfigFont = stdFont
    , textfieldConfigNinepatch = ResN "textfield.json"
    , textfieldConfigTextColor = Color 0 0 0
    , textfieldConfigSelectColor = Color 200 200 200
    }

textfield :: Widget' t TextfieldConfig Text
textfield =
    proc config -> do
     let p = textfieldConfigLayoutParam config
     stackLayout' w -<
       (config,
        ((0, 0, 0, 0), (AlignStart, AlignCenter),
         (pWeightX p, pWeightY p)))
  where
    w =
        widgetState ("", 0, 0) $
        proc ~(last, config) -> do
     let initText = textfieldConfigText config
         ev = textfieldConfigTextUpdate config
         p = textfieldConfigLayoutParam config
         fId = textfieldConfigFont config
         nId = textfieldConfigNinepatch config
         c = textfieldConfigTextColor config
         cs = textfieldConfigSelectColor config
     initEv <- once () -< ()
     let ~(content', caret', caret0')
           = maybe last (\ s -> (s, Text.length s, Text.length s))
               (ev <|> fmap (const initText) initEv)
     Right np <- resource -< nId
     let NP (Sprite _ w h) (xs, ys, xe, ye) = np
         x0 = fromIntegral w * xs
         y0 = fromIntegral h * ys
         x1 = fromIntegral w * (1 - xe)
         y1 = fromIntegral h * (1 - ye)
     widgetOutput -< ((p, False), const [])
     es <- mouseListener -< ()
     focus <- focusListener -< ()
     textInputIO -< focus
     Right f <- resource -< fId
     let ws = map ((\ (_, _, _, _, x) -> x) .
                   fontMetrics f . fromIntegral . fromEnum) $
                       Text.unpack content'
         ~(content'', caret'', caret0'') = if focus
             then keyChars es ws x0 (content', caret', caret0')
             else (content', caret', caret0')
         ws' = map ((\ (_, _, _, _, x) -> x) .
                   fontMetrics f . fromIntegral . fromEnum) $
                       Text.unpack content''
         cx = x0 + fromIntegral (sum $ take caret'' ws')
         cx' = x0 + fromIntegral (sum $ take caret0'' ws')
     t <- time -< ()
     let blink = ((t `quot` 1000) `mod` 2) == 0 && focus
     stackLayout' label -<
       (stdLabelConfig{labelConfigFont = fId, labelConfigColor = c,
                       labelConfigText = content''},
        ((x0, y0, x1, y1), (AlignStart, AlignCenter), (Nothing, Nothing)))
     widgetOutput -<
       ((stdParams{pWeightX = Just 0, pWeightY = Just 0}, False),
        \ bs@(Bounds x0' y0' x1' y1') ->
          [DrawShape c (Line (Coords (x0' + cx) (y0' + y0))
                (Coords (x0' + cx) (y1' - y1))) | caret'' == caret0'', blink]
            ++ [DrawShape cs (Rect
                  (Bounds (x0' + min cx cx') (y0' + y0) (x0' + max cx cx')
                     (y1' - y1)))
             | caret'' /= caret0'']
            ++ [NinePatch np bs $
                 Bounds (x0' + x0) (y0' + y0) (x1' - x1) (y1' - y1)])
     returnA -< ((content'', caret'', caret0''), content'')
    keyChars evs ws x0 s = foldr f s evs
      where
        ws' = scanl (+) 0 ws
        ws'' = zipWith (\a b -> (a + b) `quot` 2) ws' (tail ws')
        f (KeyEvent m k KeyDown) x = keyMap' k m x
        f (TextEvent c) (s, i, j) =
            ( Text.take (min i j) s <> c <> Text.drop (max i j) s
            , min i j + Text.length c
            , min i j + Text.length c)
        f (MouseEvent 1 ButtonDown (Coords mx _my)) (s, _, _) =
            let i' = fromMaybe (Text.length s) $ findIndex (>= floor (mx - x0)) ws''
            in (s, i', i')
        f _ x = x

textfield' :: Text -> Widget' t (Maybe Text) Text
textfield' t = proc ev ->
  textfield -< stdTextfieldConfig{ textfieldConfigTextUpdate = ev,
                                   textfieldConfigText = t }
