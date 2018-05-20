{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Input
    ( mouseListener
    , focusListener
    , widgetOutput
    , widgetOutput'
    , globals
    , time
    , Widget'
    ) where

import Control.Applicative
import Control.Arrow
import Data.Maybe (mapMaybe)
import Drawable
import GUI
import Graphics.UI.GLFW (Key(..))
import Layout
import Types
import Widget

type Widget' t i o = Widget (LayoutParam, Bool) (GUI t) i o

globals :: Widget' t () (Globals t)
globals =
    buildWidget' $ \_ -> do
        g <- guiGlobals
        return (g, globals)

time :: Widget' t () Integer
time = arr gTime <<< globals

widgetOutput :: Widget' t ((LayoutParam, Bool), Bounds -> [Drawable t]) ()
widgetOutput =
    buildWidget $ \bs ~(p, r) -> do
        guiDraw $ r bs
        return ((), p, widgetOutput)

widgetOutput' :: Widget' t [Drawable t] ()
widgetOutput' =
    buildWidget' $ \r -> do
        guiDraw r
        return ((), widgetOutput')

mouseListener :: Alternative o => Widget' t () (o (MouseButton, ButtonState))
mouseListener =
    buildWidget $ \bs _ -> do
        g <- guiGlobals
        return
            ( foldl (<|>) empty (map (f bs) (gEvents g))
            , (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
            , mouseListener)
  where
    f bs e =
        case e of
            MouseEvent but butS coords ->
                if coords `inside` bs
                    then pure (but, butS)
                    else empty
            _ -> empty

focusListener :: Widget' t () Bool
focusListener = focusListener' False
  where
    focusListener' focus =
        buildWidget $ \bs _ -> do
            g <- guiGlobals
            let focus' = foldr (f' bs) focus $ gEvents g
            return
                ( focus'
                , (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
                , focusListener' focus')
      where
        f' bs e focus' =
            case e of
                MouseEvent _but ButtonDown coords -> coords `inside` bs
                _ -> focus'
