{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Input
    ( mouseListener
    , focusListener
    , widgetOutput
    , widgetOutput'
    , resource
    , time
    , events
    , debug
    , Widget'
    ) where

import Data.Maybe (mapMaybe)
import Drawable
import GUI
import Layout
import Resources
import Types
import Widget

type Widget' t i o = Widget (LayoutParam, Bool) (GUI t) i o

debug :: Widget' t String ()
debug =
    buildWidget' $ \s -> do
        guiIO $ putStrLn s
        return ((), debug)

resource :: Widget' t ResourceId (Resource t)
resource =
    buildWidget' $ \i -> do
        res <- guiResource i
        return (res, resource)

events :: Widget' t () [Event]
events =
    buildWidget' $ \_ -> do
        es <- guiEvents
        return (es, events)

time :: Widget' t () Integer
time =
    buildWidget' $ \_ -> do
        t <- guiTime
        return (t, time)

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

mouseListener :: Widget' t () [(MouseButton, ButtonState)]
mouseListener =
    buildWidget $ \bs _ -> do
        es <- guiEvents
        return
            ( mapMaybe (f bs) es
            , (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
            , mouseListener)
  where
    f bs e =
        case e of
            MouseEvent but butS coords ->
                if coords `inside` bs
                    then Just (but, butS)
                    else Nothing
            _ -> Nothing

focusListener :: Widget' t () Bool
focusListener = focusListener' False
  where
    focusListener' focus =
        buildWidget $ \bs _ -> do
            es <- guiEvents
            let focus' = foldr (f' bs) focus es
            return
                ( focus'
                , (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
                , focusListener' focus')
      where
        f' bs e focus' =
            case e of
                MouseEvent _but ButtonDown coords -> coords `inside` bs
                _ -> focus'
