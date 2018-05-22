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
    , once
    , Widget'
    ) where

import Control.Arrow
import Data.Maybe (mapMaybe)
import Drawable
import GUI
import Layout
import Resources
import Types
import Widget

type Widget' t i o = Widget (LayoutParam, Bool) (GUI t) i o

once :: a -> Widget' t () (Maybe a)
once x = buildWidget' $ const $ return (Just x, arr $ const Nothing)

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
    buildWidget $ \(p, r) ->
        return
            ( ()
            , p
            , \bs -> do
                  guiDraw $ r bs
                  return widgetOutput)

widgetOutput' :: Widget' t [Drawable t] ()
widgetOutput' =
    buildWidget' $ \r -> do
        guiDraw r
        return ((), widgetOutput')

mouseListener :: Widget' t () [Event]
mouseListener = buildWidget $ runWidget' (Bounds 0 0 0 0)
  where
    runWidget' bs' _ = do
        es <- guiEvents
        return
            ( mapMaybe (f bs') es
            , (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
            , return . buildWidget . runWidget')
    f bs@(Bounds x0 y0 _ _) e =
        case e of
            MouseEvent but butS c@(Coords x y) ->
                if c `inside` bs -- HELP!!!
                    then Just (MouseEvent but butS (Coords (x - x0) (y - y0)))
                    else Nothing
            e -> Just e

focusListener :: Widget' t () Bool
focusListener = buildWidget $ runWidget' False (Bounds 0 0 0 0)
  where
    runWidget' focus bs' _ = do
        es <- guiEvents
        let focus' = foldr (f' bs') focus es
        return
            ( focus'
            , (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
            , return . buildWidget . runWidget' focus')
    f' bs e focus' =
        case e of
            MouseEvent _but ButtonDown coords -> coords `inside` bs
            _ -> focus'
