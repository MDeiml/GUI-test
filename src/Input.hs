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
import Control.Monad
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
debug = buildWidgetStatic' $ void . guiIO . putStrLn

resource :: Widget' t (ResourceId a) (Either String (a t))
resource = buildWidgetStatic' guiResource

events :: Widget' t () [Event]
events = buildWidgetStatic' $ const guiEvents

time :: Widget' t () Integer
time = buildWidgetStatic' $ const guiTime

widgetOutput :: Widget' t ((LayoutParam, Bool), Bounds -> [Drawable t]) ()
widgetOutput =
    buildWidgetStatic $ \(p, r) -> return ((), p, \bs -> do guiDraw $ r bs)

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
                if c `inside` bs
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
