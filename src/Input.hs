{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Input
    ( mouseListener
    , focusListener
    , widgetOutput
    , widgetOutput'
    , globals
    , resource
    , time
    , debug
    , Widget'
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Data.Maybe (mapMaybe)
import Drawable
import GUI
import Graphics.UI.GLFW (Key(..))
import Layout
import Resources
import Types
import Widget

type Widget' t i o = Widget (LayoutParam, Bool) (GUI t) i o

debug :: Widget' t String ()
debug =
    buildWidget' $ \s -> do
        liftIO $ putStrLn s
        return ((), debug)

resource :: Widget' t ResourceId (Resource t)
resource =
    buildWidget' $ \i -> do
        g <- guiGlobals
        res <- liftIO $ gResources g i
        return (res, resource)

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

mouseListener :: Widget' t () [(MouseButton, ButtonState)]
mouseListener =
    buildWidget $ \bs _ -> do
        g <- guiGlobals
        return
            ( mapMaybe (f bs) (gEvents g)
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
