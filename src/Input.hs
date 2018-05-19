{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Input
    ( Key(..)
    , KeyState(..)
    , MouseButton
    , ButtonState(..)
    , Event(..)
    , Globals(..)
    , mouseListener
    , focusListener
    , widgetOutput
    , widgetOutput'
    , Widget'
    , Cmd(..)
    ) where

import Control.Applicative
import GUI
import Graphics.UI.GLFW (Key(..))
import Layout
import Types
import Widget

type Widget' t i o = Widget (LayoutParam, Bool) (GUI t) i o

widgetOutput :: Widget' t ((LayoutParam, Bool), Bounds -> [Cmd t]) ()
widgetOutput =
    buildWidget $ \bs (p, r) -> do
        mapM_ guiCommand $ r bs
        return ((), p, widgetOutput)

widgetOutput' :: Widget' t [Cmd t] ()
widgetOutput' =
    buildWidget' $ \r -> do
        mapM_ guiCommand r
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
            let focus' = foldl (f' bs) focus $ gEvents g
            return
                ( focus'
                , (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
                , focusListener' focus')
      where
        f' bs focus' e =
            case e of
                MouseEvent _but ButtonDown coords -> coords `inside` bs
                _ -> focus
