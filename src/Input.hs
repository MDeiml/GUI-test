module Input
    ( Key(..)
    , KeyState(..)
    , MouseButton
    , ButtonState(..)
    , Event(..)
    , Globals(..)
    , mouseListener
    , focusListener
    ) where

import Control.Applicative
import Graphics.UI.GLFW (Key(..))
import Layout
import Types
import Widget

data Globals = Globals
    { gEvents :: [Event]
    , gTime :: Integer
    }

data KeyState
    = KeyUp
    | KeyDown
    | KeyRepeat

data ButtonState
    = ButtonUp
    | ButtonDown

type MouseButton = Int

data Event
    = KeyEvent Key
               KeyState
    | MouseEvent MouseButton
                 ButtonState
                 Coords
    | MouseMoveEvent Coords

mouseListener ::
       (Monoid r, Alternative o)
    => Widget Globals r LayoutParam () (o (MouseButton, ButtonState))
mouseListener =
    buildWidget $ \g bs _ ->
        ( foldl (<|>) empty (map (f bs) (gEvents g))
        , stdParams {pWeightX = Just 1, pWeightY = Just 1}
        , mempty
        , mouseListener)
  where
    f bs e =
        case e of
            MouseEvent but butS coords ->
                if coords `inside` bs
                    then pure (but, butS)
                    else empty
            _ -> empty

focusListener :: (Monoid r) => Widget Globals r LayoutParam () Bool
focusListener = focusListener' False
  where
    focusListener' focus =
        buildWidget $ \g bs _ ->
            (let focus' = foldl (f' bs) focus $ gEvents g
             in ( focus'
                , stdParams {pWeightX = Just 1, pWeightY = Just 1}
                , mempty
                , focusListener' focus'))
      where
        f' bs focus' e =
            case e of
                MouseEvent _but ButtonDown coords -> coords `inside` bs
                _ -> focus
