module Input
    ( Key(..)
    , KeyState(..)
    , MouseButton
    , ButtonState(..)
    , Event(..)
    , mouseListener
    , focusListener
    ) where

import Graphics.UI.GLFW (Key(..))
import Layout
import Types
import Widget

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
    | Time Integer

mouseListener ::
       (Monoid r)
    => Widget Event r LayoutParam () (Maybe (MouseButton, ButtonState))
mouseListener =
    buildWidget $ \e ->
        ( stdParams {pWeightX = Just 1, pWeightY = Just 1}
        , \_ bs -> (f e bs, mempty, mouseListener))
  where
    f e bs =
        case e of
            MouseEvent but butS coords ->
                if coords `inside` bs
                    then Just (but, butS)
                    else Nothing
            _ -> Nothing

focusListener :: (Monoid r) => Widget Event r LayoutParam () Bool
focusListener = focusListener' False
  where
    focusListener' f =
        buildWidget $ \e ->
            ( stdParams {pWeightX = Just 1, pWeightY = Just 1}
            , \_ bs -> (f' e bs, mempty, focusListener' $ f' e bs))
      where
        f' e bs =
            case e of
                MouseEvent _but ButtonDown coords -> coords `inside` bs
                _ -> f
