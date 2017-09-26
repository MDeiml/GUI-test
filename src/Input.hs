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
       Widget Event (LayoutItem LayoutParam) () (Maybe ( MouseButton
                                                       , ButtonState))
mouseListener =
    buildWidget $ \e bs _i ->
        ( f e bs
        , LI
          { layoutParam = stdParams {pWeightX = Just 1, pWeightY = Just 1}
          , layoutDrawables = []
          }
        , mouseListener)
  where
    f e bs =
        case e of
            MouseEvent but butS coords ->
                if coords `inside` bs
                    then Just (but, butS)
                    else Nothing
            _ -> Nothing

focusListener :: Widget Event (LayoutItem LayoutParam) () Bool
focusListener = focusListener' False
  where
    focusListener' f =
        buildWidget $ \e bs _i ->
            ( f' e bs
            , LI
              { layoutParam = stdParams {pWeightX = Just 1, pWeightY = Just 1}
              , layoutDrawables = []
              }
            , focusListener' $ f' e bs)
      where
        f' e bs =
            case e of
                MouseEvent _but ButtonDown coords -> coords `inside` bs
                _ -> f
