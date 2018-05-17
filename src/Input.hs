module Input
    ( Key(..)
    , KeyState(..)
    , MouseButton
    , ButtonState(..)
    , Event(..)
    , Globals(..)
    , mouseListener
    , focusListener
    , Widget'
    , Cmd(..)
    ) where

import Control.Applicative
import Graphics.UI.GLFW (Key(..))
import Layout
import Resources
import Types
import Widget
import Drawable

data Globals t = Globals
    { gEvents :: [Event]
    , gTime :: Integer
    , gResources :: Resources t
    } deriving (Show)

data KeyState
    = KeyUp
    | KeyDown
    | KeyRepeat
    deriving (Show)

data ButtonState
    = ButtonUp
    | ButtonDown
    deriving (Show)

type MouseButton = Int

data Event
    = KeyEvent Key
               KeyState
    | MouseEvent MouseButton
                 ButtonState
                 Coords
    | MouseMoveEvent Coords
    deriving (Show)

type Widget' t i o = Widget (Globals t) (Cmd t) (LayoutParam, Bool) i o

data Cmd t
    = LoadResource ResourceId
    | Debug String
    | Render (Drawable t)

mouseListener ::
       (Monoid r, Alternative o)
  => Widget (Globals t) r (LayoutParam, Bool) () (o (MouseButton, ButtonState))
mouseListener =
    buildWidget $ \g bs _ ->
        ( foldl (<|>) empty (map (f bs) (gEvents g))
        , (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False)
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

focusListener :: Widget (Globals t) (Cmd t) (LayoutParam, Bool) () Bool
focusListener = focusListener' False
  where
    focusListener' focus =
        buildWidget $ \g bs _ ->
            (let focus' = foldl (f' bs) focus $ gEvents g
              in (focus', (stdParams {pWeightX = Just 0, pWeightY = Just 0}, False), [], focusListener' focus'))
      where
        f' bs focus' e =
            case e of
                MouseEvent _but ButtonDown coords -> coords `inside` bs
                _ -> focus
