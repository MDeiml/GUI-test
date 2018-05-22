{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}

module GUI
    ( Globals(..)
    , KeyState(..)
    , ButtonState(..)
    , MouseButton
    , Event(..)
    , Key
    , Modifiers(..)
    , GUI
    , Cmd(..)
    , guiResource
    , guiEvents
    , guiTime
    , guiDraw
    , guiIO
    , guiStartTextInput
    , guiStopTextInput
    , runGUI
    ) where

import Control.Monad.Fix
import Drawable
import Resources
import Types

data Cmd t
    = Render (Drawable t)
    | RunIO (IO ())
    | StartTextInput
    | StopTextInput

data Globals t = Globals
    { gEvents :: [Event]
    , gTime :: Integer
    , gResources :: ResourceId -> IO (Resource t)
    }

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

type Key = Int

data Modifiers = Modifiers
    { mShift :: Bool
    , mCtrl :: Bool
    , mAlt :: Bool
    } deriving (Show, Eq)

data Event
    = KeyEvent Modifiers
               Key
               KeyState
    | CharEvent Char
    | MouseEvent MouseButton
                 ButtonState
                 Coords
    | MouseMoveEvent Coords
    deriving (Show)

newtype GUI t a =
    GUI (Globals t -> IO (a, [Cmd t]))
    deriving (Functor)

instance Applicative (GUI t) where
    pure x = GUI (const $ return (x, []))
    (<*>) (GUI f) (GUI a) =
        GUI $ \g -> do
            ~(f', cf) <- f g
            ~(a', ca) <- a g
            return (f' a', cf ++ ca)

instance Monad (GUI t) where
    (>>=) ~(GUI a) f =
        GUI $ \g -> do
            ~(a', ca) <- a g
            let (GUI b) = f a'
            ~(b', cb) <- b g
            return (b', ca ++ cb)

instance MonadFix (GUI t) where
    mfix f =
        GUI $ \g -> mdo
            let (GUI a) = f a'
            ~(a', ca) <- a g
            return (a', ca)

guiEvents :: GUI t [Event]
guiEvents = GUI $  \g -> return (gEvents g, [])

guiResource :: ResourceId -> GUI t (Resource t)
guiResource i = GUI $ \g -> do
    res <- gResources g i
    return (res, [])

guiTime :: GUI t Integer
guiTime = GUI $ \g -> return (gTime g, [])

guiDraw :: [Drawable t] -> GUI t ()
guiDraw c = GUI $ \_ -> return ((), map Render c)

guiIO :: IO () -> GUI t ()
guiIO m = GUI $ \_ -> return ((), [RunIO m])

guiStartTextInput :: Bool -> GUI t ()
guiStartTextInput b = GUI $ \_ -> return ((), [StartTextInput | b])

guiStopTextInput :: Bool -> GUI t ()
guiStopTextInput b = GUI $ \_ -> return ((), [StopTextInput | b])

runGUI :: GUI t a -> Globals t -> IO (a, [Cmd t])
runGUI (GUI f) = f
