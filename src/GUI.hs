{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}

module GUI
    ( Globals(..)
    , KeyState(..)
    , ButtonState(..)
    , MouseButton
    , Event(..)
    , GUI
    , guiGlobals
    , guiDraw
    , runGUI
    ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.IORef
import Drawable
import Graphics.UI.GLFW (Key(..))
import Resources
import Types

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

data Event
    = KeyEvent Key
               KeyState
    | CharEvent Char
    | MouseEvent MouseButton
                 ButtonState
                 Coords
    | MouseMoveEvent Coords
    deriving (Show)

newtype GUI t a =
    GUI (Globals t -> IO (a, [Drawable t]))
    deriving (Functor)

instance Applicative (GUI t) where
    pure x = GUI (const $ return (x, []))
    (<*>) (GUI f) (GUI a) =
        GUI $ \g -> do
            (f', cf) <- f g
            (a', ca) <- a g
            return (f' a', cf ++ ca)

instance Monad (GUI t) where
    (>>=) ~(GUI a) f =
        GUI $ \g -> do
            (a', ca) <- a g
            let ~(GUI b) = f a'
            (b', cb) <- b g
            return (b', ca ++ cb)

instance MonadFix (GUI t) where
    mfix f =
        GUI $ \g -> mdo
            let (GUI a) = f a'
            ~(a', ca) <- a g
            return (a', ca)

instance MonadIO (GUI t) where
    liftIO a = GUI $ \_ -> fmap (\x -> (x, [])) a

guiGlobals :: GUI t (Globals t)
guiGlobals = GUI $ \g -> return (g, [])

guiDraw :: Drawable t -> GUI t ()
guiDraw c = GUI $ \_ -> return ((), [c])

runGUI :: GUI t a -> Globals t -> IO (a, [Drawable t])
runGUI (GUI f) = f
