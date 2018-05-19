{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}
module GUI (Globals(..), KeyState(..), ButtonState(..), MouseButton, Event(..), Cmd(..), GUI(..), guiGlobals, guiCommand) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Resources
import Drawable
import Types
import Graphics.UI.GLFW (Key(..))

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

data Cmd t
    = LoadResource ResourceId
    | Debug String
    | Render (Drawable t)

newtype GUI t a = GUI (Globals t -> IO (a, [Cmd t])) deriving (Functor)

instance Applicative (GUI t) where
    pure x = GUI (const $ return (x, []))
    (<*>) (GUI f) (GUI a) = GUI $ \g -> do
        (f', cf) <- f g
        (a', ca) <- a g
        return (f' a', cf ++ ca)

instance Monad (GUI t) where
    (>>=) (GUI a) f = GUI $ \g -> do
        (a', ca) <- a g
        let (GUI b) = f a'
        (b', cb) <- b g
        return (b', ca ++ cb)

instance MonadFix (GUI t) where
    mfix f = GUI $ \g -> mdo
        let (GUI a) = f a'
        (a', ca) <- a g
        return (a', ca)

instance MonadIO (GUI t) where
    liftIO a = GUI $ \_ -> fmap (\x -> (x, [])) a

guiGlobals :: GUI t (Globals t)
guiGlobals = GUI $ \g -> return (g, [])

guiCommand :: Cmd t -> GUI t ()
guiCommand c = GUI $ \_ -> return ((), [c])
