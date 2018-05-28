{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE RecursiveDo   #-}

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

import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Text              (Text)
import           Drawable
import           Resources
import           Types

data Cmd t
    = Render (Drawable t)
    | RunIO (IO ())
    | StartTextInput
    | StopTextInput

data Globals t = Globals
    { gEvents    :: [Event]
    , gTime      :: Integer
    , gResources :: forall a. ResourceId a -> IO (Either String (a t))
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
    , mCtrl  :: Bool
    , mAlt   :: Bool
    } deriving (Show, Eq)

data Event
    = KeyEvent Modifiers
               Key
               KeyState
    | TextEvent Text
    | MouseEvent MouseButton
                 ButtonState
                 Coords
    | MouseMoveEvent Coords
    deriving (Show)

newtype GUI t m a =
    GUI (Globals t -> m (a, [Cmd t]))
    deriving (Functor)

instance Monad m => Applicative (GUI t m) where
    pure x = GUI (const $ return (x, []))
    (<*>) (GUI f) (GUI a) =
        GUI $ \g -> do
            ~(f', cf) <- f g
            ~(a', ca) <- a g
            return (f' a', cf ++ ca)

instance Monad m => Monad (GUI t m) where
    (>>=) ~(GUI a) f =
        GUI $ \g -> do
            ~(a', ca) <- a g
            let (GUI b) = f a'
            ~(b', cb) <- b g
            return (b', ca ++ cb)

instance MonadFix m => MonadFix (GUI t m) where
    mfix f =
        GUI $ \g ->
            mdo let (GUI a) = f a'
                ~(a', ca) <- a g
                return (a', ca)

instance MonadTrans (GUI t) where
    lift m = GUI $ \_ -> fmap (\x -> (x, [])) m

guiEvents :: Monad m => GUI t m [Event]
guiEvents = GUI $ \g -> return (gEvents g, [])

guiResource :: MonadIO m => ResourceId a -> GUI t m (Either String (a t))
guiResource i =
    GUI $ \g -> do
        res <- liftIO $ gResources g i
        return (res, [])

guiTime :: Monad m => GUI t m Integer
guiTime = GUI $ \g -> return (gTime g, [])

guiDraw :: Monad m => [Drawable t] -> GUI t m ()
guiDraw c = GUI $ \_ -> return ((), map Render c)

guiIO :: Monad m => IO () -> GUI t m ()
guiIO m = GUI $ \_ -> return ((), [RunIO m])

guiStartTextInput :: Monad m => GUI t m ()
guiStartTextInput = GUI $ \_ -> return ((), [StartTextInput])

guiStopTextInput :: Monad m => GUI t m ()
guiStopTextInput = GUI $ \_ -> return ((), [StopTextInput])

runGUI :: Monad m => GUI t m a -> Globals t -> m (a, [Cmd t])
runGUI (GUI f) = f
