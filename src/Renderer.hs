module Renderer
  ( mainLoop
  , Renderer(..)
  , Key(..)
  , KeyState(..)
  , Event(..)
  ) where

import Control.Monad (unless)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Drawable
import Layout
import Types
import Widget

data Key
  = Key Char
  | MouseButton Int

data KeyState
  = KeyUp
  | KeyDown

data Event
  = Event Key
          KeyState
          (Float, Float)
  | Time Integer

mainLoop :: (Renderer r) => r -> Widget Event (LayoutItem p) () o -> IO ()
mainLoop r w = do
  (width, height) <- getSize r
  events <- pollEvents r
  time <- fmap (round . (* 1000)) getPOSIXTime
  let ws =
        map (\e x -> runWidget x e [Bounds 0 0 width height] ()) $
        (Time time) : events
      (w', d) =
        foldl
          (\(x, _d) f ->
             let (_o, d, x') = f x
             in (x', d))
          (w, undefined)
          ws
  clear r
  mapM_ (render r) $ concatMap layoutDrawables d
  swapBuffers r
  c <- closing r
  unless c $ mainLoop r w'

class Renderer r where
  create :: String -> (Int, Int) -> IO r
  render :: r -> Drawable -> IO ()
  clear :: r -> IO ()
  swapBuffers :: r -> IO ()
  getSize :: r -> IO (Float, Float)
  closing :: r -> IO Bool
  pollEvents :: r -> IO [Event]
