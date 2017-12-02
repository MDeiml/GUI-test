module Renderer
    ( mainLoop
    , Renderer(..)
    , Globals(..)
    , App
    , Widget'
    ) where

import Control.Concurrent
import Control.Monad (unless, when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Drawable
import Input
import Layout
import Types
import Widget

type App = Widget Globals [Drawable] LayoutParam () ()

type Widget' i o = Widget Globals [Drawable] LayoutParam i o

mainLoop :: (Renderer r) => r -> Int -> App -> IO ()
mainLoop r fps w0 = do
    next <- getPOSIXTime
    mainLoop' w0 next
  where
    mainLoop' w next = do
        now <- getPOSIXTime
        if now >= next
            then do
                (width, height) <- getSize r
                events <- pollEvents r
                let (_, _, ds, w') =
                        runWidget
                            w
                            Globals
                            {gEvents = events, gTime = round $ next * 1000}
                            [Bounds 0 0 width height]
                            ()
                clear r
                mapM_ (render r) $ reverse $ concat ds
                swapBuffers r
                c <- closing r
                unless c $ mainLoop' w' (next + 1 / fromIntegral fps)
            else do
                threadDelay 1000
                mainLoop' w next

class Renderer r where
    create :: String -> (Int, Int) -> IO r
    render :: r -> Drawable -> IO ()
    clear :: r -> IO ()
    swapBuffers :: r -> IO ()
    getSize :: r -> IO (Float, Float)
    closing :: r -> IO Bool
    pollEvents :: r -> IO [Event]
