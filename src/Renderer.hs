module Renderer
    ( mainLoop
    , Renderer(..)
    ) where

import Control.Monad (unless, when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Drawable
import Input
import Layout
import Types
import Widget

mainLoop ::
       (Renderer r) => r -> Widget Event [Drawable] LayoutParam () o -> IO ()
mainLoop r w = do
    (width, height) <- getSize r
    events <- pollEvents r
    time <- fmap (round . (* 1000)) getPOSIXTime
    let ws =
            map (\e x -> (snd $ runWidget x e) () [Bounds 0 0 width height]) $
            Time time : events
        (w', a) =
            foldl
                (\(x, _a) f ->
                     let (_o, a, x') = f x
                     in (x', a))
                (w, undefined)
                ws
    clear r
    mapM_ (render r) $ reverse $ concat a
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
