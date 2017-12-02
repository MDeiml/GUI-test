{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module Renderer
    ( mainLoop
    , Renderer(..)
    , Globals(..)
    , App
    , Widget'
    , Cmd(..)
    ) where

import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)
import Drawable
import Input
import Layout
import Resources
import Types
import Widget

type App t = Widget' t () ()

type Widget' t i o = Widget (Globals t) (Cmd t) LayoutParam i o

data Cmd t
    = LoadResource ResourceId
    | Render (Drawable t)

loadResource' ::
       (Renderer r t) => r -> Resources t -> ResourceId -> IO (Resources t)
loadResource' re res i =
    if i `M.member` res
        then return res
        else do
            print i
            r <- loadResource re i
            return $ M.insert i r res

loadResources ::
       (Renderer r t) => r -> Resources t -> [ResourceId] -> IO (Resources t)
loadResources _ res [] = return res
loadResources r res (i:is) = do
    res' <- loadResource' r res i
    loadResources r res' is

mainLoop :: (Renderer r t) => r -> Int -> App t -> IO ()
mainLoop r fps w0 = do
    next <- getPOSIXTime
    mainLoop' w0 M.empty next
  where
    mainLoop' w res next = do
        now <- getPOSIXTime
        if now >= next
            then do
                (width, height) <- getSize r
                events <- pollEvents r
                clear r
                rec let ~(_, _, ds, w') =
                            runWidget
                                w
                                Globals
                                { gEvents = events
                                , gTime = round $ next * 1000
                                , gResources = res'
                                }
                                [Bounds 0 0 width height]
                                ()
                    res' <- loadResources r res $ mapMaybe (\case
                          LoadResource i -> Just i
                          _ -> Nothing) ds
                mapM_ (render r) $ mapMaybe (\case
                          Render d -> Just d
                          _ -> Nothing) $ reverse ds
                swapBuffers r
                c <- closing r
                unless c $ mainLoop' w' res' (next + 1 / fromIntegral fps)
            else do
                threadDelay 1000
                mainLoop' w res next

class Renderer r t | r -> t where
    create :: String -> (Int, Int) -> IO r
    render :: r -> Drawable t -> IO ()
    clear :: r -> IO ()
    swapBuffers :: r -> IO ()
    getSize :: r -> IO (Float, Float)
    closing :: r -> IO Bool
    pollEvents :: r -> IO [Event]
    loadResource :: r -> ResourceId -> IO (Resource t)
