{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Renderer
    ( mainLoop
    , loadNinpatch
    , Renderer(..)
    , Globals(..)
    , App
    ) where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
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
import GUI
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS

type App t = Widget' t () ()

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
                let (GUI gui) = runWidget w [Bounds 0 0 width height] ()
                ~(~(_, ps, w'), ds) <- gui Globals
                                { gEvents = events
                                , gTime = round $ next * 1000
                                , gResources = res
                                }
                print $ any snd ps
                res' <- loadResources r res $ mapMaybe (\case
                      LoadResource i -> Just i
                      _ -> Nothing) ds
                mapM_ (render r) $ mapMaybe (\case
                          Render d -> Just d
                          _ -> Nothing) $ reverse ds
                mapM_ putStrLn $ mapMaybe (\case
                          Debug s -> Just s
                          _ -> Nothing) ds
                swapBuffers r
                c <- closing r
                unless c $ mainLoop' w' res' (next + 1 / fromIntegral fps)
            else do
                threadDelay 1000
                mainLoop' w res next

loadNinpatch :: Renderer r t => r -> FilePath -> MaybeT IO (NinePatch t)
loadNinpatch r fp = do
    file <- lift $ BS.readFile fp
    res <- MaybeT $ return $ decode file
    (tf, xs, ys, xe, ye) <- MaybeT $ return $ flip parseMaybe res $ \obj -> do
        texFile <- obj .: "texture"
        xs' <- obj .: "xs"
        ys' <- obj .: "ys"
        xe' <- obj .: "xe"
        ye' <- obj .: "ye"
        return (texFile, xs', ys', xe', ye')
    RSpr tex@(Sprite _ w h) <- lift $ loadResource r $ ResS tf
    let f = fromIntegral
    return $ NP tex (f xs / f w, f ys / f h, f xe / f w, f ye / f h)


class Renderer r t | r -> t where
    create :: String -> (Int, Int) -> IO r
    render :: r -> Drawable t -> IO ()
    clear :: r -> IO ()
    swapBuffers :: r -> IO ()
    getSize :: r -> IO (Float, Float)
    closing :: r -> IO Bool
    pollEvents :: r -> IO [Event]
    loadResource :: r -> ResourceId -> IO (Resource t)
